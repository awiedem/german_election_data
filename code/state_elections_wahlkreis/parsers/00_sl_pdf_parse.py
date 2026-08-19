#!/usr/bin/env python3
"""Stage-0 parser for SAARLAND Landtagswahl results at WAHLKREIS level, 1980-2017.

Source (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Saarland/
    SL_1980-2022_Landtagswahl_Wahlkreis.pdf
  = Statistisches Amt Saarland, "Landtagswahlen 1980 bis 2022 nach
    Landtagswahlkreisen" (long-series table, digital text layer, 2 pages).

Why a PDF parser: the only machine-readable per-Wahlkreis source is the 2022
votemanager KERG CSV (parsed by parse_SL.R).  Every earlier election exists at
Wahlkreis level only in scans - EXCEPT this one digital long-series table,
which carries full absolute counts for all three grosse Wahlkreise for every
election 1980-2022.  No OCR is required.

Layout: one logical table split over two page-halves.
  Page 1  Wahlberechtigte, Waehler (abs/%), Ungueltige (abs/%), Gueltige
          (abs/%), then CDU / SPD / GRUENE / FDP / DIE LINKE (abs/%).
  Page 2  AfD / PIRATEN / DKP / NPD / REP / FAMILIE / Waehlergruppen /
          Sonstige (abs/%), same row order.
  Rows: 3 Wahlkreise (Saarbruecken, Saarlouis, Neunkirchen) x 10 election
  years each (2022 down to 1980).  "-" = party not on the ballot -> NA.
  "Sonstige" is the source's own residual and is emitted verbatim as a party
  (precedent: HE 2013), so stage 1 folds it into a share column.

PARSING METHOD: pdftotext -layout.  Thousands separator is a space, so digit
groups are re-merged (a 1-3 digit token followed by an exactly-3-digit token).
The merge is safe here because every count column is followed by a percent
column (comma decimal), which interrupts any cross-column merge; correctness
is nevertheless PROVEN per cell against the printed percent columns and per
row against Gueltige Stimmen (see VALIDATION).

VALIDATION (all hard; nothing is written if any check fails):
  (1) exactly 3 Wahlkreise x 10 years on both pages
  (2) per row: Waehler == Gueltige + Ungueltige
  (3) per row: sum of all 13 party counts == Gueltige Stimmen
  (4) per cell: abs/base reproduces the printed % (base: Wahlberechtigte for
      Waehler, Waehler for Un-/Gueltige, Gueltige for parties) within 0.055pp
  (5) statewide CDU+SPD shares match the official results (pinned, +-0.1pp)
  (6) independent source: the table's own 2022 row must reproduce the KERG-
      derived intermediate (parse_SL.R output) exactly, including the identity
      Sonstige == sum of the KERG parties outside the named columns

Output: data/state_elections/processed/wahlkreis/sl_pdf/SL_1980_2017_pdf_long.csv
        (read by parsers/parse_SL.R, which appends the 2022 KERG results)
Run:    python3 code/state_elections_wahlkreis/parsers/00_sl_pdf_parse.py
Requires: poppler (pdftotext) on PATH.
"""

import csv
import os
import re
import subprocess
import sys

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
PDF = os.path.join(
    ROOT, "data", "state_elections", "raw", "Landtagswahlen_Wahlkreis", "Saarland",
    "SL_1980-2022_Landtagswahl_Wahlkreis.pdf")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "sl_pdf")
OUT = os.path.join(OUT_DIR, "SL_1980_2017_pdf_long.csv")
KERG_CSV = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis",
                        "SL_ltw_wkr_long.csv")

STATE_ABBR = "SL"
STATE_NAME = "Saarland"
STIMME = "einzelstimme"

ELECTION_DATES = {
    1980: "1980-04-27", 1985: "1985-03-10", 1990: "1990-01-28",
    1994: "1994-10-16", 1999: "1999-09-05", 2004: "2004-09-05",
    2009: "2009-08-30", 2012: "2012-03-25", 2017: "2017-03-26",
    2022: "2022-03-27",  # validation only; emitted by parse_SL.R from KERG
}
YEARS = sorted(ELECTION_DATES)
EMIT_YEARS = [y for y in YEARS if y != 2022]

# wkr_nr / wkr_name exactly as the 2022 KERG intermediate uses them
WKR = {"Saarbrücken": ("001", "Wahlkreis Saarbrücken"),
       "Saarlouis":   ("002", "Wahlkreis Saarlouis"),
       "Neunkirchen": ("003", "Wahlkreis Neunkirchen")}

P1_PARTIES = ["CDU", "SPD", "GRÜNE", "FDP", "DIE LINKE"]
P2_PARTIES = ["AfD", "PIRATEN", "DKP", "NPD", "REP", "FAMILIE",
              "Wählergruppen", "Sonstige"]
ALL_PARTIES = P1_PARTIES + P2_PARTIES

# series column -> party_raw label in the 2022 KERG intermediate
KERG_MAP = {
    "CDU": "Christlich Demokratische Union Deutschlands",
    "SPD": "Sozialdemokratische Partei Deutschlands",
    "GRÜNE": "BÜNDNIS 90/DIE GRÜNEN",
    "FDP": "Freie Demokratische Partei",
    "DIE LINKE": "DIE LINKE",
    "AfD": "Alternative für Deutschland",
    "PIRATEN": "Piratenpartei Deutschland",
    "FAMILIE": "Familien-Partei Deutschlands",
    "Wählergruppen": "bunt.saar  sozial-ökologische liste",
}

# official statewide shares (Landeswahlleiter / Statistisches Amt), CDU + SPD
OFFICIAL = {
    1980: {"CDU": 44.0, "SPD": 45.4}, 1985: {"CDU": 37.3, "SPD": 49.2},
    1990: {"CDU": 33.4, "SPD": 54.4}, 1994: {"CDU": 38.6, "SPD": 49.4},
    1999: {"CDU": 45.5, "SPD": 44.4}, 2004: {"CDU": 47.5, "SPD": 30.8},
    2009: {"CDU": 34.5, "SPD": 24.5}, 2012: {"CDU": 35.2, "SPD": 30.6},
    2017: {"CDU": 40.7, "SPD": 29.6}, 2022: {"CDU": 28.5, "SPD": 43.5},
}

WK_RE = re.compile(r"^\s{1,6}(\S.*?)\s{2,}(\d{4})\s+(.*)$")
CONT_RE = re.compile(r"^\s+(\d{4})\s+(.*)$")


def clean_num(tok):
    if tok == "-":
        return None
    return int(tok.replace(" ", ""))


def clean_pct(tok):
    if tok == "-":
        return None
    return float(tok.replace(",", "."))


def tokenize(rest):
    """Split a row remainder; re-merge space-separated thousands groups.

    Merge is deliberately NON-greedy (one 1-3 digit token + exactly one 3-digit
    token): every count in this table is <= 6 digits, and the only place two
    counts are adjacent without an intervening percent column (Wahlberechtigte
    -> Waehler) is two 2-group numbers, which the pairwise merge resolves
    correctly.  A greedy merge would fuse across that column boundary.
    Correctness is proven downstream by checks (2)-(4)."""
    toks = rest.split()
    merged, i = [], 0
    while i < len(toks):
        t = toks[i]
        if (re.fullmatch(r"\d{1,3}", t) and i + 1 < len(toks)
                and re.fullmatch(r"\d{3}", toks[i + 1])):
            merged.append(t + toks[i + 1])
            i += 2
        else:
            merged.append(t)
            i += 1
    return merged


def parse_block(block):
    rows, current_wk = [], None
    for line in block.splitlines():
        if not line.strip():
            continue
        m = WK_RE.match(line)
        if m and m.group(1).strip() in WKR:
            current_wk = m.group(1).strip()
            rows.append((current_wk, int(m.group(2)), m.group(3)))
            continue
        m2 = CONT_RE.match(line)
        if m2 and current_wk:
            rows.append((current_wk, int(m2.group(1)), m2.group(2)))
    return rows


def main():
    txt = subprocess.run(["pdftotext", "-layout", PDF, "-"],
                         check=True, capture_output=True).stdout.decode("utf-8")
    pages = txt.split("\f")
    if len(pages) < 2:
        sys.exit("expected a 2-page PDF")

    t1 = {}  # (wk, year) -> dict
    for wk, year, rest in parse_block(pages[0]):
        toks = tokenize(rest)
        if len(toks) != 7 + 2 * len(P1_PARTIES):
            sys.exit(f"page-1 row {wk} {year}: {len(toks)} tokens: {toks}")
        rec = {"eligible_voters": clean_num(toks[0]),
               "number_voters": clean_num(toks[1]), "voters_pct": clean_pct(toks[2]),
               "invalid_votes": clean_num(toks[3]), "invalid_pct": clean_pct(toks[4]),
               "valid_votes": clean_num(toks[5]), "valid_pct": clean_pct(toks[6])}
        for j, p in enumerate(P1_PARTIES):
            rec[p] = clean_num(toks[7 + 2 * j])
            rec[p + "_pct"] = clean_pct(toks[8 + 2 * j])
        t1[(wk, year)] = rec

    t2 = {}
    for wk, year, rest in parse_block(pages[1]):
        toks = tokenize(rest)
        if len(toks) != 2 * len(P2_PARTIES):
            sys.exit(f"page-2 row {wk} {year}: {len(toks)} tokens: {toks}")
        rec = {}
        for j, p in enumerate(P2_PARTIES):
            rec[p] = clean_num(toks[2 * j])
            rec[p + "_pct"] = clean_pct(toks[2 * j + 1])
        t2[(wk, year)] = rec

    fails = []

    def req(cond, label):
        print(("  [ok]  " if cond else "  [FAIL]") + " " + label)
        if not cond:
            fails.append(label)

    print("VALIDATION")
    # (1) completeness
    want = {(wk, y) for wk in WKR for y in YEARS}
    req(set(t1) == want, f"page 1: all {len(want)} (Wahlkreis x year) rows present")
    req(set(t2) == want, f"page 2: all {len(want)} (Wahlkreis x year) rows present")
    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # (2) Waehler == Gueltige + Ungueltige
    bad = [k for k in want
           if t1[k]["number_voters"] != t1[k]["valid_votes"] + t1[k]["invalid_votes"]]
    req(not bad, "every row: Waehler == Gueltige + Ungueltige")
    for k in bad[:5]:
        print("           ", k, t1[k]["number_voters"], "vs",
              t1[k]["valid_votes"], "+", t1[k]["invalid_votes"])

    # (3) party sum == Gueltige
    bad = []
    for k in want:
        s = sum((t1[k].get(p) or 0) for p in P1_PARTIES) + \
            sum((t2[k].get(p) or 0) for p in P2_PARTIES)
        if s != t1[k]["valid_votes"]:
            bad.append((k, s, t1[k]["valid_votes"]))
    req(not bad, "every row: sum of 13 party counts == Gueltige Stimmen")
    for b in bad[:5]:
        print("           ", b)

    # (4) printed percent columns reproduce every count
    bad = []
    for k in want:
        r = {**t1[k], **t2[k]}
        checks = [("number_voters", "voters_pct", r["eligible_voters"]),
                  ("invalid_votes", "invalid_pct", r["number_voters"]),
                  ("valid_votes", "valid_pct", r["number_voters"])]
        checks += [(p, p + "_pct", r["valid_votes"]) for p in ALL_PARTIES]
        for cnt, pc, base in checks:
            if r.get(cnt) is None or r.get(pc) is None or not base:
                continue
            if abs(100.0 * r[cnt] / base - r[pc]) > 0.055:
                bad.append((k, cnt, r[cnt], r[pc]))
    req(not bad, "every printed % reproduces its count (rounding tolerance 0.055pp)")
    for b in bad[:8]:
        print("           ", b)

    # (5) pinned official statewide shares
    bad = []
    for y in YEARS:
        g = sum(t1[(wk, y)]["valid_votes"] for wk in WKR)
        for p, share in OFFICIAL[y].items():
            v = sum((t1[(wk, y)].get(p) or t2[(wk, y)].get(p) or 0) for wk in WKR)
            got = 100.0 * v / g
            if abs(got - share) > 0.1:
                bad.append((y, p, round(got, 2), share))
    req(not bad, "statewide CDU/SPD shares match the official results (+-0.1pp)")
    for b in bad[:8]:
        print("           ", b)

    # (6) independent source: 2022 row vs the KERG intermediate
    if not os.path.exists(KERG_CSV):
        print("  [warn] " + os.path.relpath(KERG_CSV, ROOT) +
              " missing - 2022 cross-check skipped (run parse_SL.R first)")
    else:
        kerg = {}   # (wkr_nr, party_raw) -> votes; plus turnout per wkr_nr
        turn = {}
        with open(KERG_CSV, encoding="utf-8-sig") as fh:
            for row in csv.DictReader(fh):
                v = row["votes"]
                kerg[(row["wkr_nr"], row["party_raw"])] = None if v == "" else int(v)
                turn[row["wkr_nr"]] = (int(row["eligible_voters"]),
                                       int(row["number_voters"]),
                                       int(row["valid_votes"]),
                                       int(row["invalid_votes"]))
        bad = []
        for wk, (nr, _) in WKR.items():
            r = {**t1[(wk, 2022)], **t2[(wk, 2022)]}
            if turn[nr] != (r["eligible_voters"], r["number_voters"],
                            r["valid_votes"], r["invalid_votes"]):
                bad.append((wk, "turnout", turn[nr]))
            named = 0
            for col, kp in KERG_MAP.items():
                mine = r.get(col)
                theirs = kerg.get((nr, kp))
                if (mine or 0) != (theirs or 0):
                    bad.append((wk, col, mine, theirs))
                named += theirs or 0
            rest = sum(v or 0 for (n, p), v in kerg.items()
                       if n == nr and p not in KERG_MAP.values())
            if (r.get("Sonstige") or 0) != rest:
                bad.append((wk, "Sonstige", r.get("Sonstige"), rest))
        req(not bad, "2022 row reproduces the KERG intermediate exactly "
                     "(all named parties, turnout, and Sonstige == KERG residual)")
        for b in bad[:8]:
            print("           ", b)

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- emit ----------------------------------------------------------------
    rows = []
    for y in EMIT_YEARS:
        # a party is on the ballot this year if any Wahlkreis reports a count
        on_ballot = [p for p in ALL_PARTIES
                     if any((t1[(wk, y)].get(p) if p in P1_PARTIES
                             else t2[(wk, y)].get(p)) is not None for wk in WKR)]
        for wk, (nr, name) in sorted(WKR.items(), key=lambda kv: kv[1][0]):
            r = {**t1[(wk, y)], **t2[(wk, y)]}
            for p in on_ballot:
                v = r.get(p)
                rows.append({
                    "state_abbr": STATE_ABBR, "state": STATE_NAME,
                    "election_year": y, "election_date": ELECTION_DATES[y],
                    "wkr_nr": nr, "wkr_name": name, "stimme": STIMME,
                    "eligible_voters": r["eligible_voters"],
                    "number_voters": r["number_voters"],
                    "valid_votes": r["valid_votes"],
                    "invalid_votes": r["invalid_votes"],
                    "party_raw": p, "votes": "" if v is None else v,
                })
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)
    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for y in EMIT_YEARS:
        n = sum(1 for r in rows if r["election_year"] == y)
        print(f"  {y}: {n} rows ({n // 3} parties x 3 Wahlkreise)")


if __name__ == "__main__":
    main()
