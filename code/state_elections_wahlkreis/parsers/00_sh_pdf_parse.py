#!/usr/bin/env python3
"""Stage-0 parser for SCHLESWIG-HOLSTEIN Landtagswahl 2005 results at the
WAHLKREIS level.

Source (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Schleswig-Holstein/
    SH_2005_Landtagswahl_Bericht_B_VII_2_5_05.pdf
  = Statistisches Amt fuer Hamburg und Schleswig-Holstein, "Landtagswahl in
    Schleswig-Holstein am 20. Februar 2005 - Endgueltiges Ergebnis" (B VII
    2-5/05), section 2.1 "Ergebnisse nach Landtagswahlkreisen - Anzahl"
    (pdf pages 12-15). No machine-readable Wahlkreis file exists for 2005
    (see parse_SH.R docstring); this is the only per-Wahlkreis source.
    RC4-encrypted (print/copy allowed, no owner password needed) - pdftotext
    reads it fine. 40 Wahlkreise (2005 boundaries; NOT the 35 used since
    2012 - see parse_SH.R, which does not force name/count identity across
    years).

Layout: TWO PAIRED TABLES, each split across 2 page-halves (WK 1-20 / WK
21-40 + a "Schleswig-Holstein" statewide total row).
  Table A ("Wahlberechtigte / Waehlerinnen-Waehler", pdf pp. 12+14): one row
    per Wahlkreis with Nr, Name, and 8 numeric fields: Wahlberechtigte
    insgesamt / ohne Wahlschein / mit Wahlschein / Sec.17.3 LWO, then
    Waehler insgesamt / Urnenwahl ohne Wahlschein / Urnenwahl mit Wahlschein
    / Briefwahl. eligible_voters = Wahlberechtigte insgesamt; number_voters
    = Waehler insgesamt (same for both Stimmen - one shared ballot event).
  Table B ("Stimmen ... entfallen auf", pdf pp. 13+15): one "E" (Erststimme)
    and one "Z" (Zweitstimme) row per Wahlkreis: ungueltig, gueltig, SPD,
    CDU, FDP, GRUENE, SSW, NPD, FAMILIE, Andere. The WK-row "E" line ends
    with the Wahlkreis-Nr; its "Z" line (and the terminal Land-total E/Z
    pair) do not repeat it, so Wahlkreis identity is carried by sequence
    (each Z immediately follows its own E - verified by validation (1)).
    "-" = no candidate/list entry for that party in that Wahlkreis/Stimme
    (structural, NOT printed 0); "Andere" is the source's own residual
    (composition differs between Erst- and Zweitstimme, source Sec.2.3) and
    is emitted verbatim as a party (precedent: SL/HE emit "Sonstige").
  Long Wahlkreis names wrap onto a continuation line with no further data
  (e.g. Nr 27 "Pinneberg-" / "Elbmarschen" -> "Pinneberg-Elbmarschen").

PARSING METHOD: pdftotext -layout, then a token regex
  \\d{1,3}(?:\\s\\d{3})*|-  (thousands-space-grouped integer, or the dash)
applied via finditer/findall to each data line's numeric remainder. This is
safer than a bare >=2-space column split: the printed "Schleswig-Holstein"
Land row in Table A is wide enough (long name + 7-digit totals) that
pdftotext compresses TWO of its column gaps to a single space, so a
>=2-space split silently merges "2 186 620" and "2 024 070" into one token.
The token regex avoids this because it only ever extends a number match
across a single-space gap when the following digit run merges into an
existing thousands-group of variable width; a genuine second Land-row
number is only mis-mergeable if its leading digit-group is itself exactly
3 digits (a >=6-digit second number) immediately after the first with a
single-space gap - which does not occur in this document (checked: every
adjacent single-space collision here pairs two >=7-digit millions-range
totals, both with a 1-digit leading group, so the regex cannot extend past
the first number's true end). Every row's token COUNT is hard-checked
(8 for Table A, 11 for a Table-B Wahlkreis row, 10 for a Table-B Land row),
so any unexpected merge elsewhere aborts the run instead of writing bad
data.

VALIDATION (all hard; nothing is written if any check fails):
  (1) exactly 40 Wahlkreise + 1 Land row present, in BOTH Table A and
      Table B, for BOTH Stimmen (Erststimme/Zweitstimme)
  (2a) per (Wahlkreis, Stimme): sum of the 8 party counts (incl. Andere)
       == gueltige Stimmen
  (2b) per (Wahlkreis, Stimme): Table A Waehler insgesamt == Table B
       gueltig + ungueltig, checked independently for Erst- and
       Zweitstimme
  (3) per party, per Stimme: sum over the 40 Wahlkreise == the printed
      Land-row count for that party (exact integer match)
  (4) pinned official statewide Zweitstimme shares (Landeswahlleiterin SH,
      endgueltiges Ergebnis 2005), +-0.1pp: CDU 40.2, SPD 38.7, FDP 6.6,
      GRUENE 6.2, SSW 3.6
  (bonus, not required but cheap) Table A's own Land row: Waehler ==
      gueltig+ungueltig (Land, both Stimmen) and sum of the 40 Wahlkreise'
      eligible_voters/number_voters == the Land row's printed totals.

Output: data/state_elections/processed/wahlkreis/sh_pdf/SH_2005_pdf_long.csv
        (13 cols: state_abbr,state,election_year,election_date,wkr_nr,
        wkr_name,stimme,eligible_voters,number_voters,valid_votes,
        invalid_votes,party_raw,votes - votes is "" for a structural "-")
        Read by parsers/parse_SH.R, which appends it to the existing
        2000/2009/2017/2022 Wahlkreis series.
Run:    python3 code/state_elections_wahlkreis/parsers/00_sh_pdf_parse.py
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
    ROOT, "data", "state_elections", "raw", "Landtagswahlen_Wahlkreis",
    "Schleswig-Holstein", "SH_2005_Landtagswahl_Bericht_B_VII_2_5_05.pdf")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "sh_pdf")
OUT = os.path.join(OUT_DIR, "SH_2005_pdf_long.csv")

STATE_ABBR = "SH"
STATE_NAME = "Schleswig-Holstein"
ELECTION_YEAR = 2005
ELECTION_DATE = "2005-02-20"
N_WK = 40

STIMME_MAP = {"E": "erststimme", "Z": "zweitstimme"}
PARTIES = ["SPD", "CDU", "FDP", "GRÜNE", "SSW", "NPD", "FAMILIE", "Andere"]

# pinned official statewide Zweitstimme shares (+-0.1pp)
OFFICIAL_ZWEITSTIMME = {"CDU": 40.2, "SPD": 38.7, "FDP": 6.6, "GRÜNE": 6.2, "SSW": 3.6}

NUM_TOKEN = re.compile(r"\d{1,3}(?:\s\d{3})*|–|-")
WK_LINE = re.compile(r"^(\d{2})\s+(\S.*?)\s{2,}(.*)$")
LAND_A_LINE = re.compile(r"^\s+Schleswig-Holstein\s+(.*)$")
EZ_LINE = re.compile(r"^\s*([EZ])\s+(.*)$")


def num_or_none(tok):
    tok = tok.strip()
    if tok in ("–", "-"):
        return None
    return int(tok.replace(" ", ""))


def is_table_a_page(p):
    # "Landtagswahlkreis" (not "KREISFREIE STADT"/"Gemeinde") restricts this
    # to section 2's Wahlkreis-level tables, excluding the later Kreis-/
    # Gemeinde-level tables (sections 4/5/6) that share the same header
    # words. "100,0" (the Wahlberechtigte=100% column, present on every
    # Prozent-table row) distinguishes the Anzahl pages from the percent
    # pages, which carry none of the "Anzahl"/"in Prozent" tags on their
    # OWN page (those tags print on the paired Table-B page instead).
    return ("Landtagswahlkreis" in p and "Wählerinnen/Wähler" in p
            and "Wahlberechtigte" in p and "Urnenwahl" in p and "100,0" not in p)


def is_table_b_page(p):
    return ("entfallen auf" in p and "Anzahl" in p and "in Prozent" not in p
            and "WK" in p)


def parse_table_a(pages):
    """Returns (dict nr -> {name, eligible_voters, number_voters}, land dict)."""
    wk = {}
    land = None
    for page in pages:
        lines = page.split("\n")
        i = 0
        while i < len(lines):
            line = lines[i]
            m = WK_LINE.match(line)
            if (m and 1 <= int(m.group(1)) <= N_WK
                    and re.fullmatch(r"[\d\s–\-]+", m.group(3))):
                nr = int(m.group(1))
                name = m.group(2).strip()
                rest = m.group(3)
                # a pure-text continuation line (no digits) extends the name
                # (e.g. Nr 27 "Pinneberg-" / "Elbmarschen")
                if i + 1 < len(lines):
                    nxt = lines[i + 1]
                    if (nxt.strip() and not re.match(r"^\d{2}\s", nxt)
                            and not LAND_A_LINE.match(nxt)
                            and not re.search(r"\d", nxt)):
                        name = name + nxt.strip()
                        i += 1
                toks = NUM_TOKEN.findall(rest)
                if len(toks) != 8:
                    sys.exit(f"Table A row Nr {nr:02d}: expected 8 numeric "
                             f"fields, got {len(toks)}: {toks}\n  line={line!r}")
                vals = [num_or_none(t) for t in toks]
                wk[nr] = {"name": name, "eligible_voters": vals[0],
                          "eligible_ohne_ws": vals[1], "eligible_mit_ws": vals[2],
                          "p173": vals[3], "number_voters": vals[4],
                          "urne_ohne_ws": vals[5], "urne_mit_ws": vals[6],
                          "briefwahl": vals[7]}
                i += 1
                continue
            m2 = LAND_A_LINE.match(line)
            if m2:
                toks = NUM_TOKEN.findall(m2.group(1))
                if len(toks) != 8:
                    sys.exit(f"Table A Land row: expected 8 numeric fields, "
                             f"got {len(toks)}: {toks}\n  line={line!r}")
                vals = [num_or_none(t) for t in toks]
                land = {"eligible_voters": vals[0], "eligible_ohne_ws": vals[1],
                        "eligible_mit_ws": vals[2], "p173": vals[3],
                        "number_voters": vals[4], "urne_ohne_ws": vals[5],
                        "urne_mit_ws": vals[6], "briefwahl": vals[7]}
            i += 1
    return wk, land


def parse_table_b(pages):
    """Returns dict (key, stimme_lbl) -> {invalid_votes, valid_votes, <party>: votes,...}
    where key is an int Wahlkreis-Nr or the string "LAND"."""
    out = {}
    current_key = None
    for page in pages:
        for line in page.split("\n"):
            m = EZ_LINE.match(line)
            if not m:
                continue
            marker, rest = m.group(1), m.group(2)
            toks = NUM_TOKEN.findall(rest)
            if marker == "E":
                if len(toks) == 11:
                    *val_toks, wk_tok = toks
                    current_key = int(wk_tok)
                elif len(toks) == 10:
                    val_toks = toks
                    current_key = "LAND"
                else:
                    sys.exit(f"Table B 'E' row: expected 10 or 11 numeric "
                             f"fields, got {len(toks)}: {toks}\n  line={line!r}")
            else:  # marker == "Z"
                if len(toks) != 10:
                    sys.exit(f"Table B 'Z' row: expected 10 numeric fields, "
                             f"got {len(toks)}: {toks}\n  line={line!r}")
                if current_key is None:
                    sys.exit(f"Table B 'Z' row with no preceding 'E' row: "
                             f"{line!r}")
                val_toks = toks
            vals = [num_or_none(t) for t in val_toks]
            rec = {"invalid_votes": vals[0], "valid_votes": vals[1]}
            for j, p in enumerate(PARTIES):
                rec[p] = vals[2 + j]
            out[(current_key, STIMME_MAP[marker])] = rec
    return out


def main():
    txt = subprocess.run(["pdftotext", "-layout", PDF, "-"],
                         check=True, capture_output=True).stdout.decode("utf-8")
    pages = txt.split("\f")

    a_pages = [p for p in pages if is_table_a_page(p)]
    b_pages = [p for p in pages if is_table_b_page(p)]

    fails = []

    def req(cond, label):
        print(("  [ok]  " if cond else "  [FAIL]") + " " + label)
        if not cond:
            fails.append(label)

    print("VALIDATION")
    req(len(a_pages) == 2, f"found exactly 2 Table-A (Anzahl) pages (got {len(a_pages)})")
    req(len(b_pages) == 2, f"found exactly 2 Table-B (Anzahl) pages (got {len(b_pages)})")
    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    wk_a, land_a = parse_table_a(a_pages)
    tb = parse_table_b(b_pages)

    # ---- (1) completeness: 40 Wahlkreise + Land, both tables, both Stimmen
    req(len(wk_a) == N_WK, f"Table A: exactly {N_WK} Wahlkreise present (got {len(wk_a)})")
    req(land_a is not None, "Table A: Land row present")
    req(set(wk_a) == set(range(1, N_WK + 1)), "Table A: Wahlkreis-Nr are exactly 1..40")

    wk_nrs = set(range(1, N_WK + 1))
    stimmen = ("erststimme", "zweitstimme")
    missing_b = [(k, s) for k in list(wk_nrs) + ["LAND"] for s in stimmen
                 if (k, s) not in tb]
    req(not missing_b, f"Table B: all {N_WK} Wahlkreise + Land present for both Stimmen "
                        f"(missing {len(missing_b)})")
    for m in missing_b[:10]:
        print("           missing:", m)
    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # ---- (2a) per (WK, stimme): sum of 8 parties == gueltige Stimmen
    bad = []
    for nr in wk_nrs:
        for s in stimmen:
            r = tb[(nr, s)]
            party_sum = sum((r[p] or 0) for p in PARTIES)
            if party_sum != r["valid_votes"]:
                bad.append((nr, s, party_sum, r["valid_votes"]))
    req(not bad, "(2a) every (Wahlkreis, Stimme): sum of 8 party counts == gültige Stimmen")
    for b in bad[:8]:
        print("           ", b)

    # ---- (2b) Table A Wähler == Table B gültig+ungültig, both Stimmen independently
    bad = []
    for nr in wk_nrs:
        wa = wk_a[nr]["number_voters"]
        for s in stimmen:
            r = tb[(nr, s)]
            tot = r["valid_votes"] + r["invalid_votes"]
            if wa != tot:
                bad.append((nr, s, wa, tot))
    req(not bad, "(2b) every Wahlkreis: Table A Wähler insgesamt == "
                 "Table B gültig+ungültig, independently for Erst- and Zweitstimme")
    for b in bad[:8]:
        print("           ", b)

    # ---- (3) sum over 40 Wahlkreise per party == printed Land row, exactly
    bad = []
    for s in stimmen:
        land_row = tb[("LAND", s)]
        for p in PARTIES:
            summed = sum((tb[(nr, s)][p] or 0) for nr in wk_nrs)
            if summed != (land_row[p] or 0):
                bad.append((s, p, summed, land_row[p]))
        # also check invalid/valid totals for good measure
        summed_valid = sum(tb[(nr, s)]["valid_votes"] for nr in wk_nrs)
        summed_invalid = sum(tb[(nr, s)]["invalid_votes"] for nr in wk_nrs)
        if summed_valid != land_row["valid_votes"]:
            bad.append((s, "valid_votes", summed_valid, land_row["valid_votes"]))
        if summed_invalid != land_row["invalid_votes"]:
            bad.append((s, "invalid_votes", summed_invalid, land_row["invalid_votes"]))
    req(not bad, "(3) sum over 40 Wahlkreise per party (+valid/invalid) == "
                 "printed Land row, exactly, both Stimmen")
    for b in bad[:12]:
        print("           ", b)

    # ---- (4) pinned official statewide Zweitstimme shares
    bad = []
    land_z = tb[("LAND", "zweitstimme")]
    g = land_z["valid_votes"]
    for p, share in OFFICIAL_ZWEITSTIMME.items():
        got = 100.0 * (land_z[p] or 0) / g
        if abs(got - share) > 0.1:
            bad.append((p, round(got, 2), share))
    req(not bad, "(4) pinned official statewide Zweitstimme shares match (+-0.1pp): "
                 + ", ".join(f"{p} {v}" for p, v in OFFICIAL_ZWEITSTIMME.items()))
    for b in bad[:8]:
        print("           ", b)

    # ---- bonus: Table A's own Land row internal + cross consistency
    bad = []
    for s in stimmen:
        land_row = tb[("LAND", s)]
        tot = land_row["valid_votes"] + land_row["invalid_votes"]
        if land_a["number_voters"] != tot:
            bad.append(("land_waehler_vs_gueltig_ungueltig", s,
                        land_a["number_voters"], tot))
    summed_elig = sum(wk_a[nr]["eligible_voters"] for nr in wk_nrs)
    summed_vot = sum(wk_a[nr]["number_voters"] for nr in wk_nrs)
    if summed_elig != land_a["eligible_voters"]:
        bad.append(("sum_eligible_voters_vs_land", summed_elig, land_a["eligible_voters"]))
    if summed_vot != land_a["number_voters"]:
        bad.append(("sum_number_voters_vs_land", summed_vot, land_a["number_voters"]))
    req(not bad, "(bonus) Table A Land row: Wähler==gültig+ungültig (both Stimmen) "
                 "and sum of 40 Wahlkreise == printed Land totals")
    for b in bad[:8]:
        print("           ", b)

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- emit ------------------------------------------------------------
    rows = []
    for nr in sorted(wk_nrs):
        wkr_nr = str(nr)          # unpadded, matching the 2009 (also 40-WK) convention
        wkr_name = wk_a[nr]["name"]
        for s in stimmen:
            r = tb[(nr, s)]
            for p in PARTIES:
                v = r[p]
                rows.append({
                    "state_abbr": STATE_ABBR, "state": STATE_NAME,
                    "election_year": ELECTION_YEAR, "election_date": ELECTION_DATE,
                    "wkr_nr": wkr_nr, "wkr_name": wkr_name, "stimme": s,
                    "eligible_voters": wk_a[nr]["eligible_voters"],
                    "number_voters": wk_a[nr]["number_voters"],
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
    print(f"  {N_WK} Wahlkreise x {len(stimmen)} Stimmen x {len(PARTIES)} Parteien"
          f" = {N_WK * len(stimmen) * len(PARTIES)} rows")
    print("  distinct party_raw:", PARTIES)


if __name__ == "__main__":
    main()
