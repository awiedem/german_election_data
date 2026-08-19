#!/usr/bin/env python3
"""Stage-0 parser for BAYERN Landtagswahl results at STIMMKREIS level, 2008 + 2013.

Source (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Bayern/
    BY_2008_Landtagswahl_StatBericht_BVII2-4.pdf  (234 pp)
    BY_2013_Landtagswahl_StatBericht_BVII2-4.pdf  (236 pp)
  = Bayerisches Landesamt fuer Statistik, Statistischer Bericht B VII 2-4,
    "Landtagswahl in Bayern 2008/2013 - Endgueltiges Ergebnis".

Why a PDF parser: BY publishes machine-readable Stimmkreis-level files only for
2018 and 2023 (parsed by parse_BY.R from the votemanager CSVs).  2008 and 2013
exist only as these two Statistische Berichte; both carry a genuine TEXT LAYER
(no OCR needed), one Stimmkreis per page:
  2013: pp. 15-104 (90 Stimmkreise), state fixture "1. Landes- und
        Wahlkreisergebnisse / Bayern" on p. 7 (IDENTICAL table layout to a
        Stimmkreis page, so the same parser handles it).
  2008: pp. 18-108 (91 Stimmkreise), state fixture "A. Ergebnis der Wahl..." on
        p. 5, which reports only Gesamtstimmen (= Erst + Zweit combined) with
        Sitze, NOT separate Erst-/Zweitstimmen columns (see below).  The
        Bezirk-level tables on pp. 7-17 are IMAGE-ONLY (just table-border
        glyphs, no text) and are not used anywhere.

STIMMKREIS PAGE LAYOUT (both years, identical): a header block (Stimmberechtigte,
Wähler/Wahlbeteiligung), then one table with two Erststimmen/Zweitstimmen columns,
one row per Wahlkreisvorschlag (party) in a FIXED order, each row holding 5 Erst
tokens (Anzahl, %-this-year, %-prior-year, Delta-Anzahl, Delta-%) then 5 Zweit
tokens, i.e. 10 whitespace tokens total; tokens[0] = Erststimmen Anzahl,
tokens[5] = Zweitstimmen Anzahl.  Below the party rows: "gültig"/"ungültig"/
"abgegeben", in the SAME 10-token layout - i.e. valid/invalid votes ARE reported
separately per Stimme (Erst vs Zweit; they differ, e.g. SK101/2013: gueltig Erst
65008 vs gueltig Zweit 64858), so this parser emits them per stimme, not combined.
A second, unrelated "Gesamtstimmen (= Erst- und Zweitstimmen)" table further down
each page (per-candidate, one combined count) is intentionally NOT parsed:
parse_stimmen_rows() stops at the first "abgegeben" line, which is always the one
belonging to the Erst/Zweit table above it.

"Sonstige 08"/"Sonstige 03" (2013/2008) is the report's own residual row and is
"X" (suppressed / not computed) in EVERY Stimmkreis on EVERY page and in both
state fixtures - verified below (validation "no real Sonstige count exists
anywhere") - so it is excluded from emission entirely; it never carries a count.

2008 CHARACTER CORRUPTION (labels only; digits are clean).  The font subset used
for the Stimmkreis-table text mangles some punctuation/diacritic glyphs; the
Abkuerzungen legend on p.4 and the "B. Mittelwerte" header on p.6 use a different,
uncorrupted font, which is how the substitution below was pinned and independently
confirmed (the ~90 place names shared with the clean 2013 report decode
byte-for-byte, e.g. "Ansbach?S{d{ Wei\x8fenburg?Gunzenhausen" as printed decodes to
"Ansbach-Süd, Weißenburg-Gunzenhausen", matching the 2013 report exactly):
    "," -> "{"      (decimal comma; e.g. "43{4" = "43,4")
    "." -> "!"      (leader dots AND the trailing "." in "%-P.")
    "-" -> "?"      (hyphen/minus, incl. in Wahlkreis-/place-name hyphens)
    "ü" -> "¨"      (bare diaeresis; base letter dropped)
    "ö" -> "`"      (bare grave accent standing in for the missing base letter)
    "ß" -> "\x8f"
Uppercase "Ü" (e.g. "GRÜNE") and "ä" (e.g. "Wähler") are NOT affected - both
render correctly throughout, confirmed against the clean 2013 text and the p.4
legend.  DECODE_2008 (a str.translate map) undoes exactly this substitution; it is
applied to party labels and Stimmkreis names only (never to digit tokens, which
were already established to be clean).

VALIDATION (all hard; nothing is written if any check fails):
  (1) exactly 91 (2008) / 90 (2013) Stimmkreis pages parsed, each with the fixed
      party sequence in the fixed order (enforced inline: every row must start
      with the expected label or the run aborts) and exactly 10 tokens per row.
  (1b) "Sonstige NN" is "X" (Erst AND Zweit) in every one of the 91+90 Stimmkreise
      and in both state fixtures - i.e. it never carries a real count anywhere,
      confirming it is safe to exclude from the emitted data entirely.
  (2) sum over all Stimmkreise, per party (bottom-up from the parsed SK records):
        2013: Erst and Zweit separately == the p.7 state fixture, exactly.
        2008: Erst+Zweit combined == the p.5 Gesamtstimmen state fixture, exactly
              (p.5 reports only the combined figure; there is no separate 2008
              statewide Erst/Zweit breakdown anywhere in this report - the
              Bezirk pages that would carry it are image-only).
  (3) sum of Stimmberechtigte / Wähler over all Stimmkreise == the state fixture,
      exactly, for both years.
  (4) recomputed Gesamtstimmen shares (bottom-up from the parsed SK records)
      match the pinned official Landeswahlleiter results within +-0.1pp:
        2013: CSU 47.7, SPD 20.6, FREIE WÄHLER 9.0, GRÜNE 8.6, FDP 3.3
        2008: CSU 43.4, SPD 18.6, FW 10.2, GRÜNE 9.4, FDP 8.0, DIE LINKE 4.4
  (5) per Stimmkreis, per Stimme (Erst, Zweit separately): sum of the 14/15 named
      party counts (Sonstige excluded; it is always 0) == that Stimme's own
      "gültig" row, EXACTLY - the strongest exact check the source supports,
      since valid/invalid votes are reported per Stimme, not combined.
  (5b) per Stimmkreis, per Stimme: abgegeben == gültig + ungültig, exactly.

Output: data/state_elections/processed/wahlkreis/by_pdf/BY_2008_2013_pdf_long.csv
        (read by parsers/parse_BY.R, which appends the 2018/2023 votemanager
        results)
Run:    python3 code/state_elections_wahlkreis/parsers/00_by_pdf_parse.py
Requires: poppler (pdftotext) on PATH.
"""

import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW_DIR = os.path.join(ROOT, "data", "state_elections", "raw",
                       "Landtagswahlen_Wahlkreis", "Bayern")
PDF = {
    2008: os.path.join(RAW_DIR, "BY_2008_Landtagswahl_StatBericht_BVII2-4.pdf"),
    2013: os.path.join(RAW_DIR, "BY_2013_Landtagswahl_StatBericht_BVII2-4.pdf"),
}
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "by_pdf")
OUT = os.path.join(OUT_DIR, "BY_2008_2013_pdf_long.csv")

STATE_ABBR, STATE_NAME = "BY", "Bayern"
ELECTION_DATE = {2008: "2008-09-28", 2013: "2013-09-15"}
SK_PAGE_RANGE = {2008: range(18, 109), 2013: range(15, 105)}   # 1-based PDF pages
N_SK = {2008: 91, 2013: 90}
STATE_PAGE_2013 = 7   # "1. Landes- und Wahlkreisergebnisse / Bayern" - SK-page layout
STATE_PAGE_2008 = 5   # "A. Ergebnis der Wahl..." - Gesamtstimmen-only layout

# Party sequence exactly as printed, in order, on every Stimmkreis page (RAW text
# for 2008, i.e. corrupted - this is what actually appears in the source so that
# line-prefix slicing lines up token-for-token).  Sonstige NN excluded here; it is
# handled explicitly (see SONSTIGE_RAW) because it is dropped from emission.
PARTIES_RAW = {
    2013: ["CSU", "SPD", "FREIE WÄHLER", "GRÜNE", "FDP", "DIE LINKE", "ÖDP", "REP",
           "NPD", "BP", "BüSo", "DIE FREIHEIT", "FRAUENLISTE", "DIE FRANKEN", "PIRATEN"],
    2008: ["CSU", "SPD", "GRÜNE", "FW", "FDP", "REP", "`dp", "BP", "B¨So", "BB",
           "DIE LINKE", "VIOLETTE", "NPD", "RRP"],
}
SONSTIGE_RAW = {2013: "Sonstige 08", 2008: "Sonstige 03"}

# str.translate map that undoes the 2008 Stimmkreis-table font corruption (see
# module docstring). Applied to party labels and Stimmkreis names only.
DECODE_2008 = str.maketrans({"!": ".", "{": ",", "?": "-", "¨": "ü", "`": "ö", "\x8f": "ß"})


def decode_2008(s):
    return s.translate(DECODE_2008)


# Pinned official Landeswahlleiter Gesamtstimmen (= Erst+Zweit) shares, +/- 0.1pp.
OFFICIAL_SHARES = {
    2013: {"CSU": 47.7, "SPD": 20.6, "FREIE WÄHLER": 9.0, "GRÜNE": 8.6, "FDP": 3.3},
    2008: {"CSU": 43.4, "SPD": 18.6, "FW": 10.2, "GRÜNE": 9.4, "FDP": 8.0, "DIE LINKE": 4.4},
}

SK_HEADER_RE = re.compile(r"^Stimmkreis (\d{3}) (.+)$")


# =============================================================================
# generic helpers
# =============================================================================
def pdf_pages(path):
    """1-based PDF page p == pdf_pages(path)[p - 1]."""
    txt = subprocess.run(["pdftotext", "-layout", path, "-"],
                         check=True, capture_output=True).stdout.decode("utf-8")
    return txt.split("\f")


def to_num(tok):
    return None if tok in ("X", "-", ".", "") else int(tok)


def line_value(lines, label_regex):
    """First integer on the first line whose stripped text matches label_regex."""
    rx = re.compile(label_regex)
    for l in lines:
        s = l.strip()
        if rx.match(s):
            m = re.search(r"\d+", s)
            return int(m.group()) if m else None
    return None


def find_anchor(lines, label="CSU"):
    for i, l in enumerate(lines):
        if re.match(rf"^{label}\s", l.strip()):
            return i
    return None


def parse_party_rows(lines, idx, labels_raw, where):
    """labels_raw INCLUDES the trailing Sonstige row.
    Returns {raw_label: (erst_raw_token, zweit_raw_token)}; raises on any
    structural mismatch (wrong label, wrong token count) - this IS validation (1).
    """
    out = {}
    for off, label in enumerate(labels_raw):
        if idx + off >= len(lines):
            raise SystemExit(f"{where}: ran off the page before row {off} ({label!r})")
        s = lines[idx + off].strip()
        if not s.startswith(label):
            raise SystemExit(f"{where}: row {off} expected label {label!r}, got {s[:50]!r}")
        rest = s[len(label):].strip().split()
        if len(rest) != 10:
            raise SystemExit(f"{where}: row {label!r} expected 10 tokens, got "
                             f"{len(rest)}: {rest}")
        out[label] = (rest[0], rest[5])
    return out


def parse_stimmen_rows(lines, start, where):
    """Scan from `start` for the gültig/ungültig/abgegeben (Erst, Zweit) rows.
    Stops at the first 'abgegeben': the page repeats a near-identical block later
    for the (unrelated, per-candidate) Gesamtstimmen table, which must NOT be
    picked up here."""
    out = {}
    for l in lines[start:]:
        s = l.strip()
        if not s:
            continue
        if re.match(r"^g.ltig\b", s):
            toks = s.split(None, 1)[1].split()
            out["gultig"] = (to_num(toks[0]), to_num(toks[5]))
        elif re.match(r"^ung.ltig\b", s):
            toks = s.split(None, 1)[1].split()
            out["ungultig"] = (to_num(toks[0]), to_num(toks[5]))
        elif re.match(r"^abgegeben\b", s):
            toks = s.split(None, 1)[1].split()
            out["abgegeben"] = (to_num(toks[0]), to_num(toks[5]))
            break
    missing = {"gultig", "ungultig", "abgegeben"} - set(out)
    if missing:
        raise SystemExit(f"{where}: missing row(s) {sorted(missing)} after the party block")
    return out


def parse_sk_style_page(lines, year, where):
    """Stimmkreis-table layout: header (Stimmberechtigte/Wähler) + 15/16 party
    rows (10 tokens each) + gültig/ungültig/abgegeben (10 tokens each). Used for
    every Stimmkreis page in both years AND the 2013 state fixture (p.7), which
    shares this exact layout."""
    stimmberechtigte = line_value(lines, r"^Stimmberechtigte\b")
    waehler = line_value(lines, r"^Wähler\b")
    idx = find_anchor(lines)
    if stimmberechtigte is None or waehler is None or idx is None:
        raise SystemExit(f"{where}: could not find header/party-block anchor")
    labels = PARTIES_RAW[year] + [SONSTIGE_RAW[year]]
    party_raw_tokens = parse_party_rows(lines, idx, labels, where)
    stimmen = parse_stimmen_rows(lines, idx + len(labels), where)
    return {"stimmberechtigte": stimmberechtigte, "waehler": waehler,
            "party_raw_tokens": party_raw_tokens, "stimmen": stimmen}


# =============================================================================
# per-year Stimmkreis parsing
# =============================================================================
def parse_year_sk(year):
    pages = pdf_pages(PDF[year])
    records = {}
    for pno in SK_PAGE_RANGE[year]:
        lines = pages[pno - 1].split("\n")
        m = None
        for l in lines:
            m = SK_HEADER_RE.match(l.strip())
            if m:
                break
        if not m:
            raise SystemExit(f"BY {year} p{pno}: no 'Stimmkreis NNN Name' header found")
        nr, name = m.group(1), m.group(2)
        where = f"BY {year} p{pno} (SK {nr})"
        blk = parse_sk_style_page(lines, year, where)
        if nr in records:
            raise SystemExit(f"BY {year}: duplicate Stimmkreis {nr} ({where} and earlier)")
        records[nr] = {"name": name, **blk}
    if len(records) != N_SK[year]:
        raise SystemExit(f"BY {year}: parsed {len(records)} Stimmkreise, expected {N_SK[year]}")
    return records


# =============================================================================
# state fixtures
# =============================================================================
def parse_state_2013():
    pages = pdf_pages(PDF[2013])
    lines = pages[STATE_PAGE_2013 - 1].split("\n")
    return parse_sk_style_page(lines, 2013, f"BY 2013 state fixture p{STATE_PAGE_2013}")


def parse_state_2008():
    """p.5 'A. Ergebnis der Wahl...' - Gesamtstimmen (= Erst+Zweit) ONLY, with
    Sitze, not a separate Erst-/Zweitstimmen breakdown (6 tokens/row: Anzahl-%-
    Sitze for 2008, then the same for 2003)."""
    pages = pdf_pages(PDF[2008])
    lines = pages[STATE_PAGE_2008 - 1].split("\n")
    where = f"BY 2008 state fixture p{STATE_PAGE_2008}"
    stimmberechtigte = line_value(lines, r"^Stimmberechtigte\b")
    waehler = line_value(lines, r"^Wähler\b")
    abgegeben = line_value(lines, r"^Abgegebene Gesamtstimmen\b")
    ungultig = line_value(lines, r"^Ung.ltige Gesamtstimmen\b")
    gultig = line_value(lines, r"^G.ltige Gesamtstimmen\b")
    idx = find_anchor(lines)
    if None in (stimmberechtigte, waehler, abgegeben, ungultig, gultig, idx):
        raise SystemExit(f"{where}: missing a required header field")
    labels = PARTIES_RAW[2008] + [SONSTIGE_RAW[2008]]
    party_raw_tokens = {}
    for off, label in enumerate(labels):
        s = lines[idx + off].strip()
        if not s.startswith(label):
            raise SystemExit(f"{where}: row {off} expected label {label!r}, got {s[:50]!r}")
        rest = s[len(label):].strip().split()
        if len(rest) != 6:
            raise SystemExit(f"{where}: row {label!r} expected 6 tokens, got {rest}")
        party_raw_tokens[label] = rest[0]   # 2008 Gesamtstimmen Anzahl (combined)
    return {"stimmberechtigte": stimmberechtigte, "waehler": waehler,
            "abgegeben": abgegeben, "ungultig": ungultig, "gultig": gultig,
            "party_raw_tokens": party_raw_tokens}


# =============================================================================
# validate + emit
# =============================================================================
def main():
    fails = []

    def req(cond, msg):
        print(("  [ok]   " if cond else "  [FAIL] ") + msg)
        if not cond:
            fails.append(msg)

    print("Reading", os.path.relpath(PDF[2013], ROOT))
    sk13 = parse_year_sk(2013)
    state13 = parse_state_2013()
    print("Reading", os.path.relpath(PDF[2008], ROOT))
    sk08 = parse_year_sk(2008)
    state08 = parse_state_2008()

    print("\n=========== VALIDATION (BY 2008 + 2013, Stimmkreis level) ===========")

    # (1) completeness / structure --------------------------------------------
    # (every page already had to match the fixed party sequence + 10-token rows
    # to get this far without aborting - that IS the "identical sequence" check)
    req(len(sk13) == 90, f"2013: {len(sk13)} Stimmkreise parsed with the fixed "
                         f"party sequence (expect 90)")
    req(len(sk08) == 91, f"2008: {len(sk08)} Stimmkreise parsed with the fixed "
                         f"party sequence (expect 91)")

    # (1b) Sonstige is 'X' (no real count) in EVERY Stimmkreis + both state fixtures
    bad = []
    for year, recs in ((2013, sk13), (2008, sk08)):
        sonstige = SONSTIGE_RAW[year]
        for nr, r in recs.items():
            e, z = r["party_raw_tokens"][sonstige]
            if e != "X" or z != "X":
                bad.append((year, "SK" + nr, e, z))
    e13, z13 = state13["party_raw_tokens"][SONSTIGE_RAW[2013]]
    if e13 != "X" or z13 != "X":
        bad.append((2013, "STATE", e13, z13))
    if state08["party_raw_tokens"][SONSTIGE_RAW[2008]] != "X":
        bad.append((2008, "STATE", state08["party_raw_tokens"][SONSTIGE_RAW[2008]], None))
    req(not bad, "Sonstige NN is 'X' (no real count) in all 181 Stimmkreise + both "
                "state fixtures - safe to exclude from emission")
    for b in bad[:10]:
        print("           ", b)

    # (2) SK-level sums reproduce the state fixture, exactly -------------------
    for label in PARTIES_RAW[2013]:
        mine_e = sum(to_num(sk13[nr]["party_raw_tokens"][label][0]) or 0 for nr in sk13)
        mine_z = sum(to_num(sk13[nr]["party_raw_tokens"][label][1]) or 0 for nr in sk13)
        ref_e = to_num(state13["party_raw_tokens"][label][0]) or 0
        ref_z = to_num(state13["party_raw_tokens"][label][1]) or 0
        req(mine_e == ref_e and mine_z == ref_z,
            f"2013 {label}: SK-sum Erst {mine_e} (state {ref_e}), "
            f"Zweit {mine_z} (state {ref_z})" if mine_e != ref_e or mine_z != ref_z
            else f"2013 {label}: SK-sum == state fixture (Erst {ref_e}, Zweit {ref_z})")

    for label in PARTIES_RAW[2008]:
        mine = sum((to_num(sk08[nr]["party_raw_tokens"][label][0]) or 0)
                   + (to_num(sk08[nr]["party_raw_tokens"][label][1]) or 0) for nr in sk08)
        ref = to_num(state08["party_raw_tokens"][label]) or 0
        req(mine == ref, f"2008 {decode_2008(label)}: SK-sum Erst+Zweit {mine} "
                         f"vs state Gesamtstimmen {ref}"
            if mine != ref else
            f"2008 {decode_2008(label)}: SK-sum Erst+Zweit == state Gesamtstimmen ({ref})")

    # (3) Stimmberechtigte / Wähler sums ----------------------------------------
    for year, recs, state in ((2013, sk13, state13), (2008, sk08, state08)):
        sb = sum(r["stimmberechtigte"] for r in recs.values())
        wa = sum(r["waehler"] for r in recs.values())
        req(sb == state["stimmberechtigte"] and wa == state["waehler"],
            f"{year}: SK-sum Stimmberechtigte {sb} (state {state['stimmberechtigte']}), "
            f"Wähler {wa} (state {state['waehler']})")

    # (4) recomputed Gesamtstimmen shares vs pinned official results -----------
    gesamt13 = (sum((sk13[nr]["stimmen"]["gultig"][0] or 0) for nr in sk13)
               + sum((sk13[nr]["stimmen"]["gultig"][1] or 0) for nr in sk13))
    bad = []
    for label, official in OFFICIAL_SHARES[2013].items():
        v = (sum(to_num(sk13[nr]["party_raw_tokens"][label][0]) or 0 for nr in sk13)
            + sum(to_num(sk13[nr]["party_raw_tokens"][label][1]) or 0 for nr in sk13))
        got = 100.0 * v / gesamt13
        if abs(got - official) > 0.1:
            bad.append((label, round(got, 2), official))
    req(not bad, f"2013: recomputed Gesamtstimmen shares (bottom-up from the "
                f"{len(sk13)} Stimmkreise) match the official results (+-0.1pp)")
    for b in bad:
        print("           ", b)

    gesamt08 = sum((sk08[nr]["stimmen"]["gultig"][0] or 0)
                  + (sk08[nr]["stimmen"]["gultig"][1] or 0) for nr in sk08)
    bad = []
    for label, official in OFFICIAL_SHARES[2008].items():
        v = sum((to_num(sk08[nr]["party_raw_tokens"][label][0]) or 0)
               + (to_num(sk08[nr]["party_raw_tokens"][label][1]) or 0) for nr in sk08)
        got = 100.0 * v / gesamt08
        if abs(got - official) > 0.1:
            bad.append((decode_2008(label), round(got, 2), official))
    req(not bad, f"2008: recomputed Gesamtstimmen shares (bottom-up from the "
                f"{len(sk08)} Stimmkreise) match the official results (+-0.1pp)")
    for b in bad:
        print("           ", b)

    # (5) per-Stimmkreis, per-Stimme: named parties sum EXACTLY to gültig ------
    for year, recs in ((2013, sk13), (2008, sk08)):
        labels = PARTIES_RAW[year]   # Sonstige excluded - always 0
        bad = []
        for nr, r in recs.items():
            for i, stimme in enumerate(("erst", "zweit")):
                s = sum(to_num(r["party_raw_tokens"][p][i]) or 0 for p in labels)
                if s != r["stimmen"]["gultig"][i]:
                    bad.append((nr, stimme, s, r["stimmen"]["gultig"][i]))
        req(not bad, f"{year}: every Stimmkreis x Stimme: sum(named party votes) "
                    f"== gültige Stimmen exactly ({2 * len(recs)} checks)")
        for b in bad[:8]:
            print("           ", b)

        # (5b) abgegeben == gültig + ungültig, exactly
        bad = []
        for nr, r in recs.items():
            for i, stimme in enumerate(("erst", "zweit")):
                g, u, a = (r["stimmen"]["gultig"][i], r["stimmen"]["ungultig"][i],
                          r["stimmen"]["abgegeben"][i])
                if a != g + u:
                    bad.append((nr, stimme, a, g, u))
        req(not bad, f"{year}: every Stimmkreis x Stimme: abgegeben == gültig + "
                    f"ungültig exactly")
        for b in bad[:8]:
            print("           ", b)

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- emit -------------------------------------------------------------------
    rows = []
    for year, recs in ((2013, sk13), (2008, sk08)):
        labels = PARTIES_RAW[year]
        for nr in sorted(recs):
            r = recs[nr]
            name = decode_2008(r["name"]) if year == 2008 else r["name"]
            for i, stimme in enumerate(("erststimme", "zweitstimme")):
                valid = r["stimmen"]["gultig"][i]
                invalid = r["stimmen"]["ungultig"][i]
                for label in labels:
                    v = to_num(r["party_raw_tokens"][label][i])
                    party_raw = decode_2008(label) if year == 2008 else label
                    rows.append({
                        "state_abbr": STATE_ABBR, "state": STATE_NAME,
                        "election_year": year, "election_date": ELECTION_DATE[year],
                        "wkr_nr": nr, "wkr_name": name, "stimme": stimme,
                        "eligible_voters": r["stimmberechtigte"],
                        "number_voters": r["waehler"],
                        "valid_votes": valid, "invalid_votes": invalid,
                        "party_raw": party_raw, "votes": "" if v is None else v,
                    })

    os.makedirs(OUT_DIR, exist_ok=True)
    import csv
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)
    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for year in (2008, 2013):
        for stimme in ("erststimme", "zweitstimme"):
            n = sum(1 for r in rows if r["election_year"] == year and r["stimme"] == stimme)
            print(f"  {year} {stimme:12s}: {n} rows ({n // N_SK[year]} parties x "
                 f"{N_SK[year]} Stimmkreise)")
    all_parties = sorted({r["party_raw"] for r in rows})
    print(f"\nDistinct party_raw ({len(all_parties)}):", all_parties)


if __name__ == "__main__":
    main()
