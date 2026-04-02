#!/usr/bin/env python3
"""
Extract municipality-level Zweitstimmen results from the Brandenburg 1990
Landtagswahl PDF (has text layer, not pure scan).

PDF structure:
  - Pages 16-447 (1-indexed) contain Stimmbezirk-level data in 4-page spreads:
    - Page 4n+0: metadata-left (eligible voters, voters, invalid, valid)
    - Page 4n+1: main-parties-right (SPD, CDU, PDS-LL, BÜ.90, F.D.P., CHR.L.)
    - Page 4n+2: secondary-parties-left (DBU, DFP, DSU, Grüne, REP)
    - Page 4n+3: minor-parties-right (Domow., NPD, Übrige, Sonstige)
  - Each Stimmbezirk has 2 rows: Erststimmen (1), Zweitstimmen (2)
  - Stimmbezirk codes: XXXXXX/YYY (6-digit DDR municipality + Stimmbezirk)
  - Briefwahl: 000000/999
  - WK totals: 2-digit number at end (skip these)
  - Pages 448-455: Kreise aggregates (for validation)
  - Pages 464+: Municipality register (DDR code -> name)

Strategy:
  - Parse metadata-left pages for EV, NV, IV, VV
  - Parse main-parties-right for SPD, CDU, PDS-LL, BÜ.90, F.D.P., CHR.L.
  - Parse secondary-parties-left for DSU, Grüne, REP
  - Aggregate Stimmbezirke to municipality level (by 6-digit DDR code)
  - Map DDR codes to 1990 AGS using Kreis lookup + Gemeinde×10

Output: data/state_elections/raw/Landtagswahlen/Brandenburg/bb_1990_ocr.csv
"""

import csv
import re
import sys
from collections import defaultdict
from pathlib import Path

import pdfplumber

# --- Configuration -----------------------------------------------------------

PDF_PATH = Path(__file__).resolve().parents[2] / (
    "data/state_elections/raw/Landtagswahlen/"
    "Brandenburg/Brandenburg_1990_Landtagswahl.pdf"
)

OUT_PATH = Path(__file__).resolve().parents[2] / (
    "data/state_elections/raw/Landtagswahlen/"
    "Brandenburg/bb_1990_ocr.csv"
)

# Data pages: 16-447 (1-indexed) = 15-446 (0-indexed)
PAGE_START = 15  # 0-indexed
PAGE_END = 446   # 0-indexed inclusive

# DDR Kreis code -> 1990 AGS Kreis prefix (5-digit)
DDR_KREIS_TO_AGS = {
    "0208": "12037", "0308": "12039", "0311": "12046",
    "0401": "12015", "0402": "12017", "0403": "12025",
    "0405": "12028", "0407": "12029", "0408": "12030",
    "0409": "12033", "0410": "12034", "0411": "12035",
    "0412": "12038", "0413": "12040", "0414": "12036",
    "0415": "12041", "0416": "12047", "0417": "12048",
    # Kreisfreie Städte
    "0431": "12001", "0432": "12005",
    "0501": "12011", "0502": "12014", "0503": "12016",
    "0504": "12020", "0505": "12012", "0506": "12021",
    "0507": "12024", "0508": "12042", "0509": "12045",
    "0531": "12004", "0532": "12003", "0533": "12006",
    "0601": "12013", "0602": "12018", "0603": "12019",
    "0605": "12022", "0606": "12023", "0607": "12026",
    "0609": "12031", "0610": "12032", "0611": "12043",
    "0612": "12044", "0614": "12027", "0631": "12002",
}

PARTY_ORDER = ["spd", "cdu", "pds_ll", "buendnis_90", "fdp", "chrl",
               "dbu", "dfp", "dsu", "gruene", "rep",
               "domowina", "npd"]

FIELD_ORDER = ["ags", "eligible_voters", "number_voters", "invalid_votes",
               "valid_votes"] + PARTY_ORDER


# --- Helpers -----------------------------------------------------------------

def ddr_to_ags(ddr_code):
    """Convert 6-digit DDR municipality code to 8-digit 1990 AGS."""
    kreis = ddr_code[:4]
    gem = ddr_code[4:]
    ags_prefix = DDR_KREIS_TO_AGS.get(kreis)
    if ags_prefix is None:
        return None
    ags_gem = str(int(gem) * 10).zfill(3)
    return ags_prefix + ags_gem


def parse_int(s):
    """Parse integer from text, handling OCR artifacts."""
    t = s.strip()
    t = t.replace(".", "").replace(",", "").replace(" ", "")
    t = t.replace("l", "1").replace("o", "0").replace("O", "0")
    t = re.sub(r'[^0-9]', '', t)
    if not t:
        return None
    try:
        return int(t)
    except ValueError:
        return None


def extract_stimmbezirk_code(line):
    """Extract Stimmbezirk code from a text line.
    Returns (ddr_muni_code, is_briefwahl) or (None, False).
    """
    # Match XXXXXX/YYY pattern
    m = re.search(r'(\d{6})/(\d{3})', line)
    if m:
        muni_code = m.group(1)
        is_brief = (muni_code == "000000")
        return muni_code, is_brief
    return None, False


# --- Page parsers ------------------------------------------------------------

def parse_metadata_left(page):
    """Parse metadata-left page for eligible_voters and number_voters.

    Returns list of (code, {eligible_voters, number_voters}).
    Uses Erststimmen row for EV (first integer after code+stimme),
    and for NV (second integer).
    """
    text = page.extract_text() or ''
    lines = text.split('\n')
    results = []

    for line in lines:
        line = line.strip()
        if not line:
            continue

        code, _ = extract_stimmbezirk_code(line)
        if not code or code == "000000":
            continue

        after_code = re.sub(r'\d{6}/\d{3}', '', line, count=1).strip()
        after_code = re.sub(r'^[1l]\s+', '', after_code).strip()

        integers = []
        for t in after_code.split():
            if '.' in t or ',' in t:
                continue
            cleaned = re.sub(r'[^0-9]', '', t)
            if cleaned and len(cleaned) >= 1:
                try:
                    integers.append(int(cleaned))
                except ValueError:
                    pass

        if len(integers) >= 2:
            results.append((code, {
                "eligible_voters": integers[0],
                "number_voters": integers[1],
            }))
        elif len(integers) == 1:
            results.append((code, {
                "eligible_voters": integers[0],
            }))

    return results


def parse_parties_right(page):
    """Parse main-parties-right page: SPD, CDU, PDS-LL, BÜ.90, F.D.P., CHR.L.
    Returns list of (code, {party: votes}) for Zweitstimmen rows only.
    """
    text = page.extract_text() or ''
    lines = text.split('\n')
    results = []
    current_code = None
    expect_zweit = False

    for line in lines:
        line = line.strip()
        if not line:
            continue

        # Skip header lines
        if any(h in line for h in ['Stimmabgabe', 'Stimmen entfallen',
                                    'Anzahl', 'SPD', 'CDU', 'bezirk']):
            continue

        # Check for Stimmbezirk code at end of line (Erststimmen row)
        code, brief = extract_stimmbezirk_code(line)
        if code:
            current_code = code
            expect_zweit = True
            continue

        # Check for WK total at end (just 2-digit number)
        wk_match = re.search(r'\s(\d{2})$', line)
        if wk_match and not re.search(r'\d{6}', line):
            # Could be WK total — check if it's all X's and numbers
            current_code = "WK_TOTAL"
            expect_zweit = True
            continue

        # Zweitstimmen row (no code at end)
        if expect_zweit and current_code and current_code not in ("000000", "WK_TOTAL"):
            # Parse party values: alternating count + percentage
            # "X X" or "X" means 0
            # Only parse 5 parties — CHRL (6th) is too error-prone
            # (percentage fragments get misattributed as CHRL votes)
            tokens = line.split()
            party_votes = _parse_party_values(tokens, 5)  # 5 main parties

            if party_votes:
                party_names = ["spd", "cdu", "pds_ll", "buendnis_90", "fdp"]
                result = {}
                for i, pname in enumerate(party_names):
                    if i < len(party_votes):
                        result[pname] = party_votes[i]
                    else:
                        result[pname] = 0
                results.append((current_code, result))

            expect_zweit = False
            continue

        # If we get here with expect_zweit=True, it might be a garbled line
        if expect_zweit:
            expect_zweit = False

    return results


def _detect_secondary_columns(words):
    """Detect Anzahl column positions from header words on a secondary page.
    Returns list of (party_name, col_center) sorted by x position.
    """
    # Find "Anzahl" header words (top < 120)
    anzahl_positions = []
    for w in words:
        if w['text'].startswith('Anz') and w['top'] < 120:
            center = (w['x0'] + w['x1']) / 2
            anzahl_positions.append(center)

    anzahl_positions.sort()
    party_names = ["dbu", "dfp", "dsu", "gruene", "rep"]

    if len(anzahl_positions) == 5:
        return list(zip(party_names, anzahl_positions))
    elif len(anzahl_positions) == 4:
        # DBU Anzahl header missing (common) — DFP, DSU, Grüne, REP
        # Estimate DBU from DFP position (DBU is ~66px left of DFP)
        dbu_center = anzahl_positions[0] - 66
        return list(zip(party_names, [dbu_center] + anzahl_positions))
    elif len(anzahl_positions) == 3:
        # Two headers missing — use spacing to estimate
        spacing = (anzahl_positions[-1] - anzahl_positions[0]) / 2
        return list(zip(party_names[2:], anzahl_positions))
    else:
        # Fallback to default positions
        return list(zip(party_names, [231, 298, 365, 430, 497]))


def parse_secondary_left(page):
    """Parse secondary-parties-left page: DBU, DFP, DSU, Grüne, REP.
    Uses word-level positions with dynamic column detection.
    Returns list of (code, {party: votes}) for Zweitstimmen rows only.
    """
    words = page.extract_words(x_tolerance=3, y_tolerance=3)
    if not words:
        return []

    # Detect column positions from this page's headers
    columns = _detect_secondary_columns(words)
    # Use tight boundaries: ±18px from each Anzahl center
    # This avoids picking up percentage values that sit between count columns
    COL_HALF_WIDTH = 18
    col_boundaries = [(party, center - COL_HALF_WIDTH, center + COL_HALF_WIDTH)
                      for party, center in columns]

    # Group words by y-position into rows
    rows = []
    current_row = []
    current_y = None
    for w in sorted(words, key=lambda w: (w['top'], w['x0'])):
        if current_y is None or abs(w['top'] - current_y) > 4:
            if current_row:
                rows.append(current_row)
            current_row = [w]
            current_y = w['top']
        else:
            current_row.append(w)
    if current_row:
        rows.append(current_row)

    results = []
    current_code = None
    expect_zweit = False

    for row_words in rows:
        # Build text for this row
        row_text = ' '.join(w['text'] for w in row_words)

        # Skip header rows
        if any(h in row_text for h in ['Wahlbeteiligung', 'Wahlkreis', 'Stimmbezirk',
                                        'Anzahl', 'DBU', 'DFP', 'DSU', 'Grüne', 'REP']):
            continue

        # Check for Stimmbezirk code
        code, _ = extract_stimmbezirk_code(row_text)

        # Check for WK total (starts with 2-digit number + name)
        is_wk = bool(re.match(r'^\d{2}\s+[A-Z]', row_text))

        # Check stimme indicator (x≈183-195 range)
        stimme_word = None
        for w in row_words:
            if 178 < w['x0'] < 200:
                stimme_word = w['text']
                break

        if code:
            current_code = code
            expect_zweit = True
            continue

        if is_wk:
            current_code = "WK_TOTAL"
            expect_zweit = True
            continue

        # Erststimmen row (stimme=1 or l)
        if stimme_word in ('1', 'l') and not code:
            if current_code:
                expect_zweit = True
            continue

        # Zweitstimmen row (stimme=2)
        if stimme_word == '2' and expect_zweit:
            if current_code and current_code not in ("000000", "WK_TOTAL"):
                result = {}
                for party, x_min, x_max in col_boundaries:
                    for w in row_words:
                        wc = (w['x0'] + w['x1']) / 2
                        if x_min <= wc <= x_max:
                            txt = w['text']
                            if '.' in txt or ',' in txt or txt == 'X':
                                continue
                            v = parse_int(txt)
                            if v is not None:
                                result[party] = v
                                break
                if result:
                    results.append((current_code, result))
            expect_zweit = False
            continue

        # Erststimmen without stimme indicator (could be garbled)
        if expect_zweit and not stimme_word:
            expect_zweit = False

    return results


def _parse_party_values(tokens, n_parties):
    """Parse party vote values from tokens.
    Each party has: count [percentage] or X [X] for zero.
    Returns list of vote counts.
    """
    votes = []
    i = 0
    while i < len(tokens) and len(votes) < n_parties:
        t = tokens[i]

        if t == 'X':
            votes.append(0)
            # Skip the paired X (percentage placeholder)
            if i + 1 < len(tokens) and tokens[i + 1] == 'X':
                i += 2
            else:
                i += 1
        elif '.' in t or ',' in t:
            # Token contains a decimal point — it's a percentage, skip it
            i += 1
        elif re.match(r'^\d', t) or t.startswith("'") or t.startswith("l"):
            # Check if this integer + next token form a split percentage
            # e.g., "7 .01" = "7.01%" split across two tokens
            if i + 1 < len(tokens) and re.match(r'^\.\d', tokens[i + 1]):
                i += 2  # skip both parts of split percentage
                continue
            v = parse_int(t)
            if v is not None:
                votes.append(v)
                # Skip percentage that follows
                i += 1
                if i < len(tokens) and ('.' in tokens[i] or ',' in tokens[i]):
                    i += 1  # skip percentage
            else:
                i += 1
        else:
            i += 1

    return votes


# --- Main extraction ---------------------------------------------------------

def main():
    print(f"Reading PDF: {PDF_PATH}")
    pdf = pdfplumber.open(str(PDF_PATH))
    total_pages = len(pdf.pages)
    print(f"Total pages: {total_pages}")

    # Data storage: ddr_muni_code -> {field: accumulated_value}
    muni_parties = defaultdict(lambda: defaultdict(int))  # party votes
    muni_meta = defaultdict(lambda: defaultdict(int))     # EV, NV

    n_pages_processed = 0
    n_stimmbezirke = 0

    # Process pages in groups of 4
    for group_start in range(PAGE_START, PAGE_END + 1, 4):
        pg_meta = group_start      # metadata-left
        pg_main = group_start + 1  # main-parties-right
        pg_sec = group_start + 2   # secondary-parties-left
        pg_minor = group_start + 3 # minor-parties-right

        if pg_main > PAGE_END:
            break

        # Parse metadata-left page for EV and NV
        meta_results = parse_metadata_left(pdf.pages[pg_meta])
        for code, data in meta_results:
            ddr_muni = code[:6]
            for k, v in data.items():
                muni_meta[ddr_muni][k] += v

        # Parse main-parties-right page
        main_results = parse_parties_right(pdf.pages[pg_main])
        for code, data in main_results:
            ddr_muni = code[:6]
            for k, v in data.items():
                muni_parties[ddr_muni][k] += v
            n_stimmbezirke += 1

        # Parse secondary-parties-left page
        if pg_sec <= PAGE_END:
            sec_results = parse_secondary_left(pdf.pages[pg_sec])
            for code, data in sec_results:
                ddr_muni = code[:6]
                for k, v in data.items():
                    muni_parties[ddr_muni][k] += v

        n_pages_processed += 4
        if n_pages_processed % 40 == 0:
            print(f"  Processed {n_pages_processed} pages, "
                  f"{len(muni_parties)} municipalities, "
                  f"{n_stimmbezirke} Stimmbezirke")

    print(f"\nTotal: {len(muni_parties)} DDR municipalities from "
          f"{n_stimmbezirke} Stimmbezirke")

    # Convert to AGS and build records
    records = []
    unmapped = 0
    all_ddr_codes = set(muni_parties.keys()) | set(muni_meta.keys())
    for ddr_code in sorted(all_ddr_codes):
        ags = ddr_to_ags(ddr_code)
        if ags is None:
            unmapped += 1
            continue

        parties = muni_parties[ddr_code]
        meta = muni_meta[ddr_code]

        rec = {"ags": ags}
        rec["eligible_voters"] = meta.get("eligible_voters", 0)
        rec["number_voters"] = meta.get("number_voters", 0)

        for p in PARTY_ORDER:
            rec[p] = parties.get(p, 0)

        # VV = sum of all party votes
        rec["valid_votes"] = sum(rec.get(p, 0) for p in PARTY_ORDER)
        # IV = NV - VV
        if rec["number_voters"] > 0 and rec["valid_votes"] > 0:
            rec["invalid_votes"] = max(rec["number_voters"] - rec["valid_votes"], 0)
        else:
            rec["invalid_votes"] = 0

        records.append(rec)

    print(f"AGS mapped: {len(records)}, unmapped DDR codes: {unmapped}")

    # Validate: VV should approximately equal sum of party votes
    n_ok = 0
    n_bad = 0
    total_vv = 0
    for rec in records:
        vv = rec["valid_votes"]
        party_sum = sum(rec.get(p, 0) for p in PARTY_ORDER)
        total_vv += vv
        if vv > 0 and abs(vv - party_sum) <= max(2, vv * 0.05):
            n_ok += 1
        elif vv > 0:
            n_bad += 1
            if n_bad <= 10:
                print(f"  MISMATCH: AGS={rec['ags']} VV={vv} sum={party_sum} "
                      f"diff={vv - party_sum}")

    print(f"\nValidation: {n_ok} OK, {n_bad} BAD out of {len(records)}")
    print(f"Total valid votes: {total_vv:,}")

    # Known statewide totals (Table 1.2):
    # VV = 1,273,906, SPD = 487,134, CDU = 374,572, PDS-LL = 170,804
    known_vv = 1273906
    print(f"Expected VV: {known_vv:,} ({total_vv / known_vv * 100:.1f}%)")

    # Statewide shares
    if total_vv > 0:
        print(f"\nStatewide vote shares (total VV={total_vv:,}):")
        for p in PARTY_ORDER:
            pv = sum(r.get(p, 0) for r in records)
            print(f"  {p:15s}: {pv:>8,} ({pv / total_vv * 100:.1f}%)")

    # Write CSV
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUT_PATH, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=FIELD_ORDER)
        writer.writeheader()
        for rec in records:
            row = {k: rec.get(k, 0) for k in FIELD_ORDER}
            row["ags"] = rec["ags"]
            writer.writerow(row)

    print(f"\nWrote {len(records)} records to {OUT_PATH}")


if __name__ == "__main__":
    main()
