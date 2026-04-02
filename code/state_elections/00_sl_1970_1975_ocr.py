#!/usr/bin/env python3
"""
OCR extraction of Saarland Landtag 1970 & 1975 municipality-level results.

Source: Saarland_Wahlen_in_den_Gemeinden_1970-1990.pdf (136 pages, scanned)

Structure: Pages 10-113 contain municipality data (even pages = election results,
odd pages = statistics). Each data page has one municipality's results across
all election types from 1970-1990.

We extract only Landtagswahl rows for 1970 (14.6.70) and 1975 (4.5.75).
1980-1990 is already available from the XLSX.

Uses Tesseract TSV output with word bounding boxes for position-based column
assignment (much more robust than text-based parsing for this format).

Column order: EV | Voters | Valid | SPD | CDU | F.D.P. | Grüne | DKP | NPD | Sonst. | FWG | REP
"""

import csv
import os
import re
import sys

from pdf2image import convert_from_path
import pytesseract

PDF_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "..", "..", "data", "state_elections", "raw", "Landtagswahlen",
    "Saarland", "Saarland_Wahlen_in_den_Gemeinden_1970-1990.pdf"
)
OUT_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "..", "..", "data", "state_elections", "raw", "Landtagswahlen",
    "Saarland", "sl_1970_1975_ocr.csv"
)

# Municipality name → 8-digit AGS
NAME_TO_AGS = {
    "Beckingen": "10042111",
    "Bexbach": "10045111",
    "Blieskastel": "10045112",
    "Bous": "10044122",
    "Dillingen": "10044111",
    "Ensdorf": "10044123",
    "Eppelborn": "10043111",
    "Freisen": "10046111",
    "Friedrichsthal": "10041511",
    "Gersheim": "10045113",
    "Großrosseln": "10041512",
    "Heusweiler": "10041513",
    "Homburg": "10045114",
    "Illingen": "10043112",
    "Kirkel": "10045115",
    "Kleinblittersdorf": "10041514",
    "Lebach": "10044112",
    "Losheim": "10042112",
    "Mandelbachtal": "10045116",
    "Marpingen": "10046112",
    "Merchweiler": "10043113",
    "Merzig": "10042113",
    "Mettlach": "10042114",
    "Nalbach": "10044113",
    "Namborn": "10046113",
    "Neunkirchen": "10043114",
    "Nohfelden": "10046114",
    "Nonnweiler": "10046115",
    "Oberthal": "10046116",
    "Ottweiler": "10043115",
    "Perl": "10042115",
    "Püttlingen": "10041515",
    "Quierschied": "10041516",
    "Rehlingen-Siersburg": "10044114",
    "Riegelsberg": "10041517",
    "Saarbrücken": "10041100",
    "Saarlouis": "10044115",
    "Saarwellingen": "10044116",
    "Schiffweiler": "10043116",
    "Schmelz": "10044117",
    "Schwalbach": "10044118",
    "Spiesen-Elversberg": "10043117",
    "St. Ingbert": "10045117",
    "St. Wendel": "10046117",
    "Sulzbach": "10041518",
    "Tholey": "10046118",
    "Überherrn": "10044119",
    "Völklingen": "10041519",
    "Wadgassen": "10044120",
    "Wadern": "10042116",
    "Wallerfangen": "10044121",
    "Weiskirchen": "10042117",
    # OCR misread variants
    "Metitlach": "10042114",     # Mettlach
    "Meitlach": "10042114",      # Mettlach
    "St. Ingberi": "10045117",   # St. Ingbert
    "St. Ingberl": "10045117",   # St. Ingbert
    "Riegeisborg": "10041517",   # Riegelsberg
    "Überherm": "10044119",      # Überherrn
    "Losheim am See": "10042112",
    "Rehlingen": "10044114",     # Rehlingen-Siersburg (partial match)
    "Spiesen": "10043117",       # Spiesen-Elversberg (partial match)
}

# Default column centers at 300 DPI (used as fallback)
DEFAULT_COLUMNS = {
    "ev":      650,
    "voters":  780,
    "valid":   920,
    "spd":    1050,
    "cdu":    1180,
    "fdp":    1310,
    "gruene": 1440,
    "dkp":    1570,
    "npd":    1700,
    "sonst":  1840,
    "fwg":    1960,
    "rep":    2090,
}
COL_HALF_WIDTH = 80  # wide enough to handle ±40px shifts; assign_to_column picks closest

# OCR variants of column header keywords
HEADER_PATTERNS = {
    "spd":    re.compile(r'^(?:SPD|spp|spo|SpD)$', re.I),
    "cdu":    re.compile(r'^(?:CDU|cpu|cou|CdU|CDu)$', re.I),
    "fdp":    re.compile(r'^F\.?D\.?P\.?$', re.I),
    "gruene": re.compile(r'^Gr[uü]ne$', re.I),
    "dkp":    re.compile(r'^DKP$', re.I),
    "npd":    re.compile(r'^NPD$', re.I),
    "sonst":  re.compile(r'^Sonst', re.I),
    "fwg":    re.compile(r'^FWG$', re.I),
    "rep":    re.compile(r'^REP$', re.I),
}


def detect_columns(words_df):
    """Detect column header positions dynamically from the table header row."""
    # Headers appear in the y range ~1000-1120
    header_area = words_df[(words_df.top > 950) & (words_df.top < 1150)]

    columns = dict(DEFAULT_COLUMNS)  # start with defaults

    for _, w in header_area.iterrows():
        t = str(w['text']).strip()
        x_center = w['left'] + w['width'] / 2

        for col_name, pattern in HEADER_PATTERNS.items():
            if pattern.match(t):
                columns[col_name] = int(x_center)
                break

    # Infer meta column positions from party positions
    # EV is ~400px left of SPD, voters ~270px left, valid ~130px left
    if "spd" in columns:
        spd_x = columns["spd"]
        columns["ev"] = spd_x - 400
        columns["voters"] = spd_x - 270
        columns["valid"] = spd_x - 130

    return columns


def parse_int(s):
    """Parse integer from OCR text."""
    if not s or s in ("-", ".", "x", "X", "|", "—", "–"):
        return 0
    s = s.replace("|", "").replace("'", "").replace(",", "")
    # Handle "4.077" (dot as thousand separator)
    s = re.sub(r'\.(?=\d{3})', '', s)
    s = s.replace(".", "")
    # Fix common OCR misreads
    s = s.replace("l", "1").replace("o", "0").replace("O", "0")
    s = s.strip()
    try:
        return int(s)
    except ValueError:
        return None


def find_municipality(words_df):
    """Find municipality name from page header."""
    # Look for "Gemeinde" or "Stadt" keyword, then collect the name words after it
    header = words_df[words_df.top < 1100].copy()

    for _, w in header.iterrows():
        t = str(w['text']).strip()
        if t in ('Gemeinde', 'Stadt', 'Landeshauptstadt',
                 'Kreisstadt', 'Mittelstadt'):
            # Find words on the same line (within ±10px y) to the right
            y = w['top']
            x_right = w['left'] + w['width']
            name_words = header[
                (header.top >= y - 15) &
                (header.top <= y + 15) &
                (header.left > x_right)
            ].sort_values('left')

            # Collect name words (stop at "-" delimiter)
            parts = []
            for _, nw in name_words.iterrows():
                nt = str(nw['text']).strip()
                if nt in ('-', '—', '–', '|'):
                    break
                if nt and nt != ' ':
                    parts.append(nt)

            if parts:
                name = ' '.join(parts)
                name = re.sub(r'\s+', ' ', name).strip()
                return name
    return None


def assign_to_column(x_center, columns):
    """Assign an x-position to a column name."""
    best_col = None
    best_dist = float('inf')
    for col, cx in columns.items():
        dist = abs(x_center - cx)
        if dist < COL_HALF_WIDTH and dist < best_dist:
            best_col = col
            best_dist = dist
    return best_col


def extract_ltw_rows(words_df, municipality, columns):
    """Extract Landtagswahl 1970/1975 rows using word positions."""
    results = []

    # Minimum x for data columns (slightly left of EV column)
    min_data_x = columns.get("ev", 650) - 80

    # Find all "Landtagswahl" words
    ltw_words = words_df[words_df.text.astype(str).str.contains('andtagswahl', na=False)]

    for _, ltw_word in ltw_words.iterrows():
        ltw_y = ltw_word['top']

        # Get all words on the same line (within ±15px vertically)
        line_words = words_df[
            (words_df.top >= ltw_y - 15) &
            (words_df.top <= ltw_y + 15)
        ].sort_values('left')

        # Find the date in this line
        line_text = ' '.join(str(t) for t in line_words.text.values)
        date_match = re.search(r'(\d{1,2})\.\s*(\d{1,2})\.\s*(\d{2})', line_text)
        if not date_match:
            continue

        year_2d = int(date_match.group(3))
        year = 1900 + year_2d

        if year not in (1970, 1975):
            continue

        # Collect number tokens with their x-positions (skip non-data text)
        data_words = []
        for _, w in line_words.iterrows():
            t = str(w['text']).strip()
            x = w['left']
            width = w['width']

            # Skip text before the data columns
            if x < min_data_x:
                continue

            # Skip pipe characters and very short non-numeric artifacts
            if t in ('|', '||', ',', ':', ';'):
                continue

            data_words.append({
                'text': t,
                'x': x,
                'width': width,
                'x_center': x + width / 2,
                'x_right': x + width,
            })

        # Merge tokens that are very close together (thousand separators)
        # Rule: merge if gap < 45px and first token is 1-2 digits, second is 3 digits
        merged = []
        i = 0
        while i < len(data_words):
            current = data_words[i]
            if i + 1 < len(data_words):
                next_w = data_words[i + 1]
                gap = next_w['x'] - current['x_right']
                ct = current['text']
                nt = next_w['text']
                if (gap < 45 and
                    re.match(r'^\d{1,2}$', ct) and
                    re.match(r'^\d{3}$', nt)):
                    merged.append({
                        'text': ct + nt,
                        'x': current['x'],
                        'x_center': (current['x'] + next_w['x_right']) / 2,
                        'x_right': next_w['x_right'],
                    })
                    i += 2
                    continue
            merged.append(current)
            i += 1

        # Assign each merged token to a column
        row = {"municipality": municipality, "year": year}
        for col in columns:
            row[col] = 0

        for mw in merged:
            col = assign_to_column(mw['x_center'], columns)
            if col is None:
                continue

            t = mw['text']
            if t in ('-', '.', 'x', 'X', '—', '–', 'E', 'a', 'rn', '>',
                     '5', '*'):
                row[col] = 0
            else:
                v = parse_int(t)
                if v is not None:
                    row[col] = v

        # Validate
        vv = row['valid']
        party_sum = row['spd'] + row['cdu'] + row['fdp'] + row['dkp'] + row['npd'] + row['sonst']
        if vv > 0 and abs(party_sum - vv) / vv > 0.05:
            print(f"  WARN: {municipality} {year} - party sum {party_sum} vs valid {vv} "
                  f"(diff {abs(party_sum-vv)/vv*100:.1f}%)")
            for mw in merged:
                col = assign_to_column(mw['x_center'], columns)
                col_str = col if col else "NONE"
                print(f"    x={mw['x']:4.0f} center={mw['x_center']:4.0f} → {col_str:8s} = \"{mw['text']}\"")

        results.append(row)
    return results


def match_ags(name):
    """Match municipality name to AGS, with fuzzy fallback."""
    if name in NAME_TO_AGS:
        return NAME_TO_AGS[name]

    # Normalize and try again
    normalized = name.replace("ß", "ss").replace("ü", "u").replace("ö", "o").replace("ä", "a")
    for ref_name, ags in NAME_TO_AGS.items():
        ref_norm = ref_name.replace("ß", "ss").replace("ü", "u").replace("ö", "o").replace("ä", "a")
        if normalized.lower() == ref_norm.lower():
            return ags
        # Partial match
        if ref_name.lower().startswith(name.lower()) or name.lower().startswith(ref_name.lower()):
            return ags

    return None


def main():
    print(f"Processing: {PDF_PATH}")

    all_results = []
    found_munis = set()
    missing_munis = []

    # Data pages: 10, 12, 14, ..., 112 (52 municipality pages)
    data_pages = list(range(10, 113, 2))

    for pg in data_pages:
        images = convert_from_path(PDF_PATH, first_page=pg, last_page=pg, dpi=300)
        tsv_data = pytesseract.image_to_data(images[0], lang='deu',
                                              output_type=pytesseract.Output.DATAFRAME)
        words = tsv_data[tsv_data.conf > 0].copy()

        # Find municipality name
        muni = find_municipality(words)
        if not muni:
            print(f"  Page {pg}: No municipality name found")
            continue

        ags = match_ags(muni)
        if not ags:
            print(f"  Page {pg}: Unknown municipality '{muni}'")
            missing_munis.append(muni)
            continue

        found_munis.add(ags)

        # Detect column positions from this page's header
        columns = detect_columns(words)

        rows = extract_ltw_rows(words, muni, columns)
        for r in rows:
            r['ags'] = ags
            all_results.append(r)

        years = [r['year'] for r in rows]
        status = f"LTW: {years}" if years else "NO 1970/1975 rows"
        print(f"  Page {pg}: {muni} ({ags}) - {status}")

    # ---- Post-processing: fix garbled OCR values ----
    # Known statewide totals from PDF page 130 (Saarland aggregate)
    KNOWN_VV = {1970: 643903, 1975: 706238}
    KNOWN_SPD = {1970: 262492, 1975: 295406}
    KNOWN_CDU = {1970: 308107, 1975: 347094}
    KNOWN_FDP = {1970: 28167, 1975: 52100}

    fixes = 0
    for r in all_results:
        party_sum = r['spd'] + r['cdu'] + r['fdp'] + r['dkp'] + r['npd'] + r['sonst']
        vv = r['valid']
        ev = r['ev']
        nv = r['voters']

        # Fix 1: If valid_votes is obviously garbled (10× too high vs party_sum)
        if vv > 0 and party_sum > 0:
            ratio = vv / party_sum
            if ratio > 5:
                # Try removing first digit
                vv_str = str(vv)
                new_vv = int(vv_str[1:]) if len(vv_str) > 1 else vv
                if 0.85 < new_vv / party_sum < 1.15:
                    print(f"  FIX: {r['municipality']} {r['year']}: valid {vv} → {new_vv}")
                    r['valid'] = new_vv
                    vv = new_vv
                    fixes += 1
                else:
                    # Just set VV = party_sum
                    r['valid'] = party_sum
                    vv = party_sum
                    fixes += 1

        # Fix 2: If any party > valid_votes, try dividing by 10
        for party in ('spd', 'cdu', 'fdp', 'dkp', 'npd', 'sonst'):
            if r[party] > vv and vv > 0:
                new_val = r[party] // 10
                if new_val < vv:
                    print(f"  FIX: {r['municipality']} {r['year']}: {party} {r[party]} → {new_val}")
                    r[party] = new_val
                    fixes += 1

        # Fix 3: If voters > EV * 1.1, try removing first digit
        if r['voters'] > r['ev'] * 1.1 and r['ev'] > 0:
            nv_str = str(r['voters'])
            new_nv = int(nv_str[1:]) if len(nv_str) > 1 else r['voters']
            if new_nv <= r['ev']:
                print(f"  FIX: {r['municipality']} {r['year']}: voters {r['voters']} → {new_nv}")
                r['voters'] = new_nv
                fixes += 1

        # Fix 4: Recalculate valid_votes from party_sum if they differ > 5%
        party_sum = r['spd'] + r['cdu'] + r['fdp'] + r['dkp'] + r['npd'] + r['sonst']
        vv = r['valid']
        if vv > 0 and abs(party_sum - vv) / vv > 0.05:
            r['valid'] = party_sum
            fixes += 1

    if fixes:
        print(f"  Applied {fixes} automatic fixes")

    # Summary
    print(f"\n{'='*60}")
    print(f"Municipalities found: {len(found_munis)}/52")
    if missing_munis:
        print(f"Missing: {missing_munis}")
    print(f"Total records: {len(all_results)}")

    for yr in (1970, 1975):
        recs = [r for r in all_results if r['year'] == yr]
        if not recs:
            continue
        total_vv = sum(r['valid'] for r in recs)
        total_spd = sum(r['spd'] for r in recs)
        total_cdu = sum(r['cdu'] for r in recs)
        total_fdp = sum(r['fdp'] for r in recs)
        total_dkp = sum(r['dkp'] for r in recs)
        total_npd = sum(r['npd'] for r in recs)
        print(f"\n  {yr}: {len(recs)} municipalities, VV={total_vv:,}")
        if total_vv > 0:
            print(f"    SPD: {total_spd/total_vv*100:.1f}%, CDU: {total_cdu/total_vv*100:.1f}%, "
                  f"FDP: {total_fdp/total_vv*100:.1f}%, DKP: {total_dkp/total_vv*100:.1f}%, "
                  f"NPD: {total_npd/total_vv*100:.1f}%")

    # Write CSV
    fieldnames = ["ags", "election_year", "eligible_voters", "number_voters",
                  "invalid_votes", "valid_votes", "spd", "cdu", "fdp",
                  "dkp", "npd", "sonst"]

    with open(OUT_PATH, 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for r in sorted(all_results, key=lambda x: (x['year'], x['ags'])):
            nv = r.get('voters', 0)
            vv = r.get('valid', 0)
            writer.writerow({
                "ags": r['ags'],
                "election_year": r['year'],
                "eligible_voters": r.get('ev', 0),
                "number_voters": nv,
                "invalid_votes": max(nv - vv, 0),
                "valid_votes": vv,
                "spd": r.get('spd', 0),
                "cdu": r.get('cdu', 0),
                "fdp": r.get('fdp', 0),
                "dkp": r.get('dkp', 0),
                "npd": r.get('npd', 0),
                "sonst": r.get('sonst', 0),
            })

    print(f"\nWritten to: {OUT_PATH}")


if __name__ == "__main__":
    main()
