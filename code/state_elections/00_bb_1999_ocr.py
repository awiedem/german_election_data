#!/usr/bin/env python3
"""OCR extraction of Brandenburg 1999 Landtagswahl results from scanned PDF.

Extracts municipality-level Zweitstimmen data from Table 4 (pages 16-348)
and kreisfreie Städte totals from Table 5 (pages 349-426).

Usage:
    python3 code/state_elections/00_bb_1999_ocr.py
"""

import re
import csv
import sys
from pathlib import Path
from pdf2image import convert_from_path
import pytesseract
from io import StringIO
import pandas as pd

BASE_DIR = Path(__file__).resolve().parent.parent.parent
PDF_PATH = BASE_DIR / "data/state_elections/raw/Landtagswahlen/Brandenburg/Brandenburg_1999_Landtagswahl.pdf"
OUTPUT_PATH = BASE_DIR / "data/state_elections/raw/Landtagswahlen/Brandenburg/bb_1999_ocr.csv"

# Page ranges (1-indexed)
TABLE4_START = 16
TABLE4_END = 424  # will auto-detect actual end

# Table 5: kreisfreie Städte city-total pages (manually identified)
CITY_PAGES = {
    349: "12051000",  # Brandenburg an der Havel
    366: "12052000",  # Cottbus
    386: "12053000",  # Frankfurt (Oder)
    404: "12054000",  # Potsdam
}

# Zweitstimmen row numbers → field names
ROW_MAP = {
    1: "eligible_voters",     # Wahlberechtigte (count only, no Erst/Zweit split)
    2: "number_voters",       # Wähler (count only)
    4: "invalid_votes",       # ungültige Stimmen — Zweit
    6: "valid_votes",         # gültige Stimmen — Zweit
    8: "spd",
    10: "cdu",
    12: "pds",
    14: "bfwg",
    16: "bfb",
    18: "gruene_b90",
    20: "buerger",
    22: "dvu",
    24: "fdp",
    26: "npd",
    28: "eb",
}

FIELD_ORDER = [
    "ags", "eligible_voters", "number_voters", "invalid_votes", "valid_votes",
    "spd", "cdu", "pds", "bfwg", "bfb", "gruene_b90", "buerger", "dvu", "fdp", "npd", "eb"
]


def ocr_page(image):
    """OCR a single page image, return DataFrame of word bounding boxes."""
    tsv = pytesseract.image_to_data(image, lang="deu", config="--oem 3 --psm 6")
    df = pd.read_csv(StringIO(tsv), sep="\t")
    df = df[df["text"].notna()].copy()
    df["text"] = df["text"].astype(str).str.strip()
    df = df[df["text"] != ""].copy()
    df["x_right"] = df["left"] + df["width"]
    df["y_center"] = df["top"] + df["height"] / 2
    return df


def is_table5_page(df):
    """Check if this page belongs to Table 5 (kreisfreie Städte)."""
    header = df[(df["top"] < 250) & (df["left"] < 1500)]
    text = " ".join(header.sort_values("left")["text"].values)
    return "kreisfreien" in text.lower() or "Wahlbezirken" in text


def detect_ags_codes(df):
    """Find AGS codes in the page header and return [(ags_str, x_center), ...]."""
    # Header region: y roughly 350-430 based on test data
    header = df[(df["top"] > 300) & (df["top"] < 460)].copy()

    # Collect digit-like fragments (digits, periods, possibly spaces in OCR)
    frags = []
    for _, row in header.iterrows():
        txt = row["text"]
        # Accept words that are purely digits/periods (AGS fragments)
        if re.match(r"^[\d.]+$", txt) and len(txt) >= 2:
            frags.append({
                "text": txt,
                "x": row["left"],
                "x_right": row["x_right"],
                "y": row["top"],
            })

    if not frags:
        return []

    # Sort by x position
    frags.sort(key=lambda f: f["x"])

    # Group fragments that are close together (gap < 100px, same y-band)
    groups = []
    current_group = [frags[0]]
    for f in frags[1:]:
        prev = current_group[-1]
        x_gap = f["x"] - prev["x_right"]
        y_diff = abs(f["y"] - prev["y"])
        if x_gap < 100 and y_diff < 40:
            current_group.append(f)
        else:
            groups.append(current_group)
            current_group = [f]
    groups.append(current_group)

    # Parse each group into an AGS code
    ags_list = []
    for group in groups:
        raw = "".join(g["text"] for g in group)
        clean = raw.replace(".", "").replace(" ", "")
        # Valid BB AGS: 8 digits starting with "12"
        if len(clean) == 8 and clean.startswith("12"):
            x_center = (group[0]["x"] + group[-1]["x_right"]) / 2
            ags_list.append((clean, x_center))
        elif len(clean) == 7 and clean.startswith("12"):
            # Sometimes OCR drops a digit; try padding
            # e.g., "1207032" → "12070032"? This is ambiguous, skip
            pass

    return sorted(ags_list, key=lambda x: x[1])


def define_columns(ags_list):
    """Define column x-boundaries from AGS code positions."""
    if not ags_list:
        return []

    cols = []
    for i, (ags, x_center) in enumerate(ags_list):
        if i == 0:
            x_left = x_center - 180
        else:
            x_left = (ags_list[i - 1][1] + x_center) / 2

        if i == len(ags_list) - 1:
            x_right = x_center + 250
        else:
            x_right = (x_center + ags_list[i + 1][1]) / 2

        cols.append({"ags": ags, "x_center": x_center, "x_left": x_left, "x_right": x_right})

    return cols


def find_row_positions(df):
    """Find row label positions (1-28) on the left side of the page."""
    # Row labels are at x < 250, y > 450 (below header)
    left_words = df[(df["left"] < 250) & (df["top"] > 450)].copy()

    rows = {}
    for _, w in left_words.iterrows():
        txt = w["text"]
        if re.match(r"^\d{1,2}$", txt):
            num = int(txt)
            if 1 <= num <= 28 and num not in rows:
                rows[num] = w["top"]

    return rows


def parse_cell_value(words_in_cell, col_x_center):
    """Parse a cell's words into an absolute count.

    Each cell has: absolute count (left sub-column) + percentage (right sub-column).
    Only extract the count from the LEFT zone (x <= col_x_center + 20).

    Rules:
    - "x" → 0 (party didn't compete)
    - "-" → 0
    - Words with commas → percentages, skip
    - Only use words in the count zone (left half of column)
    - Adjacent digit words → concatenate (thousand separators) → integer
    """
    if words_in_cell.empty:
        return None

    # Sort by x position
    sorted_words = words_in_cell.sort_values("left")

    # Check for "x" or "-" anywhere in cell
    for _, w in sorted_words.iterrows():
        t = w["text"]
        if t.lower() == "x":
            return 0
        if t in ("-", "—", "–"):
            return 0

    # Only consider words in the COUNT zone (left portion of column)
    # The count is always left of center; percentage is right of center
    count_zone_max_x = col_x_center + 20
    count_words = sorted_words[sorted_words["left"] <= count_zone_max_x]

    if count_words.empty:
        return None

    # Collect digit-only parts from count zone
    digit_parts = []
    prev_x_right = None
    for _, w in count_words.iterrows():
        t = w["text"]
        # Skip if contains comma (percentage leaked into count zone)
        if "," in t:
            continue
        # Clean periods (OCR artifact for thousand separators)
        t_clean = t.replace(".", "")
        if re.match(r"^\d+$", t_clean):
            # Check if this is a continuation of previous number (thousand sep)
            # or a new number. If gap > 60px, treat as separate (likely percentage)
            if prev_x_right is not None and (w["left"] - prev_x_right) > 60:
                break  # stop at first large gap — rest is percentage
            digit_parts.append(t_clean)
            prev_x_right = w["left"] + w["width"]

    if not digit_parts:
        return None

    combined = "".join(digit_parts)
    try:
        return int(combined)
    except ValueError:
        return None


def extract_page_data(df, columns, row_positions):
    """Extract Zweitstimmen data for each column/municipality on this page."""
    results = {}

    for col in columns:
        ags = col["ags"]
        results[ags] = {}

        for row_num, field in ROW_MAP.items():
            if row_num not in row_positions:
                continue

            row_y = row_positions[row_num]

            # Find words in this row within this column
            # y-tolerance: ±18px; x within column bounds
            cell_words = df[
                (df["top"] >= row_y - 18) &
                (df["top"] <= row_y + 18) &
                (df["left"] >= col["x_left"]) &
                (df["left"] <= col["x_right"]) &
                (df["left"] > 500)  # skip row labels/descriptions on left
            ]

            val = parse_cell_value(cell_words, col["x_center"])
            if val is not None:
                results[ags][field] = val

    return results


def process_page(image, page_num):
    """Process one page: OCR → detect structure → extract data → return records."""
    df = ocr_page(image)

    # Check if we've left Table 4
    if is_table5_page(df):
        return [], True  # signal to stop

    # Detect AGS codes
    ags_list = detect_ags_codes(df)
    if not ags_list:
        # Could be a Bewerber/Gewählt page (bottom of WK) or section divider
        return [], False

    columns = define_columns(ags_list)
    row_positions = find_row_positions(df)

    if not row_positions:
        return [], False

    data = extract_page_data(df, columns, row_positions)

    records = []
    for ags, fields in data.items():
        if fields:  # only include if we extracted something
            record = {"ags": ags}
            record.update(fields)
            records.append(record)

    return records, False


def validate_record(record):
    """Basic validation: check vote arithmetic."""
    vv = record.get("valid_votes")
    if vv is None or vv == 0:
        return True  # can't validate

    party_sum = sum(
        record.get(p, 0) or 0
        for p in ["spd", "cdu", "pds", "bfwg", "bfb", "gruene_b90",
                   "buerger", "dvu", "fdp", "npd", "eb"]
    )

    diff = abs(vv - party_sum)
    return diff <= max(5, vv * 0.02)  # allow 2% or 5 votes tolerance


def process_table5():
    """Extract kreisfreie Städte totals from Table 5 city-total pages."""
    records = []

    for pg, ags in CITY_PAGES.items():
        sys.stdout.write(f"\r  Table 5: page {pg} ({ags})...")
        sys.stdout.flush()

        images = convert_from_path(
            str(PDF_PATH), first_page=pg, last_page=pg, dpi=300
        )
        df = ocr_page(images[0])
        row_positions = find_row_positions(df)

        if not row_positions:
            print(f"\n  WARNING: no row positions found on page {pg}")
            continue

        # Find city-total column: locate AGS digits starting with "12" in header
        header = df[(df["top"] > 300) & (df["top"] < 500)].copy()
        city_ags_x = None
        for _, w in header.sort_values("left").iterrows():
            txt = w["text"]
            if re.match(r"^[\d.]+$", txt) and "12" in txt:
                city_ags_x = w["left"]
                break

        if city_ags_x is None:
            print(f"\n  WARNING: could not find city column on page {pg}")
            continue

        # City-total column bounds (data at x~900-1110, WB columns at x~1200+)
        col_x_left = city_ags_x - 60
        col_x_right = city_ags_x + 230
        col_x_center = city_ags_x + 60

        record = {"ags": ags}
        for row_num, field in ROW_MAP.items():
            if row_num not in row_positions:
                continue

            row_y = row_positions[row_num]
            cell_words = df[
                (df["top"] >= row_y - 18) &
                (df["top"] <= row_y + 18) &
                (df["left"] >= col_x_left) &
                (df["left"] <= col_x_right) &
                (df["left"] > 500)
            ]

            val = parse_cell_value(cell_words, col_x_center)
            if val is not None:
                record[field] = val

        records.append(record)

    return records


def main():
    print(f"PDF: {PDF_PATH}")
    print(f"Output: {OUTPUT_PATH}")

    all_records = []
    bad_records = []
    batch_size = 5  # pages per batch (manage memory)

    page = TABLE4_START
    stop = False

    while page <= TABLE4_END and not stop:
        end = min(page + batch_size - 1, TABLE4_END)
        sys.stdout.write(f"\r  Converting pages {page}-{end}...")
        sys.stdout.flush()

        images = convert_from_path(
            str(PDF_PATH), first_page=page, last_page=end, dpi=300,
            thread_count=2
        )

        for i, image in enumerate(images):
            pnum = page + i
            records, is_table5 = process_page(image, pnum)

            if is_table5:
                print(f"\n  Table 5 detected at page {pnum}, stopping Table 4 extraction.")
                stop = True
                break

            if records:
                for r in records:
                    if not validate_record(r):
                        bad_records.append((pnum, r["ags"], r))
                all_records.extend(records)
                sys.stdout.write(f"\r  Page {pnum}: {len(records)} municipalities  (total: {len(all_records)})    ")
                sys.stdout.flush()

        page = end + 1

    print(f"\n\nExtracted {len(all_records)} municipality records from Table 4.")

    # --- Table 5: kreisfreie Städte ---
    print("\nExtracting kreisfreie Städte from Table 5...")
    city_records = process_table5()
    print(f"\n  Extracted {len(city_records)} city records.")

    for cr in city_records:
        if not validate_record(cr):
            print(f"  WARNING: city {cr['ags']} failed validation")
        else:
            vv = cr.get("valid_votes", 0)
            ps = sum(cr.get(p, 0) or 0 for p in
                     ["spd", "cdu", "pds", "bfwg", "bfb", "gruene_b90",
                      "buerger", "dvu", "fdp", "npd", "eb"])
            print(f"  {cr['ags']}: EV={cr.get('eligible_voters',0):,}, VV={vv:,}, diff={abs(vv-ps)}")

    # Remove any partial kreisfreie Stadt entries from Table 4
    city_ags = set(CITY_PAGES.values())
    n_before = len(all_records)
    all_records = [r for r in all_records if r["ags"] not in city_ags]
    n_removed = n_before - len(all_records)
    if n_removed:
        print(f"  Removed {n_removed} partial kreisfreie Stadt entries from Table 4.")

    # Add city records
    all_records.extend(city_records)
    print(f"  Total records: {len(all_records)}")

    if bad_records:
        print(f"\nWARNING: {len(bad_records)} Table 4 records failed vote arithmetic check:")
        for pnum, ags, rec in bad_records[:20]:
            vv = rec.get("valid_votes", "?")
            ps = sum(rec.get(p, 0) or 0 for p in
                     ["spd", "cdu", "pds", "bfwg", "bfb", "gruene_b90",
                      "buerger", "dvu", "fdp", "npd", "eb"])
            print(f"  Page {pnum}, AGS {ags}: valid_votes={vv}, party_sum={ps}, diff={abs((vv or 0)-ps)}")

    # Write CSV
    OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUTPUT_PATH, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=FIELD_ORDER)
        writer.writeheader()
        for record in all_records:
            writer.writerow({k: record.get(k, "") for k in FIELD_ORDER})

    print(f"\nWrote {len(all_records)} records to {OUTPUT_PATH}")

    # Summary stats
    unique_ags = set(r["ags"] for r in all_records)
    print(f"Unique municipalities: {len(unique_ags)}")

    # Aggregate statewide totals for validation
    totals = {}
    for field in FIELD_ORDER[1:]:
        totals[field] = sum(r.get(field, 0) or 0 for r in all_records)

    vv = totals["valid_votes"]
    if vv > 0:
        print(f"\nStatewide totals (for validation):")
        print(f"  Eligible voters: {totals['eligible_voters']:,}")
        print(f"  Voters:          {totals['number_voters']:,}")
        print(f"  Valid votes:     {vv:,}")
        for p in ["spd", "cdu", "pds", "dvu", "fdp", "gruene_b90", "npd", "bfwg", "bfb", "buerger", "eb"]:
            pct = totals[p] / vv * 100
            print(f"  {p:15s}: {totals[p]:>10,}  ({pct:5.1f}%)")


if __name__ == "__main__":
    main()
