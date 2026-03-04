#!/usr/bin/env python3
"""OCR extraction of Brandenburg 1994 Landtagswahl results from scanned PDF.

Extracts municipality-level Zweitstimmen data from Table 4 (pages 16-407).
Double-page spread layout: left (even) pages have identifiers/totals,
right (odd) pages have party vote columns.

Party votes are reconstructed from percentages × valid_votes because
absolute numbers OCR poorly on right pages (small digits).

Usage:
    python3 code/state_elections/00_bb_1994_ocr.py [--test N]

    --test N: Process only first N page pairs with verbose output
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
PDF_PATH = BASE_DIR / "data/state_elections/raw/Landtagswahlen/Brandenburg/Brandenburg_1994_Landtagswahl.pdf"
OUTPUT_PATH = BASE_DIR / "data/state_elections/raw/Landtagswahlen/Brandenburg/bb_1994_ocr.csv"

TABLE4_START = 16
TABLE4_END = 407

# 14 parties in left-to-right column order on right page
PARTY_ORDER = [
    "spd", "cdu", "fdp", "bfwg", "gruene_b90", "buerger",
    "dsu", "graue", "rep", "kpd", "odp", "pds", "uwvb", "eb"
]

FIELD_ORDER = [
    "ags", "eligible_voters", "number_voters", "invalid_votes", "valid_votes"
] + PARTY_ORDER

# Table 5: kreisfreie Städte city-total pages (to be identified)
CITY_PAGES = {}

VERBOSE = False


# ---------------------------------------------------------------------------
# OCR helpers
# ---------------------------------------------------------------------------

def safe_ocr(image):
    """OCR with robust TSV parsing."""
    tsv = pytesseract.image_to_data(image, lang="deu", config="--oem 3 --psm 6")
    df = pd.read_csv(StringIO(tsv), sep="\t", on_bad_lines="skip", quoting=3)
    df = df[df["text"].notna()].copy()
    df["text"] = df["text"].astype(str).str.strip()
    df = df[df["text"] != ""].copy()
    df["x_right"] = df["left"] + df["width"]
    return df


def parse_number(words, x_min, x_max):
    """Parse an integer from words within an x-range. Returns int or None."""
    subset = words[(words["left"] >= x_min) & (words["left"] <= x_max)]
    if subset.empty:
        return None

    parts = []
    for _, w in subset.sort_values("left").iterrows():
        t = w["text"]
        # Fix common OCR character errors
        t = t.replace("ı", "1").replace("ł", "1").replace("O", "0")

        # Skip percentage values (contain comma = German decimal)
        if "," in t:
            continue

        # Remove thousand-separator dots
        t = t.replace(".", "")

        if t in ("x", "-", "—", "–", "="):
            return 0
        if t == "l":
            parts.append("1")
            continue
        if re.match(r"^\d+$", t):
            parts.append(t)

    if not parts:
        return None
    try:
        return int("".join(parts))
    except ValueError:
        return None


# ---------------------------------------------------------------------------
# Left page extraction (identifiers + totals)
# ---------------------------------------------------------------------------

def extract_left_page(df):
    """Extract municipality data from a left (even) page.

    Returns list of dicts with: ags, ags_y, eligible_voters, number_voters,
    invalid_votes, valid_votes.
    """
    entries = []
    for _, row in df.iterrows():
        txt = row["text"]
        x = row["left"]
        y = row["top"]
        if x < 350 and re.match(r"^12\d{6}$", txt):
            entries.append({"ags": txt, "ags_y": y})

    entries.sort(key=lambda e: e["ags_y"])

    for entry in entries:
        ags_y = entry["ags_y"]

        # E abs. row: "E" marker at x≈1080-1280 near ags_y
        e_abs_y = None
        nearby = df[(df["top"] > ags_y - 25) & (df["top"] < ags_y + 30)]
        for _, row in nearby.iterrows():
            if row["text"] == "E" and 1080 < row["left"] < 1280:
                e_abs_y = row["top"]
                break

        if e_abs_y is not None:
            e_row = df[(df["top"] > e_abs_y - 18) & (df["top"] < e_abs_y + 18)]
            entry["eligible_voters"] = parse_number(e_row, 1300, 1575)
            entry["number_voters"] = parse_number(e_row, 1590, 1870)

        # Z abs. row: "Z" marker at x≈1080-1280, y ∈ [ags_y+35, ags_y+140]
        z_abs_y = None
        z_search = df[(df["top"] > ags_y + 35) & (df["top"] < ags_y + 140)]
        for _, row in z_search.iterrows():
            if row["text"] == "Z" and 1080 < row["left"] < 1280:
                z_abs_y = row["top"]
                break

        if z_abs_y is not None:
            z_row = df[(df["top"] > z_abs_y - 18) & (df["top"] < z_abs_y + 18)]
            entry["invalid_votes"] = parse_number(z_row, 1890, 2160)
            entry["valid_votes"] = parse_number(z_row, 2170, 2450)

        # Impute VV from NV - IV if VV is missing but NV is available
        if entry.get("valid_votes") is None or entry.get("valid_votes") == 0:
            nv = entry.get("number_voters")
            iv = entry.get("invalid_votes") or 0
            if nv and nv > iv:
                entry["valid_votes"] = nv - iv

    return entries


# ---------------------------------------------------------------------------
# Right page extraction (party percentages)
# ---------------------------------------------------------------------------

def find_pct_row_ys(df_right, y_min=500):
    """Find y-positions of percentage rows on right page.

    Uses comma-containing values (NN,NN) to identify rows.
    Returns sorted list of cluster center y-positions.
    """
    pct_ys = []
    for _, row in df_right.iterrows():
        if row["top"] < y_min:
            continue
        if re.match(r"^\d+,\d+$", row["text"]):
            pct_ys.append(row["top"])

    if not pct_ys:
        return []

    pct_ys.sort()
    clusters = [[pct_ys[0]]]
    for y in pct_ys[1:]:
        if y - clusters[-1][-1] < 25:
            clusters[-1].append(y)
        else:
            clusters.append([y])

    # Keep clusters with >= 2 comma values (filters noise from stray digits)
    # But also keep single-value clusters that have neighboring zero-markers
    good = [c for c in clusters if len(c) >= 2]

    return [sum(c) / len(c) for c in good]


def extract_row_values(df_right, y_center, tolerance=15):
    """Extract percentage and zero-marker values from a row at y_center.

    Returns sorted list of (x_center, value) pairs.
    """
    row_words = df_right[
        (df_right["top"] > y_center - tolerance) &
        (df_right["top"] < y_center + tolerance)
    ]

    results = []
    for _, w in row_words.sort_values("left").iterrows():
        txt = w["text"]
        x_c = (w["left"] + w["x_right"]) / 2

        m = re.match(r"^(\d+),(\d+)$", txt)
        if m:
            pct = float(m.group(1) + "." + m.group(2))
            results.append((x_c, pct))
        elif txt in ("x", "-", "—", "–", "=", "X", "u", "U"):
            results.append((x_c, 0.0))
        elif txt == "0":
            results.append((x_c, 0.0))

    return sorted(results, key=lambda v: v[0])


def assign_to_parties_by_position(values, col_positions, tolerance=60):
    """Assign (x, pct) pairs to parties by nearest column position.

    Each value is matched to the nearest column within tolerance.
    Unmatched columns default to 0.0.
    """
    result = {p: 0.0 for p in PARTY_ORDER}
    n = min(len(col_positions), len(PARTY_ORDER))

    for x, pct in values:
        best_idx = -1
        min_dist = float("inf")
        for i in range(n):
            d = abs(x - col_positions[i])
            if d < min_dist:
                min_dist = d
                best_idx = i
        if min_dist <= tolerance and 0 <= best_idx < len(PARTY_ORDER):
            result[PARTY_ORDER[best_idx]] = pct

    return result


def assign_to_parties_by_order(values):
    """Fallback: assign sorted values to parties by left-to-right order."""
    result = {p: 0.0 for p in PARTY_ORDER}
    for i, (_, pct) in enumerate(values):
        if i < len(PARTY_ORDER):
            result[PARTY_ORDER[i]] = pct
    return result


def fix_ocr_percentages(values):
    """Fix OCR errors where digits are duplicated (e.g. 486,67 → 48,67).

    If any value > 100%, try dividing by 10. If that makes the sum
    reasonable (80-115%), return the fixed values.
    """
    pct_sum = sum(v for _, v in values)
    if pct_sum <= 115:
        return values

    fixed = []
    for x, pct in values:
        if pct > 100:
            fixed.append((x, pct / 10))
        else:
            fixed.append((x, pct))

    fixed_sum = sum(v for _, v in fixed)
    if 30 < fixed_sum < 115:
        return fixed
    return values


def assign_to_parties_hybrid(values, col_template):
    """Try position-based assignment first, fall back to order-based.

    Accepts whichever method gives a reasonable pct_sum (30-115%).
    If neither works, returns the position-based result (or order-based if no template).
    """
    # Fix OCR errors first
    values = fix_ocr_percentages(values)

    # Try position-based if template available
    if col_template:
        pos_result = assign_to_parties_by_position(values, col_template)
        pos_sum = sum(pos_result.values())
        if 30 < pos_sum < 115:
            return pos_result

    # Try order-based
    ord_result = assign_to_parties_by_order(values)
    ord_sum = sum(ord_result.values())
    if 30 < ord_sum < 115:
        return ord_result

    # Neither is good — prefer position-based if available, else order-based
    if col_template:
        return pos_result
    return ord_result


# ---------------------------------------------------------------------------
# Page pair processing
# ---------------------------------------------------------------------------

def process_page_pair(left_image, right_image, page_num, col_template=None):
    """Process one page pair (left=even, right=odd).

    Returns (records, col_template, is_table5).
    """
    df_left = safe_ocr(left_image)
    df_right = safe_ocr(right_image)

    # Table 5 detection
    header = df_left[(df_left["top"] < 400) & (df_left["left"] < 1500)]
    header_text = " ".join(header.sort_values("left")["text"].values).lower()
    if "kreisfreien" in header_text or "wahlbezirken" in header_text:
        return [], col_template, True

    # Left page: municipality identifiers and totals
    entries = extract_left_page(df_left)
    if not entries:
        if VERBOSE:
            print(f"\n    Page {page_num}: no AGS codes found (WK boundary?)")
        return [], col_template, False

    # Right page: find percentage row y-positions
    pct_row_ys = find_pct_row_ys(df_right)

    # Z% rows = every 2nd percentage row (indices 1, 3, 5, ...)
    z_pct_ys = [pct_row_ys[i] for i in range(1, len(pct_row_ys), 2)]

    # Adjust for WK-total column: if extra Z% row, skip first
    if len(z_pct_ys) == len(entries) + 1:
        z_pct_ys = z_pct_ys[1:]
    elif len(z_pct_ys) > len(entries) + 1:
        z_pct_ys = z_pct_ys[-len(entries):]

    if VERBOSE:
        print(f"\n    Page {page_num}: {len(entries)} AGS, "
              f"{len(pct_row_ys)} pct rows, {len(z_pct_ys)} Z% rows")

    # Extract values from each Z% row
    z_row_values = [extract_row_values(df_right, y) for y in z_pct_ys]

    # Build global column template from first row with exactly 14 values
    if col_template is None:
        for row in z_row_values:
            if len(row) == 14:
                col_template = [x for x, _ in row]
                if VERBOSE:
                    print(f"      Established global template: {len(col_template)} cols")
                break

    # Build records
    records = []
    for i, entry in enumerate(entries):
        vv = entry.get("valid_votes")
        record = {
            "ags": entry["ags"],
            "eligible_voters": entry.get("eligible_voters"),
            "number_voters": entry.get("number_voters"),
            "invalid_votes": entry.get("invalid_votes", 0),
            "valid_votes": vv or 0,
        }

        if vv and vv > 0 and i < len(z_row_values):
            party_pcts = assign_to_parties_hybrid(
                z_row_values[i], col_template
            )
            pct_sum = sum(party_pcts.values())

            # Sanity check: if pct_sum is wildly off, discard this row's data
            if 30 < pct_sum < 115:
                for party in PARTY_ORDER:
                    pct = party_pcts.get(party, 0.0)
                    record[party] = round(pct * vv / 100)
            else:
                for p in PARTY_ORDER:
                    record[p] = 0
                if VERBOSE:
                    print(f"      {entry['ags']}: DISCARDED pct_sum={pct_sum:.1f}%")

            if VERBOSE:
                ps = sum(record.get(p, 0) for p in PARTY_ORDER)
                n_vals = len(z_row_values[i])
                status = "OK" if abs(vv - ps) <= max(5, vv * 0.03) else "BAD"
                print(f"      {entry['ags']}: EV={entry.get('eligible_voters','?')}, "
                      f"VV={vv}, vals={n_vals}, pct={pct_sum:.1f}%, "
                      f"diff={abs(vv-ps)}, {status}")
        else:
            for p in PARTY_ORDER:
                record[p] = 0
            if VERBOSE:
                print(f"      {entry['ags']}: VV={vv or 0} (no Z% data)")

        records.append(record)

    return records, col_template, False


# ---------------------------------------------------------------------------
# Validation
# ---------------------------------------------------------------------------

def validate_record(record):
    """Check vote arithmetic: valid_votes ≈ sum(party votes)."""
    vv = record.get("valid_votes")
    if vv is None or vv == 0:
        return True
    party_sum = sum(record.get(p, 0) or 0 for p in PARTY_ORDER)
    diff = abs(vv - party_sum)
    return diff <= max(5, vv * 0.03)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    global VERBOSE

    test_pairs = None
    if "--test" in sys.argv:
        idx = sys.argv.index("--test")
        if idx + 1 < len(sys.argv):
            test_pairs = int(sys.argv[idx + 1])
            VERBOSE = True

    print(f"PDF: {PDF_PATH}")
    print(f"Output: {OUTPUT_PATH}")
    if test_pairs:
        print(f"TEST MODE: {test_pairs} page pairs")

    all_records = []
    bad_records = []
    col_template = None
    pair_count = 0

    page = TABLE4_START
    stop = False

    while page < TABLE4_END and not stop:
        if test_pairs and pair_count >= test_pairs:
            break

        left_page = page
        right_page = page + 1

        if not VERBOSE:
            sys.stdout.write(f"\r  Pages {left_page}-{right_page}...")
            sys.stdout.flush()

        images = convert_from_path(
            str(PDF_PATH), first_page=left_page, last_page=right_page,
            dpi=300, thread_count=2
        )

        if len(images) < 2:
            page += 2
            pair_count += 1
            continue

        records, col_template, is_t5 = process_page_pair(
            images[0], images[1], left_page, col_template
        )

        if is_t5:
            print(f"\n  Table 5 detected at page {left_page}, stopping.")
            stop = True
            break

        if records:
            for r in records:
                if not validate_record(r):
                    bad_records.append((left_page, r["ags"], r))
            all_records.extend(records)
            if not VERBOSE:
                sys.stdout.write(
                    f"\r  Pages {left_page}-{right_page}: "
                    f"{len(records)} municipalities (total: {len(all_records)})    "
                )
                sys.stdout.flush()

        page += 2
        pair_count += 1

    print(f"\n\nExtracted {len(all_records)} municipality records.")

    # Fix OCR-corrupted AGS codes using sequential context
    valid_kreise = set(f"120{i:02d}" for i in range(51, 55))  # 12051-12054
    valid_kreise.update(f"120{i:02d}" for i in range(60, 74))   # 12060-12073

    ags_fixes = 0
    for idx, rec in enumerate(all_records):
        kreis = rec["ags"][:5]
        if kreis in valid_kreise:
            continue
        # Find the most common valid Kreis among neighbors
        window = all_records[max(0, idx - 5):idx + 6]
        neighbor_kreise = [r["ags"][:5] for r in window
                          if r["ags"][:5] in valid_kreise]
        if neighbor_kreise:
            from collections import Counter
            best_kreis = Counter(neighbor_kreise).most_common(1)[0][0]
            old_ags = rec["ags"]
            rec["ags"] = best_kreis + rec["ags"][5:]
            ags_fixes += 1
            if VERBOSE:
                print(f"  Fixed AGS: {old_ags} → {rec['ags']}")
    if ags_fixes:
        print(f"Fixed {ags_fixes} OCR-corrupted AGS codes.")

    # Deduplicate: keep record with highest party vote sum per AGS
    from collections import defaultdict as dd2
    by_ags = dd2(list)
    for rec in all_records:
        by_ags[rec["ags"]].append(rec)
    deduped = []
    dup_count = 0
    for ags, recs in by_ags.items():
        if len(recs) == 1:
            deduped.append(recs[0])
        else:
            dup_count += 1
            best = max(recs, key=lambda r: sum(r.get(p, 0) or 0 for p in PARTY_ORDER))
            deduped.append(best)
    if dup_count:
        print(f"Deduplicated {dup_count} AGS codes (kept best record).")
    all_records = deduped

    # Aggregate kreisfreie Städte sub-area records to city level
    kfs_kreise = {"12051", "12052", "12053", "12054"}
    kfs_records = [r for r in all_records if r["ags"][:5] in kfs_kreise]
    non_kfs = [r for r in all_records if r["ags"][:5] not in kfs_kreise]

    if kfs_records:
        from collections import defaultdict
        kfs_agg = defaultdict(lambda: {f: 0 for f in FIELD_ORDER})
        for rec in kfs_records:
            city_ags = rec["ags"][:5] + "000"
            kfs_agg[city_ags]["ags"] = city_ags
            for f in FIELD_ORDER[1:]:
                kfs_agg[city_ags][f] += rec.get(f, 0) or 0

        print(f"Aggregated {len(kfs_records)} kreisfreie Stadt sub-records "
              f"into {len(kfs_agg)} city records.")
        for city_ags, agg in sorted(kfs_agg.items()):
            ps = sum(agg.get(p, 0) for p in PARTY_ORDER)
            print(f"  {city_ags}: EV={agg['eligible_voters']:,}, "
                  f"VV={agg['valid_votes']:,}, sum={ps:,}")

        all_records = non_kfs + list(kfs_agg.values())

    if bad_records:
        print(f"\nWARNING: {len(bad_records)} records failed vote arithmetic check:")
        for pnum, ags, rec in bad_records[:30]:
            vv = rec.get("valid_votes", "?")
            ps = sum(rec.get(p, 0) or 0 for p in PARTY_ORDER)
            print(f"  Page {pnum}, AGS {ags}: VV={vv}, sum={ps}, "
                  f"diff={abs((vv or 0) - ps)}")

    if not test_pairs:
        OUTPUT_PATH.parent.mkdir(parents=True, exist_ok=True)
        with open(OUTPUT_PATH, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=FIELD_ORDER)
            writer.writeheader()
            for record in all_records:
                writer.writerow({k: record.get(k, "") for k in FIELD_ORDER})
        print(f"\nWrote {len(all_records)} records to {OUTPUT_PATH}")

    unique_ags = set(r["ags"] for r in all_records)
    print(f"Unique municipalities: {len(unique_ags)}")

    # Statewide totals for validation
    totals = {}
    for field in FIELD_ORDER[1:]:
        totals[field] = sum(r.get(field, 0) or 0 for r in all_records)

    vv = totals["valid_votes"]
    if vv > 0:
        print(f"\nTotals:")
        print(f"  Eligible voters: {totals.get('eligible_voters', 0):,}")
        print(f"  Voters:          {totals.get('number_voters', 0):,}")
        print(f"  Valid votes:     {vv:,}")
        for p in ["spd", "cdu", "pds", "fdp", "gruene_b90", "rep",
                   "dsu", "bfwg", "buerger", "graue", "kpd", "odp", "uwvb", "eb"]:
            if p in totals:
                pct = totals[p] / vv * 100
                print(f"  {p:15s}: {totals[p]:>10,}  ({pct:5.1f}%)")


if __name__ == "__main__":
    main()
