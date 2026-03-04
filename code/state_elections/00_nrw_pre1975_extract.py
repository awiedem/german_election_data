#!/usr/bin/env python3
"""
00_nrw_pre1975_extract.py
Extract NRW pre-1975 Landtagswahl results at Kreis / kreisfreie-Stadt level.
Uses Table 2 from each year's official PDF (kreisfreie Städte und Kreise).

Approach:
  1. Render each page at 300 DPI, run Tesseract TSV for word bounding-boxes.
  2. Detect data rows by y-position density (not garbled Lfd. Nr. values).
  3. Pair rows into (a=total, b=Briefwahl), keep only "a" rows.
  4. Assign Lfd. Nr. sequentially from known geographic unit list.
  5. Parse values using count/percentage alternation pattern.

Output: data/state_elections/raw/Landtagswahlen/Nordrhein-Westfalen/nrw_pre1975_kreis.csv
"""

import os, re, csv, sys
from pathlib import Path
from pdf2image import convert_from_path
import pytesseract
import pandas as pd

HERE = Path(__file__).resolve().parent
RAW_DIR = HERE.parent.parent / "data" / "state_elections" / "raw" / "Landtagswahlen" / "Nordrhein-Westfalen"
OUT_PATH = RAW_DIR / "nrw_pre1975_kreis.csv"

# ══════════════════════════════════════════════════════════════════
# Geographic units — ordered exactly as they appear in Table 2.
# ══════════════════════════════════════════════════════════════════

GEO_1970 = [
    # --- Reg.-Bez. Düsseldorf ---
    (1, "Düsseldorf", "krfr"), (2, "Duisburg", "krfr"),
    (3, "Essen", "krfr"), (4, "Krefeld", "krfr"),
    (5, "Leverkusen", "krfr"), (6, "Mönchengladbach", "krfr"),
    (7, "Mülheim a.d. Ruhr", "krfr"), (8, "Neuss", "krfr"),
    (9, "Oberhausen", "krfr"), (10, "Remscheid", "krfr"),
    (11, "Rheydt", "krfr"), (12, "Solingen", "krfr"),
    (13, "Wuppertal", "krfr"),
    (14, "Kreis Dinslaken", "kreis"), (15, "Kreis Düsseldorf-Mettmann", "kreis"),
    (16, "Kreis Geldern", "kreis"), (17, "Kreis Grevenbroich", "kreis"),
    (18, "Kreis Kempen-Krefeld", "kreis"), (19, "Kreis Kleve", "kreis"),
    (20, "Kreis Moers", "kreis"), (21, "Kreis Rees", "kreis"),
    (22, "Rhein-Wupper-Kreis", "kreis"),
    (23, "Reg.-Bez. Düsseldorf", "agg"),
    (24, "davon Krfr. Städte (D)", "agg"), (25, "davon Kreise (D)", "agg"),
    # --- Reg.-Bez. Köln ---
    (26, "Bonn", "krfr"), (27, "Köln", "krfr"),
    (28, "Kreis Bergheim (Erft)", "kreis"), (29, "Kreis Euskirchen", "kreis"),
    (30, "Kreis Köln", "kreis"), (31, "Oberbergischer Kreis", "kreis"),
    (32, "Rheinisch-Bergischer Kreis", "kreis"), (33, "Rhein-Sieg-Kreis", "kreis"),
    (34, "Reg.-Bez. Köln", "agg"),
    (35, "davon Krfr. Städte (K)", "agg"), (36, "davon Kreise (K)", "agg"),
    # --- Reg.-Bez. Aachen ---
    (37, "Aachen", "krfr"),
    (38, "Kreis Aachen", "kreis"), (39, "Kreis Düren", "kreis"),
    (40, "Kreis Erkelenz", "kreis"), (41, "Kreis Jülich", "kreis"),
    (42, "Kreis Monschau", "kreis"), (43, "Kreis Schleiden", "kreis"),
    (44, "Selfkantkreis Geilenkirchen-Heinsberg", "kreis"),
    (45, "Reg.-Bez. Aachen", "agg"),
    (46, "davon Krfr. Städte (AC)", "agg"), (47, "davon Kreise (AC)", "agg"),
    # --- Reg.-Bez. Münster ---
    (48, "Bocholt", "krfr"), (49, "Bottrop", "krfr"),
    (50, "Gelsenkirchen", "krfr"), (51, "Gladbeck", "krfr"),
    (52, "Münster", "krfr"), (53, "Recklinghausen", "krfr"),
    (54, "Kreis Ahaus", "kreis"), (55, "Kreis Beckum", "kreis"),
    (56, "Kreis Borken", "kreis"), (57, "Kreis Coesfeld", "kreis"),
    (58, "Kreis Lüdinghausen", "kreis"), (59, "Kreis Münster", "kreis"),
    (60, "Kreis Recklinghausen", "kreis"), (61, "Kreis Steinfurt", "kreis"),
    (62, "Kreis Tecklenburg", "kreis"), (63, "Kreis Warendorf", "kreis"),
    (64, "Reg.-Bez. Münster", "agg"),
    (65, "davon Krfr. Städte (MS)", "agg"), (66, "davon Kreise (MS)", "agg"),
    # --- Reg.-Bez. Detmold ---
    (67, "Bielefeld", "krfr"),
    (68, "Kreis Bielefeld", "kreis"), (69, "Kreis Büren", "kreis"),
    (70, "Kreis Detmold", "kreis"), (71, "Kreis Halle (Westf.)", "kreis"),
    (72, "Kreis Herford", "kreis"), (73, "Kreis Höxter", "kreis"),
    (74, "Kreis Lemgo", "kreis"), (75, "Kreis Lübbecke", "kreis"),
    (76, "Kreis Minden", "kreis"), (77, "Kreis Paderborn", "kreis"),
    (78, "Kreis Warburg", "kreis"), (79, "Kreis Wiedenbrück", "kreis"),
    (80, "Reg.-Bez. Detmold", "agg"),
    (81, "davon Krfr. Städte (DT)", "agg"), (82, "davon Kreise (DT)", "agg"),
    # --- Reg.-Bez. Arnsberg ---
    (83, "Bochum", "krfr"), (84, "Castrop-Rauxel", "krfr"),
    (85, "Dortmund", "krfr"), (86, "Hagen", "krfr"),
    (87, "Hamm", "krfr"), (88, "Herne", "krfr"),
    (89, "Iserlohn", "krfr"), (90, "Lünen", "krfr"),
    (91, "Wanne-Eickel", "krfr"), (92, "Wattenscheid", "krfr"),
    (93, "Witten", "krfr"),
    (94, "Kreis Arnsberg", "kreis"), (95, "Kreis Brilon", "kreis"),
    (96, "Ennepe-Ruhr-Kreis", "kreis"), (97, "Kreis Iserlohn", "kreis"),
    (98, "Kreis Lippstadt", "kreis"), (99, "Kreis Lüdenscheid", "kreis"),
    (100, "Kreis Meschede", "kreis"), (101, "Kreis Olpe", "kreis"),
    (102, "Kreis Siegen", "kreis"), (103, "Kreis Soest", "kreis"),
    (104, "Kreis Unna", "kreis"), (105, "Kreis Wittgenstein", "kreis"),
    (106, "Reg.-Bez. Arnsberg", "agg"),
    (107, "davon Krfr. Städte (AR)", "agg"), (108, "davon Kreise (AR)", "agg"),
    # --- NRW totals ---
    (109, "Nordrhein-Westfalen", "agg"),
    (110, "davon Krfr. Städte (NRW)", "agg"), (111, "davon Kreise (NRW)", "agg"),
]

GEO_LOOKUP_1970 = {nr: (name, typ) for nr, name, typ in GEO_1970}

# ══════════════════════════════════════════════════════════════════
# Election configurations
# ══════════════════════════════════════════════════════════════════

ELECTIONS = {
    1970: {
        "pdf": "Nordrhein-Westfalen_1970_Landtagswahl.pdf",
        # (left_page, right_page, lfd_start, lfd_end) — pages are 1-indexed
        "page_pairs": [
            (26, 27, 1, 25),    # Reg.-Bez. Düsseldorf
            (28, 29, 26, 47),   # Köln + Aachen
            (30, 31, 48, 66),   # Münster
            (32, 33, 67, 82),   # Detmold
            (34, 35, 83, 111),  # Arnsberg + NRW totals
        ],
        "parties": ["spd", "cdu", "fdp", "zentrum", "uap", "dkp", "npd"],
        "geo_lookup": GEO_LOOKUP_1970,
        "known_totals": {
            "valid_votes": 8588041,
            "spd_pct": 46.1, "cdu_pct": 46.3, "fdp_pct": 5.5,
        },
    },
}

# ══════════════════════════════════════════════════════════════════
# OCR helpers
# ══════════════════════════════════════════════════════════════════

def extract_tsv(pdf_path, page_num, dpi=300):
    """Render page at `dpi` and return Tesseract TSV as DataFrame."""
    images = convert_from_path(str(pdf_path), first_page=page_num,
                               last_page=page_num, dpi=dpi)
    tsv = pytesseract.image_to_data(
        images[0], lang="deu", config="--oem 3 --psm 6",
        output_type=pytesseract.Output.DATAFRAME,
    )
    w = tsv[(tsv.conf > 10) & tsv.text.notna()].copy()
    w["text"] = w["text"].astype(str).str.strip()
    w = w[w["text"] != ""]
    w["center_x"] = w["left"] + w["width"] / 2
    w["center_y"] = w["top"] + w["height"] / 2
    w["right"] = w["left"] + w["width"]
    return w


def ocr_fix_numeric(text):
    """Fix common OCR mis-reads in numeric context."""
    for old, new in [
        ("ı", "1"), ("İ", "1"), ("|", "1"), ("!", "1"),
        ("O", "0"), ("Q", "0"),
        ("&", "8"), ("$", "8"),
        ("?", "7"),
        ("}", "5"), ("{", "6"),
        (";", "5"), (":", "3"),
    ]:
        text = text.replace(old, new)
    # In purely-numeric context
    if re.match(r"^[\dl oO,.\-SsZzTu&;:]+$", text):
        for old, new in [("l", "1"), ("o", "0"), ("S", "5"), ("s", "5"),
                         ("Z", "2"), ("z", "2"), ("T", "7"), ("u", "4")]:
            text = text.replace(old, new)
    return text


def parse_int(text):
    """Parse a numeric string to int.  Return 0 for dashes / empty."""
    if not text or text.strip() in ("-", "--", "---", ".", ",", "—", "–", ""):
        return 0
    text = ocr_fix_numeric(text)
    text = re.sub(r"[^\d]", "", text)
    if not text:
        return 0
    return int(text)


def is_percentage(text):
    """Percentage values: '46,0', '7,1', '0,3', or OCR variants like '46.0', '7,ı'."""
    t = text.strip()
    # Standard: digits, comma/period, 1 digit
    if re.match(r"^-?\d{1,3}[,.]\d$", t):
        return True
    # OCR variant: comma/period + misread digit
    if re.match(r"^-?\d{1,3}[,.][\dıİl|!oO]$", t):
        return True
    # Period variant with extra digit (e.g. "46.01" garbled)
    if re.match(r"^-?\d{1,3}[,.]\d{1,2}$", t) and len(t) <= 6:
        return True
    return False


def is_dash(text):
    """Check if token is a dash (zero value)."""
    return text.strip() in ("-", "--", "---", "—", "–")


# ══════════════════════════════════════════════════════════════════
# Row detection — find data rows by y-position clustering
# ══════════════════════════════════════════════════════════════════

def find_data_rows(words_df, y_header_end=550, pair_gap=50):
    """
    Find "a"-row y-positions on a page.

    Strategy:
    1. Bin words by y (20px bins).
    2. Keep bins with ≥ 4 tokens.
    3. Merge nearby rows (< 15px apart) that are the same row split across bins.
    4. Pair consecutive rows into (a, b) where gap < pair_gap.
    5. Return only the "a" row y-positions (first of each pair).
    """
    data = words_df[words_df["top"] > y_header_end].copy()
    if data.empty:
        return []

    # Bin by y (20px bins)
    data["y_bin"] = (data["top"] / 20).astype(int)
    bin_counts = data.groupby("y_bin").size()
    busy_bins = bin_counts[bin_counts >= 4].index

    # Median y per bin
    raw_ys = []
    for yb in sorted(busy_bins):
        group = data[data["y_bin"] == yb]
        raw_ys.append(group["top"].median())
    raw_ys.sort()

    # Merge nearby rows (within 15px) — same row split across bin boundaries
    row_ys = []
    i = 0
    while i < len(raw_ys):
        cluster = [raw_ys[i]]
        while i + 1 < len(raw_ys) and raw_ys[i + 1] - cluster[0] < 15:
            cluster.append(raw_ys[i + 1])
            i += 1
        row_ys.append(sum(cluster) / len(cluster))
        i += 1

    # Pair into (a, b): "a" and "b" rows are ~30px apart,
    # different entries are ~60-90px apart.
    a_rows = []
    i = 0
    while i < len(row_ys):
        a_rows.append(row_ys[i])
        if i + 1 < len(row_ys) and row_ys[i + 1] - row_ys[i] < pair_gap:
            i += 2
        else:
            i += 1
    return a_rows


# ══════════════════════════════════════════════════════════════════
# Token merging
# ══════════════════════════════════════════════════════════════════

def merge_thousands(tokens, max_gap=40):
    """
    Merge adjacent tokens that are fragments of a thousands-separated
    number (e.g. "153" + "772" → "153772").
    Each token: (left, text, width).
    """
    if not tokens:
        return []
    merged = [list(tokens[0])]
    for tok in tokens[1:]:
        prev = merged[-1]
        actual_gap = tok[0] - (prev[0] + prev[2])  # tok.left - prev.right

        prev_digits = re.sub(r"[^\d]", "", ocr_fix_numeric(prev[1]))
        tok_digits = re.sub(r"[^\d]", "", ocr_fix_numeric(tok[1]))

        # Merge if: previous has 1-3 digits, current has exactly 3 digits,
        # gap is small, and neither looks like a percentage.
        if (prev_digits and 1 <= len(prev_digits) <= 3
                and len(tok_digits) == 3 and tok_digits.isdigit()
                and 0 < actual_gap < max_gap
                and not is_percentage(prev[1]) and not is_percentage(tok[1])
                and not is_dash(prev[1]) and not is_dash(tok[1])):
            prev[1] = prev[1] + tok[1]
            prev[2] = tok[0] + tok[2] - prev[0]  # extend width
        else:
            merged.append(list(tok))
    return merged


# ══════════════════════════════════════════════════════════════════
# Right-page extraction  (party votes)
# ══════════════════════════════════════════════════════════════════

def extract_right_page(words_df, parties, lfd_start, n_rows):
    """
    From a right-side Table-2 page, extract vote counts.

    Returns dict:  lfd_nr → {valid_votes, spd, cdu, ...}
    """
    # Right-edge cutoff: exclude Lfd. Nr. column (x > 2100 at 300 DPI)
    DATA_RIGHT = 2100

    # Find "a" row y-positions
    a_rows = find_data_rows(words_df)
    print(f"    Detected {len(a_rows)} 'a' rows (expected {n_rows})")

    if len(a_rows) < n_rows:
        print(f"    WARNING: fewer rows detected than expected!")
    if len(a_rows) > n_rows:
        print(f"    WARNING: more rows detected ({len(a_rows)}) than expected ({n_rows}), trimming")
        a_rows = a_rows[:n_rows]

    n_parties = len(parties)
    results = {}

    for idx, y_center in enumerate(a_rows):
        lfd_nr = lfd_start + idx
        y_band = 15  # ± pixels

        row_words = words_df[
            (words_df["top"] > y_center - y_band)
            & (words_df["top"] < y_center + y_band)
            & (words_df["left"] < DATA_RIGHT)
        ].sort_values("left")

        if row_words.empty:
            results[lfd_nr] = {"_flag": "empty row"}
            continue

        # Build tokens: (left, text, width)
        tokens = [(int(r["left"]), str(r["text"]), int(r["width"]))
                  for _, r in row_words.iterrows()]

        # Merge thousands-separated fragments
        merged = merge_thousands(tokens, max_gap=40)

        # Parse the value sequence.
        # Expected pattern: valid_votes, (party1_count, party1_pct), (party2_count, party2_pct), ...
        # Percentages have comma; counts are integers; dashes = 0.
        values = []  # list of (type, value) where type = 'count', 'pct', 'dash'
        for _, txt, _ in merged:
            txt_clean = txt.strip()
            if is_dash(txt_clean):
                values.append(("dash", 0))
            elif is_percentage(txt_clean):
                values.append(("pct", txt_clean))
            else:
                n = parse_int(txt_clean)
                if n is not None:
                    values.append(("count", n))

        # Extract counts only (skip percentages).
        # The sequence after filtering should be:
        # [valid_votes, spd, cdu, fdp, zentrum, uap, dkp, npd]
        # But dashes count as values too (they have both count=0 and pct="-").
        # Actually, a dash in the sequence represents both the count AND pct.
        # So we can't just filter by type — we need to reconstruct the pattern.

        # Better approach: walk the values, expecting alternating count/pct for parties.
        # Position 0: valid_votes (count)
        # Position 1: spd_count, Position 2: spd_pct
        # Position 3: cdu_count, Position 4: cdu_pct
        # etc.
        # A dash occupies one position (counts as both count and pct? or just count?)

        # Simplest: just extract counts (non-percentage values) in order.
        # A dash is a count (=0). A percentage is skipped.
        # This works IF percentages are always detected correctly.
        counts = []
        for typ, val in values:
            if typ in ("count", "dash"):
                counts.append(val)
            # "pct" is skipped

        row_data = {}
        expected_n = 1 + n_parties  # valid_votes + parties
        if len(counts) >= expected_n:
            row_data["valid_votes"] = counts[0]
            for i, p in enumerate(parties):
                row_data[p] = counts[1 + i]
            if len(counts) > expected_n:
                row_data["_extra_counts"] = counts[expected_n:]
        elif len(counts) > 0:
            row_data["valid_votes"] = counts[0]
            for i, p in enumerate(parties):
                row_data[p] = counts[1 + i] if 1 + i < len(counts) else 0
            row_data["_flag"] = f"only {len(counts)} counts (expected {expected_n})"
        else:
            row_data["_flag"] = "no counts parsed"

        results[lfd_nr] = row_data

    return results


# ══════════════════════════════════════════════════════════════════
# Left-page extraction  (eligible voters, number voters, invalid)
# ══════════════════════════════════════════════════════════════════

def extract_left_page(words_df, lfd_start, n_rows):
    """
    From a left-side Table-2 page, extract metadata per row.
    Returns dict:  lfd_nr → {eligible_voters, number_voters, invalid_votes}
    """
    # The left page has names in the left half and numbers in the right half.
    # We detect rows the same way as the right page.
    DATA_LEFT_START = 600   # Numbers start after ~600px (names are to the left)

    a_rows = find_data_rows(words_df)
    print(f"    Detected {len(a_rows)} 'a' rows on left page (expected {n_rows})")

    if len(a_rows) > n_rows:
        a_rows = a_rows[:n_rows]

    results = {}
    for idx, y_center in enumerate(a_rows):
        lfd_nr = lfd_start + idx
        y_band = 15

        # Extract only the numeric portion (right half of left page)
        row_words = words_df[
            (words_df["top"] > y_center - y_band)
            & (words_df["top"] < y_center + y_band)
            & (words_df["left"] > DATA_LEFT_START)
        ].sort_values("left")

        if row_words.empty:
            results[lfd_nr] = {}
            continue

        tokens = [(int(r["left"]), str(r["text"]), int(r["width"]))
                  for _, r in row_words.iterrows()]
        merged = merge_thousands(tokens, max_gap=40)

        # Extract count values (skip percentages and text)
        nums = []
        for _, txt, _ in merged:
            if is_percentage(txt):
                continue
            if is_dash(txt):
                nums.append(0)
                continue
            cleaned = re.sub(r"[^\d]", "", ocr_fix_numeric(txt))
            if cleaned and len(cleaned) >= 3:  # minimum 3 digits for meaningful counts
                nums.append(int(cleaned))

        # Expected: eligible_voters, number_voters, [turnout% skipped], invalid_votes
        row_data = {}
        if len(nums) >= 3:
            row_data["eligible_voters"] = nums[0]
            row_data["number_voters"] = nums[1]
            row_data["invalid_votes"] = nums[2]
        elif len(nums) == 2:
            row_data["eligible_voters"] = nums[0]
            row_data["number_voters"] = nums[1]
        elif len(nums) == 1:
            row_data["eligible_voters"] = nums[0]

        results[lfd_nr] = row_data

    return results


# ══════════════════════════════════════════════════════════════════
# Post-processing / validation
# ══════════════════════════════════════════════════════════════════

def validate_row(row, parties):
    """Check internal consistency."""
    issues = []
    vv = row.get("valid_votes")
    if vv is None or vv == 0:
        issues.append("missing valid_votes")
        return issues

    party_sum = sum(row.get(p, 0) or 0 for p in parties)

    if party_sum > 0 and abs(vv - party_sum) > max(vv * 0.05, 50):
        issues.append(f"VV={vv} vs sum={party_sum} (diff={vv - party_sum})")

    for p in parties:
        val = row.get(p, 0) or 0
        if val > vv * 1.1:
            issues.append(f"{p}={val} > VV={vv}")

    ev = row.get("eligible_voters")
    nv = row.get("number_voters")
    if ev and nv and nv > ev * 1.05:
        issues.append(f"voters={nv} > eligible={ev}")

    return issues


def postprocess_row(row, parties):
    """Auto-fix common OCR errors."""
    vv = row.get("valid_votes", 0) or 0
    party_sum = sum(row.get(p, 0) or 0 for p in parties)

    if vv == 0 and party_sum > 0:
        row["valid_votes"] = party_sum
        return

    # valid_votes ~10× party_sum → prepended digit
    if party_sum > 0 and vv > party_sum * 5:
        vv_str = str(vv)
        if len(vv_str) > 1:
            trimmed = int(vv_str[1:])
            if abs(trimmed - party_sum) < party_sum * 0.05:
                row["valid_votes"] = trimmed
                vv = trimmed

    # Any party > valid_votes → probably ×10
    for p in parties:
        val = row.get(p, 0) or 0
        if val > vv and vv > 0:
            if abs(val // 10 - party_sum // len(parties)) < party_sum:
                row[p] = val // 10

    # Recompute valid_votes from sum if they diverge > 5%
    new_sum = sum(row.get(p, 0) or 0 for p in parties)
    vv = row.get("valid_votes", 0) or 0
    if new_sum > 0 and abs(vv - new_sum) > max(vv * 0.05, 100):
        row["valid_votes"] = new_sum


# ══════════════════════════════════════════════════════════════════
# Main
# ══════════════════════════════════════════════════════════════════

def extract_year(year):
    cfg = ELECTIONS[year]
    pdf_path = RAW_DIR / cfg["pdf"]
    parties = cfg["parties"]
    geo = cfg["geo_lookup"]

    print(f"\n{'='*60}")
    print(f"NRW {year} — {cfg['pdf']}")
    print(f"{'='*60}")

    all_right = {}
    all_left = {}

    for left_pg, right_pg, lfd_start, lfd_end in cfg["page_pairs"]:
        n_rows = lfd_end - lfd_start + 1
        print(f"\n  Pages {left_pg}/{right_pg}  Lfd. {lfd_start}-{lfd_end} ({n_rows} items)")

        # Right page
        print(f"  OCR right page {right_pg}...")
        rw = extract_tsv(pdf_path, right_pg)
        rd = extract_right_page(rw, parties, lfd_start, n_rows)
        all_right.update(rd)

        # Left page
        print(f"  OCR left page {left_pg}...")
        lw = extract_tsv(pdf_path, left_pg)
        ld = extract_left_page(lw, lfd_start, n_rows)
        all_left.update(ld)

    # Combine
    records = []
    flagged = 0
    for nr, (name, typ) in sorted(geo.items()):
        if typ == "agg":
            continue

        row = {"name": name, "type": typ, "election_year": year}

        if nr in all_right:
            row.update(all_right[nr])
        else:
            row["_flag"] = f"missing right (Lfd.{nr})"

        if nr in all_left:
            left = all_left[nr]
            for k in ("eligible_voters", "number_voters", "invalid_votes"):
                if k in left and k not in row:
                    row[k] = left[k]

        # Compute invalid_votes if missing
        if "invalid_votes" not in row and row.get("number_voters") and row.get("valid_votes"):
            row["invalid_votes"] = row["number_voters"] - row["valid_votes"]

        postprocess_row(row, parties)
        issues = validate_row(row, parties)
        if issues:
            row["_issues"] = "; ".join(issues)
            flagged += 1
            print(f"    ISSUE Lfd.{nr} {name}: {'; '.join(issues)}")

        records.append(row)

    # Statewide validation
    print(f"\n  --- Statewide totals ---")
    total_vv = sum(r.get("valid_votes", 0) or 0 for r in records)
    print(f"  Valid votes: {total_vv:>12,}")
    for p in parties:
        tp = sum(r.get(p, 0) or 0 for r in records)
        pct = tp / total_vv * 100 if total_vv > 0 else 0
        print(f"  {p:>10s}: {tp:>12,}  ({pct:.1f}%)")

    kt = cfg.get("known_totals", {})
    if "valid_votes" in kt:
        diff_pct = abs(total_vv - kt["valid_votes"]) / kt["valid_votes"] * 100
        print(f"  Expected VV: {kt['valid_votes']:,}  (diff: {diff_pct:.1f}%)")

    print(f"\n  Rows: {len(records)}, flagged: {flagged}")
    return records


def main():
    all_records = []
    for year in sorted(ELECTIONS.keys()):
        records = extract_year(year)
        all_records.extend(records)

    if not all_records:
        print("No records!")
        return

    # Collect all party columns across years
    all_parties = set()
    for cfg in ELECTIONS.values():
        all_parties.update(cfg["parties"])
    all_parties = sorted(all_parties)

    fieldnames = ["name", "type", "election_year",
                  "eligible_voters", "number_voters", "invalid_votes", "valid_votes"]
    fieldnames.extend(all_parties)

    clean = [{k: v for k, v in r.items() if not k.startswith("_")} for r in all_records]

    print(f"\nWriting {len(clean)} records to {OUT_PATH}")
    with open(OUT_PATH, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        w.writeheader()
        w.writerows(clean)
    print("Done!")


if __name__ == "__main__":
    main()
