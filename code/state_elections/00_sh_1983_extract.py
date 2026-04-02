#!/usr/bin/env python3
"""
Extract municipality-level results from the Schleswig-Holstein 1983
Landtagswahl PDF (text-layer based, not OCR).

PDF structure:
  - Pages 17-116 (0-indexed) contain municipality data as two-page spreads
  - Odd 0-indexed pages (17,19,21,...) = LEFT = metadata
    (NR, NAME, eligible_voters, voters, invalid_votes)
  - Even 0-indexed pages (18,20,22,...) = RIGHT = votes
    (NR, valid_votes, CDU, SPD, FDP, SSW, DKP, DGL, GR, FP, FSU, EB)
  - Two rows per municipality: absolute values, then percentages (skip pct)
  - Municipality codes: "KK GGG" → AGS "01KKK GGG" (SH = state 01)

Strategy:
  - Right pages: adaptive column detection per page using header word positions
  - Left pages: position-based extraction, matched to right pages by order
  - Both: use word-level extraction with (x, y) positions

Output: data/state_elections/raw/Landtagswahlen/Schleswig-Holstein/sh_1983_extracted.csv
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
    "Schleswig-Holstein/Schleswig-Holstein_1983_Landtagswahl.pdf"
)

OUT_PATH = Path(__file__).resolve().parents[2] / (
    "data/state_elections/raw/Landtagswahlen/"
    "Schleswig-Holstein/sh_1983_extracted.csv"
)

PAGE_START = 17
PAGE_END = 116

PARTY_ORDER = ["cdu", "spd", "fdp", "ssw", "dkp", "dgl", "gruene", "fp",
               "fsu", "einzelbewerber"]

# Header patterns for detecting party columns on right pages (garbled OCR)
# Each pattern: (party_name, regex matching garbled header word)
PARTY_HEADER_PATTERNS = [
    ("cdu",    re.compile(r'^[cC][dDoO][uU]$', re.I)),
    ("spd",    re.compile(r'^[sS][pP][dDoO]$', re.I)),
    ("fdp",    re.compile(r'^[fF][dDoO][pP]$', re.I)),
    ("ssw",    re.compile(r'^[sS][sS][wWvV]$', re.I)),
    ("dkp",    re.compile(r'^[dDoO][iIkK][cCpP]$', re.I)),
    ("dgl",    re.compile(r'^[dDoO][&gG][lL]$', re.I)),
    ("gruene", re.compile(r'^[&gG6][rR]$', re.I)),
    ("fp",     re.compile(r'^[fF][pP]$', re.I)),
    ("fsu",    re.compile(r'^[fF][sS][uU]$', re.I)),
    ("einzelbewerber", re.compile(r'^(EINZEL|[bB][eE])', re.I)),
]

# Patterns for header words on left pages
LEFT_HEADER_WORDS = [
    ("eligible_voters", re.compile(r'SA.*T$|INSGE', re.I)),  # SAMT / INSGESAMT
    ("number_voters",   re.compile(r'SA.*T$|INSGE', re.I)),  # second INSGESAMT
    ("invalid_votes",   re.compile(r'NEN$|TI.*GE|UNGUEL', re.I)),  # STIMMEN / TIGE
]

FIELD_ORDER = ["ags", "eligible_voters", "number_voters", "invalid_votes",
               "valid_votes"] + PARTY_ORDER


# --- Helpers ------------------------------------------------------------------

def fix_code(raw: str) -> str:
    """Fix OCR artifacts in municipality codes."""
    t = raw
    for old, new in [("C", "0"), ("O", "0"), ("G", "0"), ("L", "0"),
                     ("l", "1"), ("!", "1"), ("~", ""), ("'", ""),
                     (":", ""), ("·", ""), ("(", "0"), ("<", "4"),
                     (";", "5"), ("r", "0"), ("f", "0"), ("t", "0"),
                     (".", ""), (">", "")]:
        t = t.replace(old, new)
    # Keep only digits
    t = re.sub(r'[^0-9]', '', t)
    return t


def to_ags(kreis: str, gemeinde: str) -> str:
    """Convert Kreis + Gemeinde to 8-digit AGS."""
    k = kreis.zfill(3)
    g = gemeinde.zfill(3)
    return f"01{k}{g}"


def parse_number(text: str) -> int | None:
    """Parse an integer from text, handling OCR artifacts."""
    t = text.strip()
    # Skip percentage values (contain comma)
    if ',' in t:
        return None
    t = t.replace(".", "").replace(" ", "")
    t = t.replace("~", "").replace("!", "1").replace("?", "7")
    t = t.replace("l", "1")
    # Handle "lt" and "tt" → "4"
    t = re.sub(r'[lt]t', '4', t)
    t = re.sub(r't([0-9])', r'4\1', t)
    t = re.sub(r'[^0-9]', '', t)
    if not t:
        return None
    try:
        return int(t)
    except ValueError:
        return None


def is_integer_word(text: str) -> bool:
    """Check if a word looks like an integer (not a percentage)."""
    t = text.strip()
    if ',' in t:
        return False
    if t in ('*', '.', '-', '—', '–'):
        return False
    # Must contain at least one digit
    return bool(re.search(r'\d', t))


# --- Right page extraction ----------------------------------------------------

def detect_right_columns(page):
    """Detect party column positions from the header row of a right page.
    Returns dict: {party_name: x0_center, ...} and the NR code x-range.
    """
    words = page.extract_words()
    if not words:
        return None, None

    # Find header words: short words at y < 130 that match party patterns
    header_words = [w for w in words if w["top"] < 130]

    party_positions = {}
    for w in header_words:
        t = w["text"].strip()
        for party, pattern in PARTY_HEADER_PATTERNS:
            if pattern.match(t) and party not in party_positions:
                party_positions[party] = w["x0"]
                break

    if len(party_positions) < 5:
        return None, None

    return party_positions, words


def extract_right_page(page, last_kreis):
    """Extract municipality vote data from a right page.
    Returns: list of (code_tuple, data_dict) in page order, and last kreis code.
    """
    party_positions, words = detect_right_columns(page)
    if party_positions is None:
        return [], last_kreis

    # Build column boundaries from party positions
    # Sort parties by x-position
    sorted_parties = sorted(party_positions.items(), key=lambda x: x[1])

    # The valid_votes column is before the first party
    first_party_x = sorted_parties[0][1]

    # NR codes are always at x0 < 40; VV starts at x0 ≈ 50+
    # Use a fixed code_limit since NR code position is consistent
    code_limit_fixed = 45

    # Build boundaries: midpoint between adjacent party columns
    col_boundaries = []
    # valid_votes column: from code_limit to just before first party
    # CDU values start at x0 ≈ first_party_x - 12, so use -15 as boundary
    vv_boundary = first_party_x - 15
    col_boundaries.append(("valid_votes", vv_boundary))

    for i, (party, x0) in enumerate(sorted_parties):
        if i + 1 < len(sorted_parties):
            next_x0 = sorted_parties[i + 1][1]
            boundary = (x0 + next_x0) / 2
        else:
            boundary = 9999
        col_boundaries.append((party, boundary))

    # Merge character-level splits before grouping
    words = _merge_nearby_words(words, x_gap=5, y_tolerance=3)
    # Group words by y-position
    words.sort(key=lambda w: (w["top"], w["x0"]))
    # Use tight y_tolerance to avoid merging data + percentage rows
    rows = _group_words(words, y_tolerance=3)

    results = []
    for row_words in rows:
        # Skip rows at y < 120 (headers)
        if row_words[0]["top"] < 120:
            continue

        # Filter out percentage words from the row:
        # keep only non-comma integer-like words for data extraction
        row_texts = [w["text"] for w in row_words]

        # Check 1: if ALL integer-like values are small and there's a "*",
        # this is a pure percentage row → skip
        has_star = any(t == "*" for t in row_texts)
        comma_count = sum(1 for t in row_texts if ',' in t)

        if has_star and comma_count >= 2:
            continue

        if comma_count >= 3:
            continue

        # Check 2: first integer looks like "100,0" variant
        first_int = None
        for t in row_texts:
            if re.match(r'\d', t):
                first_int = t
                break
        if first_int and re.match(
                r'^1[0-9OoGgCc][0-9OoGgCc],?[0-9OoGgCc]$'
                r'|^10010$|^1[0-9OoGgCc]{3}$'
                r'|^100[,.]', first_int):
            continue

        # Check 3: skip rows that look like stray percentage fragments
        int_vals = []
        for w in row_words:
            if w["x0"] >= code_limit_fixed:
                v = parse_number(w["text"])
                if v is not None:
                    int_vals.append(v)
        if int_vals and max(int_vals) < 10 and len(int_vals) <= 3:
            continue

        # Skip page number rows
        row_text = " ".join(row_texts)
        if re.match(r'^-\s*\d+\s*-$', row_text.strip()):
            continue

        # Look for NR code by position (x0 < 45, where codes always are)
        code_words = [w for w in row_words if w["x0"] < code_limit_fixed
                      and is_integer_word(w["text"])]

        # Extract kreis and gemeinde from code words
        kreis = None
        gemeinde = None
        for w in code_words:
            t = w["text"].strip()
            fixed = fix_code(t)
            if len(fixed) == 2 and kreis is None:
                kreis = fixed
            elif len(fixed) == 3 and gemeinde is None:
                gemeinde = fixed
            elif len(fixed) >= 3 and gemeinde is None:
                # Try taking last 3 digits
                gemeinde = fixed[-3:]

        if gemeinde is None:
            continue

        if kreis is None:
            kreis = last_kreis
        else:
            last_kreis = kreis

        if kreis is None:
            continue

        # Extract data values using column positions
        data_words = [w for w in row_words if w["x0"] >= code_limit_fixed - 5
                      and is_integer_word(w["text"])]

        data = {}
        for w in data_words:
            val = parse_number(w["text"])
            if val is None:
                continue
            x0 = w["x0"]
            # Find which column this belongs to
            prev_boundary = 0
            for col_name, boundary in col_boundaries:
                if x0 < boundary:
                    if col_name not in data:
                        data[col_name] = val
                    break
                prev_boundary = boundary

        results.append(((kreis, gemeinde), data))

    return results, last_kreis


# --- Left page extraction -----------------------------------------------------

def extract_left_page(page):
    """Extract metadata (EV, NV, IV) from a left page.
    Returns: list of {eligible_voters, number_voters, invalid_votes} in page order.
    """
    words = page.extract_words()
    if not words:
        return []

    # Merge character-level splits
    words = _merge_nearby_words(words, x_gap=5, y_tolerance=3)

    # Find column positions adaptively
    # Look for header words at y < 120
    header_words = [w for w in words if w["top"] < 120]

    # Find the "INSGESAMT" or "SAMT" positions for eligible_voters and voters
    # On left pages, the layout has:
    #   EV group (x ≈ 215-245), EV_without (≈260-285), EV_with (≈300-320)
    #   NV group (x ≈ 370-400), NV_with (≈415-440), IV (≈525-550)
    # These vary by page. Find them from the header.

    # Use all numeric words at y > 120 that look like integers
    words.sort(key=lambda w: (w["top"], w["x0"]))

    # Strategy: find all "100,0" words (percentage row markers)
    # Then look just ABOVE each 100,0 for the data row
    pct_markers = []
    for w in words:
        t = w["text"].strip()
        if t in ("100,0", "10C,O", "1OO,O", "10C,0", "100,('", "1GO,O",
                 "100'!'0", "10c,0", "1CO,O", "100", "10010"):
            if w["top"] > 120:
                pct_markers.append(w)

    results = []
    used_ys = set()

    for marker in pct_markers:
        marker_y = marker["top"]
        marker_x1 = marker["x1"]

        # Find data words just ABOVE this percentage marker (5-20px above)
        # at the same x-range (eligible_voters column)
        ev_x_range = (marker_x1 - 30, marker_x1 + 5)

        # Look for integer word above the marker in the EV column
        ev_word = None
        for w in words:
            if (marker_y - 18 < w["top"] < marker_y - 3
                    and ev_x_range[0] < w["x1"] < ev_x_range[1]
                    and is_integer_word(w["text"])
                    and ',' not in w["text"]):
                ev_word = w
                break

        if ev_word is None:
            continue

        data_y = ev_word["top"]
        if any(abs(data_y - uy) < 10 for uy in used_ys):
            continue
        used_ys.add(data_y)

        ev_val = parse_number(ev_word["text"])

        # Find number_voters: integer word at similar y, x shifted right
        # NV is typically at x1 ≈ marker_x1 + 150 (offset ~150px right)
        nv_val = None
        for w in words:
            if (abs(w["top"] - data_y) < 10
                    and marker_x1 + 100 < w["x1"] < marker_x1 + 180
                    and is_integer_word(w["text"])
                    and ',' not in w["text"]):
                val = parse_number(w["text"])
                if val is not None:
                    nv_val = val
                    break

        # Find invalid_votes: integer word at similar y, rightmost column
        # IV is typically at x1 ≈ marker_x1 + 300
        iv_val = 0
        for w in words:
            if (abs(w["top"] - data_y) < 10
                    and w["x1"] > marker_x1 + 260
                    and is_integer_word(w["text"])
                    and ',' not in w["text"]):
                val = parse_number(w["text"])
                if val is not None:
                    iv_val = val
                    break

        if ev_val is not None and ev_val > 5:
            results.append({
                "eligible_voters": ev_val,
                "number_voters": nv_val,
                "invalid_votes": iv_val,
                "_y": data_y,
            })

    # Sort by y position
    results.sort(key=lambda r: r["_y"])
    return results


# --- Word merging & grouping --------------------------------------------------

def _merge_nearby_words(words, x_gap=5, y_tolerance=3):
    """Merge adjacent single-character words into combined words.

    On some PDF pages, pdfplumber splits numbers into individual characters
    (e.g., "1541" becomes four words: "1", "5", "4", "1" at ~4px gaps).
    This function merges ONLY when both adjacent words are single characters
    and the gap is tiny, to avoid merging across column boundaries.
    """
    if not words:
        return words

    # Sort by y then x
    words = sorted(words, key=lambda w: (w["top"], w["x0"]))

    merged = []
    i = 0
    while i < len(words):
        current = dict(words[i])  # copy
        j = i + 1
        while j < len(words):
            nxt = words[j]
            # Must be on same line
            if abs(nxt["top"] - current["top"]) > y_tolerance:
                break
            gap = nxt["x0"] - current["x1"]
            next_len = len(nxt["text"].strip())
            # Only merge if:
            # 1. Gap is tiny (< x_gap) — typical char-level split gap is ~4px
            # 2. Next word is a single character (the hallmark of char splitting)
            # 3. The character is digit-like or punctuation (not a letter word)
            next_text = nxt["text"].strip()
            is_digit_like = bool(re.match(r'^[\d,.*\-~!?]$', next_text))
            if 0 <= gap < x_gap and next_len == 1 and is_digit_like:
                current["text"] = current["text"] + nxt["text"]
                current["x1"] = nxt["x1"]
                if "bottom" in nxt and "bottom" in current:
                    current["bottom"] = max(current["bottom"], nxt["bottom"])
                j += 1
            else:
                break
        merged.append(current)
        i = j

    return merged


def _group_words(words, y_tolerance=5):
    """Group words by y-position into rows."""
    if not words:
        return []
    rows = []
    current_row = [words[0]]
    current_y = words[0]["top"]
    for w in words[1:]:
        if abs(w["top"] - current_y) <= y_tolerance:
            current_row.append(w)
        else:
            rows.append(current_row)
            current_row = [w]
            current_y = w["top"]
    rows.append(current_row)
    return rows


# --- Main extraction ----------------------------------------------------------

def main():
    print(f"Reading PDF: {PDF_PATH}")
    pdf = pdfplumber.open(str(PDF_PATH))
    total_pages = len(pdf.pages)
    print(f"Total pages: {total_pages}")

    all_records = []
    last_kreis = None

    # Process page pairs: (left, right)
    for left_pg in range(PAGE_START, PAGE_END + 1, 2):
        right_pg = left_pg + 1
        if right_pg > PAGE_END:
            break

        # Extract right page (votes) — has municipality codes
        right_results, last_kreis = extract_right_page(
            pdf.pages[right_pg], last_kreis)

        # Extract left page (metadata) — ordered list without codes
        left_results = extract_left_page(pdf.pages[left_pg])

        # Match by order
        for i, (code, vote_data) in enumerate(right_results):
            kreis, gemeinde = code
            ags = to_ags(kreis, gemeinde)

            rec = {"ags": ags}
            rec.update(vote_data)

            # Match with left page data by position
            if i < len(left_results):
                left_rec = left_results[i]
                rec["eligible_voters"] = left_rec.get("eligible_voters", 0)
                rec["number_voters"] = left_rec.get("number_voters", 0)
                rec["invalid_votes"] = left_rec.get("invalid_votes", 0)

            # Fill missing party columns with 0
            for party in PARTY_ORDER:
                if party not in rec:
                    rec[party] = 0

            all_records.append(rec)

        if left_pg % 20 == PAGE_START % 20:
            print(f"  Pages {left_pg}-{right_pg}: "
                  f"{len(right_results)} right, {len(left_results)} left → "
                  f"total {len(all_records)}")

    print(f"\nTotal records: {len(all_records)}")

    # Check for duplicate AGS codes
    ags_counts = defaultdict(int)
    for rec in all_records:
        ags_counts[rec["ags"]] += 1
    dupes = {k: v for k, v in ags_counts.items() if v > 1}
    if dupes:
        print(f"Duplicate AGS codes: {len(dupes)}")
        for ags, count in sorted(dupes.items())[:10]:
            print(f"  {ags}: {count} occurrences")
        # Deduplicate: keep record with highest party vote sum
        by_ags = defaultdict(list)
        for rec in all_records:
            by_ags[rec["ags"]].append(rec)
        deduped = []
        for ags, recs in sorted(by_ags.items()):
            if len(recs) == 1:
                deduped.append(recs[0])
            else:
                best = max(recs, key=lambda r: sum(r.get(p, 0) or 0
                                                    for p in PARTY_ORDER))
                deduped.append(best)
        all_records = deduped
        print(f"After dedup: {len(all_records)} records")

    # Post-processing: fix VV from party sums where they mismatch
    # When VV != party_sum, the party_sum is usually more reliable
    # (VV might have captured a wrong value from a nearby row)
    n_vv_fixed = 0
    for rec in all_records:
        vv = rec.get("valid_votes", 0) or 0
        party_sum = sum(rec.get(p, 0) or 0 for p in PARTY_ORDER)
        if party_sum > 0 and abs(vv - party_sum) > max(2, vv * 0.02):
            rec["valid_votes"] = party_sum
            n_vv_fixed += 1
    print(f"Fixed VV from party sums: {n_vv_fixed} records")

    # Fix invalid AGS codes using neighbor context
    valid_kreise = set()
    for i in range(1, 5):
        valid_kreise.add(f"01{i:03d}")
    for i in range(51, 63):
        valid_kreise.add(f"01{i:03d}")

    n_ags_fixed = 0
    for idx, rec in enumerate(all_records):
        kreis5 = rec["ags"][:5]
        if kreis5 in valid_kreise:
            continue
        # Look at neighbors to infer correct Kreis
        window = all_records[max(0, idx - 5):idx + 6]
        neighbor_kreise = [r["ags"][:5] for r in window
                          if r["ags"][:5] in valid_kreise]
        if neighbor_kreise:
            from collections import Counter
            best = Counter(neighbor_kreise).most_common(1)[0][0]
            old_ags = rec["ags"]
            rec["ags"] = best + rec["ags"][5:]
            n_ags_fixed += 1
    print(f"Fixed AGS codes: {n_ags_fixed} records")

    # Validate: VV should equal sum of party votes
    n_ok = 0
    n_bad = 0
    total_vv = 0
    for rec in all_records:
        vv = rec.get("valid_votes", 0) or 0
        party_sum = sum(rec.get(p, 0) or 0 for p in PARTY_ORDER)
        total_vv += vv
        if vv > 0 and abs(vv - party_sum) <= max(2, vv * 0.02):
            n_ok += 1
        elif vv > 0:
            n_bad += 1
            if n_bad <= 15:
                print(f"  MISMATCH: AGS={rec['ags']} VV={vv} sum={party_sum} "
                      f"diff={vv - party_sum}")

    print(f"\nValidation: {n_ok} OK, {n_bad} BAD out of {len(all_records)}")
    print(f"Total valid votes: {total_vv}")

    # Validate left page matching
    n_left_ok = 0
    n_left_bad = 0
    for rec in all_records:
        vv = rec.get("valid_votes", 0) or 0
        nv = rec.get("number_voters", 0) or 0
        iv = rec.get("invalid_votes", 0) or 0
        ev = rec.get("eligible_voters", 0) or 0
        if vv > 0 and nv > 0:
            expected_nv = vv + iv
            if abs(nv - expected_nv) <= max(2, nv * 0.02):
                n_left_ok += 1
            else:
                n_left_bad += 1
                if n_left_bad <= 5:
                    print(f"  LEFT MISMATCH: AGS={rec['ags']} VV={vv} IV={iv} "
                          f"NV={nv} expected_NV={expected_nv}")
    print(f"Left matching: {n_left_ok} OK, {n_left_bad} BAD")

    # Statewide shares
    if total_vv > 0:
        print(f"\nStatewide vote shares (total VV={total_vv}):")
        for p in PARTY_ORDER:
            pv = sum(r.get(p, 0) or 0 for r in all_records)
            print(f"  {p}: {pv:>8d} ({pv / total_vv * 100:.1f}%)")

    # Check for valid SH AGS codes (should start with 01001-01004 or 01051-01062)
    valid_kreis = set()
    for i in range(1, 5):
        valid_kreis.add(f"01{i:03d}")
    for i in range(51, 63):
        valid_kreis.add(f"01{i:03d}")
    bad_ags = [r["ags"] for r in all_records if r["ags"][:5] not in valid_kreis]
    if bad_ags:
        print(f"\nInvalid AGS codes: {len(bad_ags)}")
        for a in bad_ags[:10]:
            print(f"  {a}")

    # Write CSV
    OUT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(OUT_PATH, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=FIELD_ORDER)
        writer.writeheader()
        for rec in all_records:
            row = {k: rec.get(k, 0) or 0 for k in FIELD_ORDER}
            row["ags"] = rec["ags"]
            writer.writerow(row)

    print(f"\nWrote {len(all_records)} records to {OUT_PATH}")


if __name__ == "__main__":
    main()
