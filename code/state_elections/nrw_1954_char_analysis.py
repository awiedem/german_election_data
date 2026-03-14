#!/usr/bin/env python3
"""
Analyze character data from NRW 1954 Landtagswahl PDF, pages 159-164.
Examines font sizes, character distributions, and row-level structure.
"""

import pdfplumber
from collections import Counter, defaultdict

PDF_PATH = (
    "/Users/vincentheddesheimer/Princeton Dropbox/Vincent Heddesheimer/"
    "german_election_data/data/state_elections/raw/Landtagswahlen/"
    "Nordrhein-Westfalen/Nordrhein-Westfalen_1954_Landtagswahl.pdf"
)

PAGES = list(range(159, 165))  # 0-indexed: pages 160-165 in PDF numbering


def round_size(s, precision=1):
    return round(s, precision)


def group_by_y(chars, tol=3):
    """Group characters into rows by y-position with tolerance."""
    if not chars:
        return []
    sorted_chars = sorted(chars, key=lambda c: (c["top"], c["x0"]))
    rows = []
    current_row = [sorted_chars[0]]
    current_y = sorted_chars[0]["top"]

    for c in sorted_chars[1:]:
        if abs(c["top"] - current_y) <= tol:
            current_row.append(c)
        else:
            rows.append(current_row)
            current_row = [c]
            current_y = c["top"]
    rows.append(current_row)
    return rows


def assemble_row_text(row_chars, gap_threshold=4):
    """Assemble characters into text, inserting spaces for gaps."""
    sorted_chars = sorted(row_chars, key=lambda c: c["x0"])
    text = ""
    prev_x1 = None
    for c in sorted_chars:
        if prev_x1 is not None and (c["x0"] - prev_x1) > gap_threshold:
            text += " "
        text += c["text"] if c["text"] else ""
        prev_x1 = c["x1"] if "x1" in c else c["x0"] + 5
    return text


def main():
    pdf = pdfplumber.open(PDF_PATH)

    for page_idx in PAGES:
        page = pdf.pages[page_idx]
        chars = page.chars
        pdf_page_num = page_idx + 1
        print(f"\n{'='*80}")
        print(f"PAGE {pdf_page_num} (0-indexed: {page_idx})")
        print(f"{'='*80}")
        print(f"Total characters: {len(chars)}")

        # --- (a) Font size distribution ---
        size_counts = Counter()
        for c in chars:
            size_counts[round_size(c["size"])] += 1

        print(f"\n--- Font size distribution ---")
        for sz in sorted(size_counts.keys()):
            print(f"  size {sz:6.1f}: {size_counts[sz]:5d} chars")

        # --- (b) Most common font size in 6.0-6.7 range ---
        sizes_6x = {sz: cnt for sz, cnt in size_counts.items() if 6.0 <= sz <= 6.7}
        if sizes_6x:
            best_6x = max(sizes_6x, key=sizes_6x.get)
            print(f"\n--- Most common 6.x font size: {best_6x} ({sizes_6x[best_6x]} chars) ---")

            chars_6x = [c for c in chars if round_size(c["size"]) == best_6x]
            char_counts = Counter(c["text"] for c in chars_6x)
            print(f"  Unique characters ({len(char_counts)}):")
            for ch, cnt in sorted(char_counts.items(), key=lambda x: -x[1]):
                display = repr(ch)
                print(f"    {display:8s}: {cnt:5d}")
        else:
            print("\n  No characters in 6.0-6.7 range!")
            best_6x = None

        # --- (c) Row-by-row text from 6.x font layer ---
        if best_6x is not None:
            chars_6x = [c for c in chars if round_size(c["size"]) == best_6x]
            rows = group_by_y(chars_6x)
            print(f"\n--- Row-by-row text from {best_6x} font layer ({len(rows)} rows) ---")
            for i, row in enumerate(rows):
                text = assemble_row_text(row)
                y_pos = row[0]["top"]
                print(f"  Row {i:3d} (y={y_pos:7.1f}): {text}")

    # --- (4) First 15 and last 5 rows for pages 160 and 161 ---
    print(f"\n{'='*80}")
    print("DETAILED ROW VIEW: Pages 160 and 161")
    print(f"{'='*80}")

    for page_idx in [159, 160]:
        page = pdf.pages[page_idx]
        chars = page.chars
        pdf_page_num = page_idx + 1

        size_counts = Counter(round_size(c["size"]) for c in chars)
        sizes_6x = {sz: cnt for sz, cnt in size_counts.items() if 6.0 <= sz <= 6.7}
        if not sizes_6x:
            print(f"\n  Page {pdf_page_num}: No 6.x chars found!")
            continue
        best_6x = max(sizes_6x, key=sizes_6x.get)

        chars_6x = [c for c in chars if round_size(c["size"]) == best_6x]
        rows = group_by_y(chars_6x)

        print(f"\n--- Page {pdf_page_num}: First 15 rows (font {best_6x}) ---")
        for i, row in enumerate(rows[:15]):
            text = assemble_row_text(row)
            y_pos = row[0]["top"]
            n_chars = len(row)
            print(f"  Row {i:3d} (y={y_pos:7.1f}, {n_chars:3d} chars): {text}")

        print(f"\n--- Page {pdf_page_num}: Last 5 rows (font {best_6x}) ---")
        for i, row in enumerate(rows[-5:], start=len(rows) - 5):
            text = assemble_row_text(row)
            y_pos = row[0]["top"]
            n_chars = len(row)
            print(f"  Row {i:3d} (y={y_pos:7.1f}, {n_chars:3d} chars): {text}")

    # --- (5) All chars in 6.x layer for page 160, row by row ---
    print(f"\n{'='*80}")
    print("CHAR-LEVEL DETAIL: Page 160 (all 6.x chars with x0 positions)")
    print(f"{'='*80}")

    page = pdf.pages[159]
    chars = page.chars
    size_counts = Counter(round_size(c["size"]) for c in chars)
    sizes_6x = {sz: cnt for sz, cnt in size_counts.items() if 6.0 <= sz <= 6.7}
    if sizes_6x:
        best_6x = max(sizes_6x, key=sizes_6x.get)
        chars_6x = [c for c in chars if round_size(c["size"]) == best_6x]
        rows = group_by_y(chars_6x)

        print(f"Font size: {best_6x}, Total rows: {len(rows)}")
        for i, row in enumerate(rows):
            sorted_row = sorted(row, key=lambda c: c["x0"])
            y_pos = row[0]["top"]
            char_detail = "  ".join(
                f"{c['text']}@{c['x0']:.1f}" for c in sorted_row
            )
            print(f"\n  Row {i:3d} (y={y_pos:.1f}):")
            print(f"    {char_detail}")
    else:
        print("  No 6.x chars found on page 160!")

    pdf.close()
    print("\nDone.")


if __name__ == "__main__":
    main()
