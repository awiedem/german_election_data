#!/usr/bin/env python3
"""
Extract Brandenburg 1994 Landtagswahl Zweitstimmen from scanned PDF.

Pipeline: pdf2image (400 DPI) → binarize → Tesseract word-level → row grouping
          → structural parsing → percentage cross-validation → AGS fix → dedup → aggregate

Source: Brandenburg_1994_Landtagswahl.pdf, Table 4 (pages 17-407)
Output: bb_1994_ocr.csv (same format as R ingestion expects)

Usage:  source .venv/bin/activate && python3 code/state_elections/00_bb_1994_extract.py
"""

import csv
import re
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

import cv2
import numpy as np
from pdf2image import convert_from_path
import pytesseract


# ── Configuration ──────────────────────────────────────────────────────────

PDF_PATH = Path("data/state_elections/raw/Landtagswahlen/Brandenburg/"
                "Brandenburg_1994_Landtagswahl.pdf")
OUTPUT_PATH = Path("data/state_elections/raw/Landtagswahlen/Brandenburg/"
                   "bb_1994_ocr.csv")

DPI = 400
FIRST_PAGE = 17
LAST_PAGE = 407

PARTY_COLS = [
    "spd", "cdu", "fdp", "bfwg", "gruene_b90", "buerger",
    "dsu", "graue", "rep", "kpd", "odp", "pds", "uwvb", "eb"
]
# "Sonstige" is the 15th column on right pages — goes into "other" if needed
N_RIGHT_COLS = 15  # 14 parties + Sonstige

CSV_FIELDS = ["ags", "eligible_voters", "number_voters", "invalid_votes",
              "valid_votes"] + PARTY_COLS

# Valid Brandenburg Kreis codes (5-digit prefixes)
VALID_KREISE = (
    {f"120{i:02d}" for i in range(51, 55)} |   # kreisfreie Städte
    {f"120{i:02d}" for i in range(60, 74)}      # Landkreise
)

# Official Urnenwahl Zweitstimmen = total minus Briefwahl
# Total (PDF p.10): VV=1,072,019 | Briefwahl (PDF p.11): VV=70,503
OFFICIAL_URNENWAHL = {
    "valid_votes": 1_001_516,
    "spd": 544_772, "cdu": 189_206, "fdp": 21_986, "bfwg": 2_295,
    "gruene_b90": 28_835, "buerger": 9_690, "dsu": 1_850, "graue": 2_870,
    "rep": 11_744, "kpd": 0, "odp": 1_249, "pds": 183_153,
    "uwvb": 3_866, "eb": 0,
}


# ── OCR helpers ────────────────────────────────────────────────────────────

def fix_ocr(s: str) -> str:
    """Fix common Tesseract misreads in scanned typewriter text."""
    return (s.replace("ı", "1").replace("İ", "1").replace("ﬁ", "11")
             .replace("|", "1").replace("!", "1")
             .replace("&", "8").replace("§", "8"))


def parse_int(s: str) -> int | None:
    """Parse integer from OCR'd text. Returns 0 for x/-/empty, None if unparseable."""
    s = s.strip()
    if not s or s in ("x", "-", "—", "–", "*", "X", ".", ","): return 0
    s = fix_ocr(s).replace(",", "").replace(".", "").replace(" ", "")
    digits = re.sub(r"[^0-9]", "", s)
    return int(digits) if digits else None


def parse_pct(s: str) -> float | None:
    """Parse German percentage (comma = decimal)."""
    s = s.strip()
    if not s or s in ("x", "-", "—", "–", "*", "X"): return 0.0
    s = fix_ocr(s).replace(",", ".")
    try: return float(re.sub(r"[^0-9.]", "", s))
    except ValueError: return None


# ── Image processing ───────────────────────────────────────────────────────

def page_to_binary(pdf_path, page_num, dpi=400):
    """Convert a single PDF page to binarized numpy array."""
    imgs = convert_from_path(str(pdf_path), first_page=page_num,
                             last_page=page_num, dpi=dpi)
    gray = cv2.cvtColor(np.array(imgs[0]), cv2.COLOR_RGB2GRAY)
    _, binary = cv2.threshold(gray, 160, 255, cv2.THRESH_BINARY)
    return binary


def get_word_rows(binary_img, y_tol=20):
    """Run Tesseract and return words grouped into rows by y-position."""
    data = pytesseract.image_to_data(
        binary_img, lang="deu", config="--psm 6 --oem 1",
        output_type=pytesseract.Output.DICT)
    words = []
    for i in range(len(data["text"])):
        txt = data["text"][i].strip()
        if txt and int(data["conf"][i]) > 0:
            words.append({"text": txt, "conf": int(data["conf"][i]),
                          "x": data["left"][i],
                          "y": data["top"][i] + data["height"][i] // 2})
    if not words:
        return []
    words.sort(key=lambda w: (w["y"], w["x"]))
    rows, cur = [], [words[0]]
    for w in words[1:]:
        if abs(w["y"] - cur[-1]["y"]) < y_tol:
            cur.append(w)
        else:
            rows.append(sorted(cur, key=lambda w: w["x"]))
            cur = [w]
    rows.append(sorted(cur, key=lambda w: w["x"]))
    return rows


# ── Left page extraction ──────────────────────────────────────────────────

def is_z_abs_row(row):
    """Check if row is a 'LT94 Z abs.' row."""
    texts = [w["text"] for w in row]
    has_z = any(t in ("Z", "z") for t in texts)
    has_abs = any("abs" in t.lower() for t in texts)
    return has_z and has_abs


def extract_left_page(binary_img):
    """
    Extract municipality metadata from a left page.
    Returns list of dicts: {ags, name, eligible_voters, number_voters,
                            invalid_votes, valid_votes, z_pcts}
    """
    rows = get_word_rows(binary_img)
    if not rows:
        return []

    munis = []
    i = 0
    while i < len(rows):
        row = rows[i]
        if not row:
            i += 1; continue

        # Look for E abs rows starting with 8-digit AGS
        first_text = fix_ocr(row[0]["text"])
        ags_m = re.match(r"(\d{7,8})", first_text)

        if ags_m:
            ags = ags_m.group(1).zfill(8)

            # Municipality name: words between AGS and numeric/LT tokens
            name_parts = []
            for w in row[1:]:
                t = w["text"]
                if re.match(r"^\d{2,3}$", t) and w["x"] > 800: break
                if t.upper().startswith("LT") or t in ("E", "Z"): break
                name_parts.append(t)
            name = " ".join(name_parts).strip(", ")

            # E abs values: numbers at x > 1100
            e_nums = []
            for w in row:
                if w["x"] > 1100:
                    v = parse_int(w["text"])
                    if v is not None:
                        e_nums.append(v)

            # Find Z abs row (2 rows below E abs typically)
            z_row = None
            z_pct_row = None
            for j in range(i + 1, min(i + 6, len(rows))):
                if is_z_abs_row(rows[j]):
                    z_row = rows[j]
                    if j + 1 < len(rows):
                        z_pct_row = rows[j + 1]
                    break

            if z_row:
                # Z abs numbers: skip LT94/Z/abs tokens
                skip = {"LT94", "LTM", "LT", "LT9%4", "LT9G4", "Z", "z",
                        "abs.", "abs,", "abs"}
                z_nums = []
                for w in z_row:
                    if w["x"] > 1800 and w["text"] not in skip:
                        v = parse_int(w["text"])
                        if v is not None:
                            z_nums.append(v)

                rec = {"ags": ags, "name": name}

                # E abs: [BTW_nr?, EV, Wähler, ungültig_E, gültig_E]
                if len(e_nums) >= 4:
                    rec["eligible_voters"] = e_nums[-4]
                    rec["number_voters"] = e_nums[-3]
                elif len(e_nums) >= 2:
                    rec["eligible_voters"] = e_nums[0]
                    rec["number_voters"] = e_nums[1] if len(e_nums) > 1 else 0

                # Z abs: [ungültig_Z, gültig_Z]
                if len(z_nums) >= 2:
                    rec["invalid_votes"] = z_nums[-2]
                    rec["valid_votes"] = z_nums[-1]
                elif len(z_nums) == 1:
                    rec["invalid_votes"] = 0
                    rec["valid_votes"] = z_nums[0]
                else:
                    rec["invalid_votes"] = 0
                    rec["valid_votes"] = 0

                # Z pct row for cross-validation
                z_pcts = []
                if z_pct_row:
                    for w in z_pct_row:
                        if "," in w["text"]:
                            p = parse_pct(w["text"])
                            if p is not None:
                                z_pcts.append(p)
                rec["_z_pcts"] = z_pcts

                munis.append(rec)

        i += 1

    return munis


# ── Right page extraction ─────────────────────────────────────────────────

def classify_right_row(row):
    """Classify a right-page row as ABS (numbers only) or PCT (has commas)."""
    n_comma = sum(1 for w in row if "," in w["text"])
    return "PCT" if n_comma >= 3 else "ABS"


def extract_right_page(binary_img, n_munis):
    """
    Extract party votes from a right page.
    Returns list of dicts (one per municipality) with party vote counts.

    ABS rows have exactly 15 tokens (one per party+Sonstige column).
    The i-th token maps to the i-th party column.
    """
    rows = get_word_rows(binary_img)
    if not rows:
        return []

    # Find the first ABS data row (skip headers)
    data_start = None
    for idx, row in enumerate(rows):
        if idx < 3: continue  # skip page number and header text
        if classify_right_row(row) == "ABS" and len(row) >= 10:
            data_start = idx
            break

    if data_start is None:
        return []

    # Process in groups of 4: E_abs, E_pct, Z_abs, Z_pct
    party_data = []
    idx = data_start
    muni_count = 0

    while idx + 3 < len(rows) and muni_count < n_munis:
        # Rows: E abs, E pct, Z abs, Z pct
        e_abs = rows[idx]
        e_pct = rows[idx + 1]
        z_abs = rows[idx + 2]
        z_pct = rows[idx + 3]

        # Verify classification
        if classify_right_row(z_abs) != "ABS":
            # Row alignment lost — try to recover by scanning forward
            found = False
            for scan in range(idx, min(idx + 8, len(rows))):
                if (classify_right_row(rows[scan]) == "ABS" and
                    scan + 2 < len(rows) and
                    classify_right_row(rows[scan + 2]) == "ABS"):
                    idx = scan
                    z_abs = rows[idx + 2]
                    z_pct = rows[idx + 3] if idx + 3 < len(rows) else []
                    found = True
                    break
            if not found:
                idx += 1
                continue

        # Parse Z abs: position i → party column i
        rec = {}
        for j, party in enumerate(PARTY_COLS):
            if j < len(z_abs):
                v = parse_int(z_abs[j]["text"])
                rec[party] = v if v is not None else 0
            else:
                rec[party] = 0

        # Also capture Sonstige (column 15)
        if len(z_abs) > 14:
            rec["_sonstige"] = parse_int(z_abs[14]["text"]) or 0

        # Parse Z pct for cross-validation
        z_pct_vals = []
        if z_pct and classify_right_row(z_pct) == "PCT":
            for w in z_pct:
                if "," in w["text"]:
                    p = parse_pct(w["text"])
                    if p is not None:
                        z_pct_vals.append(p)
        rec["_z_pct_party"] = z_pct_vals

        party_data.append(rec)
        idx += 4
        muni_count += 1

    return party_data


# ── Cross-validation ──────────────────────────────────────────────────────

def cross_validate(rec):
    """Cross-validate party votes against percentage row. Returns list of warnings."""
    vv = rec.get("valid_votes", 0) or 0
    if vv == 0:
        return []

    warns = []
    party_sum = sum(rec.get(p, 0) or 0 for p in PARTY_COLS)

    if abs(party_sum - vv) > max(5, vv * 0.05):
        warns.append(f"party_sum={party_sum} vs VV={vv} (diff={party_sum - vv})")

    # Check party votes against percentages
    z_pcts = rec.get("_z_pct_party", [])
    for j, party in enumerate(PARTY_COLS):
        if j < len(z_pcts) and z_pcts[j] > 0.5:
            expected = round(z_pcts[j] * vv / 100)
            actual = rec.get(party, 0) or 0
            if actual > 0 and abs(expected - actual) > max(3, actual * 0.15):
                warns.append(f"{party}: pct={z_pcts[j]}% implies ~{expected}, got {actual}")

    return warns


# ── Post-processing ───────────────────────────────────────────────────────

def fix_ags_codes(records):
    """Fix OCR-corrupted AGS codes using neighbor context."""
    n_fixed = 0
    for idx, rec in enumerate(records):
        kreis = rec["ags"][:5]
        if kreis in VALID_KREISE:
            continue
        window = records[max(0, idx - 5):idx + 6]
        neighbors = [r["ags"][:5] for r in window if r["ags"][:5] in VALID_KREISE]
        if neighbors:
            best = Counter(neighbors).most_common(1)[0][0]
            old = rec["ags"]
            rec["ags"] = best + rec["ags"][5:]
            n_fixed += 1
    if n_fixed:
        print(f"Fixed {n_fixed} OCR-corrupted AGS codes.")
    return records


def deduplicate(records):
    """Keep record with highest party vote sum per AGS."""
    by_ags = defaultdict(list)
    for r in records:
        by_ags[r["ags"]].append(r)
    deduped = []
    n_dup = 0
    for ags, recs in by_ags.items():
        if len(recs) == 1:
            deduped.append(recs[0])
        else:
            n_dup += 1
            best = max(recs, key=lambda r: sum(r.get(p, 0) or 0 for p in PARTY_COLS))
            deduped.append(best)
    if n_dup:
        print(f"Deduplicated {n_dup} AGS codes.")
    return deduped


def aggregate_kreisfreie(records):
    """Aggregate kreisfreie Städte sub-area records to city level."""
    kfs = {"12051", "12052", "12053", "12054"}
    kfs_recs = [r for r in records if r["ags"][:5] in kfs]
    non_kfs = [r for r in records if r["ags"][:5] not in kfs]

    if not kfs_recs:
        return records

    agg = defaultdict(lambda: {f: 0 for f in CSV_FIELDS})
    for rec in kfs_recs:
        city_ags = rec["ags"][:5] + "000"
        agg[city_ags]["ags"] = city_ags
        for f in CSV_FIELDS[1:]:
            agg[city_ags][f] += rec.get(f, 0) or 0

    print(f"Aggregated {len(kfs_recs)} kreisfreie Stadt sub-records "
          f"into {len(agg)} city records.")
    return non_kfs + list(agg.values())


# ── Validation ────────────────────────────────────────────────────────────

def validate_totals(records):
    """Compare aggregated totals against official Urnenwahl numbers."""
    print("\n=== State-level validation (Urnenwahl only) ===")
    for col in ["valid_votes"] + PARTY_COLS:
        total = sum(r.get(col, 0) or 0 for r in records)
        official = OFFICIAL_URNENWAHL.get(col)
        if official is not None and official > 0:
            pct = (total - official) / official * 100
            ok = "OK" if abs(pct) < 2.0 else "!!"
            print(f"  {ok} {col:18s}: {total:>10,} (official {official:>10,}, "
                  f"diff {total - official:>+8,} = {pct:+.2f}%)")
        elif official == 0:
            print(f"  -- {col:18s}: {total:>10,} (official 0)")


# ── Main ──────────────────────────────────────────────────────────────────

def main():
    if not PDF_PATH.exists():
        print(f"ERROR: PDF not found: {PDF_PATH}")
        sys.exit(1)

    print(f"Extracting BB 1994 from {PDF_PATH}")
    print(f"Pages {FIRST_PAGE}-{LAST_PAGE}, {DPI} DPI\n")

    all_records = []
    n_warnings = 0
    t0 = time.time()
    pair = 0

    # Page pairs: right=odd (17,19,...), left=even (18,20,...)
    page = FIRST_PAGE
    while page + 1 <= LAST_PAGE:
        right_page = page      # odd = party columns
        left_page = page + 1   # even = metadata

        try:
            left_bin = page_to_binary(PDF_PATH, left_page, DPI)
            right_bin = page_to_binary(PDF_PATH, right_page, DPI)
        except Exception as e:
            print(f"  Skip pages {right_page}-{left_page}: {e}")
            page += 2
            continue

        # Extract left page (metadata)
        munis = extract_left_page(left_bin)

        if not munis:
            # Likely a Wahlkreis summary or non-data page
            page += 2
            pair += 1
            continue

        # Extract right page (party votes)
        parties = extract_right_page(right_bin, n_munis=len(munis))

        # Merge
        for i, muni in enumerate(munis):
            rec = dict(muni)
            if i < len(parties):
                for p in PARTY_COLS:
                    rec[p] = parties[i].get(p, 0)
                rec["_z_pct_party"] = parties[i].get("_z_pct_party", [])

            # Cross-validate
            warns = cross_validate(rec)
            if warns:
                n_warnings += len(warns)
                if len(all_records) < 50:  # only print first batch
                    print(f"  WARN {rec['ags']} ({rec.get('name','')}): "
                          + "; ".join(warns[:2]))

            all_records.append(rec)

        pair += 1
        if pair % 20 == 0:
            print(f"  {pair} page pairs, {len(all_records)} munis "
                  f"({time.time() - t0:.0f}s)")

        page += 2

    elapsed = time.time() - t0
    print(f"\nRaw extraction: {len(all_records)} municipalities from "
          f"{pair} page pairs in {elapsed:.0f}s")
    print(f"Cross-validation warnings: {n_warnings}")

    # Post-processing
    all_records = fix_ags_codes(all_records)
    all_records = deduplicate(all_records)
    all_records = aggregate_kreisfreie(all_records)

    # Validate
    validate_totals(all_records)

    # Write CSV
    with open(OUTPUT_PATH, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=CSV_FIELDS, extrasaction="ignore")
        writer.writeheader()
        for rec in all_records:
            out = {}
            for field in CSV_FIELDS:
                val = rec.get(field, 0)
                out[field] = val if val is not None else 0
            writer.writerow(out)

    print(f"\nWritten {len(all_records)} records to {OUTPUT_PATH}")


if __name__ == "__main__":
    main()
