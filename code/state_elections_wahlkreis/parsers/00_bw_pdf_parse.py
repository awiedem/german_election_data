#!/usr/bin/env python3
"""Stage-0 parser for BADEN-WÜRTTEMBERG Landtagswahl results at WAHLKREIS
level, 2001 + 2006 + 2011 (extends the machine-readable-only coverage of
parse_BW.R, which already handles 2016/2021/2026 from XLSX/CSV).

Source (raw, read-only), each 32 pp., text-layer PDF, Statistisches Landesamt
Baden-Württemberg, series "Statistische Berichte B VII 2":
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Baden-Württemberg/
    BW_2006_Landtagswahl_Wahlkreis_BVII2.pdf  (2006 current + 2001 comparison)
    BW_2011_Landtagswahl_Wahlkreis_BVII2.pdf  (2011 current + 2006 comparison)

Tabelle 1 ("Endgültige Ergebnisse ... mit Vergleichsangaben von <year-4> in
den Landtagswahlkreisen") spans PDF pages 4-22 in both reports: a Land
Baden-Württemberg total block, then 4 Regierungsbezirk subtotals interspersed
among the 70 Wahlkreise, printed two-per-page side by side. Each row carries
THREE column-pairs (Anzahl/%): current year, prior year, Veränderung
(Anzahl/%-Punkte) -- so the SAME rows that report the 2006 (or 2011) current
result also report the prior election's comparison figures, which is where
BW's 2001 (from the 2006 report) and the 2006 cross-check (from the 2011
report) data come from. Page range 4-22 is enforced explicitly: page 22 also
contains the start of "2. Sitzverteilung" whose row labels ("Gültige
Stimmen" as a bare column header, no Wahlkreis context) would otherwise be
mis-picked-up as spurious data rows.

PARSING METHOD (coordinate-based; do NOT switch to naive whitespace
splitting -- German thousands separator is a SPACE, so column identity must
come from x-position, not token order). Numbers are right-aligned; a block's
column anchors (x1 of each of its 3 or 6 semantic slots -- see next
paragraph) are bootstrapped from its own "Wähler" row (turnout is never NA),
then every other row in the block is assigned to those anchors by nearest
x1. This is the same method validated for HE (00_he_pdf_parse.py) and BB.

QUIRK -- pdfplumber silently drops '%'-column glyphs on some pages, even
though `pdftotext -layout` renders them fine (confirmed by direct word-level
inspection; this looks like a broken/partial ToUnicode CMap for a specific
embedded font subset used only for those digits). Where this happens, a
block's "Wähler" row yields 3 merged atoms/Wahlkreis (Anzahl only: current,
prior, Veränderung) instead of 6 (Anzahl+% for each). The parser bootstraps
either width transparently (`bootstrap_anchors`) and decodes accordingly
(`decode_cells`): 2006 report -- only the WK68/69/70 page (p. 22) falls back
to 3-slot; every other block has full 6-slot (with %) data. 2011 report --
EVERY block is 3-slot; no percentages are recoverable from this document via
pdfplumber. Neither loss affects the emitted data (percentages are not part
of the output schema) or validation (4), which is vacuously satisfied for
2011 and checked wherever % values exist in 2006.

QUIRK -- unlabelled continuation rows. A Wahlkreis with MORE THAN ONE
independent candidate ("Einzelbewerber") prints a second Anzahl/%/
Veränderung line directly beneath the labelled "Einzelbewerber" row, with no
repeated row label (seen in BW_2011 WK 35 Mannheim I: label row gives
candidate 1's 281 votes, an unlabelled line 11.5pt below gives candidate 2's
1,527). The parser recognises a purely-numeric/×/–/+ row within 20pt of the
last labelled row as a continuation of that label and SUMS it in
(`_add_cells`); this is required for validations (2) and (3) to pass exactly.

QUIRK -- "×" is BW's NA marker (party not on that year's ballot / not stood
in that Wahlkreis), same role as "–" (exact zero is also "–" for the
turnout/Gültige rows). Both are treated as None, matching the 2018 Hessen
parser's "x" convention.

QUIRK -- 2 party rows in the 2011 report graft a PREDECESSOR party's 2006
figures onto a differently-named CURRENT row via a footnote, rather than
listing the predecessor by its own name or folding it into the residual:
  "DIE LINKE2)"        prior(2006) col = WASG's 2006 count
       (footnote 2: "2006: WASG (2007 Vereinigung WASG und Die Linke.)")
  "Volksabstimmung1)"  prior(2006) col = "Deutschland"'s 2006 count
       (footnote 1: "2006: Deutschland.")
Both are cross-checked explicitly below (WASG and Deutschland are BW_2006's
own row labels). The remaining 2006-only fringe parties with NO 2011 row at
all (ADM, AGFG, DPP, ZENTRUM, GRAUE, Die Tierschutzpartei, UNABHÄNGIGE) are
folded into the 2011 report's own "Sonstige (nur 2006)" residual, exactly as
"Sonstige (nur 2001)" folds BW_2006's fringe 2001 parties.

Footnote markers directly appended to a party label with no separating space
("DIE LINKE2)", "Volksabstimmung1)") are stripped for party_raw ("DIE
LINKE", "Volksabstimmung"); "Sonstige (nur <year>)" residual rows are kept
verbatim, matching the precedent in parse_HE.R / 00_sl_pdf_parse.py.

VALIDATION (all hard; nothing is written if any check fails):
  (1) exactly 70 Wahlkreise per year (2001, 2006, 2011)
  (2) per (Wahlkreis, year): sum(party votes, incl. Einzelbewerber and the
      2001 "Sonstige" residual) == Gültige Stimmen exactly; Wähler ==
      Gültige + Ungültige
  (3) sum over the 70 Wahlkreise, per party, == the report's own printed
      Land Baden-Württemberg row, exactly, for both years' current AND
      prior columns
  (4) per-cell % reproduction wherever a % value was actually recovered
      (rounding tolerance 0.055pp)
  (5) pinned official statewide shares (+-0.1pp): 2006 CDU 44.2/SPD
      25.2/GRÜNE 11.7/FDP 10.7/WASG 3.1; 2001 CDU 44.8/SPD 33.3/FDP 8.1/
      GRÜNE 7.7/REP 4.4; 2011 CDU 39.0/GRÜNE 24.2/SPD 23.1/FDP 5.3/
      LINKE 2.8
  (6) the two starred-Wahlkreis sets (boundary-recomputed comparison
      figures, footnote "*)") are derived from each report's own header
      cells and pinned: 11 in the 2006 report (2001-on-2006-boundaries),
      37 in the 2011 report (2006-on-2011-boundaries)
  (7) INDEPENDENT SOURCE cross-check: BW_2011's prior(2006) columns must
      reproduce BW_2006's current(2006) columns exactly on the 33
      Wahlkreise the 2011 report does NOT mark as boundary-recomputed
      (shared-label parties + the two footnoted grafts + the fringe
      residual, separately); AND the statewide (all 70 WK) totals must
      match exactly too, once the two grafts are matched by their footnoted
      predecessor party rather than by label -- confirmed: BW's 2006-on-
      2011-boundary recomputation is exactly vote-conserving statewide, not
      just on the unstarred subset.

Output: data/state_elections/processed/wahlkreis/bw_pdf/BW_2001_2011_pdf_long.csv
        (read by parsers/parse_BW.R, which appends the 2016/2021/2026 results)
Run:    python3 code/state_elections_wahlkreis/parsers/00_bw_pdf_parse.py
"""
import csv
import os
import re
import sys

import pdfplumber

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW_DIR = os.path.join(ROOT, "data", "state_elections", "raw",
                       "Landtagswahlen_Wahlkreis", "Baden-Württemberg")
PDF_2006 = os.path.join(RAW_DIR, "BW_2006_Landtagswahl_Wahlkreis_BVII2.pdf")
PDF_2011 = os.path.join(RAW_DIR, "BW_2011_Landtagswahl_Wahlkreis_BVII2.pdf")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "bw_pdf")
OUT = os.path.join(OUT_DIR, "BW_2001_2011_pdf_long.csv")

STATE_ABBR, STATE_NAME = "BW", "Baden-Württemberg"
STIMME = "einzelstimme"
ELECTION_DATE = {2001: "2001-03-25", 2006: "2006-03-26", 2011: "2011-03-27"}
PAGE_RANGE = range(4, 23)   # Tabelle 1: PDF pages 4-22 inclusive, both reports

# =============================================================================
# row-label config, per report
# =============================================================================
ROW_LABEL_3 = "Wahlberechtigte"
ROW_LABELS_TURNOUT = ["Wähler", "Ungültige Stimmen", "Gültige Stimmen"]

PARTY_ROWS_2006 = [
    "CDU", "SPD", "FDP", "GRÜNE", "Deutschland", "ADM", "AGFG", "WASG", "DPP",
    "ZENTRUM", "GRAUE", "REP", "Die Tierschutzpartei", "NPD", "ödp", "PBC",
    "Die PARTEI", "RSB", "UNABHÄNGIGE", "Einzelbewerber", "Sonstige (nur 2001)",
]
PARTY_ROWS_2011 = [
    "CDU", "SPD", "GRÜNE", "FDP", "Volksabstimmung1)", "AUF", "BIG", "BüSo",
    "DKP", "DIE LINKE2)", "REP", "DIE VIOLETTEN", "FAMILIE", "NPD", "ödp",
    "PBC", "Die PARTEI", "PIRATEN", "RSB", "Einzelbewerber", "Sonstige (nur 2006)",
]
# party_raw output name: strip a footnote digit+')' appended with no space
CLEAN_PARTY = {"Volksabstimmung1)": "Volksabstimmung", "DIE LINKE2)": "DIE LINKE"}

ALL_LABELS_2006 = [ROW_LABEL_3] + ROW_LABELS_TURNOUT + PARTY_ROWS_2006
ALL_LABELS_2011 = [ROW_LABEL_3] + ROW_LABELS_TURNOUT + PARTY_ROWS_2011

# validation (7): parties with the SAME row label in both reports
SHARED_LABELS = ["CDU", "SPD", "GRÜNE", "FDP", "REP", "NPD", "ödp", "PBC",
                  "Die PARTEI", "RSB", "Einzelbewerber"]
# footnoted grafts: 2011's row -> the 2006-report row it grafts as its prior(2006) col
GRAFTS = {"DIE LINKE2)": "WASG", "Volksabstimmung1)": "Deutschland"}
# 2006-only parties with NO row at all in 2011 -> folded into its "Sonstige (nur 2006)"
FRINGE_2006_ONLY = ["ADM", "AGFG", "DPP", "ZENTRUM", "GRAUE",
                     "Die Tierschutzpartei", "UNABHÄNGIGE"]

# pinned official statewide shares (+-0.1pp)
OFFICIAL = {
    2006: {"CDU": 44.2, "SPD": 25.2, "GRÜNE": 11.7, "FDP": 10.7, "WASG": 3.1},
    2001: {"CDU": 44.8, "SPD": 33.3, "FDP": 8.1, "GRÜNE": 7.7, "REP": 4.4},
    2011: {"CDU": 39.0, "GRÜNE": 24.2, "SPD": 23.1, "FDP": 5.3, "DIE LINKE2)": 2.8},
}
N_STARRED_2006 = 11   # 2001-on-2006-boundaries
N_STARRED_2011 = 37   # 2006-on-2011-boundaries

# =============================================================================
# generic geometry / number-parsing helpers (word-level, coordinate-based)
# =============================================================================
NUMLIKE = re.compile(r"^[\d,]+$")
DATA_TOKEN_RE = re.compile(r"^[\d,]+$")
NA_TOKENS = {"–", "-", "—", "×"}
MERGE_GAP = 4.0     # pt; thousands-separator intra-number gap
SIGN_CAP = 22.0      # pt; max gap between a lone sign atom and its magnitude. A cap
                     # this tight occasionally leaves the d_n sign of a wide delta
                     # column un-merged (observed gap 22.3pt on some pages), so
                     # nearest-anchor assignment can then attach that bare sign to the
                     # PRECEDING pct_prev cell instead ("0,6" + "+" -> "0,6+"). Raising
                     # the cap to fix that is unsafe: it also risks merging across a
                     # genuine WK-block boundary during the (still-combined, both-WK)
                     # Wähler-row bootstrap. Left at 22.0; the resulting rare glued-sign
                     # artifact is repaired narrowly in parse_num() instead (percentage
                     # fields only -- votes/counts are never affected, see there).


def cluster_rows(words, tol=1.6):
    """Group words into visual rows by a fixed per-row top anchor (NOT a
    running/chaining comparison -- every word compares to the FIRST word's
    top in its row, so a superscript-shifted label doesn't drag the anchor)."""
    words = sorted(words, key=lambda w: w["top"])
    rows, cur, cur_top = [], [], None
    for w in words:
        if cur_top is None or abs(w["top"] - cur_top) <= tol:
            cur.append(w)
            cur_top = w["top"] if cur_top is None else cur_top
        else:
            rows.append(cur)
            cur, cur_top = [w], w["top"]
    if cur:
        rows.append(cur)
    return rows


def digit_merge(data_words):
    """Merge only unambiguous thousands-separator digit groups (tight x-gap)."""
    data_words = sorted(data_words, key=lambda w: w["x0"])
    atoms = []
    cur_text = cur_x0 = cur_x1 = None
    for w in data_words:
        t = w["text"]
        if cur_text is None:
            cur_text, cur_x0, cur_x1 = t, w["x0"], w["x1"]
            continue
        gap = w["x0"] - cur_x1
        last_group = cur_text.split(" ")[-1]
        if gap < MERGE_GAP and re.match(r"^\d+$", last_group) and re.match(r"^\d+$", t):
            cur_text = cur_text + " " + t
            cur_x1 = w["x1"]
        else:
            atoms.append({"text": cur_text, "x0": cur_x0, "x1": cur_x1})
            cur_text, cur_x0, cur_x1 = t, w["x0"], w["x1"]
    if cur_text is not None:
        atoms.append({"text": cur_text, "x0": cur_x0, "x1": cur_x1})
    return atoms


def parse_num(tok):
    if tok is None:
        return None
    tok = tok.strip()
    if tok in NA_TOKENS or tok == "":
        return None
    sign = 1
    if tok[:1] in ("–", "-", "—"):
        sign = -1
        tok = tok[1:].strip()
    elif tok.startswith("+"):
        tok = tok[1:].strip()
    tok = tok.replace(" ", "")
    # a lone sign glued onto the END of an otherwise-complete number is the
    # d_n/d_pct sign of the NEXT cell, mis-anchored here because it fell just
    # outside SIGN_CAP of its own magnitude (see SIGN_CAP comment above). Its
    # own magnitude was correctly assigned elsewhere, so this trailing sign
    # carries no information for THIS cell and is dropped. Percentage fields
    # only; votes/counts never take this path (integers here are always
    # complete, un-suffixed tokens by construction of the anchor split).
    if len(tok) > 1 and tok[-1] in ("+", "–", "-", "—"):
        tok = tok[:-1]
    if tok == "":
        return None
    try:
        val = float(tok.replace(",", ".")) if "," in tok else int(tok)
    except ValueError:
        return "PARSE_ERROR:" + tok
    return sign * val


def merge_signs(atoms, cap=SIGN_CAP):
    """Merge a lone sign atom with the immediately following magnitude atom
    (bounded gap, so a standalone NA marker near an unrelated column
    boundary is never absorbed -- see PARSE_NOTES precedent in parse_bw2.py)."""
    merged = []
    i = 0
    while i < len(atoms):
        a = atoms[i]
        if a["text"] in ("+", "–", "-", "—") and i + 1 < len(atoms):
            b = atoms[i + 1]
            gap = b["x0"] - a["x1"]
            if gap < cap and NUMLIKE.match(b["text"].replace(" ", "")):
                merged.append({"text": a["text"] + " " + b["text"], "x0": a["x0"], "x1": b["x1"]})
                i += 2
                continue
        merged.append(a)
        i += 1
    return merged


def bootstrap_anchors(atoms, n_blocks):
    """Bootstrap per-block column anchors (x1 of each semantic slot) from a
    fully-populated row (Wähler). Accepts 6 slots/block (Anzahl+% for
    current/prior/delta) or 3 (Anzahl only -- see module docstring, the
    '%'-glyph-drop quirk). Returns a list of length n_blocks, each a list of
    anchor x1's, or None if the row doesn't cleanly divide."""
    merged = merge_signs(atoms)
    total = len(merged)
    if n_blocks not in (1, 2) or total % n_blocks != 0:
        return None
    per = total // n_blocks
    if per not in (3, 6):
        return None
    if n_blocks == 1:
        return [[a["x1"] for a in merged]]
    half = total // 2
    return [[a["x1"] for a in merged[:half]], [a["x1"] for a in merged[half:]]]


def assign_to_anchors(atoms, anchors):
    cells = {i: [] for i in range(len(anchors))}
    for a in atoms:
        idx = min(range(len(anchors)), key=lambda i: abs(a["x1"] - anchors[i]))
        cells[idx].append(a)
    out = []
    for i in range(len(anchors)):
        parts = sorted(cells[i], key=lambda a: a["x0"])
        out.append(" ".join(p["text"] for p in parts) if parts else None)
    return out


def decode_cells(cell_texts):
    vals = [parse_num(t) for t in cell_texts]
    if len(vals) == 6:
        return {"n_curr": vals[0], "pct_curr": vals[1], "n_prev": vals[2],
                "pct_prev": vals[3], "d_n": vals[4], "d_pct": vals[5]}
    if len(vals) == 3:
        return {"n_curr": vals[0], "pct_curr": None, "n_prev": vals[1],
                "pct_prev": None, "d_n": vals[2], "d_pct": None}
    raise ValueError(f"unexpected cell count {len(vals)}: {cell_texts}")


def decode_3(cell_texts):
    vals = [parse_num(t) for t in cell_texts]
    return {"n_curr": vals[0], "n_prev": vals[1], "d_n": vals[2]}


def _is_pure_data_row(texts):
    for t in texts:
        if t in ("×", "–", "-", "—", "+"):
            continue
        if DATA_TOKEN_RE.match(t):
            continue
        return False
    return True


def _add_cells(dst, src):
    """Accumulate an unlabelled continuation row (2nd Einzelbewerber
    candidate) into an existing decoded row. None+None stays None."""
    if dst is None:
        return src
    out = dict(dst)
    for k in ("n_curr", "n_prev", "d_n"):
        a, b = dst.get(k), src.get(k)
        out[k] = None if a is None and b is None else (a or 0) + (b or 0)
    for k in ("pct_curr", "pct_prev", "d_pct"):
        out[k] = None
    return out


# =============================================================================
# main per-page / per-block parser
# =============================================================================
def parse_pdf(path, all_labels, page_range=PAGE_RANGE):
    """Returns a list of blocks: {wk, name, umgerechnet, rows, anchors}.
    wk is 'Land', 'RB', or a zero-padded 2-digit Wahlkreis number string."""
    results = []
    pdf = pdfplumber.open(path)
    pages = [pdf.pages[i - 1] for i in page_range]
    cur_blocks = []
    last_label, last_top = None, None

    def flush():
        nonlocal cur_blocks, last_label, last_top
        results.extend(cur_blocks)
        cur_blocks, last_label, last_top = [], None, None

    for page in pages:
        words = page.extract_words(use_text_flow=False, keep_blank_chars=False)
        rows = cluster_rows(words)
        rows.sort(key=lambda r: min(w["top"] for w in r))
        for row in rows:
            row_sorted = sorted(row, key=lambda w: w["x0"])
            texts = [w["text"] for w in row_sorted]
            row_top = min(w["top"] for w in row)

            if "Land" in texts and "Baden-Württemberg" in texts and "Wahlkreis" not in texts:
                flush()
                cur_blocks = [{"wk": "Land", "name": "Baden-Württemberg", "umgerechnet": False,
                                "rows": {}, "anchors": None}]
                continue

            if texts.count("Wahlkreis") >= 1 or texts.count("Regierungsbezirk") >= 1:
                flush()
                idxs = [i for i, t in enumerate(texts) if t in ("Wahlkreis", "Regierungsbezirk")]
                idxs.append(len(texts))
                blocks = []
                for a, b in zip(idxs, idxs[1:]):
                    seg = texts[a:b]
                    if seg[0] == "Regierungsbezirk":
                        blocks.append({"wk": "RB", "name": "Regierungsbezirk " + " ".join(seg[1:]),
                                        "umgerechnet": False, "rows": {}, "anchors": None})
                        continue
                    wk_num = seg[1] if len(seg) > 1 else None
                    name_parts = seg[2:]
                    umger = False
                    if name_parts and name_parts[-1] == "*)":
                        umger = True
                        name_parts = name_parts[:-1]
                    blocks.append({"wk": wk_num, "name": " ".join(name_parts), "umgerechnet": umger,
                                    "rows": {}, "anchors": None})
                cur_blocks = blocks
                continue

            if not cur_blocks:
                continue

            label, label_nwords = None, 0
            for nwords in (4, 3, 2, 1):
                cand = " ".join(texts[:nwords]).strip()
                if cand in all_labels:
                    label, label_nwords = cand, nwords
                    break

            # unlabelled continuation row (2nd Einzelbewerber candidate)
            if (label is None and last_label is not None and last_top is not None
                    and row_top - last_top < 20 and _is_pure_data_row(texts)
                    and last_label != ROW_LABEL_3
                    and all(b["anchors"] is not None for b in cur_blocks)):
                atoms = digit_merge(row_sorted)
                n_blocks = len(cur_blocks)
                if n_blocks == 1:
                    atom_groups = [atoms]
                else:
                    right_first_anchor = cur_blocks[1]["anchors"][0]
                    left = [a for a in atoms if a["x1"] < right_first_anchor - 30]
                    right = [a for a in atoms if a not in left]
                    atom_groups = [left, right]
                for blk, ats in zip(cur_blocks, atom_groups):
                    if not ats:
                        continue
                    cells = assign_to_anchors(merge_signs(ats), blk["anchors"])
                    try:
                        decoded = decode_cells(cells)
                    except ValueError:
                        continue
                    blk["rows"][last_label] = _add_cells(blk["rows"].get(last_label), decoded)
                last_top = row_top
                continue

            data_words = row_sorted[label_nwords:]
            if not data_words or not label:
                continue

            atoms = digit_merge(data_words)
            n_blocks = len(cur_blocks)

            if label == ROW_LABEL_3:
                merged = merge_signs(atoms)
                if n_blocks == 1:
                    groups = [merged]
                else:
                    half = len(merged) // 2 if len(merged) % 2 == 0 else None
                    groups = [merged[:half], merged[half:]] if half else [merged]
                for blk, ats in zip(cur_blocks, groups):
                    blk["rows"][label] = (decode_3([a["text"] for a in ats])
                                          if len(ats) == 3 else {"_raw": [a["text"] for a in ats]})
                last_label, last_top = label, row_top
                continue

            if label == "Wähler" and all(b["anchors"] is None for b in cur_blocks):
                anc = bootstrap_anchors(atoms, n_blocks)
                if anc:
                    for blk, a in zip(cur_blocks, anc):
                        blk["anchors"] = a

            if any(b["anchors"] is None for b in cur_blocks):
                last_label, last_top = label, row_top
                continue

            if n_blocks == 1:
                atom_groups = [atoms]
            else:
                right_first_anchor = cur_blocks[1]["anchors"][0]
                left = [a for a in atoms if a["x1"] < right_first_anchor - 30]
                right = [a for a in atoms if a not in left]
                atom_groups = [left, right]

            for blk, ats in zip(cur_blocks, atom_groups):
                cells = assign_to_anchors(merge_signs(ats), blk["anchors"])
                try:
                    blk["rows"][label] = decode_cells(cells)
                except ValueError as e:
                    blk["rows"][label] = {"_error": str(e)}
            last_label, last_top = label, row_top

    flush()
    return results


# =============================================================================
# main: parse, validate, emit
# =============================================================================
def main():
    fails = []

    def req(cond, msg):
        print(("  [ok]   " if cond else "  [FAIL] ") + msg)
        if not cond:
            fails.append(msg)

    print("Reading", os.path.relpath(PDF_2006, ROOT))
    res06 = parse_pdf(PDF_2006, ALL_LABELS_2006)
    print("Reading", os.path.relpath(PDF_2011, ROOT))
    res11 = parse_pdf(PDF_2011, ALL_LABELS_2011)

    wk06 = {r["wk"]: r for r in res06 if r["wk"] not in ("Land", "RB")}
    wk11 = {r["wk"]: r for r in res11 if r["wk"] not in ("Land", "RB")}
    land06 = next(r for r in res06 if r["wk"] == "Land")
    land11 = next(r for r in res11 if r["wk"] == "Land")

    print("\n=========== VALIDATION (BW 2001 + 2006 + 2011, Wahlkreis level) ===========")

    # (1) 70 Wahlkreise per report
    req(len(wk06) == 70, f"2006 report: 70 Wahlkreis blocks parsed ({len(wk06)})")
    req(len(wk11) == 70, f"2011 report: 70 Wahlkreis blocks parsed ({len(wk11)})")
    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # (2) per (Wahlkreis, year): party sum == Gültige; Wähler == Gültige+Ungültige
    def check_integrity(wk_dict, party_rows, field):
        bad = []
        for k, r in wk_dict.items():
            rows = r["rows"]
            gue = rows.get("Gültige Stimmen", {}).get(field)
            ung = rows.get("Ungültige Stimmen", {}).get(field)
            wae = rows.get("Wähler", {}).get(field)
            if wae is None or gue is None or ung is None:
                bad.append((k, "missing turnout", wae, gue, ung)); continue
            if wae != gue + ung:
                bad.append((k, "Wähler != Gültige+Ungültige", wae, gue, ung))
            s = sum((rows.get(p, {}).get(field) or 0) for p in party_rows)
            if s != gue:
                bad.append((k, "party sum != Gültige", s, gue))
        return bad

    bad = check_integrity(wk06, PARTY_ROWS_2006, "n_curr")
    req(not bad, f"2006: per-WK party sum == Gültige Stimmen; Wähler == Gültige+Ungültige [{len(bad)} bad]")
    for b in bad[:8]: print("    ", b)
    bad = check_integrity(wk06, PARTY_ROWS_2006, "n_prev")
    req(not bad, f"2001 (BW_2006 prior cols): per-WK party sum == Gültige Stimmen; Wähler == G+U [{len(bad)} bad]")
    for b in bad[:8]: print("    ", b)
    bad = check_integrity(wk11, PARTY_ROWS_2011, "n_curr")
    req(not bad, f"2011: per-WK party sum == Gültige Stimmen; Wähler == Gültige+Ungültige [{len(bad)} bad]")
    for b in bad[:8]: print("    ", b)

    # (3) sum over 70 Wahlkreise, per party/meta, == the report's own Land row
    META = ["Wahlberechtigte", "Wähler", "Ungültige Stimmen", "Gültige Stimmen"]

    def check_land(wk_dict, land, party_rows, field):
        bad = []
        for p in party_rows + META:
            mine = sum((wk_dict[k]["rows"].get(p, {}).get(field) or 0) for k in wk_dict)
            theirs = land["rows"].get(p, {}).get(field) or 0
            if mine != theirs:
                bad.append((p, mine, theirs))
        return bad

    bad = check_land(wk06, land06, PARTY_ROWS_2006, "n_curr")
    req(not bad, f"2006: WK-sum == Land row, all parties+meta [{len(bad)} bad]")
    for b in bad[:10]: print("    ", b)
    bad = check_land(wk06, land06, PARTY_ROWS_2006, "n_prev")
    req(not bad, f"2001: WK-sum == Land row (BW_2006 prior col), all parties+meta [{len(bad)} bad]")
    for b in bad[:10]: print("    ", b)
    bad = check_land(wk11, land11, PARTY_ROWS_2011, "n_curr")
    req(not bad, f"2011: WK-sum == Land row, all parties+meta [{len(bad)} bad]")
    for b in bad[:10]: print("    ", b)

    # (4) per-cell % reproduction wherever a % value was recovered
    # base row for each row's own printed %%: Wähler is a %% of Wahlberechtigte;
    # Ungültige/Gültige Stimmen and every party are a %% of Wähler... except the
    # report actually bases Ungültige/Gültige/parties on GÜLTIGE+UNGÜLTIGE, i.e.
    # on Wähler (turnout), for Ungültige/Gültige, and on GÜLTIGE STIMMEN for
    # every party row (the standard "vote share" denominator).
    BASE_ROW = {"Wähler": "Wahlberechtigte", "Ungültige Stimmen": "Wähler",
               "Gültige Stimmen": "Wähler"}

    def check_pct(wk_dict, party_rows):
        worst, n_checked, worst_ex = 0.0, 0, None
        for k, r in wk_dict.items():
            for label, cell in r["rows"].items():
                if not isinstance(cell, dict) or "n_curr" not in cell:
                    continue
                base_label = BASE_ROW.get(label, "Gültige Stimmen")
                for nfield, pfield in (("n_curr", "pct_curr"), ("n_prev", "pct_prev")):
                    v, p = cell.get(nfield), cell.get(pfield)
                    base = r["rows"].get(base_label, {}).get(nfield)
                    if v is None or p is None or not base:
                        continue
                    n_checked += 1
                    d = abs(100.0 * v / base - p)
                    if d > worst:
                        worst, worst_ex = d, (k, label, nfield, v, base_label, base, p)
        return worst, n_checked, worst_ex

    worst, n_checked, worst_ex = check_pct(wk06, PARTY_ROWS_2006)
    req(worst <= 0.055, f"2006: recomputed %% == printed %% wherever present ({n_checked} cells checked, max diff {worst:.4f}pp)")
    if worst > 0.055:
        print("    worst case:", worst_ex)
    worst, n_checked, _ = check_pct(wk11, PARTY_ROWS_2011)
    req(n_checked == 0 and worst == 0.0,
        f"2011: no %% glyphs recoverable via pdfplumber on this document (check (4) vacuous: {n_checked} cells)")

    # (5) pinned official statewide shares
    gue06 = sum((wk06[k]["rows"].get("Gültige Stimmen", {}).get("n_curr") or 0) for k in wk06)
    gue01 = sum((wk06[k]["rows"].get("Gültige Stimmen", {}).get("n_prev") or 0) for k in wk06)
    gue11 = sum((wk11[k]["rows"].get("Gültige Stimmen", {}).get("n_curr") or 0) for k in wk11)

    def check_shares(wk_dict, field, gue, official):
        bad = []
        for p, share in official.items():
            v = sum((wk_dict[k]["rows"].get(p, {}).get(field) or 0) for k in wk_dict)
            got = 100.0 * v / gue
            if abs(got - share) > 0.1:
                bad.append((p, round(got, 2), share))
        return bad

    bad = check_shares(wk06, "n_curr", gue06, OFFICIAL[2006])
    req(not bad, f"2006 pinned statewide shares match (+-0.1pp) [{bad}]")
    bad = check_shares(wk06, "n_prev", gue01, OFFICIAL[2001])
    req(not bad, f"2001 pinned statewide shares match (+-0.1pp) [{bad}]")
    bad = check_shares(wk11, "n_curr", gue11, OFFICIAL[2011])
    req(not bad, f"2011 pinned statewide shares match (+-0.1pp) [{bad}]")

    # (6) starred-Wahlkreis sets, derived from each report's own header cells, pinned
    starred06 = sorted((k for k in wk06 if wk06[k]["umgerechnet"]), key=int)
    starred11 = sorted((k for k in wk11 if wk11[k]["umgerechnet"]), key=int)
    print(f"\n  2001-recomputed-onto-2006-boundaries (starred in BW_2006 report), n={len(starred06)}:")
    print(f"    {starred06}")
    print(f"  2006-recomputed-onto-2011-boundaries (starred in BW_2011 report), n={len(starred11)}:")
    print(f"    {starred11}")
    req(len(starred06) == N_STARRED_2006, f"starred06 count == {N_STARRED_2006}")
    req(len(starred11) == N_STARRED_2011, f"starred11 count == {N_STARRED_2011}")

    # (7) INDEPENDENT SOURCE cross-check: BW_2011's prior(2006) cols vs BW_2006's current cols
    unstarred = [k for k in wk06 if k not in starred11]
    mism = []
    for k in unstarred:
        for p in SHARED_LABELS:
            a = wk06[k]["rows"].get(p, {}).get("n_curr")
            b = wk11[k]["rows"].get(p, {}).get("n_prev")
            if (a or 0) != (b or 0):
                mism.append((k, p, a, b))
        for m in META:
            a = wk06[k]["rows"].get(m, {}).get("n_curr")
            b = wk11[k]["rows"].get(m, {}).get("n_prev")
            if (a or 0) != (b or 0):
                mism.append((k, m, a, b))
    req(not mism, f"2011-report's 2006-prior-cols == 2006-report's 2006-current-cols on the "
                  f"{len(unstarred)} unstarred WK, shared-label parties+meta [{len(mism)} mismatches]")
    for m in mism[:10]: print("    ", m)

    graft_mism = []
    for wk11_label, wk06_label in GRAFTS.items():
        for k in unstarred:
            a = wk06[k]["rows"].get(wk06_label, {}).get("n_curr")
            b = wk11[k]["rows"].get(wk11_label, {}).get("n_prev")
            if (a or 0) != (b or 0):
                graft_mism.append((k, wk06_label, "->", wk11_label, a, b))
    req(not graft_mism, f"footnoted grafts (WASG->DIE LINKE2), Deutschland->Volksabstimmung1)) "
                        f"match on unstarred WK [{len(graft_mism)} bad]")
    for m in graft_mism[:10]: print("    ", m)

    fringe_mism = []
    for k in unstarred:
        a = sum((wk06[k]["rows"].get(p, {}).get("n_curr") or 0) for p in FRINGE_2006_ONLY)
        b = wk11[k]["rows"].get("Sonstige (nur 2006)", {}).get("n_prev") or 0
        if a != b:
            fringe_mism.append((k, a, b))
    req(not fringe_mism, f"2006-only fringe parties sum == 2011-report's 'Sonstige (nur 2006)' "
                         f"prior-col, unstarred WK [{len(fringe_mism)} bad]")
    for m in fringe_mism[:10]: print("    ", m)

    tot_mism = []
    for p in SHARED_LABELS:
        a = sum((wk06[k]["rows"].get(p, {}).get("n_curr") or 0) for k in wk06)
        b = sum((wk11[k]["rows"].get(p, {}).get("n_prev") or 0) for k in wk11)
        if a != b:
            tot_mism.append((p, a, b))
    for wk11_label, wk06_label in GRAFTS.items():
        a = sum((wk06[k]["rows"].get(wk06_label, {}).get("n_curr") or 0) for k in wk06)
        b = sum((wk11[k]["rows"].get(wk11_label, {}).get("n_prev") or 0) for k in wk11)
        if a != b:
            tot_mism.append((f"{wk06_label}->{wk11_label}", a, b))
    a = sum((wk06[k]["rows"].get(p, {}).get("n_curr") or 0) for k in wk06 for p in FRINGE_2006_ONLY)
    b = sum((wk11[k]["rows"].get("Sonstige (nur 2006)", {}).get("n_prev") or 0) for k in wk11)
    if a != b:
        tot_mism.append(("fringe->Sonstige(nur 2006)", a, b))
    req(not tot_mism, f"statewide (all 70 WK, incl. the 37 boundary-recomputed ones) 2006 totals "
                      f"match exactly between the two reports -- i.e. BW's boundary recomputation "
                      f"is exactly vote-conserving, once matched by footnoted predecessor "
                      f"[{len(tot_mism)} bad]")
    for m in tot_mism[:10]: print("    ", m)

    distinct_party_raw = sorted(set(list(PARTY_ROWS_2006) + list(PARTY_ROWS_2011)) -
                                {"Sonstige (nur 2001)", "Sonstige (nur 2006)"})
    print(f"\n  distinct party_raw (pre-cleaning, union of both reports' own-party rows), "
          f"n={len(distinct_party_raw)}:")
    print(f"    {distinct_party_raw}")

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        for f in fails:
            print(" -", f)
        sys.exit(1)

    # --- emit ------------------------------------------------------------------
    def on_ballot(wk_dict, party_rows, field):
        return [p for p in party_rows
                if any(wk_dict[k]["rows"].get(p, {}).get(field) is not None for k in wk_dict)]

    rows = []

    def emit_year(year, wk_dict, party_rows, field, starred_set):
        parties = on_ballot(wk_dict, party_rows, field)
        for k in sorted(wk_dict, key=int):
            r = wk_dict[k]["rows"]
            elig = r.get("Wahlberechtigte", {}).get(field)
            wae = r.get("Wähler", {}).get(field)
            gue = r.get("Gültige Stimmen", {}).get(field)
            ung = r.get("Ungültige Stimmen", {}).get(field)
            for p in parties:
                v = r.get(p, {}).get(field)
                rows.append({
                    "state_abbr": STATE_ABBR, "state": STATE_NAME,
                    "election_year": year, "election_date": ELECTION_DATE[year],
                    "wkr_nr": k, "wkr_name": wk_dict[k]["name"], "stimme": STIMME,
                    "eligible_voters": elig, "number_voters": wae,
                    "valid_votes": gue, "invalid_votes": ung,
                    "party_raw": CLEAN_PARTY.get(p, p), "votes": "" if v is None else v,
                    "flag_wkr_boundaries_recomputed": int(k in starred_set),
                })
        return len(parties)

    n06 = emit_year(2006, wk06, PARTY_ROWS_2006, "n_curr", set())
    n01 = emit_year(2001, wk06, PARTY_ROWS_2006, "n_prev", set(starred06))
    n11 = emit_year(2011, wk11, PARTY_ROWS_2011, "n_curr", set())

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)

    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    print(f"  2006: 70 WK x {n06} parties = {70 * n06} rows (flag always 0)")
    print(f"  2001: 70 WK x {n01} parties = {70 * n01} rows "
          f"({len(starred06)} WK flagged boundary-recomputed)")
    print(f"  2011: 70 WK x {n11} parties = {70 * n11} rows (flag always 0)")


if __name__ == "__main__":
    main()
