#!/usr/bin/env python3
"""Stage-0 parser for HESSEN Landtagswahl results at WAHLKREIS level, 2018 + 2013.

Source (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Hessen/
    HE_2018_Landtagswahl_Wahlkreis_BVII2-4.pdf
  = Hessisches Statistisches Landesamt, Statistischer Bericht B VII 2-4 - 5j/18,
    "Die Wahl zum Hessischen Landtag am 28. Oktober 2018 - Endgueltige Ergebnisse"
    (3., aktualisierte Auflage, August 2024).

Why a PDF parser: HSL publishes machine-readable constituency results only for
2023 (the open-data CSV parsed by parse_HE.R).  For every earlier election the
Wahlkreis figures exist solely in the printed/scanned "Statistische Berichte".
The 2018 issue is the one issue of series B VII 2-4 that is digitised, and it
carries a TEXT LAYER, so no OCR is required.  Its Table 12 reports BOTH the 2018
and the 2013 results per Wahlkreis, which is where HE 2013 comes from.

  Table  1  (PDF p. 14)      statewide totals, parties named explicitly.
                             Used ONLY as the validation fixture.
  Table 12.1 (PDF p. 42-53)  per Wahlkreis, WAHLKREISSTIMMEN (= Erststimme),
                             2018 and 2013, two-page spreads.
  Table 12.2 (PDF p. 54-65)  per Wahlkreis, LANDESSTIMMEN (= Zweitstimme),
                             2018 and 2013, two-page spreads.
  Table 15   (PDF p. 262)    the six parties that Table 12.2 lumps into
                             "Sonstige" for 2018, per Wahlkreis.  Merged in, so
                             HE 2018 Landesstimmen has NO residual.

!! 2013 IS ON THE 2018 WAHLKREIS BOUNDARIES !!
   The Dec-2017 LWG amendment re-cut some Wahlkreise.  Both this report and the
   companion "Vergleichszahlen" report (B VII 2-1 - 5j/18) state that the 2013
   figures were recomputed onto the 2018 Wahlkreiseinteilung ("Fuer diese
   Gegenueberstellung wurden die Wahlkreisergebnisse auf die neue [...]
   Wahlkreiseinteilung umgerechnet").  Eleven units moved: Lichtenfels 5->6,
   Nieste 2->9, Ludwigsau 11->10, Eiterfeld 14->11, Fernwald 18->19, Laubach
   19->20, Heidenrod 29->28, Frankfurt Stadtbezirk 531 (Schwanheim) 37->34,
   Niederdorfelden 40->41, Gross-Rohrheim 54->55.  The genuine 2013-boundary
   report (B VII 2-4 - 5j/13) is not digitised anywhere, so every published HE
   2013 row carries flag_wkr_boundaries_recomputed = 1.  2018 rows carry 0.

PARSING METHOD (coordinate based; do NOT switch to naive whitespace splitting).
  German thousands separator in this report is a SPACE, so "4 372 788" arrives
  as three separate words and a text-order split cannot tell "3 818 88 122"
  (= 3818, 88122) from (3, 81888, 122).  Numbers are RIGHT-ALIGNED, intra-number
  gaps are ~2 pt and inter-column gaps >= 8 pt, so tokens are merged on the gap
  and every merged cell is then required to land on the column's right edge.

  Column IDENTITY is pinned (COLS_* below) rather than derived from the stacked,
  hyphenated headers ("PIRA-/TEN", "MENSCH-/LICHE/WELT").  It was established
  from the header word x-positions and is re-verified on every run against
  Table 1, which names each party: the 55 Wahlkreise must sum to Table 1's
  statewide count for EVERY party in EVERY series, exactly.  Getting the column
  order wrong therefore cannot pass silently.  (The naive reading of the text
  layer does get it wrong: it puts ODP where "Die PARTEI" belongs.)

Zeichenerklaerung (report p. 3): "x" = cell blocked (party did not stand) -> NA;
"—" = exactly zero -> 0.

Output: data/state_elections/processed/wahlkreis/he_pdf/HE_2018_2013_pdf_long.csv
        (read by parsers/parse_HE.R, which appends the 2023 open-data results)
Run:    python3 code/state_elections_wahlkreis/parsers/00_he_pdf_parse.py
"""

import csv
import itertools
import os
import re
import sys

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
PDF = os.path.join(
    ROOT, "data", "state_elections", "raw", "Landtagswahlen_Wahlkreis", "Hessen",
    "HE_2018_Landtagswahl_Wahlkreis_BVII2-4.pdf",
)
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "he_pdf")
OUT = os.path.join(OUT_DIR, "HE_2018_2013_pdf_long.csv")

STATE_ABBR, STATE_NAME = "HE", "Hessen"
N_WKR = 55
ELECTION_DATE = {2018: "2018-10-28", 2013: "2013-09-22"}

# --- 1-based PDF page numbers (verified by page-header scan) ------------------
P_TABLE1 = 14
P_T121 = list(range(42, 54))   # 12.1 Wahlkreisstimmen, left/right spreads
P_T122 = list(range(54, 66))   # 12.2 Landesstimmen,    left/right spreads
P_T15 = 262                    # 15. sonstige Parteien 2018 (Landesstimmen)

# --- pinned column layouts ----------------------------------------------------
# Left page of both 12.1 and 12.2 (11 numeric columns).
COLS_LEFT = [
    "eligible_voters", "_sperrvermerk", "_wahlschein_12a2",
    "number_voters", "_waehler_wahlschein", "invalid_votes", "valid_votes",
    "CDU", "SPD", "GRÜNE", "DIE LINKE",
]
# Right page of 12.1 (Wahlkreisstimmen), 16 numeric columns.
COLS_RIGHT_121 = [
    "FDP", "AfD", "PIRATEN", "FREIE WÄHLER", "Die PARTEI", "ÖDP",
    "DIE VIOLETTEN", "LKR", "MENSCHLICHE WELT", "Tierschutzpartei", "V-Partei3",
    "APPD", "ÖkoLinX Hessen", "DiB", "NEV", "Sonstige",
]
# Right page of 12.2 (Landesstimmen), 14 numeric columns.
COLS_RIGHT_122 = [
    "FDP", "AfD", "PIRATEN", "FREIE WÄHLER", "NPD", "Die PARTEI", "ÖDP",
    "Graue Panther", "BüSo", "AD-Demokraten", "Bündnis C", "BGE",
    "DIE VIOLETTEN", "Sonstige",
]
# Table 15: the six parties inside 12.2's 2018 "Sonstige" (Anzahl,% per party).
COLS_T15 = [
    "LKR", "MENSCHLICHE WELT", "Die Humanisten", "Gesundheitsforschung",
    "Tierschutzpartei", "V-Partei3",
]
META_COLS = {c for c in COLS_LEFT if c.startswith("_") or c in (
    "eligible_voters", "number_voters", "invalid_votes", "valid_votes")}

# Table 1 party labels -> the label this parser emits (they differ only in the
# superscript 3 of V-Partei3, which the two tables typeset differently).
T1_ALIAS = {"V-Partei³": "V-Partei3"}

# Independent cross-check + the 2013 REP breakout: the companion report
#   HE_2018_2013_Landtagswahl_Wahlkreis_Vergleichszahlen_BVII2-1.pdf
# = B VII 2-1 - 5j/18 "Vergleichszahlen zur Landtagswahl 2018", published July
# 2018, i.e. six years before the 3rd edition of B VII 2-4 parsed above.  Its
# Table 6 reports the SAME 2013-on-2018-boundary results per Wahlkreis, so every
# figure the two documents share must agree exactly; the run aborts if not.
# It additionally breaks REP out of what B VII 2-4 lumps into "Sonstige", so the
# 2013 REP column is grafted in (and subtracted from Sonstige) from here.
PDF_VGL = os.path.join(
    ROOT, "data", "state_elections", "raw", "Landtagswahlen_Wahlkreis", "Hessen",
    "HE_2018_2013_Landtagswahl_Wahlkreis_Vergleichszahlen_BVII2-1.pdf",
)
P_VGL_T6 = list(range(22, 34))          # 1-based; Table 6 spreads
VGL_ROWS = ("L13L", "L13W", "E14", "B17Z")
VGL_LEFT = ["eligible_voters", "number_voters", "_turnout_pct", "invalid_votes",
            "_invalid_pct", "valid_votes", "CDU", "_", "SPD", "_", "GRÜNE", "_",
            "DIE LINKE", "_"]
VGL_RIGHT = ["FDP", "_", "AfD", "_", "PIRATEN", "_", "FREIE WÄHLER", "_", "NPD", "_",
             "Die PARTEI", "_", "REP", "_", "Tierschutzpartei", "_", "Sonstige", "_"]
VGL_XMIN = 145.0                        # left page: label column ends before this

# The two reports agree EXACTLY on 53 of the 55 Wahlkreise.  They differ, for
# 2013 only, on Frankfurt am Main I (WK 34) and Frankfurt am Main IV (WK 37),
# and the difference is precisely the Schwanheim Stadtbezirk 531 that the 2018
# Wahlkreiseinteilung moved from WK 37 to WK 34: the July-2018 Vergleichszahlen
# recomputed that transfer, the August-2024 results report did not.  Every
# quantity mirrors between the two Wahlkreise (WK34 + WK37 is identical in both
# documents), which is asserted below - so this is a documented difference in
# what the two reports mean by "2013", not a parsing error.
#
# GERDA publishes the B VII 2-4 figures, i.e. WK 34 and WK 37 carry their own
# 2013 boundaries while the other 53 Wahlkreise are recomputed onto the 2018
# ones.  That is what flag_wkr_boundaries_recomputed records, per row.
VGL_BOUNDARY_DIFF_WKR = (34, 37)

MERGE_GAP = 4.0     # pt; intra-number gaps are ~2, inter-column gaps >= 8
EDGE_TOL = 3.5      # pt; the % and Anzahl rows of one column can differ by ~1
DASH = {"—", "–", "-"}


# =============================================================================
# generic geometry helpers
# =============================================================================
def page_lines(page, ytol=3.0):
    """Words grouped into visual lines, each sorted left to right.

    Grouped by running distance rather than by rounding `top` into fixed
    buckets: a row's label and its figures can sit ~1 pt apart (different
    baselines), and a bucket boundary falling between them silently splits the
    row in two - which in Table 1 dropped six party rows (AD-Demokraten, DIE
    VIOLETTEN, Die Humanisten, V-Partei3, DiB, REP) from the validation fixture.
    Table rows here are >= 9 pt apart, so ytol = 3 pt cannot merge two rows.
    """
    ws = page.extract_words(keep_blank_chars=False, use_text_flow=False)
    ws.sort(key=lambda w: (w["top"], w["x0"]))
    out, cur, cur_top = [], [], None
    for w in ws:
        if cur and w["top"] - cur_top > ytol:
            out.append(sorted(cur, key=lambda t: t["x0"]))
            cur, cur_top = [], None
        if not cur:
            cur_top = w["top"]
        cur.append(w)
    if cur:
        out.append(sorted(cur, key=lambda t: t["x0"]))
    return out


def merge_cells(toks):
    """Merge space-separated German thousands groups into single cells.

    A token is glued to the running cell when it is exactly three digits, the
    cell so far is all digits, and the horizontal gap is below MERGE_GAP.
    Returns [(text, x1)] with x1 = right edge of the cell.
    """
    cells = []
    for t in toks:
        txt, x0, x1 = t["text"], t["x0"], t["x1"]
        if cells:
            ptxt, px1 = cells[-1]
            if (re.fullmatch(r"\d{3}", txt) and re.fullmatch(r"\d+", ptxt)
                    and x0 - px1 < MERGE_GAP):
                cells[-1] = (ptxt + txt, x1)
                continue
        cells.append((txt, x1))
    return cells


def is_value(txt):
    return txt in DASH or txt == "x" or bool(re.fullmatch(r"\d+([.,]\d+)?", txt))


def to_num(txt):
    """'x' -> None (party did not stand); dash -> 0; else the number."""
    if txt == "x":
        return None
    if txt in DASH:
        return 0
    return float(txt.replace(",", ".")) if "," in txt else int(txt)


def value_cells(toks):
    """Trailing run of numeric/x/dash cells on a line, right-aligned."""
    cells = merge_cells(toks)
    vals = []
    for txt, x1 in reversed(cells):
        if is_value(txt):
            vals.append((txt, x1))
        else:
            break
    return list(reversed(vals))


def check_edges(vals, edges, where):
    """Every cell must sit on its column's right edge (guards a bad merge)."""
    for i, (txt, x1) in enumerate(vals):
        if abs(x1 - edges[i]) > EDGE_TOL:
            raise SystemExit(
                f"[{where}] cell {i} '{txt}' right edge {x1:.1f} does not match "
                f"column edge {edges[i]:.1f} - column alignment broke")


# =============================================================================
# Table 12.1 / 12.2 - the two-page spreads
# =============================================================================
BLOCK_RE = re.compile(r"^([1-9]\d?)$")


def parse_spread_side(page, ncol, side):
    """Parse one page of a 12.x spread.

    Returns (counts, pcts), each {block_key: {year: [values]}} with block_key
    'Land' or 1..55 and len(values) == ncol.

    Row identification differs by side, because only the left page labels its
    rows.  Left: the row itself carries "18|13" + "Anzahl|%".  Right: the four
    rows are unlabelled, so they are identified by their vertical offset from
    the block header (Anzahl rows sit within ~22 pt of it, the two % rows
    below).  A block whose rows are cut by a page break simply contributes the
    rows that are on the page; assemble_records() then fails loudly if any
    (Wahlkreis, year) never got its Anzahl row.
    """
    counts, pcts = {}, {}
    cur, cur_top, edges, seen = None, None, None, 0
    for toks in page_lines(page):
        texts = [t["text"] for t in toks]
        if not texts:
            continue
        top = toks[0]["top"]

        # --- labelled data row (left page) ----------------------------------
        labelled = (len(texts) > 1 and texts[0] in ("18", "13")
                    and texts[1] in ("Anzahl", "%"))
        if not labelled:
            # --- block header ------------------------------------------------
            key = None
            if texts[0] == "Land" and len(texts) > 1 and texts[1] == "Hessen":
                key = "Land"
            elif texts[-2:] == ["Land", "Hessen"]:
                key = "Land"
            elif (len(texts) > 1 and BLOCK_RE.fullmatch(texts[0])
                  and not is_value(texts[1])):
                nr = int(texts[0])
                if 1 <= nr <= N_WKR:
                    key = nr
            if key is not None:
                cur, cur_top, seen = key, top, 0
                continue

        vals = value_cells(toks)
        if len(vals) != ncol or cur is None:
            continue
        if edges is None:
            edges = [x for _, x in vals]
        check_edges(vals, edges, f"p{page.page_number} block {cur} row {seen}")
        nums = [to_num(t) for t, _ in vals]

        if labelled:
            year = 2018 if texts[0] == "18" else 2013
            (counts if texts[1] == "Anzahl" else pcts).setdefault(cur, {})[year] = nums
        else:
            # unlabelled (right page): position within the block decides
            if seen > 3:
                raise SystemExit(f"p{page.page_number} block {cur}: more than 4 data rows")
            is_pct = seen >= 2
            offset = top - cur_top
            if (offset >= 22) != is_pct:
                raise SystemExit(
                    f"p{page.page_number} block {cur} row {seen}: vertical offset "
                    f"{offset:.1f} pt contradicts the fixed 18/13/18%/13% row order")
            year = 2018 if seen % 2 == 0 else 2013
            (pcts if is_pct else counts).setdefault(cur, {})[year] = nums
        seen += 1
    return counts, pcts


def parse_table12(pages, cols_right, label):
    """Parse a whole 12.x table (six spreads) into per-Wahlkreis records."""
    left_c, left_p, right_c, right_p = {}, {}, {}, {}
    for pno in pages:
        page = pdfplumber.open(PDF).pages[pno - 1]
        if (pno - pages[0]) % 2 == 0:
            c, p = parse_spread_side(page, len(COLS_LEFT), "left")
            _merge_into(left_c, c), _merge_into(left_p, p)
        else:
            c, p = parse_spread_side(page, len(cols_right), "right")
            _merge_into(right_c, c), _merge_into(right_p, p)
    keys = sorted(set(left_c) & set(right_c), key=lambda k: (k == "Land", k))
    missing = ({"Land"} | set(range(1, N_WKR + 1))) - set(keys)
    if missing:
        raise SystemExit(f"[{label}] missing blocks: {sorted(missing, key=str)}")
    rec = {}
    for k in keys:
        rec[k] = {}
        for yr in (2018, 2013):
            for src_name, store in (("left", left_c), ("right", right_c)):
                if yr not in store.get(k, {}):
                    raise SystemExit(f"[{label}] block {k}: no {yr} Anzahl row on the "
                                     f"{src_name} page (page break inside the block?)")
            row = dict(zip(COLS_LEFT, left_c[k][yr]))
            row.update(dict(zip(cols_right, right_c[k][yr])))
            pct = dict(zip(COLS_LEFT, left_p.get(k, {}).get(yr, [None] * len(COLS_LEFT))))
            pct.update(dict(zip(cols_right, right_p.get(k, {}).get(yr, [None] * len(cols_right)))))
            rec[k][yr] = (row, pct)
    return rec


def _merge_into(dst, src):
    for k, v in src.items():
        dst.setdefault(k, {}).update(v)


# =============================================================================
# Table 1 (statewide, parties named) and Table 15 (2018 sonstige per Wahlkreis)
# =============================================================================
T1_SERIES = ["2018_wk", "2018_ls", "2013_wk", "2013_ls", "btw2017"]


def parse_table1():
    """Statewide fixture: {series: {party: count}} plus the header aggregates."""
    page = pdfplumber.open(PDF).pages[P_TABLE1 - 1]
    out = {s: {} for s in T1_SERIES}
    edges = None
    for toks in page_lines(page):
        cells = merge_cells(toks)
        vals, i = [], len(cells)
        while i > 0 and is_value(cells[i - 1][0]):
            i -= 1
            vals.append(cells[i])
        vals.reverse()
        if len(vals) != 10:
            continue
        label = " ".join(t for t, _ in cells[:i]).strip()
        if not label:
            continue
        if edges is None:
            edges = [x for _, x in vals]
        check_edges(vals, edges, f"table1 '{label}'")
        label = T1_ALIAS.get(label, label)
        for j, s in enumerate(T1_SERIES):          # (Anzahl, %) pairs
            out[s][label] = to_num(vals[2 * j][0])
    for s in T1_SERIES:
        if "Gültige Stimmen" not in out[s]:
            raise SystemExit("table 1: could not locate 'Gültige Stimmen' row")
    return out


def parse_table15():
    """{wkr_nr: {party: count}} for the six 2018-Landesstimmen sonstige parties."""
    page = pdfplumber.open(PDF).pages[P_T15 - 1]
    out, edges = {}, None
    for toks in page_lines(page):
        cells = merge_cells(toks)
        vals, i = [], len(cells)
        while i > 0 and is_value(cells[i - 1][0]):
            i -= 1
            vals.append(cells[i])
        vals.reverse()
        if len(vals) != 2 * len(COLS_T15):
            continue
        head = [t for t, _ in cells[:i]]
        if not head:
            continue
        if head[:2] == ["Land", "Hessen"]:
            key = "Land"
        elif BLOCK_RE.fullmatch(head[0]) and 1 <= int(head[0]) <= N_WKR:
            key = int(head[0])
        else:
            continue
        if edges is None:
            edges = [x for _, x in vals]
        check_edges(vals, edges, f"table15 {key}")
        out[key] = {p: to_num(vals[2 * j][0]) for j, p in enumerate(COLS_T15)}
    missing = ({"Land"} | set(range(1, N_WKR + 1))) - set(out)
    if missing:
        raise SystemExit(f"table 15: missing rows {sorted(missing, key=str)}")
    return out


def wahlkreis_names():
    """Nr -> name, read from the 12.1 block headers (left pages)."""
    names = {}
    for pno in P_T121[::2]:
        page = pdfplumber.open(PDF).pages[pno - 1]
        for toks in page_lines(page):
            texts = [t["text"] for t in toks]
            if (len(texts) > 1 and BLOCK_RE.fullmatch(texts[0])
                    and not is_value(texts[1]) and toks[0]["x0"] < 70):
                nr = int(texts[0])
                if 1 <= nr <= N_WKR:
                    names.setdefault(nr, " ".join(texts[1:]))
    if len(names) != N_WKR:
        raise SystemExit(f"only {len(names)}/{N_WKR} Wahlkreis names found")
    return names


# =============================================================================
# Vergleichszahlen B VII 2-1 Table 6 - independent 2013 source
# =============================================================================
def parse_vgl_side(page, cols, side):
    """One page of a Table-6 spread -> {block_key: {series: [values]}}."""
    out, cur, cur_top, edges, seen = {}, None, None, None, 0
    for toks in page_lines(page):
        texts = [t["text"] for t in toks]
        if not texts:
            continue
        top = toks[0]["top"]
        labelled = side == "left" and texts[0] in ("L", "E", "B")
        if not labelled:
            key = None
            if texts[:2] == ["Land", "Hessen"] or texts[-2:] == ["Land", "Hessen"]:
                key = "Land"
            elif (len(texts) > 1 and BLOCK_RE.fullmatch(texts[0])
                  and not is_value(texts[1])):
                nr = int(texts[0])
                if 1 <= nr <= N_WKR:
                    key = nr
            if key is not None:
                cur, cur_top, seen = key, top, 0
                continue
        # the label column ("L 13 W") holds digits too, so cut it off by x
        body = [t for t in toks if t["x0"] >= VGL_XMIN] if side == "left" else toks
        vals = value_cells(body)
        if len(vals) != len(cols) or cur is None:
            continue
        if edges is None:
            edges = [x for _, x in vals]
        check_edges(vals, edges, f"vgl p{page.page_number} block {cur} row {seen}")
        if labelled:
            ser = {("L", "13", "L"): "L13L", ("L", "13", "W"): "L13W",
                   ("E", "14"): "E14", ("B", "17", "Z"): "B17Z"}.get(tuple(texts[:3]))
            if ser is None:
                ser = {("E", "14"): "E14"}.get(tuple(texts[:2]))
            if ser is None:
                raise SystemExit(f"vgl p{page.page_number}: unknown row label {texts[:3]}")
        else:
            if seen > 3:
                raise SystemExit(f"vgl p{page.page_number} block {cur}: >4 data rows")
            ser = VGL_ROWS[seen]
        out.setdefault(cur, {})[ser] = [to_num(t) for t, _ in vals]
        seen += 1
    return out


def parse_vgl():
    """{block_key: {'L13L'|'L13W': {quantity: value}}} from Table 6."""
    left, right = {}, {}
    doc = pdfplumber.open(PDF_VGL)
    for pno in P_VGL_T6:
        page = doc.pages[pno - 1]
        if (pno - P_VGL_T6[0]) % 2 == 0:
            _merge_into(left, parse_vgl_side(page, VGL_LEFT, "left"))
        else:
            _merge_into(right, parse_vgl_side(page, VGL_RIGHT, "right"))
    rec = {}
    for k in sorted(set(left) & set(right), key=str):
        rec[k] = {}
        for ser in ("L13L", "L13W"):
            if ser not in left.get(k, {}) or ser not in right.get(k, {}):
                raise SystemExit(f"vgl block {k}: {ser} row missing (page break?)")
            row = {}
            for name, v in zip(VGL_LEFT, left[k][ser]):
                if name != "_":
                    row[name] = v
            for name, v in zip(VGL_RIGHT, right[k][ser]):
                if name != "_":
                    row[name] = v
            rec[k][ser] = row
    missing = ({"Land"} | set(range(1, N_WKR + 1))) - set(rec)
    if missing:
        raise SystemExit(f"vgl: missing blocks {sorted(missing, key=str)}")
    return rec


# =============================================================================
# assemble + validate + write
# =============================================================================
def party_cols(cols_right):
    return ["CDU", "SPD", "GRÜNE", "DIE LINKE"] + list(cols_right)


def main():
    fails = []

    def req(cond, msg):
        print(("  [ok]   " if cond else "  [FAIL] ") + msg)
        if not cond:
            fails.append(msg)

    print("Reading", os.path.relpath(PDF, ROOT))
    t1 = parse_table1()
    t121 = parse_table12(P_T121, COLS_RIGHT_121, "12.1 Wahlkreisstimmen")
    t122 = parse_table12(P_T122, COLS_RIGHT_122, "12.2 Landesstimmen")
    t15 = parse_table15()
    vgl = parse_vgl()
    names = wahlkreis_names()
    wkrs = list(range(1, N_WKR + 1))

    tables = {"erststimme": (t121, COLS_RIGHT_121), "zweitstimme": (t122, COLS_RIGHT_122)}
    series_of = {("erststimme", 2018): "2018_wk", ("erststimme", 2013): "2013_wk",
                 ("zweitstimme", 2018): "2018_ls", ("zweitstimme", 2013): "2013_ls"}

    print("\n=========== VALIDATION (HE 2018 + 2013, Wahlkreis level) ===========")

    # (1) Wahlkreis blocks -----------------------------------------------------
    req(len(names) == N_WKR and len(set(names.values())) == N_WKR,
        f"{len(names)} Wahlkreise with unique names")

    # (2) per-row vote integrity: sum(parties) == gueltige Stimmen -------------
    for stimme, (tab, cr) in tables.items():
        pcs = party_cols(cr)
        worst = 0
        for k in wkrs:
            for yr in (2018, 2013):
                row = tab[k][yr][0]
                s = sum(v for p in pcs if (v := row[p]) is not None)
                worst = max(worst, abs(s - row["valid_votes"]))
        req(worst == 0, f"{stimme}: sum(party votes) == valid_votes in all "
                        f"{2 * N_WKR} Wahlkreis-years (max |diff| = {worst})")

    # (3) 55 Wahlkreise reproduce Table 1 statewide, per party -----------------
    #     This is what pins the COLUMN IDENTITY: Table 1 names every party.
    for (stimme, yr), ser in series_of.items():
        tab, cr = tables[stimme]
        bad = []
        for p in party_cols(cr):
            if p == "Sonstige":
                continue
            mine = sum(v for k in wkrs if (v := tab[k][yr][0][p]) is not None)
            ref = t1[ser].get(p)
            ref = 0 if ref is None else ref
            if mine != ref:
                bad.append(f"{p}: parser {mine} vs Table 1 {ref}")
        req(not bad, f"{stimme} {yr}: all {len(party_cols(cr)) - 1} named parties "
                     f"match Table 1 statewide exactly")
        for b in bad:
            print("           ", b)

    # (4) turnout aggregates reproduce Table 1 --------------------------------
    agg = {"eligible_voters": "Wahlberechtigte", "number_voters": "Wähler / Wahlbeteiligung",
           "invalid_votes": "Ungültige Stimmen", "valid_votes": "Gültige Stimmen"}
    for (stimme, yr), ser in series_of.items():
        tab, _ = tables[stimme]
        bad = [f"{c}: {sum(tab[k][yr][0][c] for k in wkrs)} vs {t1[ser][lab]}"
               for c, lab in agg.items()
               if sum(tab[k][yr][0][c] for k in wkrs) != t1[ser][lab]]
        req(not bad, f"{stimme} {yr}: Wahlberechtigte/Wähler/ungültig/gültig match Table 1")
        for b in bad:
            print("           ", b)

    # (5) Land Hessen block equals the sum of its Wahlkreise -------------------
    for stimme, (tab, cr) in tables.items():
        bad = []
        for yr in (2018, 2013):
            for p in party_cols(cr) + ["valid_votes", "eligible_voters"]:
                mine = sum(v for k in wkrs if (v := tab[k][yr][0][p]) is not None)
                land = tab["Land"][yr][0][p]
                if (land or 0) != mine:
                    bad.append(f"{yr} {p}: WK sum {mine} vs Land row {land}")
        req(not bad, f"{stimme}: 55 Wahlkreise sum to the report's own 'Land Hessen' row")
        for b in bad[:6]:
            print("           ", b)

    # (6) recomputed shares reproduce the report's printed percentages ---------
    for stimme, (tab, cr) in tables.items():
        worst, where = 0.0, ""
        for k in wkrs:
            for yr in (2018, 2013):
                row, pct = tab[k][yr]
                for p in party_cols(cr):
                    if row[p] is None or pct.get(p) is None:
                        continue
                    d = abs(100 * row[p] / row["valid_votes"] - pct[p])
                    if d > worst:
                        worst, where = d, f"WK{k} {yr} {p}"
        req(worst <= 0.06, f"{stimme}: recomputed % == printed % everywhere "
                           f"(max |diff| = {worst:.3f} pp at {where})")

    # (7) Table 15 reconciles with 12.2's 2018 'Sonstige' ----------------------
    bad = [f"WK{k}: table15 {sum(t15[k].values())} vs 12.2 Sonstige {t122[k][2018][0]['Sonstige']}"
           for k in wkrs
           if sum(t15[k].values()) != (t122[k][2018][0]["Sonstige"] or 0)]
    req(not bad, "Table 15: the six parties sum to Table 12.2's 2018 'Sonstige' in every Wahlkreis")
    for b in bad[:6]:
        print("           ", b)
    bad = [f"{p}: {sum(t15[k][p] for k in wkrs)} vs {t1['2018_ls'][p]}"
           for p in COLS_T15 if sum(t15[k][p] for k in wkrs) != t1["2018_ls"][p]]
    req(not bad, "Table 15: each party's 55 Wahlkreise match Table 1 statewide")
    for b in bad:
        print("           ", b)

    # (8) the residual 'Sonstige' columns are what Table 1 says they are ------
    for stimme, (tab, cr) in tables.items():
        for yr in (2018, 2013):
            ser = series_of[(stimme, yr)]
            named = set(party_cols(cr)) - {"Sonstige"}
            if yr == 2018 and stimme == "zweitstimme":
                named |= set(COLS_T15)
            expect = sum(v for p, v in t1[ser].items()
                         if p not in named and v is not None
                         and p not in ("Wahlberechtigte", "Wähler / Wahlbeteiligung",
                                       "Ungültige Stimmen", "Gültige Stimmen"))
            got = sum(v for k in wkrs if (v := tab[k][yr][0]["Sonstige"]) is not None)
            if yr == 2018 and stimme == "zweitstimme":
                got = 0
            req(got == expect,
                f"{stimme} {yr}: residual 'Sonstige' ({got}) == Table 1's unlisted parties ({expect})")

    # (9) INDEPENDENT SOURCE: the 2018 Vergleichszahlen report -----------------
    vgl_ser = {"erststimme": "L13W", "zweitstimme": "L13L"}
    for stimme, (tab, cr) in tables.items():
        shared = [c for c in (["eligible_voters", "number_voters", "invalid_votes",
                               "valid_votes"] + party_cols(cr))
                  if c in VGL_LEFT + VGL_RIGHT and c != "Sonstige"]
        ser = vgl_ser[stimme]
        bad, pair_bad, differ = [], [], set()
        for k in wkrs:
            row, ref = tab[k][2013][0], vgl[k][ser]
            for c in shared:
                if (row[c] or 0) != (ref[c] or 0):
                    if k in VGL_BOUNDARY_DIFF_WKR:
                        differ.add(c)
                    else:
                        bad.append(f"WK{k} {c}: B VII 2-4 {row[c]} vs B VII 2-1 {ref[c]}")
        req(not bad, f"{stimme} 2013: every shared quantity agrees with the independent "
                     f"Vergleichszahlen report in the {N_WKR - len(VGL_BOUNDARY_DIFF_WKR)} "
                     f"Wahlkreise unaffected by the Schwanheim transfer")
        for b in bad[:8]:
            print("           ", b)
        # the known pair must differ only by moving votes BETWEEN the two, so
        # their combined figures have to be identical in both documents
        for c in shared:
            mine = sum(tab[k][2013][0][c] or 0 for k in VGL_BOUNDARY_DIFF_WKR)
            ref = sum(vgl[k][ser][c] or 0 for k in VGL_BOUNDARY_DIFF_WKR)
            if mine != ref:
                pair_bad.append(f"{c}: B VII 2-4 {mine} vs B VII 2-1 {ref}")
        req(not pair_bad,
            f"{stimme} 2013: WK{'+WK'.join(map(str, VGL_BOUNDARY_DIFF_WKR))} combined are "
            f"identical in both reports (the difference is purely the Stadtbezirk transfer)")
        for b in pair_bad[:8]:
            print("           ", b)
        req(bool(differ), f"{stimme} 2013: the Schwanheim transfer is still visible in "
                          f"WK{'/'.join(map(str, VGL_BOUNDARY_DIFF_WKR))} "
                          f"({len(differ)} quantities) - if this ever passes silently the "
                          f"reports were reconciled and the flag needs revisiting")

    # (10) the Vergleichszahlen parse itself reconciles to Table 1 ------------
    #      (REP is broken out there but sits inside B VII 2-4's residual; GERDA
    #      keeps the single-source B VII 2-4 breakdown, so this is validation
    #      only - see the README for the per-Wahlkreis 2013 REP figures.)
    for stimme in tables:
        ser = vgl_ser[stimme]
        bad = []
        for pty in ("CDU", "SPD", "GRÜNE", "DIE LINKE", "FDP", "AfD", "PIRATEN",
                    "FREIE WÄHLER", "NPD", "Die PARTEI", "REP", "Tierschutzpartei"):
            tot = sum(v for k in wkrs if (v := vgl[k][ser][pty]) is not None)
            ref = t1[series_of[(stimme, 2013)]].get(pty) or 0
            if tot != ref:
                bad.append(f"{pty}: Vergleichszahlen {tot} vs Table 1 {ref}")
        req(not bad, f"{stimme} 2013: the Vergleichszahlen's own 55 Wahlkreise reproduce "
                     f"Table 1 for all 12 parties it names")
        for b in bad:
            print("           ", b)

    if fails:
        print(f"\n{len(fails)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)

    # --- emit -----------------------------------------------------------------
    rows = []
    for stimme, (tab, cr) in tables.items():
        for yr in (2018, 2013):
            for k in wkrs:
                row = tab[k][yr][0]
                parties = {p: row[p] for p in party_cols(cr)}
                if stimme == "zweitstimme" and yr == 2018:
                    del parties["Sonstige"]
                    parties.update({p: t15[k][p] for p in COLS_T15})
                emitted = sum(v for v in parties.values() if v is not None)
                if emitted != row["valid_votes"]:
                    raise SystemExit(
                        f"emit {stimme} {yr} WK{k}: parties sum to {emitted}, "
                        f"valid_votes is {row['valid_votes']}")
                for p, v in parties.items():
                    rows.append({
                        "state_abbr": STATE_ABBR, "state": STATE_NAME,
                        "election_year": yr, "election_date": ELECTION_DATE[yr],
                        "wkr_nr": f"{k:02d}", "wkr_name": names[k], "stimme": stimme,
                        "eligible_voters": row["eligible_voters"],
                        "number_voters": row["number_voters"],
                        "valid_votes": row["valid_votes"],
                        "invalid_votes": row["invalid_votes"],
                        "party_raw": p, "votes": "" if v is None else v,
                        "flag_wkr_boundaries_recomputed":
                            int(yr == 2013 and k not in VGL_BOUNDARY_DIFF_WKR),
                    })
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)
    print(f"\nWrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for yr in (2018, 2013):
        for stimme in ("erststimme", "zweitstimme"):
            n = sum(1 for r in rows if r["election_year"] == yr and r["stimme"] == stimme)
            print(f"  {yr} {stimme:12s}: {n} rows "
                  f"({n // N_WKR} parties x {N_WKR} Wahlkreise)")


if __name__ == "__main__":
    main()
