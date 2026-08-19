#!/usr/bin/env python3
"""Stage-0 parser for SACHSEN Landtagswahl results at WAHLKREIS level, 2004 + 2009.

Sources (raw, read-only):
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Sachsen/
    SN_2009_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf   (277 pp)
    SN_2004_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf   (264 pp)
  = Statistisches Landesamt des Freistaates Sachsen, series B VII 2-2, the
    official final reports for the 4th (2004) and 5th (2009) Sächsischer Landtag.

Why a PDF parser: 2004 and 2009 are the only two Sachsen Landtagswahlen with no
machine-readable Wahlkreis-level release (1994 HTML, 1999 CSV, 2014/2019/2024
XLSX are all handled directly by parse_SN.R).  Both reports carry a full digital
text layer, so no OCR is involved.

Both reports have the same table numbering, and we use four tables per report:
   7  Wahlberechtigte, Wahlbeteiligung, gültige Direkt- und Listenstimmen
   8  Direkt- und Listenstimmen: gültig / ungültig (abs + %)
   9  Gültige Direktstimmen nach Parteien und anderen Wahlvorschlägen
  10  Gültige Listenstimmen nach Parteien
Sachsen uses Direktstimme = erststimme and Listenstimme = zweitstimme.
Tables 9 and 10 are wide tables split over facing half-pages (left page = row
labels + first columns, right page = remaining columns).  Table 9 additionally
splits its ~23 columns into two column BLOCKS, so all 61 rows (60 Wahlkreise +
the printed "Sachsen" total) appear once per block, four page-pairs per block.

PARSING METHOD, 2009 (pdfplumber, coordinate-based):
  Naive whitespace tokenization fuses adjacent short values, because the
  thousands separator is a space.  Instead, words on a line are merged into
  number groups by horizontal gap (< 8 pt inside a number, > 20 pt between
  columns) and each group's RIGHT edge is snapped to a column anchor taken from
  the printed percent line directly below the absolute line (numbers are
  right-aligned, so abs and % right edges agree to within ~2 pt).  Anchors are
  therefore re-derived for every single row and never hard-coded.

PARSING METHOD, 2004 (pikepdf + fontTools, glyph-name decoding):
  The 2004 PDF's Type1/Type3 subsets have no ToUnicode CMap; their /Encoding
  /Differences arrays name glyphs /G<N>, where N indexes the standard Macintosh
  glyph order.  pdftotext/pdfplumber therefore emit mojibake.  We rebuild the
  per-font byte -> Unicode map from /Differences via
  standardGlyphOrder[N] -> AGL2UV, then re-parse the raw content stream with
  pikepdf.parse_content_stream, tracking Tf font switches and the text matrix,
  and decode Tj/TJ byte strings ourselves.  This generator emits one show-text
  operator per (part of a) table row with the column padding preserved inside
  the string, so once decoded a row is split on runs of >= 2 spaces (the
  thousands separator is exactly ONE space).  Every field is then required to
  match a strict number/percent/"x" pattern and every row is required to yield
  exactly the expected number of fields, so any collapsed column gap aborts.

VALIDATION (all hard; nothing is written if any check fails), per year and
per stimme:
  (1) exactly 60 Wahlkreise plus the printed "Sachsen" row, numbered 1..60
  (2) sum over the 60 Wahlkreise == the printed Sachsen row, for EVERY party
      and for eligible/voters/valid/invalid
  (3) per Wahlkreis: sum of party votes == gültige Stimmen, and
      Wähler == gültige + ungültige, for both stimmen
  (4) per cell: abs/gültige reproduces the printed percent (0.06 pp tolerance)
  (5) pinned official statewide Listenstimmen shares (+-0.1 pp)
  (6) table 7 and table 8 must agree on gültige Stimmen for every Wahlkreis
  (7) 2004 only: zero unresolved glyph codes on every decoded table page
  (8) 2004 only: the Wahlkreis names and the 2004 turnout figures must match
      the independent 2004 columns printed in the 2009 report (tables 7 and 8),
      for the 47 Wahlkreise not redrawn between the two elections; the other 13
      must differ, since the 2009 report shows 2004 on the 2009 boundaries.
      Statewide Direktstimmen are excluded: the 2009 report prints them in the
      version adjusted for the 2006 Wiederholungswahl in Wahlkreis 31, whereas
      we emit the original 2004 result as printed in the 2004 report.
  (9) the number of emitted (Wahlkreis x party) erststimme rows equals the
      number of Direktkandidaten each report states in its Vorbemerkungen
      (2004: 391, 2009: 398); zweitstimme rows equal 60 x Landeslisten

Output: data/state_elections/processed/wahlkreis/sn_pdf/SN_2004_2009_pdf_long.csv
        (read by parsers/parse_SN.R, which re-validates and appends it)
Run:    python3 code/state_elections_wahlkreis/parsers/00_sn_pdf_parse.py
Requires: pikepdf, fontTools (pip install pikepdf fonttools), pdfplumber.
"""

import csv
import os
import re
import sys
from collections import defaultdict

import pdfplumber
import pikepdf
from fontTools.agl import AGL2UV
from fontTools.ttLib.standardGlyphOrder import standardGlyphOrder

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW = os.path.join(ROOT, "data", "state_elections", "raw",
                   "Landtagswahlen_Wahlkreis", "Sachsen")
PDF_2009 = os.path.join(RAW, "SN_2009_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf")
PDF_2004 = os.path.join(RAW, "SN_2004_Landtagswahl_StatistischerBericht_B-VII-2-2.pdf")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed", "wahlkreis", "sn_pdf")
OUT = os.path.join(OUT_DIR, "SN_2004_2009_pdf_long.csv")

STATE_ABBR = "SN"
STATE_NAME = "Sachsen"
ELECTION_DATE = {2004: "2004-09-19", 2009: "2009-08-30"}
N_WKR = 60
TOTAL_LABEL = "Sachsen"

# --- table geometry (1-based PDF page numbers) --------------------------------
# Table 9 alternates two column blocks A/B; each block covers all 61 rows in
# four (left, right) page-pairs of 17/17/17/10 rows.
PAGES = {
    2009: {
        "t7": [32, 33, 34, 35],
        "t8": [36, 37, 38, 39],
        "t9A": [(40, 41), (44, 45), (48, 49), (52, 53)],
        "t9B": [(42, 43), (46, 47), (50, 51), (54, 55)],
        "t10": [(56, 57), (58, 59), (60, 61), (62, 63)],
    },
    2004: {
        "t7": [30, 31, 32, 33],
        "t8": [34, 35, 36, 37],
        "t9A": [(38, 39), (42, 43), (46, 47), (50, 51)],
        "t9B": [(40, 41), (44, 45), (48, 49), (52, 53)],
        "t10": [(54, 55), (56, 57), (58, 59), (60, 61)],
    },
}

# --- column layouts, in printed left-to-right order ---------------------------
# Labels are the report's own Kurzbezeichnungen / Präsentationskürzel, as
# printed in the table headers (2009 report p. 6, 2004 report p. 7/8).
PARTIES = {
    2009: {
        # table 10 (Listenstimmen): the 16 Landeslisten, in Listennummer order
        "t10L": ["CDU", "DIE LINKE", "SPD", "NPD", "FDP", "GRÜNE"],
        "t10R": ["Die Tierschutzpartei", "PBC", "BüSo", "DSU", "REP",
                 "Freie Sachsen", "FP Deutschlands", "HUMANWIRTSCHAFT",
                 "PIRATEN", "SVP"],
        # table 9 (Direktstimmen): 13 parties + 9 andere Wahlvorschläge
        "t9AL": ["CDU", "DIE LINKE", "SPD", "NPD", "FDP"],
        "t9AR": ["GRÜNE", "PBC", "BüSo", "DSU", "Freie Sachsen", "FP Deutschlands"],
        "t9BL": ["HUMANWIRTSCHAFT", "SVP", "BILDUNG", "DOSE", "Förster", "Freie Bürger"],
        "t9BR": ["Für unsere Region", "FW Sachsen", "Frieden", "Gerechtigkeit",
                 "Nitzsche"],
    },
    2004: {
        # table 10 (Listenstimmen): the 13 Landeslisten, in Listennummer order
        "t10L": ["CDU", "PDS", "SPD", "GRÜNE", "NPD"],
        "t10R": ["FDP", "DSU", "PBC", "GRAUE", "BüSo", "AUFBRUCH", "DGG",
                 "Tierschutz"],
        # table 9 (Direktstimmen): 13 parties + 10 andere Kreiswahlvorschläge
        "t9AL": ["CDU", "PDS", "SPD", "GRÜNE", "NPD"],
        "t9AR": ["FDP", "DSU", "PBC", "BüSo", "DGG", "Bürgerbewegung"],
        "t9BL": ["Heine", "Pohl", "FP Deutschlands", "Freie Wähler", "FW Penig",
                 "Offensive D"],
        "t9BR": ["Schmidt", "PLB", "REP", "Schaudienst", "Unabhängige", "WERNER"],
    },
}

# Header signature = all header-region text of a half-page, whitespace removed.
# Pinned so that a changed column layout aborts instead of silently remapping
# parties onto the wrong columns.  Identical across the four page-pairs of a
# block, which is itself asserted.
HEADER_PIN = {
    (2009, "t9AL"): "Wk-WahlkreisInsgesamtNr.LandCDUDIELINKESPDNPDFDP",
    (2009, "t9AR"): "FPWahlkreisWk-FreieGRÜNEPBCBüSoDSUDeutsch-LandNr.Sachsenlands",
    (2009, "t9BL"): "Wk-WahlkreisHUMAN-Nr.LandWIRT-SVPBILDUNGDOSEFörsterFreieBürgerSCHAFT",
    (2009, "t9BR"): "WahlkreisWk-FürunsereGerechtig-FWSachsenFriedenNitzscheLandNr.Regionkeit",
    (2009, "t10L"): "Wk-WahlkreisInsgesamtNr.LandCDUDIELINKESPDNPDFDPGRÜNE",
    (2009, "t10R"): "DieFPHUMAN-Wk-FreieTierschutz-PBCBüSoDSUREPDeutsch-WIRT-"
                    "PIRATENSVPNr.SachsenparteilandsSCHAFT",
    (2004, "t9AL"): "Wk-WahlkreisInsgesamtNr.LandCDUPDSSPDGRÜNENPD",
    (2004, "t9AR"): "WahlkreisWk-Bürger-LandNr.FDPDSUPBCBüSoDGGbewegung",
    (2004, "t9BL"): "Wk-WahlkreisFPFreieFWOffen-Nr.LandHeinePohlDeutsch-"
                    "WählerPenigsiveDlands",
    (2004, "t9BR"): "WahlkreisWk-Schau-Unab-WER-LandNr.SchmidtPLBREPdiensthängigeNER",
    (2004, "t10L"): "Wk-WahlkreisInsgesamtNr.LandCDUPDSSPDGRÜNENPD",
    (2004, "t10R"): "Wk-Nr.FDPDSUPBCGRAUEBüSoAUFBRUCHDGGTierschutz",
}

# Kreiswahlvorschlaege / Landeslisten as counted in each report's Vorbemerkungen
# (2009 report p. 7: "398 Direktkandidaten/-innen", 16 Listenparteien;
#  2004 report p. 7/8: "391 Direktkandidaten/innen", 13 Listenparteien).
N_DIREKTKANDIDATEN = {2004: 391, 2009: 398}
N_LANDESLISTEN = {2004: 13, 2009: 16}

# official statewide Listenstimmen shares (Landeswahlleiter Sachsen)
OFFICIAL_L = {
    2009: {"CDU": 40.2, "DIE LINKE": 20.6, "SPD": 10.4, "FDP": 10.0,
           "GRÜNE": 6.4, "NPD": 5.6},
    2004: {"CDU": 41.1, "PDS": 23.6, "SPD": 9.8, "NPD": 9.2, "FDP": 5.9,
           "GRÜNE": 5.1},
}

# Wahlkreise redrawn between the 2004 and the 2009 election.  The 2009 report
# prints the 2004 result on the 2009 boundaries, so exactly these 13 differ
# between the two reports (statewide totals are unaffected).
BOUNDARY_CHANGED_2004_2009 = ["12", "15", "18", "19", "31", "32", "52", "54",
                              "55", "56", "57", "59", "60"]

PCT_TOL = 0.06        # rounding tolerance for a printed one-decimal percent
GAP_INSIDE = 8.0      # < this pt gap joins two words into one number
SNAP_TOL = 12.0       # max pt distance from a column anchor

FAILS = []


def req(cond, label, detail=None):
    print(("  [ok]  " if cond else "  [FAIL]") + " " + label)
    if not cond:
        FAILS.append(label)
        for d in (detail or [])[:8]:
            print("           ", d)


def die_if_failed():
    if FAILS:
        print(f"\n{len(FAILS)} VALIDATION FAILURE(S) - nothing written.")
        sys.exit(1)


def num(tok):
    """'26 663' -> 26663 ; 'x' / '-' -> None."""
    if tok in ("x", "X", "-", "."):
        return None
    return int(tok.replace(" ", "").replace(" ", ""))


def pct(tok):
    if tok in ("x", "X", "-", "."):
        return None
    return float(tok.replace(",", "."))


# =============================================================================
# 2009: pdfplumber word geometry
# =============================================================================
def pp_lines(page, ytol=2.5):
    """Words grouped into visual lines, each sorted left to right."""
    buckets, tops = defaultdict(list), []
    for w in sorted(page.extract_words(x_tolerance=1.5), key=lambda w: w["top"]):
        for t in tops:
            if abs(w["top"] - t) <= ytol:
                buckets[t].append(w)
                break
        else:
            tops.append(w["top"])
            buckets[w["top"]].append(w)
    return [sorted(buckets[t], key=lambda w: w["x0"]) for t in sorted(tops)]


VALUE_RE = re.compile(r"^(?:\d+|\d+,\d+|x)$")


def value_groups(line):
    """Merge value words into number groups; return [(right_edge, text)]."""
    vals = [w for w in line if VALUE_RE.match(w["text"])]
    groups, cur = [], []
    for w in vals:
        if cur and w["x0"] - cur[-1]["x1"] < GAP_INSIDE:
            cur.append(w)
        else:
            if cur:
                groups.append(cur)
            cur = [w]
    if cur:
        groups.append(cur)
    return [(g[-1]["x1"], " ".join(w["text"] for w in g)) for g in groups]


def snap(groups, anchors):
    """Assign number groups to column anchors by right edge; None where absent."""
    out = [None] * len(anchors)
    leftover = []
    for x1, txt in groups:
        best, bd = None, SNAP_TOL
        for i, a in enumerate(anchors):
            if abs(x1 - a) < bd:
                best, bd = i, abs(x1 - a)
        if best is None:
            leftover.append((x1, txt))
        elif out[best] is not None:
            leftover.append((x1, txt))   # two groups on one anchor -> caller aborts
        else:
            out[best] = txt
    return out, leftover


def is_pct_line(line):
    txts = [w["text"] for w in line]
    if any(t == "absolut" for t in txts):
        return False
    if any(t == "%" for t in txts):
        return True
    vals = [t for t in txts if VALUE_RE.match(t)]
    return bool(vals) and sum("," in t for t in vals) * 2 >= len(vals)


def is_furniture(txt):
    """Page furniture that is not part of the column header."""
    return bool(
        re.match(r"^\s*(Noch:\s*)?\d+\.\s", txt)     # table caption, line 1
        or "Freistaat" in txt                          # table caption, line 2
        or re.match(r"^\s*\d{1,3}\s*$", txt)           # printed page number
        or "entfielen" in txt                          # spanner caption
        or "Statistisches Landesamt" in txt            # page footer
    )


def header_sig_from_texts(texts):
    return re.sub(r"\s+", "", "".join(t for t in texts if not is_furniture(t)))


def header_sig(lines, first_data_idx):
    """Whitespace-free concatenation of the header region of a half-page."""
    return header_sig_from_texts(
        [" ".join(w["text"] for w in line) for line in lines[:first_data_idx]])


def marker_at(line, word):
    for i, w in enumerate(line):
        if w["text"] == word:
            return i
    return None


def cells_of(line, marker, side):
    """Value groups of a data line, with the row label / marker cut away.

    Left pages read "<nr> <name> absolut  v v v", right pages read
    "v v v  absolut <name> <nr>"; table 10's right pages carry no marker at all,
    just the values followed by the Wahlkreis number.
    """
    mi = marker_at(line, marker)
    if mi is None:
        return value_groups(line), False
    return (value_groups(line[mi + 1:]) if side == "left"
            else value_groups(line[:mi])), True


def parse_2009_half(page, n_cols, side):
    """Return ([labels], [[cell,...]], [[pctcell,...]], header_signature).

    labels are ('1','Plauen') / ('Sachsen', None) on left pages, None on right.
    """
    lines = pp_lines(page)
    rows, labels, pcts, first_data = [], [], [], None
    i = 0
    while i < len(lines):
        line = lines[i]
        if is_pct_line(line) or not value_groups(line):
            i += 1
            continue
        # candidate absolute line: the next line must be its percent line
        if i + 1 >= len(lines) or not is_pct_line(lines[i + 1]):
            i += 1
            continue
        pgroups, _ = cells_of(lines[i + 1], "%", side)
        if len(pgroups) != n_cols:
            i += 1
            continue
        groups, marked = cells_of(line, "absolut", side)
        anchors = [x1 for x1, _ in pgroups]
        cells, leftover = snap(groups, anchors)
        pcells, pleft = snap(pgroups, anchors)
        if pleft:
            sys.exit(f"page {page.page_number}: percent line has stray tokens {pleft}")
        txts = [w["text"] for w in line]
        if side == "left":
            if not marked:
                sys.exit(f"page {page.page_number}: left data line without "
                         f"'absolut': {txts}")
            stop = txts.index("absolut")
            if txts[0] == TOTAL_LABEL:
                labels.append((TOTAL_LABEL, None))
            else:
                # a long Wahlkreis name wraps onto the percent line, in front
                # of the "%" marker
                ptxts = [w["text"] for w in lines[i + 1]]
                cont = ptxts[:ptxts.index("%")] if "%" in ptxts else []
                name = " ".join(txts[1:stop] + cont).strip()
                labels.append((txts[0], name))
            allowed = 0
        else:
            labels.append(None)
            # unmarked right pages keep a trailing Wahlkreis number (absent on
            # the Sachsen row); marked ones have it after the 'absolut' cut
            allowed = None if marked else len(leftover)
        if allowed is not None and len(leftover) != allowed:
            sys.exit(f"page {page.page_number} row {len(rows)}: "
                     f"unassigned tokens {leftover}")
        if not marked and len(leftover) > 1:
            sys.exit(f"page {page.page_number} row {len(rows)}: "
                     f"{len(leftover)} unassigned tokens {leftover}")
        if first_data is None:
            first_data = i
        rows.append(cells)
        pcts.append(pcells)
        i += 2
    return labels, rows, pcts, header_sig(lines, first_data if first_data else 0)


def parse_2009_wide(pdf, pairs, key, year):
    """Table 9 block or table 10: stitch left/right half-pages into rows."""
    left_parties = PARTIES[year][key + "L"]
    right_parties = PARTIES[year][key + "R"]
    has_total_col = key.startswith("t10") or key.endswith("A")
    n_left = len(left_parties) + (1 if has_total_col else 0)
    n_right = len(right_parties)
    out, seen_sig = {}, {}
    order = []
    for lp, rp in pairs:
        lab, lrows, lpct, lsig = parse_2009_half(pdf.pages[lp - 1], n_left, "left")
        _, rrows, rpct, rsig = parse_2009_half(pdf.pages[rp - 1], n_right, "right")
        for tag, sig in ((key + "L", lsig), (key + "R", rsig)):
            pin = HEADER_PIN[(year, tag)]
            if sig != pin:
                sys.exit(f"{year} page {lp if tag.endswith('L') else rp}: "
                         f"header signature\n  got {sig!r}\n  want {pin!r}")
            seen_sig[tag] = sig
        if len(lrows) != len(rrows):
            sys.exit(f"{year} pages {lp}/{rp}: {len(lrows)} vs {len(rrows)} rows")
        for k in range(len(lrows)):
            nr, name = lab[k]
            key_nr = TOTAL_LABEL if nr == TOTAL_LABEL else f"{int(nr):02d}"
            rec = out.setdefault(key_nr, {"name": name, "parties": {}, "pct": {}})
            if name and rec["name"] and name != rec["name"]:
                sys.exit(f"{year} {key_nr}: name {name!r} vs {rec['name']!r}")
            if key_nr not in order:
                order.append(key_nr)
            cells = lrows[k][1:] if has_total_col else lrows[k]
            pcells = lpct[k][1:] if has_total_col else lpct[k]
            if has_total_col:
                rec["insgesamt"] = num(lrows[k][0])
            for p, c, pc in zip(left_parties + right_parties,
                                list(cells) + list(rrows[k]),
                                list(pcells) + list(rpct[k])):
                if c is None or pc is None:
                    sys.exit(f"{year} {key_nr} {p}: missing cell")
                rec["parties"][p] = num(c)
                rec["pct"][p] = pct(pc)
    return out, order


NUMTOK = r"\d{1,3}(?: \d{3})*"


YEAR_RE = re.compile(r"(?:19|20)\d{2}")


def parse_2009_t7t8(pdf, pages, n_values, table, refs):
    """Tables 7/8: one row per (Wahlkreis, reference year); returns {(nr,y): vals}.

    A data line is "<nr> <name> <ref-year> v v ..." (first line of a Wahlkreis
    block), "<ref-year> v v ..." (a continuation line), or - when the name is
    too long for its column - "<name continued> <ref-year> v v ...".
    """
    out, names = {}, {}
    for pg in pages:
        cur = None
        for line in pp_lines(pdf.pages[pg - 1]):
            txts = [w["text"] for w in line]
            yidx = next((i for i, t in enumerate(txts)
                         if YEAR_RE.fullmatch(t)), None)
            if yidx is None:
                continue
            strict = True
            if yidx > 0 and (txts[0] == TOTAL_LABEL
                             or re.fullmatch(r"\d{1,2}", txts[0])):
                cur = (TOTAL_LABEL if txts[0] == TOTAL_LABEL
                       else f"{int(txts[0]):02d}")
                if cur != TOTAL_LABEL:
                    names[cur] = re.sub(r"\s*\d\)$", "",
                                        " ".join(txts[1:yidx]).strip())
            elif yidx > 0:
                # wrapped Wahlkreis name; also guards against caption lines,
                # which only ever appear before the first data row of a page
                if cur is None or cur == TOTAL_LABEL:
                    continue
                names[cur] = f"{names[cur]} {' '.join(txts[:yidx])}".strip()
                strict = False
            if cur is None:
                continue
            vals = [t for _, t in value_groups(line[yidx + 1:])]
            if len(vals) != n_values:
                if not strict:
                    continue
                sys.exit(f"table {table} p{pg} {cur} {txts[yidx]}: "
                         f"{len(vals)} values {vals}")
            out[(cur, int(txts[yidx]))] = vals
    want = sorted([f"{i:02d}" for i in range(1, N_WKR + 1)] + [TOTAL_LABEL])
    for ref in refs:
        got = sorted(k for k, y in out if y == ref)
        if got != want:
            sys.exit(f"table {table}: {len(got)} areas for reference year "
                     f"{ref}, expected 61")
    return out, names


# =============================================================================
# 2004: glyph-name decoding
# =============================================================================
def build_font_maps(page):
    fonts = page.Resources.get("/Font", None)
    maps = {}
    if fonts is None:
        return maps
    for fname, fobj in fonts.items():
        enc = fobj.get("/Encoding", None)
        cmap = {}
        if enc is not None and "/Differences" in enc:
            code = None
            for item in enc.Differences:
                if isinstance(item, (int, pikepdf.Integer)):
                    code = int(item)
                    continue
                gname = str(item).lstrip("/")
                ch = None
                if gname[:1] in ("G", "c") and gname[1:].isdigit():
                    gi = int(gname[1:])
                    if 0 <= gi < len(standardGlyphOrder):
                        std = standardGlyphOrder[gi]
                        if std in AGL2UV:
                            ch = chr(AGL2UV[std])
                elif gname in AGL2UV:
                    ch = chr(AGL2UV[gname])
                cmap[code] = ch
                code += 1
        maps[str(fname)] = cmap
    return maps


def _mul(a, b):
    return [a[0] * b[0] + a[1] * b[2], a[0] * b[1] + a[1] * b[3],
            a[2] * b[0] + a[3] * b[2], a[2] * b[1] + a[3] * b[3],
            a[4] * b[0] + a[5] * b[2] + b[4], a[4] * b[1] + a[5] * b[3] + b[5]]


UNRESOLVED = re.compile(r"<\d+>")


def decoded_chunks(page):
    """[(x, y, text)] for every show-text operator, decoded through /Differences."""
    fmaps = build_font_maps(page)
    font, tm, tlm, out = None, [1, 0, 0, 1, 0, 0], [1, 0, 0, 1, 0, 0], []

    def dec(raw):
        m = fmaps.get(font, {})
        return "".join(m[b] if m.get(b) is not None else f"<{b}>" for b in raw)

    for operands, operator in pikepdf.parse_content_stream(page):
        op = str(operator)
        if op == "BT":
            tm = tlm = [1, 0, 0, 1, 0, 0]
        elif op == "Tf":
            font = str(operands[0])
        elif op == "Tm":
            tlm = [float(v) for v in operands]
            tm = list(tlm)
        elif op in ("Td", "TD"):
            tlm = _mul([1, 0, 0, 1, float(operands[0]), float(operands[1])], tlm)
            tm = list(tlm)
        elif op == "T*":
            tlm = _mul([1, 0, 0, 1, 0, -12], tlm)
            tm = list(tlm)
        elif op in ("Tj", "'", '"'):
            out.append((tm[4], tm[5], dec(bytes(operands[-1]))))
        elif op == "TJ":
            out.append((tm[4], tm[5], "".join(
                dec(bytes(el)) for el in operands[0]
                if isinstance(el, (pikepdf.String, bytes)))))
    return out


def dec_lines(page, ytol=1.0):
    """Decoded chunks joined into one string per visual line, left to right."""
    buckets, ys = defaultdict(list), []
    for x, y, s in sorted([c for c in decoded_chunks(page) if c[2].strip()],
                          key=lambda c: -c[1]):
        for yy in ys:
            if abs(y - yy) <= ytol:
                buckets[yy].append((x, s))
                break
        else:
            ys.append(y)
            buckets[y].append((x, s))
    return ["  ".join(s for _, s in sorted(buckets[y])) for y in ys]


FIELD_RE = re.compile(rf"^(?:{NUMTOK}|\d+,\d+|x)$")


def split_fields(s):
    """Split a decoded row remainder on runs of >= 2 spaces; validate each field."""
    fields = [f.strip() for f in re.split(r"\s{2,}", s.strip()) if f.strip()]
    for f in fields:
        if not FIELD_RE.match(f):
            return None
    return fields


def parse_2004_half(page, n_cols, side):
    """Same contract as parse_2009_half, but for the decoded 2004 report.

    Left pages read "<nr><name>absolut v v v" / "% p p p"; table 9's right pages
    read "v v v absolut<name><nr>"; table 10's right pages carry no marker at
    all, just "v v v <nr>" followed by "p p p".
    """
    lines = dec_lines(page)
    labels, rows, pcts, header = [], [], [], []
    first_data = None
    for s in lines:
        marker = "absolut" if "absolut" in s else ("%" if "%" in s else None)
        if side == "left":
            if marker is None:
                fields = None
            else:
                head, _, rest = s.partition(marker)
                fields = split_fields(rest)
        elif marker is not None:
            head = None
            rest, _, _tail = s.partition(marker)
            fields = split_fields(rest)
        else:
            # unmarked right page: values, then the Wahlkreis number (absent on
            # the Sachsen row, which is the only row with exactly n_cols fields
            # and no decimal comma)
            head = None
            fields = split_fields(s)
            if (fields is not None and len(fields) == n_cols + 1
                    and re.fullmatch(r"\d{1,2}", fields[-1])):
                fields = fields[:-1]
            marker = ("%" if fields and
                      sum("," in f for f in fields) * 2 >= len(fields)
                      else "absolut")
        if fields is None or len(fields) != n_cols:
            if first_data is None:
                header.append(s)
            continue
        if marker == "%":
            # a long Wahlkreis name wraps onto the percent line, in front of "%"
            if side == "left" and head and head.strip() and labels:
                nr, nm = labels[-1]
                if nr != TOTAL_LABEL:
                    labels[-1] = (nr, (f"{nm} {head.strip()}").strip())
            pcts.append(fields)
            continue
        if side == "left":
            head = head.strip()
            if head.startswith(TOTAL_LABEL):
                labels.append((TOTAL_LABEL, None))
            else:
                m = re.match(r"^(\d+)\s*(.*?)\s*$", head)
                if not m:
                    sys.exit(f"2004 p{page.index + 1}: bad row label {head!r}")
                labels.append((m.group(1), m.group(2)))
        else:
            labels.append(None)
        rows.append(fields)
        if first_data is None:
            first_data = len(rows)
    return labels, rows, pcts, header_sig_from_texts(header)


def parse_2004_wide(pdf, pairs, key, year=2004):
    left_parties = PARTIES[year][key + "L"]
    right_parties = PARTIES[year][key + "R"]
    has_total_col = key.startswith("t10") or key.endswith("A")
    n_left = len(left_parties) + (1 if has_total_col else 0)
    n_right = len(right_parties)
    out, order = {}, []
    for lp, rp in pairs:
        lab, lrows, lpct, lsig = parse_2004_half(pdf.pages[lp - 1], n_left, "left")
        _, rrows, rpct, rsig = parse_2004_half(pdf.pages[rp - 1], n_right, "right")
        for tag, sig, pg in ((key + "L", lsig, lp), (key + "R", rsig, rp)):
            pin = HEADER_PIN[(year, tag)]
            if sig != pin:
                sys.exit(f"{year} page {pg}: header signature\n"
                         f"  got  {sig!r}\n  want {pin!r}")
        if not (len(lrows) == len(rrows) == len(lpct) == len(rpct)):
            sys.exit(f"{year} pages {lp}/{rp}: row counts "
                     f"{len(lrows)}/{len(rrows)}/{len(lpct)}/{len(rpct)}")
        for k in range(len(lrows)):
            nr, name = lab[k]
            key_nr = TOTAL_LABEL if nr == TOTAL_LABEL else f"{int(nr):02d}"
            rec = out.setdefault(key_nr, {"name": name, "parties": {}, "pct": {}})
            if name and rec["name"] and name != rec["name"]:
                sys.exit(f"{year} {key_nr}: name {name!r} vs {rec['name']!r}")
            if key_nr not in order:
                order.append(key_nr)
            cells = lrows[k][1:] if has_total_col else lrows[k]
            pcells = lpct[k][1:] if has_total_col else lpct[k]
            if has_total_col:
                rec["insgesamt"] = num(lrows[k][0])
            for p, c, pc in zip(left_parties + right_parties,
                                list(cells) + list(rrows[k]),
                                list(pcells) + list(rpct[k])):
                rec["parties"][p] = num(c)
                rec["pct"][p] = pct(pc)
    return out, order


def parse_2004_t7t8(pdf, pages, n_values, table, refs):
    """Tables 7/8 of the 2004 report; rows are '<nr><name><ref> v v ...'."""
    out, names = {}, {}
    cur = None
    for pg in pages:
        for s in dec_lines(pdf.pages[pg - 1]):
            # "<nr><name><ref> v v ..." on the first line of a block, "<ref> v v ..."
            # on its continuation lines.  The reference year is glued straight
            # onto the first value in the Sachsen row, so no space is required.
            m = re.match(rf"^\s*({TOTAL_LABEL}|\d{{1,2}})?\s*(.*?)"
                         r"((?:19|20)\d{2})\s*(\S.*)$", s)
            if not m:
                continue
            fields = split_fields(m.group(4))
            if fields is None or len(fields) != n_values:
                continue
            if m.group(1) == TOTAL_LABEL:
                cur = TOTAL_LABEL
            elif m.group(1):
                cur = f"{int(m.group(1)):02d}"
                names[cur] = re.sub(r"\s*\d\)$", "", (m.group(2) or "").strip())
            if cur is None:
                continue
            out[(cur, int(m.group(3)))] = fields
    want = sorted([f"{i:02d}" for i in range(1, N_WKR + 1)] + [TOTAL_LABEL])
    for ref in refs:
        got = sorted(k for k, y in out if y == ref)
        if got != want:
            sys.exit(f"table {table}: {len(got)} areas for reference year "
                     f"{ref}, expected 61")
    return out, {k: v for k, v in names.items() if v}


# =============================================================================
# assembly
# =============================================================================
def collect(year, pdf_plumber=None, pdf_pike=None):
    """Return dict: nr -> {'name', 'turnout': {...}, 'erst': {p: v}, 'zweit': {...}}."""
    pg = PAGES[year]
    if year == 2009:
        t7, names7 = parse_2009_t7t8(pdf_plumber, pg["t7"], 5, 7, [2009, 2004])
        t8, _ = parse_2009_t7t8(pdf_plumber, pg["t8"], 8, 8, [2009, 2004])
        wide = lambda k: parse_2009_wide(pdf_plumber, pg[k], k, year)
    else:
        t7, names7 = parse_2004_t7t8(pdf_pike, pg["t7"], 5, 7, [2004])
        t8, _ = parse_2004_t7t8(pdf_pike, pg["t8"], 8, 8, [2004])
        wide = lambda k: parse_2004_wide(pdf_pike, pg[k], k, year)

    t9a, order9 = wide("t9A")
    t9b, _ = wide("t9B")
    t10, order10 = wide("t10")

    keys = [f"{i:02d}" for i in range(1, N_WKR + 1)] + [TOTAL_LABEL]
    req(order9 == keys, f"{year} table 9 rows: 60 Wahlkreise 01..60 + Sachsen",
        [f"got {len(order9)} rows, first divergence at "
         f"{next((a for a, b in zip(order9, keys) if a != b), '(length)')}"])
    req(order10 == keys, f"{year} table 10 rows: 60 Wahlkreise 01..60 + Sachsen")
    req(sorted(t9b) == sorted(keys), f"{year} table 9 block B rows match block A")

    data = {}
    for k in keys:
        e_parties = dict(t9a[k]["parties"])
        e_parties.update(t9b[k]["parties"])
        e_pct = dict(t9a[k]["pct"])
        e_pct.update(t9b[k]["pct"])
        v7 = t7.get((k, year))
        v8 = t8.get((k, year))
        if v7 is None or v8 is None:
            sys.exit(f"{year} {k}: missing table 7 / table 8 row")
        data[k] = {
            "name": t9a[k]["name"] or names7.get(k),
            # table 7: Wahlberechtigte, Wähler, Wähler %, gültige D, gültige L
            "eligible": num(v7[0]), "voters": num(v7[1]), "voters_pct": pct(v7[2]),
            "valid_d7": num(v7[3]), "valid_l7": num(v7[4]),
            # table 8: gültig D, %, ungültig D, %, gültig L, %, ungültig L, %
            "valid_d": num(v8[0]), "valid_d_pct": pct(v8[1]),
            "invalid_d": num(v8[2]), "invalid_d_pct": pct(v8[3]),
            "valid_l": num(v8[4]), "valid_l_pct": pct(v8[5]),
            "invalid_l": num(v8[6]), "invalid_l_pct": pct(v8[7]),
            "erst": e_parties, "erst_pct": e_pct,
            "erst_total": t9a[k]["insgesamt"],
            "zweit": t10[k]["parties"], "zweit_pct": t10[k]["pct"],
            "zweit_total": t10[k]["insgesamt"],
        }
    return data


def validate(year, data):
    wkr = [f"{i:02d}" for i in range(1, N_WKR + 1)]
    tot = data[TOTAL_LABEL]
    parties_e = PARTIES[year]["t9AL"] + PARTIES[year]["t9AR"] + \
        PARTIES[year]["t9BL"] + PARTIES[year]["t9BR"]
    parties_z = PARTIES[year]["t10L"] + PARTIES[year]["t10R"]

    # (2) sum over Wahlkreise == printed Sachsen row
    for stimme, plist, key, totkey in (("erststimme", parties_e, "erst", "erst_total"),
                                       ("zweitstimme", parties_z, "zweit", "zweit_total")):
        bad = []
        for p in plist:
            s = sum(data[w][key].get(p) or 0 for w in wkr)
            t = tot[key].get(p) or 0
            if s != t:
                bad.append((p, s, t, s - t))
        req(not bad, f"{year} {stimme}: sum over 60 Wahlkreise == printed Sachsen "
                     f"row for all {len(plist)} parties", bad)
        s = sum(data[w][totkey] for w in wkr)
        req(s == tot[totkey],
            f"{year} {stimme}: sum of Insgesamt == printed Sachsen Insgesamt "
            f"({s} vs {tot[totkey]})")

    for label, fld in (("Wahlberechtigte", "eligible"), ("Wähler", "voters"),
                       ("gültige D", "valid_d"), ("ungültige D", "invalid_d"),
                       ("gültige L", "valid_l"), ("ungültige L", "invalid_l")):
        s = sum(data[w][fld] for w in wkr if data[w][fld] is not None)
        t = tot[fld]
        req(t is None or s == t,
            f"{year} turnout: sum of {label} == printed Sachsen row ({s} vs {t})")

    # (3) per Wahlkreis identities
    bad = []
    for w in wkr:
        d = data[w]
        se = sum(v for v in d["erst"].values() if v is not None)
        sz = sum(v for v in d["zweit"].values() if v is not None)
        if se != d["erst_total"] or d["erst_total"] != d["valid_d"]:
            bad.append((w, "D", se, d["erst_total"], d["valid_d"]))
        if sz != d["zweit_total"] or d["zweit_total"] != d["valid_l"]:
            bad.append((w, "L", sz, d["zweit_total"], d["valid_l"]))
    req(not bad, f"{year}: per Wahlkreis, sum of party votes == Insgesamt == "
                 f"gültige Stimmen (both stimmen)", bad)

    bad = [(w, data[w]["voters"], data[w]["valid_d"], data[w]["invalid_d"],
            data[w]["valid_l"], data[w]["invalid_l"])
           for w in wkr
           if data[w]["voters"] != data[w]["valid_d"] + data[w]["invalid_d"]
           or data[w]["voters"] != data[w]["valid_l"] + data[w]["invalid_l"]]
    req(not bad, f"{year}: per Wahlkreis, Wähler == gültige + ungültige "
                 f"(both stimmen)", bad)

    # (6) table 7 and table 8 agree
    bad = [(w, data[w]["valid_d7"], data[w]["valid_d"],
            data[w]["valid_l7"], data[w]["valid_l"]) for w in wkr
           if data[w]["valid_d7"] != data[w]["valid_d"]
           or data[w]["valid_l7"] != data[w]["valid_l"]]
    req(not bad, f"{year}: table 7 and table 8 agree on gültige Stimmen", bad)

    # (4) printed percents reproduce the counts
    bad = []
    for w in wkr + [TOTAL_LABEL]:
        d = data[w]
        checks = [(d["voters"], d["voters_pct"], d["eligible"]),
                  (d["valid_d"], d["valid_d_pct"], d["voters"]),
                  (d["invalid_d"], d["invalid_d_pct"], d["voters"]),
                  (d["valid_l"], d["valid_l_pct"], d["voters"]),
                  (d["invalid_l"], d["invalid_l_pct"], d["voters"])]
        checks += [(d["erst"][p], d["erst_pct"][p], d["erst_total"])
                   for p in parties_e]
        checks += [(d["zweit"][p], d["zweit_pct"][p], d["zweit_total"])
                   for p in parties_z]
        for v, p, base in checks:
            if v is None or p is None or not base:
                continue
            if abs(100.0 * v / base - p) > PCT_TOL:
                bad.append((w, v, p, round(100.0 * v / base, 3)))
    req(not bad, f"{year}: every printed percent reproduces its count "
                 f"(tolerance {PCT_TOL} pp)", bad)

    # (5) pinned official statewide Listenstimmen shares
    bad = []
    for p, share in OFFICIAL_L[year].items():
        got = 100.0 * tot["zweit"][p] / tot["zweit_total"]
        if abs(got - share) > 0.1:
            bad.append((p, round(got, 2), share))
    req(not bad, f"{year}: statewide Listenstimmen shares match the official "
                 f"result (+-0.1 pp)", bad)

    # Wahlkreis numbering / names
    req(all(data[w]["name"] for w in wkr),
        f"{year}: a Wahlkreis name was read for all 60 Wahlkreise")


def cross_check_2004(d04, t7_09, t8_09, names09):
    """The 2009 report also prints the 2004 columns - an independent source.

    The 2009 report shows the 2004 result on the 2009 Wahlkreis boundaries.
    For the 13 Wahlkreise whose boundaries were redrawn between the two
    elections the two reports therefore differ by construction; everywhere else
    they must agree to the vote.  We emit the 2004 report's own figures, i.e.
    the 2004 result on 2004 boundaries.
    """
    wkr = [f"{i:02d}" for i in range(1, N_WKR + 1)]
    bad = [(w, d04[w]["name"], names09.get(w)) for w in wkr
           if d04[w]["name"] != names09.get(w)]
    req(not bad, "2004: Wahlkreis names identical to the 2009 report "
                 "(same 60 numbers and names in both)", bad)

    same, differ = [], []
    for w in wkr:
        v7, v8 = t7_09.get((w, 2004)), t8_09.get((w, 2004))
        if v7 is None or v8 is None:
            sys.exit(f"2009 report: no 2004 row for Wahlkreis {w}")
        d = d04[w]
        # (their value, our value); "x" cells in the 2009 report are skipped
        pairs = [(num(v7[0]), d["eligible"]), (num(v7[1]), d["voters"]),
                 (pct(v7[2]), d["voters_pct"]),
                 (num(v7[3]), d["valid_d"]), (num(v7[4]), d["valid_l"]),
                 (num(v8[0]), d["valid_d"]), (num(v8[2]), d["invalid_d"]),
                 (num(v8[4]), d["valid_l"]), (num(v8[6]), d["invalid_l"])]
        diffs = [(a, b) for a, b in pairs if a is not None and a != b]
        (differ if num(v7[0]) != d["eligible"] else same).append((w, diffs))

    changed = [w for w, _ in differ]
    req(changed == sorted(BOUNDARY_CHANGED_2004_2009),
        f"2004: exactly the {len(BOUNDARY_CHANGED_2004_2009)} Wahlkreise "
        f"redrawn before 2009 differ from the 2009 report's 2004 columns",
        [f"got {changed}", f"want {sorted(BOUNDARY_CHANGED_2004_2009)}"])
    bad = [(w, diffs) for w, diffs in same if diffs]
    req(not bad, f"2004: the other {len(same)} Wahlkreise reproduce the 2009 "
                 f"report's 2004 turnout figures exactly", bad)

    # statewide: everything except Direktstimmen, which the 2009 report prints
    # in its 2006 Wiederholungswahl version
    tot = d04[TOTAL_LABEL]
    s7, s8 = t7_09[(TOTAL_LABEL, 2004)], t8_09[(TOTAL_LABEL, 2004)]
    bad = [x for x in [("Wahlberechtigte", num(s7[0]), tot["eligible"]),
                       ("Wähler", num(s7[1]), tot["voters"]),
                       ("gültige L", num(s7[4]), tot["valid_l"]),
                       ("ungültige L", num(s8[6]), tot["invalid_l"])]
           if x[1] != x[2]]
    req(not bad, "2004: statewide Wahlberechtigte / Wähler / Listenstimmen "
                 "match the 2009 report", bad)
    print(f"           Direktstimmen statewide deliberately not compared: "
          f"2004 report {tot['valid_d']} (original 2004 election) vs "
          f"2009 report {num(s8[0])} (2006 Wiederholungswahl in Wahlkreis 31)")


def check_2004_decoding(pdf):
    """(7) no glyph code may stay unresolved on any page we read."""
    pg = PAGES[2004]
    pages = list(pg["t7"]) + list(pg["t8"])
    for k in ("t9A", "t9B", "t10"):
        for a, b in pg[k]:
            pages += [a, b]
    total_chars, unresolved, bad_pages = 0, 0, []
    for p in sorted(set(pages)):
        txt = "".join(c[2] for c in decoded_chunks(pdf.pages[p - 1]))
        total_chars += len(txt)
        n = len(UNRESOLVED.findall(txt))
        unresolved += n
        if n:
            bad_pages.append((p, n))
    req(not bad_pages, f"2004: zero unresolved glyph codes on all "
                       f"{len(set(pages))} decoded table pages "
                       f"({total_chars} characters)", bad_pages)
    return len(set(pages)), total_chars


# =============================================================================
def main():
    for f in (PDF_2009, PDF_2004):
        if not os.path.exists(f):
            sys.exit(f"missing source PDF: {f}")

    print("VALIDATION")
    pdf09 = pdfplumber.open(PDF_2009)
    pdf04 = pikepdf.Pdf.open(PDF_2004)

    n_pages_04, n_chars_04 = check_2004_decoding(pdf04)
    die_if_failed()

    d09 = collect(2009, pdf_plumber=pdf09)
    d04 = collect(2004, pdf_pike=pdf04)
    die_if_failed()

    validate(2009, d09)
    validate(2004, d04)

    t7_09, names09 = parse_2009_t7t8(pdf09, PAGES[2009]["t7"], 5, 7, [2009, 2004])
    t8_09, _ = parse_2009_t7t8(pdf09, PAGES[2009]["t8"], 8, 8, [2009, 2004])
    cross_check_2004(d04, t7_09, t8_09, names09)

    die_if_failed()

    # --- emit ---------------------------------------------------------------
    rows = []
    for year, data in ((2004, d04), (2009, d09)):
        plist = {
            "erststimme": (PARTIES[year]["t9AL"] + PARTIES[year]["t9AR"]
                           + PARTIES[year]["t9BL"] + PARTIES[year]["t9BR"]),
            "zweitstimme": PARTIES[year]["t10L"] + PARTIES[year]["t10R"],
        }
        for i in range(1, N_WKR + 1):
            w = f"{i:02d}"
            d = data[w]
            for stimme, key, valid, invalid in (
                    ("erststimme", "erst", d["valid_d"], d["invalid_d"]),
                    ("zweitstimme", "zweit", d["valid_l"], d["invalid_l"])):
                for p in plist[stimme]:
                    v = d[key].get(p)
                    if v is None:      # party did not stand here (printed "x")
                        continue
                    rows.append({
                        "state_abbr": STATE_ABBR, "state": STATE_NAME,
                        "election_year": year, "election_date": ELECTION_DATE[year],
                        "wkr_nr": w, "wkr_name": d["name"], "stimme": stimme,
                        "eligible_voters": d["eligible"],
                        "number_voters": d["voters"],
                        "valid_votes": valid, "invalid_votes": invalid,
                        "party_raw": p, "votes": v,
                    })

    # (9) the emitted rows must reproduce the candidate counts the reports state
    for year in (2004, 2009):
        n_e = sum(1 for r in rows
                  if r["election_year"] == year and r["stimme"] == "erststimme")
        n_z = sum(1 for r in rows
                  if r["election_year"] == year and r["stimme"] == "zweitstimme")
        req(n_e == N_DIREKTKANDIDATEN[year],
            f"{year}: {n_e} Wahlkreis candidacies emitted == the "
            f"{N_DIREKTKANDIDATEN[year]} Direktkandidaten the report states")
        req(n_z == N_WKR * N_LANDESLISTEN[year],
            f"{year}: {n_z} Listenstimmen rows == 60 Wahlkreise x "
            f"{N_LANDESLISTEN[year]} Landeslisten")
    die_if_failed()

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        wr.writeheader()
        wr.writerows(rows)

    print(f"\n2004 decoding: {n_pages_04} table pages, {n_chars_04} characters, "
          f"0 unresolved glyph codes")
    print(f"Wrote {len(rows)} rows -> {os.path.relpath(OUT, ROOT)}")
    for year in (2004, 2009):
        for stimme in ("erststimme", "zweitstimme"):
            n = sum(1 for r in rows
                    if r["election_year"] == year and r["stimme"] == stimme)
            npar = len({r["party_raw"] for r in rows
                        if r["election_year"] == year and r["stimme"] == stimme})
            print(f"  {year} {stimme:12s}: {n:5d} rows, {npar:2d} distinct party_raw")
    print("\nDistinct party_raw:")
    for year in (2004, 2009):
        ps = sorted({r["party_raw"] for r in rows if r["election_year"] == year})
        print(f"  {year} ({len(ps)}): " + " | ".join(ps))


if __name__ == "__main__":
    main()
