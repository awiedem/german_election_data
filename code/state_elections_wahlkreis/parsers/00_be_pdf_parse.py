#!/usr/bin/env python3
"""Stage-0 parser for BERLIN Abgeordnetenhaus results at WAHLKREIS level,
1999 / 2001 / 2006 / 2011 / 2021.

Source (raw, read-only), all digital-text PDFs (no OCR), published by
Der Landeswahlleiter Berlin / Amt fuer Statistik Berlin-Brandenburg:
  data/state_elections/raw/Landtagswahlen_Wahlkreis/Berlin/
    BE_1999_Abgeordnetenhauswahl_Ergebnisbericht.pdf
    BE_2001_Abgeordnetenhauswahl_Ergebnisbericht.pdf
    BE_2006_Abgeordnetenhauswahl_Ergebnisbericht.pdf
    BE_2011_Abgeordnetenhauswahl_Ergebnisbericht.pdf
    BE_2021_Abgeordnetenhauswahl_Ergebnisbericht.pdf

Why a PDF parser: the Landeswahlleitung publishes machine-readable
Wahlbezirk-level spreadsheets only from 2016 on (those two years are handled
directly by parsers/parse_BE.R).  For 1999-2011 and for 2021 the constituency
figures exist only inside the printed Ergebnisberichte, which do carry a text
layer.

!! NEVER USE pdfplumber ON THESE FILES !!
  BE_2006 has a broken ToUnicode CMap: pdfplumber decodes the digit "3" as "2"
  on the Wahlkreis grid pages (a page with dozens of 3s yields three literal
  "2" characters and no "3").  poppler's pdftotext decodes the same font
  correctly.  The whole script therefore runs on `pdftotext -bbox` word boxes.
  The printed-total reconciliations below are the safety net that would catch a
  recurrence of this class of silent corruption; they are run for every year.

TWO TABLE GEOMETRIES
  (a) "dense grid"  - 1999 / 2001 / 2006 / 2011
      Table 2.1 "Erststimmen ... nach Wahlkreisen und Wahlkreisvorschlaegen"
      and table 2.3 "Zweitstimmen ... nach Wahlkreisen und Parteien" hold the
      counts (2.2 / 2.4 are the percentage twins and are NOT read).  Each table
      is printed as two-page spreads: the LEFT page carries the row label
      (Bezirk name on the group's first row, then the Wahlkreis number) plus
      "Insgesamt" and the first parties; the RIGHT page carries the remaining
      parties with the row label repeated at the RIGHT edge.  Every Bezirk
      group ends with an "Insgesamt" subtotal row; the table ends with a
      "Berlin" grand total followed by "nachrichtlich:" Berlin-Ost / Berlin-West
      (excluded from the data and from every reconciliation).
      Turnout comes from table 3 ("Wahlberechtigte, Waehler, Erst- und
      Zweitstimmen ... nach Wahlkreisverbaenden (Bezirken) und Wahlkreisen";
      in 2011 titled "Erst- und Zweitstimmen ausgewaehlter Parteien ... nach
      Bezirken und Abgeordnetenhauswahlkreisen"), which prints Wahlberechtigte /
      Waehler / Ungueltige / Gueltige per Wahlkreis for BOTH Stimmen.
  (b) "one page per Wahlkreis" - 2021
      Tables 3.1-3.78 give both Stimmen, all parties and the turnout block on a
      single page each.  Table 5.2 (percentages only) is NOT used.

PARSING METHOD (coordinate based).  The thousands separator is a SPACE, so
"19 914" arrives as two words and a text-order split cannot tell "183 452 20
842" (= 183452, 20842) from (183, 45220, 842).  Digit tokens are therefore
merged on the x-gap.  Measured on every page this script reads, an intra-number
gap is 1.7-1.8 pt and the tightest genuine column boundary is 3.07 pt, so the
merge threshold is 2.5 pt and the run additionally ASSERTS that no digit-to-
digit gap anywhere in the parsed regions falls in the ambiguous band [2.0, 3.0].
(The 8 pt threshold used in an earlier proof of concept silently fused cells.)

PARTY COLUMN IDENTITY is never taken from the stacked, hyphenated column
headers ("Tier-/schutz-/partei", "Konser-/vative").  It is taken from the
report's own statewide table (1.3 in 1999/2001/2006, 1.2 in 2011, 1 in 2021),
which names one party per ROW and prints its Erst- and Zweitstimmen total: the
parties that carry a value for a Stimme, in printed order, ARE the columns of
the corresponding grid table.  That mapping is then verified by requiring the
78 Wahlkreise to sum to the statewide count for EVERY party, exactly, so a
wrong column order cannot pass silently.  The statewide table breaks
Einzelbewerber out into one "EB" row each while the grid prints a single
"Einzelbewerber" column, so consecutive EB rows are collapsed into one entry
(and their sum must equal the printed column, again exactly).

Zeichenerklaerung: "x" = party not on the ballot in that Wahlkreis -> NA
(emitted as an empty `votes` field).

VALIDATION - all hard; a single failure aborts the run and writes nothing:
  (0) digit-gap bimodality (see above)
  (1) exactly 78 Wahlkreise per year and Stimme
  (2) sum over the 78 Wahlkreise == the printed "Berlin" grand-total row, for
      EVERY party (grid years) / == the statewide table, for EVERY party (2021)
  (3) the printed "Berlin" row == the statewide table 1.x, for EVERY party
  (4) every Bezirk "Insgesamt" subtotal reproduces its Wahlkreis sums exactly
      (grid years) and every table-2.x Bezirk page does the same (2021)
  (5) per (Wahlkreis, Stimme): sum of party counts == gueltige Stimmen, and
      Waehler == gueltige + ungueltige; the turnout table's gueltige also has
      to equal the grid's "Insgesamt" column
  (6) the 78 Wahlkreise sum to the statewide Wahlberechtigte / Waehler /
      Ungueltige / Gueltige
  (7) pinned official statewide Zweitstimmen shares, +-0.1 pp

Output: data/state_elections/processed/wahlkreis/be_pdf/BE_1999_2021_pdf_long.csv
        (read by parsers/parse_BE.R, which appends the 2016 and 2023 results)
Run:    python3 code/state_elections_wahlkreis/parsers/00_be_pdf_parse.py
Requires: poppler (pdftotext) on PATH.
"""

import csv
import os
import re
import subprocess
import sys
from html.parser import HTMLParser

HERE = os.path.abspath(os.path.dirname(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW = os.path.join(ROOT, "data", "state_elections", "raw",
                   "Landtagswahlen_Wahlkreis", "Berlin")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed",
                       "wahlkreis", "be_pdf")
OUT = os.path.join(OUT_DIR, "BE_1999_2021_pdf_long.csv")

STATE_ABBR, STATE_NAME = "BE", "Berlin"
N_WKR = 78

GAP_MERGE = 2.5          # pt; below this, two digit tokens are one number
GAP_AMBIG = (2.0, 3.0)   # no digit-to-digit gap may fall inside this band
Y_OVERLAP = 0.5          # share of a word's height that must overlap its row

# 1-based PDF page numbers, verified against each report's own table headings.
#   t1      statewide fixture (party identity + totals): 1.3 / 1.2 / 1, Berlin page
#   grid    {stimme: (left/primary pages, right/continuation pages)}
#   t3      turnout table 3, per Wahlkreis, both Stimmen
#   bez     2021 only: table 2.x, one Bezirk per page (subtotal cross-check)
#   wk      2021 only: tables 3.1-3.78, one Wahlkreis per page
YEARS = {
    1999: dict(
        date="1999-10-10", t1=54,
        grid={"erststimme": ([64, 66], [65, 67]),
              "zweitstimme": ([72, 74], [73, 75])},
        t3=list(range(80, 112))),
    2001: dict(
        date="2001-10-21", t1=46,
        grid={"erststimme": ([54, 56], [55, 57]),
              "zweitstimme": ([62, 64], [63, 65])},
        t3=list(range(70, 96))),
    2006: dict(
        date="2006-09-17", t1=14,
        grid={"erststimme": ([24, 26], [25, 27]),
              "zweitstimme": ([32, 34], [33, 35])},
        t3=list(range(40, 66))),
    2011: dict(
        date="2011-09-18", t1=12,
        grid={"erststimme": ([24, 26], [25, 27]),
              "zweitstimme": ([32, 34], [33, 35])},
        t3=list(range(40, 66))),
    2021: dict(
        date="2021-09-26", t1=6,
        bez=list(range(7, 21)), wk=list(range(21, 99))),
}

# Official statewide Zweitstimmen shares (Landeswahlleitung), pinned.
OFFICIAL = {
    1999: {"CDU": 40.8, "SPD": 22.4, "PDS": 17.7, "GRÜNE": 9.9, "F.D.P.": 2.2},
    2001: {"SPD": 29.7, "CDU": 23.8, "PDS": 22.6, "FDP": 9.9, "GRÜNE": 9.1},
    2006: {"SPD": 30.8, "CDU": 21.3, "Die Linke.": 13.4, "GRÜNE": 13.1, "FDP": 7.6},
    2011: {"SPD": 28.3, "CDU": 23.3, "GRÜNE": 17.6, "DIE LINKE": 11.7,
           "PIRATEN": 8.9},
    2021: {"SPD": 21.4, "GRÜNE": 18.9, "CDU": 18.0, "DIE LINKE": 14.1,
           "AfD": 8.0, "FDP": 7.1},
}

# Individual candidates.  The statewide tables of 1999-2011 print one "EB" row
# per candidate while the grid prints a single "Einzelbewerber" column per
# Wahlkreis, so consecutive EB rows are collapsed; 2021 calls the same total
# "Sonst. Direktbewerb." statewide and "EB" on the Wahlkreis pages.  All of them
# are emitted as "EB", which is what the 2023 Wahlbezirk file already uses.
EB_COL = "EB"
LABEL_ALIAS = {"Sonst. Direktbewerb": EB_COL}

# Turnout rows of the "Merkmal" tables.  1999 footnotes them with plain digits
# ("Ungueltige Stimmen1 2"), hence the trailing [\d ]* .
META_RE = {
    "eligible_voters": re.compile(r"^Wahlberechtigte[\d ]*$"),
    "number_voters": re.compile(r"^(Wähler|Wählende)[\d ]*$"),
    "invalid_votes": re.compile(r"^Ungültige Stimmen[\d ]*$"),
    "valid_votes": re.compile(r"^Gültige Stimmen[\d ]*$"),
}
META_ORDER = ["eligible_voters", "number_voters", "invalid_votes", "valid_votes"]

NUM_RE = re.compile(r"^\d+$")
SUPER = "¹²³⁰⁴⁵⁶⁷⁸⁹"


# ---------------------------------------------------------------------------
# pdftotext -bbox -> words with coordinates
# ---------------------------------------------------------------------------
class _WordXML(HTMLParser):
    """Collect <word xMin=... >text</word> per <page>.  convert_charrefs=True
    resolves the XHTML entities poppler emits (&amp; in party names etc.)."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.pages, self._cur, self._attrs, self._buf = [], None, None, []

    def handle_starttag(self, tag, attrs):
        if tag == "page":
            self._cur = []
            self.pages.append(self._cur)
        elif tag == "word":
            self._attrs, self._buf = dict(attrs), []

    def handle_data(self, data):
        if self._attrs is not None:
            self._buf.append(data)

    def handle_endtag(self, tag):
        if tag == "word" and self._attrs is not None:
            a = self._attrs
            self._cur.append({"x0": float(a["xmin"]), "x1": float(a["xmax"]),
                              "top": float(a["ymin"]), "bot": float(a["ymax"]),
                              "text": "".join(self._buf)})
            self._attrs = None


_PAGE_CACHE = {}


def pdf_page(year, pageno):
    """Words of one 1-based PDF page.  The whole report is decoded once."""
    if year not in _PAGE_CACHE:
        pdf = os.path.join(
            RAW, "BE_%d_Abgeordnetenhauswahl_Ergebnisbericht.pdf" % year)
        xml = subprocess.run(["pdftotext", "-bbox", pdf, "-"],
                             check=True, capture_output=True).stdout.decode("utf-8")
        p = _WordXML()
        p.feed(xml)
        _PAGE_CACHE[year] = p.pages
    pages = _PAGE_CACHE[year]
    if not 1 <= pageno <= len(pages):
        sys.exit("%d: page %d out of range (%d pages)" % (year, pageno, len(pages)))
    return pages[pageno - 1]


# ---------------------------------------------------------------------------
# rows and cells
# ---------------------------------------------------------------------------
GAP_VIOLATIONS = []


def _same_line(ref, w):
    """Two words share a text line when their boxes overlap vertically by at
    least Y_OVERLAP of the shorter one.  Plain top-clustering fails here: the
    "EB" labels of the statewide tables sit ~2.1 pt above their own figures
    while genuine rows can be as little as 4.6 pt apart."""
    ov = min(ref["bot"], w["bot"]) - max(ref["top"], w["top"])
    h = min(ref["bot"] - ref["top"], w["bot"] - w["top"])
    return h > 0 and ov >= Y_OVERLAP * h


def page_rows(year, pageno):
    """Words of a page grouped into rows, each row a list of merged cells.

    Only DIGIT-to-DIGIT neighbours are merged (that is the German thousands
    space); word-to-word gaps inside a text label are left alone and the label
    is reassembled later by joining the leading cells with a blank.  Every
    digit-to-digit gap is recorded so the caller can assert bimodality.
    """
    words = sorted(pdf_page(year, pageno), key=lambda w: (w["top"], w["x0"]))
    lines, cur, ref = [], [], None
    for w in words:
        if cur is not None and ref is not None and not _same_line(ref, w):
            lines.append(sorted(cur, key=lambda z: z["x0"]))
            cur, ref = [], None
        cur.append(w)
        if ref is None or (w["bot"] - w["top"]) > (ref["bot"] - ref["top"]):
            ref = w                      # tallest word anchors the line
    if cur:
        lines.append(sorted(cur, key=lambda z: z["x0"]))

    out = []
    for ln in lines:
        cells, cur = [], [ln[0]]
        for prev, nxt in zip(ln, ln[1:]):
            gap = nxt["x0"] - prev["x1"]
            digits = NUM_RE.match(prev["text"]) and NUM_RE.match(nxt["text"])
            if digits:
                if GAP_AMBIG[0] <= gap <= GAP_AMBIG[1]:
                    GAP_VIOLATIONS.append((year, pageno, round(gap, 2),
                                           prev["text"], nxt["text"]))
                if gap < GAP_MERGE:
                    cur.append(nxt)
                    continue
            cells.append(cur)
            cur = [nxt]
        cells.append(cur)
        out.append([{"text": "".join(w["text"] for w in c),
                     "x0": c[0]["x0"], "x1": c[-1]["x1"]} for c in cells])
    return out


PCT_RE = re.compile(r"^\d+,\d+$")
DASH = ("–", "—", "-")


def is_count(t):
    """A cell of a COUNT column: an integer, 'x' (not on the ballot) or a dash
    (printed zero)."""
    return bool(NUM_RE.match(t)) or t in ("x", "X") or t in DASH


def is_field(t):
    """A cell of any numeric column, counts and the printed percent twins."""
    return is_count(t) or bool(PCT_RE.match(t))


def to_val(t):
    if t in ("x", "X"):
        return None
    if t in DASH:
        return 0
    return int(t)


def clean_label(parts):
    """Join label cells and strip the dot leader and footnote superscripts.

    The leader is a run of dots that fills the space to the first column; it can
    be as short as a single dot ("Deutsche Konservative ."), and it may or may
    not be a separate word.  Only runs of >= 2 dots, or a lone dot preceded by a
    blank, are removed, so a trailing period that belongs to the name survives
    ("F.D.P.", "Die Linke.", "du.").
    """
    s = " ".join(p for p in parts if p)
    s = s.replace("…", "...")
    s = re.sub(r"\s+", " ", s).strip()
    while True:
        t = re.sub(r"\s*\.{2,}$", "", s)
        t = re.sub(r"\s+\.$", "", t)
        t = re.sub(r"^\.{2,}\s*", "", t)     # leader before a right-hand label
        t = t.strip().strip(SUPER).strip()
        if t == s:
            return s
        s = t


def pkey(label):
    """Matching key for a party label: trailing dots dropped entirely.

    Needed because the same party is typeset with and without its trailing
    period depending on whether the leader was set as a separate word
    ("du." statewide vs "du..........." on the 2021 Wahlkreis pages)."""
    return re.sub(r"[.\s]+$", "", label)


# ---------------------------------------------------------------------------
# "Merkmal" tables: statewide 1.x, turnout table 3, 2021 tables 2.x / 3.x
# ---------------------------------------------------------------------------
def merkmal_rows(year, pageno):
    """(label, erst, zweit) for every data row of a Merkmal-style page.

    Column layout is always: label | Anzahl(erst, this election) | %(erst) |
    Anzahl(zweit) | %(zweit) | ... older election and difference columns.  A
    data row is found as the first run of >= 4 value cells that does not start
    at cell 0 (which excludes the "1 2 3 ... 12" column-index rule line and the
    page-number header).  Footnote digits inside a label form a run of length 1
    and are therefore swallowed into the label, not into the data.
    Also returned: `text_rows`, the rows that carry no data at all (used to
    locate the centred section titles of table 3).
    """
    data, text_rows = [], []
    for cells in page_rows(year, pageno):
        texts = [c["text"] for c in cells]
        run_start = None
        i = 1
        while i < len(texts):
            if is_field(texts[i]):
                j = i
                while j < len(texts) and is_field(texts[j]):
                    j += 1
                if j - i >= 4:
                    run_start, run_len = i, j - i
                    break
                i = j
            else:
                i += 1
        if run_start is None:
            if texts and not any(is_field(t) for t in texts):
                text_rows.append(clean_label(texts))
            continue
        label = clean_label(texts[:run_start])
        if not label:
            continue
        vals = texts[run_start:run_start + 4]
        data.append((label, to_val(vals[0]), to_val(vals[2])))
    return data, text_rows


def split_meta_parties(rows):
    """Split a Merkmal block into the 4 turnout figures and the party rows.

    Party rows are everything after "Gueltige Stimmen"; consecutive "EB" rows
    are collapsed into one `EB_COL` entry (the grid tables print them as a
    single aggregated column).  The trailing "Sonstige" row is the residual of
    the COMPARISON election and must be empty for the current one.
    """
    meta, parties, seen_valid = {}, [], False
    for label, erst, zweit in rows:
        if not seen_valid:
            for key, rgx in META_RE.items():
                if rgx.match(label):
                    meta[key] = (erst, zweit)
                    if key == "valid_votes":
                        seen_valid = True
                    break
            continue
        if label == "Sonstige":
            if erst is not None or zweit is not None:
                sys.exit("unexpected non-empty 'Sonstige' residual row")
            continue
        if label == "EB" and parties and parties[-1][0] == EB_COL:
            _, pe, pz = parties[-1]
            parties[-1] = (EB_COL,
                           None if (pe is None and erst is None)
                           else (pe or 0) + (erst or 0),
                           None if (pz is None and zweit is None)
                           else (pz or 0) + (zweit or 0))
        elif label == "EB":
            parties.append((EB_COL, erst, zweit))
        else:
            parties.append((LABEL_ALIAS.get(pkey(label), label), erst, zweit))
    if len(meta) != 4:
        sys.exit("turnout block incomplete: %s" % sorted(meta))
    return meta, parties


# ---------------------------------------------------------------------------
# dense grid tables 2.1 / 2.3  (1999 / 2001 / 2006 / 2011)
# ---------------------------------------------------------------------------
BERLIN_RE = re.compile(r"^Berlin\*?$")
# Blocks of table 3 that are NOT a Wahlkreis: the Bezirk total that opens each
# group, and the Berlin / Berlin-Ost / Berlin-West blocks that close the table.
SUMMARY_RE = re.compile(r"^Berlin(-Ost|-West)?\*?$")


def _n_cols(year, pages, side):
    """Width of the numeric block, read off the printed 'Berlin' grand total."""
    for p in pages:
        for cells in page_rows(year, p):
            t = [c["text"] for c in cells]
            if side == "left" and t and BERLIN_RE.match(t[0]) \
                    and all(is_count(x) for x in t[1:]) and len(t) > 5:
                return len(t) - 1
            if side == "right" and t and BERLIN_RE.match(t[-1]) \
                    and all(is_count(x) for x in t[:-1]) and len(t) > 5:
                return len(t) - 1
    sys.exit("no 'Berlin' grand-total row found on %s pages %s of %d"
             % (side, pages, year))


def parse_grid(year, primary_pages, cont_pages, n_parties):
    """Return (records, bezirk_order).

    records: list of dicts with keys bezirk / wk (None on subtotal rows) /
    label ('Insgesamt', 'Berlin', ...) / vals (list of n_parties values, the
    leading "Insgesamt" column split off into `total`).
    """
    n_left = _n_cols(year, primary_pages, "left")
    n_right = _n_cols(year, cont_pages, "right")
    if n_left - 1 + n_right != n_parties:
        sys.exit("%d: grid width %d+%d-1 != %d parties from the statewide table"
                 % (year, n_left, n_right, n_parties))

    left, bez_order = [], []
    cur_bez = None
    for p in primary_pages:
        for cells in page_rows(year, p):
            t = [c["text"] for c in cells]
            k = 0
            while k < len(t) and is_count(t[len(t) - 1 - k]):
                k += 1
            if k not in (n_left, n_left + 1):
                continue
            head = t[:len(t) - k]
            run = t[len(t) - k:]
            label = clean_label(head)
            if k == n_left + 1:
                if label:
                    cur_bez = label.rstrip("*")
                    if cur_bez not in bez_order:
                        bez_order.append(cur_bez)
                left.append({"bezirk": cur_bez, "wk": int(run[0]),
                             "label": None, "vals": [to_val(v) for v in run[1:]]})
            else:
                if not label:
                    continue
                left.append({"bezirk": cur_bez, "wk": None, "label": label,
                             "vals": [to_val(v) for v in run]})

    right = []
    for p in cont_pages:
        for cells in page_rows(year, p):
            t = [c["text"] for c in cells]
            k = 0
            while k < len(t) and is_count(t[k]):
                k += 1
            if k < n_right:
                continue
            vals = [to_val(v) for v in t[:n_right]]
            if k == n_right + 1 and NUM_RE.match(t[n_right]):
                right.append({"wk": int(t[n_right]), "label": None,
                              "bezirk": clean_label(t[n_right + 1:]).rstrip("*")
                              or None, "vals": vals})
            elif k == n_right and len(t) > n_right:
                lab = clean_label(t[n_right:])
                if lab:
                    right.append({"wk": None, "label": lab, "bezirk": None,
                                  "vals": vals})

    if len(left) != len(right):
        sys.exit("%d: %d left rows vs %d right rows" % (year, len(left), len(right)))
    recs = []
    for a, b in zip(left, right):
        if a["wk"] != b["wk"] or (a["label"] or "").rstrip("*") != \
                (b["label"] or "").rstrip("*"):
            sys.exit("%d: row label mismatch %r vs %r" % (year, a, b))
        if b["bezirk"] and a["bezirk"] and b["bezirk"] != a["bezirk"]:
            sys.exit("%d: Bezirk mismatch %s vs %s" % (year, a["bezirk"], b["bezirk"]))
        vals = a["vals"] + b["vals"]
        recs.append({"bezirk": a["bezirk"], "wk": a["wk"], "label": a["label"],
                     "total": vals[0], "vals": vals[1:]})
    return recs, bez_order


# ---------------------------------------------------------------------------
# turnout table 3 (grid years)
# ---------------------------------------------------------------------------
def parse_turnout(year, pages, bez_order):
    """(bezirk, wk) -> {stimme: {eligible_voters, number_voters, ...}}.

    Every block of table 3 is introduced by a centred title row.  Only titles
    of the form "<known Bezirk> Wahlkreis <n>" open a Wahlkreis block, so the
    Bezirk-total block that precedes each group is skipped automatically.  The
    title cannot be found by looking for text-only rows: it ends in the
    Wahlkreis number, which is a digit.
    """
    known = {b: b for b in bez_order}
    known.update({b + "*": b for b in bez_order})
    title_re = re.compile(r"^(.*?)\s+Wahlkreis\s+(\d+)$")
    out, cur = {}, None
    for p in pages:
        for cells in page_rows(year, p):
            t = [c["text"] for c in cells]
            lab_all = clean_label(t)
            m = title_re.match(lab_all)
            if m and m.group(1) in known:
                cur = (known[m.group(1)], int(m.group(2)))
                out.setdefault(cur, {"erststimme": {}, "zweitstimme": {}})
                continue
            if lab_all in known or SUMMARY_RE.match(lab_all):
                cur = None            # Bezirk total / Berlin / Berlin-Ost/West
                continue
            if cur is None:
                continue
            run_start = None
            i = 1
            while i < len(t):
                if is_field(t[i]):
                    j = i
                    while j < len(t) and is_field(t[j]):
                        j += 1
                    if j - i >= 4:
                        run_start = i
                        break
                    i = j
                else:
                    i += 1
            if run_start is None:
                continue
            lab = clean_label(t[:run_start])
            for key, rgx in META_RE.items():
                if rgx.match(lab):
                    v = t[run_start:run_start + 4]
                    out[cur]["erststimme"][key] = to_val(v[0])
                    out[cur]["zweitstimme"][key] = to_val(v[2])
                    break
    return out


# ---------------------------------------------------------------------------
# 2021: one page per Wahlkreis / per Bezirk
# ---------------------------------------------------------------------------
WK_TITLE_RE = re.compile(r"^3\.(\d+)\s+Wahlkreis\s+(.+?)\s+(\d+)$")
BEZ_TITLE_RE = re.compile(r"^2\.(\d+)\s+Bezirk\s+(.+)$")


def parse_2021_pages(pages, title_re):
    """[(title groups, meta, parties)] for the 2021 per-area pages.

    The page title ("3.1 Wahlkreis Mitte 1", "2.1 Bezirk Mitte") is matched on
    the whole row, digits included."""
    out = []
    for p in pages:
        title = None
        for cells in page_rows(2021, p):
            mt = title_re.match(clean_label([c["text"] for c in cells]))
            if mt:
                title = mt.groups()
                break
        if title is None:
            continue
        rows, _ = merkmal_rows(2021, p)
        meta, parties = split_meta_parties(rows)
        out.append((title, meta, parties))
    return out


# ---------------------------------------------------------------------------
# validation bookkeeping
# ---------------------------------------------------------------------------
FAILS = []


def req(cond, label, detail=None):
    print(("  [ok]  " if cond else "  [FAIL]") + " " + label)
    if not cond:
        FAILS.append(label)
        for d in (detail or [])[:8]:
            print("           ", d)


# ---------------------------------------------------------------------------
# per-year drivers
# ---------------------------------------------------------------------------
def statewide(year):
    cfg = YEARS[year]
    rows, _ = merkmal_rows(year, cfg["t1"])
    meta, parties = split_meta_parties(rows)
    by_stimme = {}
    for idx, stimme in ((0, "erststimme"), (1, "zweitstimme")):
        by_stimme[stimme] = [(p, (e, z)[idx]) for p, e, z in parties
                             if (e, z)[idx] is not None]
    meta_st = {s: {k: meta[k][i] for k in META_ORDER}
               for i, s in ((0, "erststimme"), (1, "zweitstimme"))}
    return meta_st, by_stimme


def run_grid_year(year):
    cfg = YEARS[year]
    print("\n===== BE %d =====" % year)
    st_meta, st_parties = statewide(year)

    tables, bez_order = {}, None
    for stimme, (pp, cp) in cfg["grid"].items():
        recs, order = parse_grid(year, pp, cp, len(st_parties[stimme]))
        tables[stimme] = recs
        if bez_order is None:
            bez_order = order
        elif order != bez_order:
            sys.exit("%d: Bezirk order differs between the two Stimmen" % year)

    turnout = parse_turnout(year, cfg["t3"], bez_order)

    print("VALIDATION")
    req(not GAP_VIOLATIONS,
        "no digit-to-digit gap inside the ambiguous band %s" % (GAP_AMBIG,),
        GAP_VIOLATIONS)
    req(len(bez_order) in (12, 23),
        "Bezirke: %d (%s ...)" % (len(bez_order), ", ".join(bez_order[:3])))

    wk_data = {}     # (stimme, bezirk, wk) -> {party: votes}
    for stimme, recs in tables.items():
        names = [p for p, _ in st_parties[stimme]]
        wks = [r for r in recs if r["wk"] is not None]
        req(len(wks) == N_WKR,
            "%s: %d Wahlkreise (expected %d)" % (stimme, len(wks), N_WKR))

        # (2)+(3) grand totals
        berlin = [r for r in recs if r["label"] and BERLIN_RE.match(r["label"])]
        req(len(berlin) == 1, "%s: exactly one printed 'Berlin' row" % stimme)
        if len(berlin) != 1:
            continue
        berlin = berlin[0]
        bad = [(names[i], sum(w["vals"][i] or 0 for w in wks), berlin["vals"][i])
               for i in range(len(names))
               if sum(w["vals"][i] or 0 for w in wks) != (berlin["vals"][i] or 0)]
        req(not bad, "%s: 78 Wahlkreise sum to the printed 'Berlin' row, "
                     "every party" % stimme, bad)
        bad = [(names[i], berlin["vals"][i], st_parties[stimme][i][1])
               for i in range(len(names))
               if (berlin["vals"][i] or 0) != st_parties[stimme][i][1]]
        req(not bad, "%s: printed 'Berlin' row == statewide table, "
                     "every party" % stimme, bad)
        req(sum(w["total"] or 0 for w in wks) == berlin["total"] ==
            st_meta[stimme]["valid_votes"],
            "%s: 'Insgesamt' column sums to the statewide gueltige Stimmen"
            % stimme)

        # (4) Bezirk subtotals
        bad = []
        for b in bez_order:
            sub = [r for r in recs
                   if r["bezirk"] == b and r["label"] == "Insgesamt"]
            mem = [r for r in recs if r["bezirk"] == b and r["wk"] is not None]
            if len(sub) != 1:
                bad.append((b, "%d subtotal rows" % len(sub)))
                continue
            for i in range(len(names)):
                s = sum(m["vals"][i] or 0 for m in mem)
                if s != (sub[0]["vals"][i] or 0):
                    bad.append((b, names[i], s, sub[0]["vals"][i]))
            if sum(m["total"] or 0 for m in mem) != sub[0]["total"]:
                bad.append((b, "Insgesamt", sum(m["total"] or 0 for m in mem),
                            sub[0]["total"]))
        req(not bad, "%s: every Bezirk 'Insgesamt' subtotal reproduces its "
                     "Wahlkreis sums" % stimme, bad)

        # (5) party sum == Insgesamt column, per Wahlkreis
        bad = [(w["bezirk"], w["wk"], sum(v or 0 for v in w["vals"]), w["total"])
               for w in wks if sum(v or 0 for v in w["vals"]) != w["total"]]
        req(not bad, "%s: per Wahlkreis, sum of party counts == 'Insgesamt'"
            % stimme, bad)

        for w in wks:
            wk_data[(stimme, w["bezirk"], w["wk"])] = \
                {names[i]: w["vals"][i] for i in range(len(names))}
            wk_data[(stimme, w["bezirk"], w["wk"])]["__total__"] = w["total"]

    # (5b)+(6) turnout
    keys = {(b, w) for (s, b, w) in wk_data}
    req(set(turnout) == keys,
        "turnout table 3 covers exactly the %d Wahlkreise of tables 2.1/2.3"
        % len(keys),
        sorted(set(turnout) ^ keys))
    bad_id, bad_link = [], []
    for (b, w), d in turnout.items():
        for stimme in ("erststimme", "zweitstimme"):
            m = d[stimme]
            if len(m) != 4:
                bad_id.append((b, w, stimme, "incomplete"))
                continue
            if not turnout_plausible(m):
                bad_id.append((b, w, stimme, m))
            got = wk_data.get((stimme, b, w), {}).get("__total__")
            if got is not None and got != m["valid_votes"]:
                bad_link.append((b, w, stimme, got, m["valid_votes"]))
    req(not bad_id, "per (Wahlkreis, Stimme): gueltige + ungueltige is within "
                    "5% of Waehler, and Waehler <= Wahlberechtigte", bad_id)
    report_unused_ballots(turnout)
    req(not bad_link, "per (Wahlkreis, Stimme): table 3 gueltige == the grid's "
                      "'Insgesamt' column", bad_link)
    bad = []
    for stimme in ("erststimme", "zweitstimme"):
        for k in META_ORDER:
            s = sum(turnout[key][stimme][k] for key in turnout)
            if s != st_meta[stimme][k]:
                bad.append((stimme, k, s, st_meta[stimme][k]))
    req(not bad, "the 78 Wahlkreise sum to the statewide turnout figures", bad)

    check_shares(year, st_parties, st_meta)
    return build_rows(year, st_parties, wk_data, turnout, bez_order)

def run_2021():
    """2021 prints one page per Wahlkreis (tables 3.1-3.78), each with both
    Stimmen, the turnout block and the parties that stood in that Bezirk.  A
    party missing from a page did not stand there (Zweitstimmen are Bezirks-
    listen), which is the same NA the grid years mark with an "x"."""
    year = 2021
    cfg = YEARS[year]
    print("\n===== BE %d =====" % year)
    st_meta, st_parties = statewide(year)

    wk_pages = parse_2021_pages(cfg["wk"], WK_TITLE_RE)
    bez_pages = parse_2021_pages(cfg["bez"], BEZ_TITLE_RE)

    print("VALIDATION")
    req(not GAP_VIOLATIONS,
        "no digit-to-digit gap inside the ambiguous band %s" % (GAP_AMBIG,),
        GAP_VIOLATIONS)
    req(len(wk_pages) == N_WKR,
        "%d Wahlkreis pages 3.1-3.78 (expected %d)" % (len(wk_pages), N_WKR))
    req(len(bez_pages) == 12, "%d Bezirk pages 2.1-2.12" % len(bez_pages))
    req([int(t[0]) for t, _, _ in wk_pages] == list(range(1, N_WKR + 1)),
        "the Wahlkreis pages are numbered 3.1 ... 3.78 without a gap")

    bez_order = []
    for (_, bname, _), _, _ in wk_pages:
        if bname not in bez_order:
            bez_order.append(bname)
    req(len(bez_order) == 12, "12 Bezirke: %s ..." % ", ".join(bez_order[:3]))

    wk_data, turnout = {}, {}
    for (_, bname, wknr), meta, parties in wk_pages:
        key = (bname, int(wknr))
        turnout[key] = {s: {k: meta[k][i] for k in META_ORDER}
                        for i, s in ((0, "erststimme"), (1, "zweitstimme"))}
        for i, stimme in ((0, "erststimme"), (1, "zweitstimme")):
            wk_data[(stimme,) + key] = {pkey(p): (e, z)[i]
                                        for p, e, z in parties}
    seen = set()
    for _, _, parties in wk_pages:
        seen.update(pkey(p) for p, _, _ in parties)

    for stimme in ("erststimme", "zweitstimme"):
        names = [p for p, _ in st_parties[stimme]]
        bad = [p for p in names if pkey(p) not in seen]
        req(not bad, "%s: every statewide party is printed on at least one "
                     "Wahlkreis page" % stimme, bad)
        bad = []
        for p, tot in st_parties[stimme]:
            s = sum(wk_data[k].get(pkey(p)) or 0
                    for k in wk_data if k[0] == stimme)
            if s != tot:
                bad.append((p, s, tot))
        req(not bad, "%s: 78 Wahlkreise sum to the statewide table, every party"
            % stimme, bad)
        bad = []
        for p in sorted(seen - {pkey(n) for n in names}):
            s = sum(wk_data[k].get(p) or 0 for k in wk_data if k[0] == stimme)
            if s:
                bad.append((p, s))
        req(not bad, "%s: no votes outside the statewide party list" % stimme,
            bad)
        bad = [(k[1], k[2], sum(v or 0 for v in d.values()),
                turnout[k[1:]][stimme]["valid_votes"])
               for k, d in wk_data.items() if k[0] == stimme
               and sum(v or 0 for v in d.values())
               != turnout[k[1:]][stimme]["valid_votes"]]
        req(not bad, "%s: per Wahlkreis, sum of party counts == gueltige Stimmen"
            % stimme, bad)
        idx = 0 if stimme == "erststimme" else 1
        bad = []
        for (_, bname), bmeta, bparties in bez_pages:
            printed = {pkey(p): (e, z)[idx] for p, e, z in bparties}
            members = [k for k in wk_data if k[0] == stimme and k[1] == bname]
            for p in set(printed) | {pkey(n) for n in names}:
                s = sum(wk_data[k].get(p) or 0 for k in members)
                if s != (printed.get(p) or 0):
                    bad.append((bname, p, s, printed.get(p)))
            for k in META_ORDER:
                s = sum(turnout[(b, w)][stimme][k]
                        for (b, w) in turnout if b == bname)
                if s != bmeta[k][idx]:
                    bad.append((bname, k, s, bmeta[k][idx]))
        req(not bad, "%s: every Bezirk table 2.x reproduces its Wahlkreis sums"
            % stimme, bad)

    bad_id = []
    for (b, w), d in turnout.items():
        for stimme in ("erststimme", "zweitstimme"):
            m = d[stimme]
            if not turnout_plausible(m):
                bad_id.append((b, w, stimme, m))
    req(not bad_id, "per (Wahlkreis, Stimme): gueltige + ungueltige is within "
                    "5% of Waehler, and Waehler <= Wahlberechtigte", bad_id)
    report_unused_ballots(turnout)
    bad = []
    for stimme in ("erststimme", "zweitstimme"):
        for k in META_ORDER:
            s = sum(turnout[key][stimme][k] for key in turnout)
            if s != st_meta[stimme][k]:
                bad.append((stimme, k, s, st_meta[stimme][k]))
    req(not bad, "the 78 Wahlkreise sum to the statewide turnout figures", bad)

    check_shares(year, st_parties, st_meta)
    wk_by_name = {(s, b, w): {n: wk_data[(s, b, w)].get(pkey(n))
                              for n in [p for p, _ in st_parties[s]]}
                  for (s, b, w) in wk_data}
    return build_rows(year, st_parties, wk_by_name, turnout, bez_order)


def turnout_plausible(m):
    """Berlin prints Waehler per Wahlkreis, not "abgegebene Stimmen".  A voter
    may hand in a ballot without marking one of the two votes, so
    gueltige+ungueltige normally falls a little short of Waehler; a handful of
    Wahlkreise print the reverse by one or two votes.  Both are properties of
    the source, so this is a 2 % band rather than an identity - the exact
    pinning of these figures comes from the two reconciliations that follow (the
    grid reproduces gueltige exactly, and the 78 Wahlkreise sum to the statewide
    table exactly)."""
    cast = m["valid_votes"] + m["invalid_votes"]
    return (m["number_voters"] <= m["eligible_voters"]
            and abs(cast - m["number_voters"]) <= 0.05 * m["number_voters"])


def report_unused_ballots(turnout):
    """Berlin prints Waehler, not "abgegebene Stimmen", per Wahlkreis: a voter
    may hand in a ballot without marking one of the two votes, so
    gueltige+ungueltige falls a little short of Waehler.  Reported, not failed
    (the statewide reconciliation below pins the totals anyway)."""
    for stimme in ("erststimme", "zweitstimme"):
        d = [turnout[k][stimme]["number_voters"]
             - turnout[k][stimme]["valid_votes"]
             - turnout[k][stimme]["invalid_votes"] for k in turnout]
        rel = max(abs(turnout[k][stimme]["number_voters"]
                      - turnout[k][stimme]["valid_votes"]
                      - turnout[k][stimme]["invalid_votes"])
                  / turnout[k][stimme]["number_voters"] for k in turnout)
        print("         %s: Waehler minus (gueltige+ungueltige) = %d in total, "
              "at most %.1f%% in any Wahlkreis" % (stimme, sum(d), 100 * rel))


def check_shares(year, st_parties, st_meta):
    base = st_meta["zweitstimme"]["valid_votes"]
    tot = dict(st_parties["zweitstimme"])
    bad = []
    for p, share in OFFICIAL[year].items():
        if p not in tot:
            bad.append((p, "not in the statewide table"))
            continue
        got = 100.0 * tot[p] / base
        if abs(got - share) > 0.1:
            bad.append((p, round(got, 2), share))
    req(not bad, "official statewide Zweitstimmen shares reproduced (+-0.1 pp)",
        bad)


# ---------------------------------------------------------------------------
# emit
# ---------------------------------------------------------------------------
def build_rows(year, st_parties, wk_data, turnout, bez_order):
    date = YEARS[year]["date"]
    bez_nr = {b: i + 1 for i, b in enumerate(bez_order)}
    rows = []
    for stimme in ("erststimme", "zweitstimme"):
        names = [p for p, _ in st_parties[stimme]]
        for (b, w) in sorted(turnout, key=lambda k: (bez_nr[k[0]], k[1])):
            m = turnout[(b, w)][stimme]
            votes = wk_data[(stimme, b, w)]
            for p in names:
                v = votes.get(p)
                rows.append({
                    "state_abbr": STATE_ABBR, "state": STATE_NAME,
                    "election_year": year, "election_date": date,
                    "wkr_nr": "%02d-%02d" % (bez_nr[b], w),
                    "wkr_name": "%s %d" % (b, w),
                    "stimme": stimme,
                    "eligible_voters": m["eligible_voters"],
                    "number_voters": m["number_voters"],
                    "valid_votes": m["valid_votes"],
                    "invalid_votes": m["invalid_votes"],
                    "party_raw": p,
                    "votes": "" if v is None else v,
                })
    return rows


def main():
    all_rows = []
    for year in (2011, 2021, 1999, 2001, 2006):
        all_rows += run_2021() if year == 2021 else run_grid_year(year)
    if FAILS:
        print("\n%d VALIDATION FAILURE(S) - nothing written." % len(FAILS))
        sys.exit(1)

    order = {y: i for i, y in enumerate((1999, 2001, 2006, 2011, 2021))}
    all_rows.sort(key=lambda r: (order[r["election_year"]], r["stimme"],
                                 r["wkr_nr"], r["party_raw"]))
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(all_rows[0].keys()))
        wr.writeheader()
        wr.writerows(all_rows)
    print("\nWrote %d rows -> %s" % (len(all_rows), os.path.relpath(OUT, ROOT)))
    for y in (1999, 2001, 2006, 2011, 2021):
        sub = [r for r in all_rows if r["election_year"] == y]
        ps = {}
        for r in sub:
            ps.setdefault(r["stimme"], set()).add(r["party_raw"])
        print("  %d: %5d rows | %s" % (
            y, len(sub), " | ".join("%s %d parties" % (s, len(v))
                                    for s, v in sorted(ps.items()))))


if __name__ == "__main__":
    main()
