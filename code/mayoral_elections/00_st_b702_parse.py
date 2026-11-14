#!/usr/bin/env python3
"""Stage-0 parser for the StaLA Sachsen-Anhalt registers "Landräte und Bürgermeister".

Source: Statistisches Landesamt Sachsen-Anhalt, Statistische Berichte B VII 702
"Landräte und Bürgermeister" (the 2004 issue carries the order number 2V005).
Four issues were scanned for GERDA on request (Vorgang 2026_1083, e-mail of
2026-11-13, OCR text layer by the StaLA); the 2011 issue is public in the
Statistische Bibliothek. All five sit in
data/mayoral_elections/raw/sachsen_anhalt/stala_b702/ (see README.md there).

Each issue lists every office holder in office on its Stichtag:
  1    the elected Landräte (5-digit Kreis code)
  2.2  the elected Bürgermeister of every Gemeinde (8-digit AGS)
plus, depending on the issue, the hauptamtliche Bürgermeister again (2.3 in
2004/2007/2009, a subset of 2.2), Bürgermeister elected after the Stichtag
(2004 2.4, 2009 2.5), Bürgermeister of Gemeinden dissolved after their election
(2009 2.4, 2010 2.4), and the Verbandsgemeindebürgermeister (2010/2011 2.3).
A record is two lines (2004, 2007) or three lines (2009-2011):
  AGS  Gemeinde  [Ha|HaOB|OB]  Last, First   [Partei]  Anz. Bew.
                               Beruf/Tätigkeit         Wahltag  [Wahltag SW]
                               [Geburtsjahr]
The parser reads pdfplumber words and places them by x position relative to
the page's own column headers ("Name", "Partei", "Anz."), so it follows the
layout drift between issues and pages.

The 2007 and 2009 issues add an annex with the full results of the
Oberbürgermeister elections of the kreisfreie Städte and of the Landrat
elections; it is parsed separately (results_*), with arithmetic checks.

Outputs (gitignored: they name office holders with profession and birth year,
and the annex names losing candidates):
  stala_b702/st_b702_register_restricted.csv
  stala_b702/st_b702_results_restricted.csv

Run: python3 code/mayoral_elections/00_st_b702_parse.py [--debug]
"""
from __future__ import annotations

import csv
import re
import sys
from collections import Counter, defaultdict
from datetime import date
from pathlib import Path

import pdfplumber

ROOT = Path(__file__).resolve().parents[2]
RAW = ROOT / "data" / "mayoral_elections" / "raw" / "sachsen_anhalt" / "stala_b702"
OUT_REGISTER = RAW / "st_b702_register_restricted.csv"
OUT_RESULTS = RAW / "st_b702_results_restricted.csv"

ISSUES = [
    ("2V005_01.01.2004_klein_ocred_.pdf", "2004-01-01"),
    ("3B702_01.08.2007_klein_ocred_.pdf", "2007-08-01"),
    ("3B702_01.01.2009_klein_ocred_.pdf", "2009-01-01"),
    ("3B702_01.03.2010_klein_ocred_.pdf", "2010-03-01"),
    ("6B702_2011.pdf", "2011-10-01"),
]

DEBUG = "--debug" in sys.argv

# ---------------------------------------------------------------- helpers

OCR_DIGIT = str.maketrans({"O": "0", "o": "0", "D": "0", "l": "1", "I": "1", "i": "1",
                           "|": "1", "S": "5", "B": "8", "Z": "2", "z": "2"})
FLAG_RE = re.compile(r"^\|?(Ha|HaOB|HaO8|Ha0B|OB|0B)\|?$")
TITLE_RE = re.compile(r"^(Dr\.|Prof\.|Dipl\.-[A-Za-zäöü.]+|Dr\.-Ing\.|Dr\.med\.)$")


def norm_code(tok: str, n: int) -> str | None:
    """Return a Sachsen-Anhalt code of n digits, repairing common OCR letter/digit swaps."""
    t = tok.strip(".,:;|")
    if re.fullmatch(rf"15\d{{{n - 2}}}", t):
        return t
    if len(t) == n:
        u = t.translate(OCR_DIGIT)
        if re.fullmatch(rf"15\d{{{n - 2}}}", u):
            return u
    return None


DATE_RE = re.compile(r"(?<!\d)(\d{1,2})\.(\d{1,2})\.(\d{4}|\d{2})(?!\d)")


def parse_dates(text: str) -> list[str]:
    """Dates from an OCR line: ',' read for '.', doubled dots, stray degree signs.

    The 2010/2011 lists of Verbandsgemeinde mayors and of dissolved Gemeinden
    print two-digit years (29.11.09); every year there is 19xx/20xx with 90+ = 19xx.
    """
    t = text.replace(",", ".").replace("°", "").replace("..", ".")
    t = re.sub(r"(\d)\s*\.\s*(\d)", r"\1.\2", t)
    out = []
    for d_, m_, y_ in DATE_RE.findall(t):
        y = int(y_)
        if len(y_) == 2:
            y += 1900 if y >= 90 else 2000
        try:
            out.append(date(y, int(m_), int(d_)).isoformat())
        except ValueError:
            out.append(f"INVALID:{d_}.{m_}.{y_}")
    return out


def split_person(raw: str) -> tuple[str, str, str]:
    """'Dr. Trümper, Lutz' -> ('Dr.', 'Trümper', 'Lutz'); 'Ermrich, Michael, Dr.' too."""
    s = re.sub(r"\s+", " ", raw).strip(" ,;|")
    titles = []
    parts = [p.strip() for p in s.split(",")]
    # trailing title field ("Ermrich, Michael, Dr.")
    while len(parts) > 2 and TITLE_RE.match(parts[-1]):
        titles.append(parts.pop())
    last = parts[0] if parts else ""
    first = ", ".join(parts[1:]) if len(parts) > 1 else ""
    toks = last.split(" ")
    while toks and TITLE_RE.match(toks[0]):
        titles.append(toks.pop(0))
    last = " ".join(toks)
    ftoks = first.split(" ")
    while ftoks and TITLE_RE.match(ftoks[0]):
        titles.append(ftoks.pop(0))
    first = " ".join(ftoks)
    return " ".join(titles), last.strip(), first.strip()


def _clusters(ys: list[float], gap: float, wide: float = 5.0) -> tuple[int, int]:
    """(number of clusters, number of clusters spanning more than `wide` pt)."""
    ys = sorted(ys)
    if not ys:
        return 0, 0
    n, n_wide, start, prev = 1, 0, ys[0], ys[0]
    for y in ys[1:]:
        if y - prev > gap:
            n_wide += (prev - start) > wide
            n, start = n + 1, y
        prev = y
    n_wide += (prev - start) > wide
    return n, n_wide


def estimate_skew(words: list[dict]) -> float:
    """Slope s (pt per pt) with which a printed line drops from left to right.

    The scans are slightly rotated: within one record the right-hand tokens sit
    up to ~6 pt lower than the AGS. The slope that packs the words into the
    fewest lines is taken (ties: the smallest |s|). A wrong slope can also pack
    two neighbouring rows (10 pt apart) into one cluster; such a cluster spans
    far more than a printed line (1-3 pt), so every cluster wider than 5 pt
    counts as two extra lines (code review 2026-11-13).
    """
    best = (10**9, 0.0)
    for k in range(-30, 31):
        s = k / 1000
        n, n_wide = _clusters([w["top"] - s * w["x0"] for w in words], 2.5)
        score = n + 2 * n_wide
        if (score, abs(s)) < (best[0], abs(best[1])):
            best = (score, s)
    return best[1]


def group_lines(words: list[dict], skew: float = 0.0, gap: float = 3.5) -> list[list[dict]]:
    for w in words:
        w["y"] = w["top"] - skew * w["x0"]
    words = sorted(words, key=lambda w: w["y"])
    lines: list[list[dict]] = []
    for w in words:
        if lines and w["y"] - lines[-1][-1]["y"] <= gap:
            lines[-1].append(w)
        else:
            lines.append([w])
    return [sorted(l, key=lambda w: w["x0"]) for l in lines]


def squash(text: str) -> str:
    return re.sub(r"[^a-zäöüß0-9]", "", text.lower())


def page_section(lines: list[list[dict]], prev: str | None) -> str | None:
    head = squash(" ".join(w["text"] for l in lines if l[0]["top"] < 125 for w in l))
    if "alphabetisch" in head:
        return "index"
    if "ergebnissederwahlen" in head:
        return "results"
    if "kreisübersicht" in head or "grafik" in head:
        return "overview"
    if "verbandsgemeindebürgermeister" in head and "verzeichnis" in head:
        return "vg_bm"
    if "hauptamtlich" in head and "verzeichnis" in head:
        return "bm_ha"
    if "nichtmehrimamt" in head or "aufgelöst" in head or "späteraufgel" in head:
        return "bm_former"
    if "nachdem1januar2004" in head or "seitdem1januar2009" in head:
        return "bm_after"
    if "verzeichnisdergewähltenlandr" in head:
        return "landrat"
    if "verzeichnisdergewähltenbürgermeister" in head or "verzeichnisdergewähltenbürger" in head:
        return "bm"
    if "inhaltsverzeichnis" in head or "vorbemerkung" in head:
        return "front"
    return prev


# ---------------------------------------------------------------- register

REGISTER_SECTIONS = {"landrat", "bm", "bm_ha", "bm_after", "bm_former", "vg_bm"}
FOOTNOTE_START = re.compile(r"^(\d\)|[¹²³*]+\)?|\*)$")
FOOTNOTE_WORDS = re.compile(
    r"Gemeinde bestand|Neubildung|[Nn]icht mehr im Amt|noch nicht im Amt|Amtsantritt|zurückgetreten"
    r"|wurde ein neuer|Wahl am|verstorben|Zeitpunkt der Erstellung|eingesetzte")
MARKER_TOKENS = {"”", "»", "“", "*", "?", "1)", "2)", "3)", "*)", "\"", "„", "°"}
ACTING_RE = re.compile(r"\(\s*amt\.?\s*Bürgermeister(in)?\s*\)", re.I)


def header_anchors(lines: list[list[dict]]) -> dict[str, float]:
    """x positions of the column headers 'Name', 'Partei', 'Anz.' (when legible)."""
    a: dict[str, float] = {}
    for l in lines:
        if l[0]["top"] > 200:
            break
        for w in l:
            t = w["text"].strip("|")
            if t == "Name" and "name" not in a:
                a["name"] = w["x0"]
            elif t == "Partei" and "partei" not in a:
                a["partei"] = w["x0"]
            elif t.startswith("Anz") and "anz" not in a:
                a["anz"] = w["x0"]
    return a


def record_start(line: list[dict], section: str) -> tuple[str | None, int]:
    """(code, number of leading tokens it used) if the line opens a record."""
    w0 = line[0]
    if w0["x0"] > 135:
        return None, 0
    if section == "vg_bm":
        k = norm_code(w0["text"], 5)
        if k and len(line) > 1 and re.fullmatch(r"\d{2}", line[1]["text"].strip("_")):
            return f"{k}-{line[1]['text'].strip('_')}", 2
        return None, 0
    n = 5 if section == "landrat" else 8
    c = norm_code(w0["text"], n)
    return (c, 1) if c else (None, 0)


def parse_register(pdf_path: Path, stand: str) -> tuple[list[dict], dict]:
    recs: list[dict] = []
    diag = {"pages": Counter(), "skew": [], "name_x_from_header": 0}
    section = None
    pages = []
    with pdfplumber.open(pdf_path) as doc:
        for pno, page in enumerate(doc.pages, start=1):
            words = page.extract_words(x_tolerance=1.5, keep_blank_chars=False)
            if not words:
                continue
            skew = estimate_skew(words)
            lines = group_lines(words, skew)
            section = page_section(lines, section)
            diag["pages"][section] += 1
            if section in REGISTER_SECTIONS:
                diag["skew"].append(skew)
                pages.append((pno, section, lines, header_anchors(lines)))

    # issue-level offsets of the 'Partei'/'Anz.' headers from the name column,
    # used on pages whose headers the OCR garbled
    offs: dict[str, list[float]] = defaultdict(list)
    page_name_x = {}
    for pno, section, lines, anc in pages:
        firsts = []
        for l in lines:
            code, _ = record_start(l, section)
            if code or l[0]["top"] < 95:
                continue
            for w in l:
                if w["x0"] > 150 and not parse_dates(w["text"]) and re.search(r"[A-Za-zÄÖÜäöü0-9]", w["text"]):
                    firsts.append(round(w["x0"]))
                    break
        nx = Counter(firsts).most_common(1)[0][0] if firsts else None
        if nx is None and "name" in anc:
            nx = anc["name"] - 20
            diag["name_x_from_header"] += 1
        page_name_x[pno] = nx
        if nx is not None:
            for k in ("partei", "anz"):
                if k in anc:
                    offs[k].append(anc[k] - nx)
    med = {k: sorted(v)[len(v) // 2] for k, v in offs.items()}

    for pno, section, lines, anc in pages:
        nx = page_name_x[pno]
        if nx is None:
            continue
        name_x0 = nx - 8
        party_x0 = (anc["partei"] if "partei" in anc else nx + med["partei"]) - 32
        anz_x0 = (anc["anz"] if "anz" in anc else nx + med["anz"]) - 9
        cur = None
        kreis = None
        footnotes: list[str] = []
        in_footnote = False
        page_recs = []
        for l in lines:
            if l[0]["top"] < 95:  # running head and page number
                continue
            txt = " ".join(w["text"] for w in l)
            code, used = record_start(l, section)
            if code:
                in_footnote = False
            elif section != "landrat" and section != "vg_bm":
                k = next((norm_code(w["text"], 5) for w in l[:3] if norm_code(w["text"], 5)), None)
                if k and (l[0]["text"].startswith("Noch") or norm_code(l[0]["text"], 5)):
                    kreis = k
                    continue
            if not code and (in_footnote or FOOTNOTE_START.match(l[0]["text"]) or
                             (FOOTNOTE_WORDS.search(txt) and l[0]["x0"] < name_x0 - 60)):
                in_footnote = True
                footnotes.append(txt)
                continue
            if code:
                cur = {
                    "stand": stand, "source_file": pdf_path.name, "page": pno, "section": section,
                    "code": code, "kreis_heading": kreis, "unit_name": [], "office_flag": "",
                    "name_raw": [], "party_raw": [], "anz_raw": [], "profession": [],
                    "dates": [], "birth_year_raw": "", "extra": [], "marker": False,
                }
                recs.append(cur)
                page_recs.append(cur)
                for w in l[used:]:
                    x, t = w["x0"], w["text"]
                    if t in MARKER_TOKENS or re.fullmatch(r"\d\)", t):
                        cur["marker"] = True
                        continue
                    if parse_dates(t) and x > nx + 120:
                        cur["dates"].extend(parse_dates(t))
                    elif x >= anz_x0:
                        cur["anz_raw"].append(t)
                    elif x >= party_x0:
                        cur["party_raw"].append(t)
                    elif x >= name_x0:
                        cur["name_raw"].append(t)
                    elif FLAG_RE.match(t) and x >= nx - 70:
                        cur["office_flag"] = t.strip("|").replace("0", "O").replace("8", "B")
                    else:
                        cur["unit_name"].append(t)
                continue
            if cur is None:
                continue
            if re.search(r"Name des|Beruf/T|Geburtsjahr|Wahltag", txt) and not parse_dates(txt):
                continue  # repeated column header
            ds = parse_dates(txt)
            left = [w for w in l if w["x0"] < name_x0 - 4]
            mid = [w for w in l if name_x0 - 4 <= w["x0"] and not (parse_dates(w["text"]) and w["x0"] > nx + 120)
                   and w["x0"] < anz_x0 - 30]
            for w in l:
                if w["text"] in MARKER_TOKENS:
                    cur["marker"] = True
            left_txt = " ".join(w["text"] for w in left if w["text"] not in MARKER_TOKENS)
            if left_txt and re.search(r"[A-Za-zÄÖÜäöü]{3,}", left_txt):
                cur["unit_name"].append(left_txt)
            mid_txt = " ".join(w["text"] for w in mid if w["text"] not in MARKER_TOKENS).strip()
            yr = re.fullmatch(r"[1lI][9g][0-9OolIS]{2}", mid_txt.replace(" ", ""))
            if yr:
                cur["birth_year_raw"] = mid_txt.replace(" ", "")
            elif mid_txt:
                cur["profession"].append(mid_txt)
            if ds:
                cur["dates"].extend(ds)
            right_nodate = [w for w in l if w["x0"] >= anz_x0 - 30 and not parse_dates(w["text"])]
            if right_nodate:
                cur["extra"].append(" ".join(w["text"] for w in right_nodate))
        fn = " / ".join(footnotes)
        for r in page_recs:
            r["page_footnotes"] = fn
    diag["skew_max"] = max((abs(s) for s in diag["skew"]), default=0)
    return recs, diag


def clean_name_ocr(s: str) -> str:
    """Undo the OCR artifacts seen in the name column.

    The table rule left of the name column is read as a leading capital or as
    noise ('MMuster', 'KBeispiel', '_ Muster', '© ...'); 'Dr' loses its dot.
    Spelling slips inside a name ('MusterrnannI' for 'Mustermann') are left as printed and
    caught by the comparison with GERDA.
    """
    s = re.sub(r"^[^A-Za-zÄÖÜäöü]+", "", s)
    s = re.sub(r"^Dr\s+", "Dr. ", s)
    s = re.sub(r"^(Dr\.\s+)?[MKIl|]([A-ZÄÖÜ][a-zäöüß])", r"\1\2", s)
    s = re.sub(r"^(Dr\.\s+)?([A-ZÄÖÜ])[A-Z]([a-zäöüß]{2,})", r"\1\2\3", s)  # 'MBAeispiel' (invented)
    return s.strip(" _")


def finish_register(r: dict) -> dict:
    name_raw = re.sub(r"\s+", " ", " ".join(r["name_raw"])).strip()
    acting = bool(ACTING_RE.search(name_raw))
    name_clean = clean_name_ocr(ACTING_RE.sub("", name_raw).strip(" ,;"))
    name_clean = re.sub(r"\s+[^\w\s,.()-]+$", "", name_clean)  # trailing footnote glyphs
    title, last, first = split_person(name_clean)
    by = r["birth_year_raw"].translate(OCR_DIGIT) if r["birth_year_raw"] else ""
    anz = " ".join(r["anz_raw"]).strip(" |")
    n_cand = anz if re.fullmatch(r"\d{1,2}", anz) else ""
    prof = " ".join(r["profession"])
    prof = re.sub(r"(\w)- (\w)", r"\1\2", prof)
    prof = re.sub(r"\s+", " ", prof).strip(" ,;|")
    party = re.sub(r"\s+", " ", " ".join(r["party_raw"])).strip(" |;:,.")
    ds = r["dates"]
    unit = re.sub(r"\s+", " ", " ".join(r["unit_name"])).strip(" |;:.-_’'")
    unit = re.sub(r"\s+,", ",", unit).lstrip("_ ")
    unit = re.sub(r"^[^A-Za-zÄÖÜ]+", "", unit)           # '© Ditfurt', '— Retzau'
    unit = re.sub(r"\s+(Landkreis|Kreisangehörige Gemeinden)\b.*$", "", unit)  # heading text caught below
    unit = re.sub(r"\s*\|.*$|\s+Gemeinde$", "", unit)  # column header caught with the first record
    code = r["code"]
    if "-" in code:      # Verbandsgemeinde: Kreis + two-digit number
        ags, vg = "", code
    else:
        ags, vg = (code + "000" if len(code) == 5 else code), ""
    return {
        "stand": r["stand"],
        "section": r["section"],
        "office": "Landrat" if r["section"] == "landrat" else
                  ("Verbandsgemeindebürgermeister" if r["section"] == "vg_bm" else "Bürgermeister"),
        "ags": ags,
        "vg_key": vg,
        "kreis_heading": r["kreis_heading"] or "",
        "unit_name": unit,
        "office_flag": r["office_flag"],
        "acting": acting,
        "name_raw": name_raw,
        "title": title,
        "last_name": last,
        "first_name": first,
        "party": party,
        "n_candidates": n_cand,
        "n_candidates_raw": anz,
        "profession": prof,
        "election_date": ds[0] if ds else "",
        "runoff_date": ds[1] if len(ds) > 1 else "",
        "n_dates": len(ds),
        "birth_year": by if re.fullmatch(r"19\d\d", by) else "",
        "birth_year_raw": r["birth_year_raw"],
        "footnote_marker": r["marker"],
        "page_footnotes": r.get("page_footnotes", "") if r["marker"] or acting else "",
        "extra": " | ".join(r["extra"]),
        "source_file": r["source_file"],
        "page": r["page"],
    }


# ---------------------------------------------------------------- results annex

PCT_TOKEN = re.compile(r"^\d{1,3},\d$")


def num_tokens(s: str) -> list[str]:
    """Digit groups and percentages of an OCR line; noise glyphs dropped."""
    out = []
    for t in s.split():
        t = t.strip(";:|'‘’\"“”")
        if re.fullmatch(r"\d{1,3}\.\d{3}", t):  # '164.420'
            t = t.replace(".", "")
        t = t.rstrip(".")
        if re.fullmatch(r"\d+", t) or PCT_TOKEN.fullmatch(t):
            out.append(t)
    return out


def read_count(toks: list[str], i: int, limit: int | None) -> tuple[int | None, int]:
    """One count from toks[i]: thousands groups separated by spaces are joined
    while they are three digits and the value stays within `limit`."""
    if i >= len(toks) or "," in toks[i]:
        return None, i
    v = toks[i]
    i += 1
    while (i < len(toks) and re.fullmatch(r"\d{3}", toks[i]) and len(v) < 7
           and (limit is None or int(v + toks[i]) <= limit)):
        v += toks[i]
        i += 1
    return int(v), i


def read_pct(toks: list[str], i: int, count: int, base: int | None) -> tuple[str | None, int]:
    """The percentage after a count: '23,4', or one whose comma the OCR lost
    ('11' for '1,1') when it agrees with count/base to within 0.15 points."""
    if i < len(toks):
        t = toks[i]
        if PCT_TOKEN.fullmatch(t):
            return t.replace(",", "."), i + 1
        if base and re.fullmatch(r"\d{2,3}", t) and abs(int(t) / 10 - 100 * count / base) <= 0.15:
            return f"{int(t) / 10:.1f}", i + 1
    return None, i


def read_rounds(s: str, limits: list, bases: list) -> list[tuple[int, str | None]]:
    toks = num_tokens(s)
    out: list[tuple[int, str | None]] = []
    i = 0
    for j in range(len(limits)):
        while i < len(toks) and "," in toks[i]:
            i += 1
        c, i2 = read_count(toks, i, limits[j])
        if c is None:
            break
        p, i = read_pct(toks, i2, c, bases[j])
        out.append((c, p))
    return out


def split_two(s: str, nr: int) -> list[int]:
    """The Wahlberechtigte line: one or two counts and no percentages. A 1-2
    digit token not followed by a 3-digit group is OCR noise; two counts must
    lie within 2 % of each other."""
    toks = [t for t in num_tokens(s) if "," not in t]
    toks = [t for k, t in enumerate(toks)
            if len(t) >= 3 or (k + 1 < len(toks) and len(toks[k + 1]) == 3)]
    best = None
    for k in (range(1, len(toks)) if nr == 2 else []):
        a, b = toks[:k], toks[k:]
        if all(len(x) == 3 for x in a[1:]) and all(len(x) == 3 for x in b[1:]):
            va, vb = int("".join(a)), int("".join(b))
            if va and abs(va - vb) / va < 0.02:
                best = [va, vb]
    if best:
        return best
    if nr == 2:
        return []  # no plausible split: leave both electorates unknown (checked below)
    v = "".join(toks)
    return [int(v)] if toks and all(len(x) == 3 for x in toks[1:]) and len(v) <= 8 else []


CAND_RE = re.compile(r"^(?P<who>[^\d]*?[A-Za-zÄÖÜäöüß.)\]!])[\s;:,'‘’©]+(?P<rest>\d.*)$")


def parse_results(pdf_path: Path, stand: str) -> tuple[list[dict], list[dict]]:
    with pdfplumber.open(pdf_path) as doc:
        texts = [(i + 1, p.extract_text() or "") for i, p in enumerate(doc.pages)]
    blocks: list[dict] = []
    cur = None
    head_re = re.compile(r"^(15\d{3})\s+(Oberbürgermeisterwahl|Landratswahl)\s+am\s+([\d.,]+)"
                         r"(?:\s+Stichwahl\s+am\s+([\d.,]+))?")
    for pno, text in texts:
        if "Ergebnisse der Wahlen" not in text:
            continue
        for line in text.splitlines():
            s = line.strip()
            m = head_re.match(s)
            if m:
                cur = {"stand": stand, "source_file": pdf_path.name, "page": pno,
                       "kreis": m.group(1), "election_type": m.group(2),
                       "hw_date": (parse_dates(m.group(3)) or [""])[0],
                       "sw_date": (parse_dates(m.group(4)) or [""])[0] if m.group(4) else "",
                       "lines": []}
                blocks.append(cur)
                continue
            if cur is not None:
                cur["lines"].append(s)
    elections, candidates, problems = [], [], []
    for b in blocks:
        two = bool(b["sw_date"])
        nr = 2 if two else 1
        e = {k: b[k] for k in ("stand", "source_file", "page", "kreis", "election_type", "hw_date", "sw_date")}
        for k in ("eligible", "voters", "invalid", "valid"):
            e[f"{k}_hw"] = e[f"{k}_sw"] = ""
        e.update({"winner_line": "", "unit": "", "repairs": ""})
        in_cands = False
        unit_bits: list[str] = []
        cands: list[dict] = []
        for s in b["lines"]:
            if s.startswith("Abkürzungen"):
                break
            if s.startswith("Noch:"):
                continue
            if "Wahlberechtigte" in s:
                i = s.find("Wahlberechtigte")
                if i > 0:
                    unit_bits.append(s[:i].strip())
                v = split_two(s[i + len("Wahlberechtigte"):], nr)
                e["eligible_hw"] = v[0] if v else ""
                e["eligible_sw"] = v[1] if len(v) > 1 else ""
                continue
            lab = next((l for l in ("Wähler/Wahlbeteiligung", "Ungültige Stimmen", "Gültige Stimmen") if s.startswith(l)), None)
            if lab:
                key = {"Wähler/Wahlbeteiligung": "voters", "Ungültige Stimmen": "invalid", "Gültige Stimmen": "valid"}[lab]
                if key == "voters":
                    lim = [e["eligible_hw"] or None, e["eligible_sw"] or e["eligible_hw"] or None]
                else:
                    lim = [e["voters_hw"] or None, e["voters_sw"] or None]
                v = read_rounds(s[len(lab):], lim[:nr], lim[:nr])
                e[f"{key}_hw"] = v[0][0] if v else ""
                e[f"{key}_sw"] = v[1][0] if len(v) > 1 else ""
                e[f"_pct_{key}_hw"] = v[0][1] if v else None
                e[f"_pct_{key}_sw"] = v[1][1] if len(v) > 1 else None
                continue
            if s.startswith("davon ent"):
                in_cands = True
                continue
            if re.match(r"^(Landrat|Landrätin|Oberbürgermeister|Oberbürgermeisterin)\s*[:;]", s):
                e["winner_line"] = re.split(r"[:;]", s, maxsplit=1)[1].strip()
                in_cands = False
                continue
            if in_cands:
                m = CAND_RE.match(s)
                if not m:
                    continue
                who, rest = m.group("who").strip(), m.group("rest")
                party = ""
                pm = re.search(r"\(([^)]*)\)\s*$", who)
                if pm:
                    party = pm.group(1).strip()
                    who = who[: pm.start()].strip()
                lims = [e["valid_hw"] or None, e["valid_sw"] or None][:nr]
                nums = read_rounds(rest, lims, lims)
                sw = nums[1] if two and len(nums) > 1 and nums[1][1] is not None else None
                t_, l_, f_ = split_person(who)
                cands.append({
                    "stand": b["stand"], "kreis": b["kreis"], "election_type": b["election_type"],
                    "hw_date": b["hw_date"], "sw_date": b["sw_date"], "name_raw": who,
                    "title": t_, "last_name": l_, "first_name": f_, "party": party,
                    "votes_hw": nums[0][0] if nums else "", "pct_hw": nums[0][1] if nums else "",
                    "votes_sw": sw[0] if sw else "", "pct_sw": sw[1] if sw else "",
                    "line_raw": s,
                })
            elif not e["eligible_hw"] and s and not s.startswith("Anzahl"):
                unit_bits.append(re.sub(r"\s*Anzahl.*$", "", s))
        e["unit"] = " ".join(x for x in unit_bits if x)
        # identity voters = invalid + valid; the invalid count is the one the
        # OCR breaks (a lost comma joins it with the percentage)
        rep = []
        for rnd in ("hw", "sw")[:nr]:
            vo, inv, va = e[f"voters_{rnd}"], e[f"invalid_{rnd}"], e[f"valid_{rnd}"]
            if vo != "" and va != "" and (inv == "" or vo != inv + va):
                e[f"invalid_{rnd}"] = vo - va
                rep.append(f"invalid_{rnd} {inv}->{vo - va}")
            s_ = sum(c[f"votes_{rnd}"] for c in cands if c[f"votes_{rnd}"] != "")
            e[f"check_sum_{rnd}"] = s_ - va if va != "" else ""
        e["repairs"] = "; ".join(rep)
        # Every printed percentage must agree with its count and base, every
        # electorate must be read, and the candidate votes must add up (a
        # failed thousands-group split would otherwise pass unseen).
        tag = f"{b['source_file']} p{b['page']} {b['kreis']} {b['hw_date']}"
        for rnd in ("hw", "sw")[:nr]:
            el_, vo = e[f"eligible_{rnd}"], e[f"voters_{rnd}"]
            if el_ == "" or vo == "" or e[f"valid_{rnd}"] == "":
                problems.append(f"{tag} {rnd}: electorate, voters or valid votes not read")
                continue
            if e[f"check_sum_{rnd}"] != 0:
                problems.append(f"{tag} {rnd}: candidate votes - valid = {e[f'check_sum_{rnd}']}")
            for key, num, base in (("voters", vo, el_), ("invalid", e[f"invalid_{rnd}"], vo),
                                   ("valid", e[f"valid_{rnd}"], vo)):
                pct = e.get(f"_pct_{key}_{rnd}")
                if pct is not None and base and abs(100 * num / base - float(pct)) > 0.15:
                    problems.append(f"{tag} {rnd}: {key} {num}/{base} vs printed {pct} %")
            for c in cands:
                if c[f"votes_{rnd}"] != "" and c[f"pct_{rnd}"] and \
                        abs(100 * c[f"votes_{rnd}"] / e[f"valid_{rnd}"] - float(c[f"pct_{rnd}"])) > 0.15:
                    problems.append(f"{tag} {rnd}: {c['last_name']} {c[f'votes_{rnd}']} vs printed {c[f'pct_{rnd}']} %")
        for k in [k for k in e if k.startswith("_pct_")]:
            del e[k]
        for c in cands:
            c.update({k: e[k] for k in e if k not in c})
        candidates.extend(cands)
        elections.append(e)
    if problems:
        raise SystemExit("results annex failed its checks:\n  " + "\n  ".join(problems))
    return elections, candidates


# ---------------------------------------------------------------- main

def main() -> None:
    all_recs, all_el, all_ca = [], [], []
    for fname, stand in ISSUES:
        p = RAW / fname
        if not p.exists():
            sys.exit(f"missing source file {p}")
        recs, diag = parse_register(p, stand)
        rows = [finish_register(r) for r in recs]
        all_recs.extend(rows)
        cnt = Counter(r["section"] for r in rows)
        print(f"{fname}: pages by section {dict(diag['pages'])}; records {dict(cnt)}; max skew {diag['skew_max']:.3f}; "
              f"name column from header on {diag['name_x_from_header']} pages")
        if "Ergebnisse der Wahlen der Oberbürgermeister" in "".join(
                (pg.extract_text() or "") for pg in pdfplumber.open(p).pages[-12:]):
            el, ca = parse_results(p, stand)
            all_el.extend(el)
            all_ca.extend(ca)
            print(f"   results annex: {len(el)} elections, {len(ca)} candidate lines")

    reg_cols = list(all_recs[0].keys())
    with OUT_REGISTER.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=reg_cols)
        w.writeheader()
        w.writerows(all_recs)
    print(f"wrote {OUT_REGISTER.name}: {len(all_recs)} rows")

    # results: one long table, one row per candidate, election columns repeated
    res_cols = ["stand", "kreis", "unit", "election_type", "hw_date", "sw_date",
                "eligible_hw", "voters_hw", "invalid_hw", "valid_hw",
                "eligible_sw", "voters_sw", "invalid_sw", "valid_sw", "winner_line",
                "check_sum_hw", "check_sum_sw", "repairs",
                "name_raw", "title", "last_name", "first_name", "party",
                "votes_hw", "pct_hw", "votes_sw", "pct_sw", "line_raw", "source_file", "page"]
    with OUT_RESULTS.open("w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=res_cols)
        w.writeheader()
        for c in all_ca:
            w.writerow({k: c.get(k, "") for k in res_cols})
    print(f"wrote {OUT_RESULTS.name}: {len(all_ca)} rows")


if __name__ == "__main__":
    main()
