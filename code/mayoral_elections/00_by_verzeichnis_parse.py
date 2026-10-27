#!/usr/bin/env python3
"""Stage-0 parser for the Bayern Bürgermeisterverzeichnisse 1978-2014.

Sources: the seven printed registers of the Bayerisches Landesamt für Statistik, scanned
by its library in October 2026 (2014 added on 12.10.2026), in data/mayoral_elections/raw/bayern/lfstat_verzeichnisse/.
That folder's README covers provenance, the loose-leaf caveat and the Heft 502 problem.
Each register lists the Oberbürgermeister, the Landräte and every Erster Bürgermeister
in office at the binder's last Ergänzungslieferung.

Pipeline:
  1. 00_by_verzeichnis_ocr.py has already OCR'd every page (Tesseract, 300 DPI, word TSV).
  2. Each register page is cut into entries using its own column geometry, inferred
     from where the vote shares, election dates and birth dates sit on that page.
  3. Every entry is matched to its round in the raw Bayern source workbook
     ("Wahlen seit 1945") on (Kreis, Wahltag, winner's share, Gemeinde name). The match
     yields the source's own "Tag des ersten Amtsantritt", which is GERDA's Bayern
     person key (ags, amtsantritt) in 03_mayor_panel.R. Nothing is inferred about
     identity beyond what the source already records.
  4. Matched entries are collapsed to one row per person.
     - Name: from the most legible volume (printed 2008 > 2002 > 1996 > 1990 > 1984
       > typewritten 1978).
     - Birth year: must agree across volumes; disagreement is reported.
     - Profession: before first election where the volume prints it, else current.

Outputs (all in data/mayoral_elections/raw/bayern/):
  by_verzeichnis_persons.csv              PUBLIC: ags, amtsantritt, office, name parts,
                                          birth year, profession, salaried flag, sources
  by_verzeichnis_persons_restricted.csv   gitignored: + exact birth date, birthplace
  by_verzeichnis_entries_restricted.csv   gitignored: every parsed entry, with match status
Run:  python3 code/mayoral_elections/00_by_verzeichnis_ocr.py
      python3 code/mayoral_elections/00_by_verzeichnis_parse.py
"""

import argparse
import csv
import datetime as dt
import difflib
import os
import re
import statistics
import sys
import unicodedata
from collections import Counter, defaultdict

import openpyxl

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
RAW = os.path.join(ROOT, "data", "mayoral_elections", "raw", "bayern")
SRC = os.path.join(RAW, "lfstat_verzeichnisse")
CACHE = os.path.join(SRC, "_ocr_cache")
WORKBOOK = os.path.join(RAW, "20251114_Wahlen_seit_1945.xlsx")
OUT_PERSONS = os.path.join(RAW, "by_verzeichnis_persons.csv")
OUT_PERSONS_R = os.path.join(RAW, "by_verzeichnis_persons_restricted.csv")
OUT_ENTRIES_R = os.path.join(RAW, "by_verzeichnis_entries_restricted.csv")
OUT_ELECTIONS = os.path.join(RAW, "by_verzeichnis_elections.csv")
BY2026 = os.path.join(RAW, "by2026_parsed.csv")
OFFICE_TITLE_RE = re.compile(r"(Erster?\s+B\S*rgerm|Oberb\S*rgerm|^B\S*rgermeister|^Landrat|"
                             r"Landr\S*tin|1\.\s*B\S*rgerm)", re.I)

# volume -> (pdf stem, name order, birth precision, legibility rank: higher = better)
VOLUMES = {
    "1978": ("BY_Verzeichnis_1978_Heft375", "given_first", "date", 1),
    "1984": ("BY_Verzeichnis_1984_Heft406", "surname_first", "date", 2),
    "1990": ("BY_Verzeichnis_1990_Heft463", "surname_first", "year", 3),
    "1996": ("BY_Verzeichnis_1996_Heft502", "surname_first", "year", 4),
    "2002": ("BY_Verzeichnis_2002_B73516", "surname_first", "year", 5),
    "2008": ("BY_Verzeichnis_2008", "surname_first", "year", 6),
    # Statistische Berichte B VII 3-7: Oberbürgermeister and Landräte only, same layout.
    # 2008b is the post-election state (Stand 1.5.2008) that the 2008 binder overwrote.
    "2008b": ("B7370C_2008_OB_Landraete", "surname_first", "year", 5),
    "2014": ("B7370C_2014_OB_Landraete", "surname_first", "year", 7),
    # The full 2014 register (B73516 201451, Stand 1.5.2014), scanned by the LfStat
    # library and uploaded on 12.10.2026: same layout as 2002/2008. Ranked below EVERY
    # earlier register, so it names only the people none of them names and changes no
    # existing spelling: its OCR is a little noisier (a final "l" or "gl" lost or
    # doubled) and it drops the "sen."/"jun." that earlier volumes print.
    "2014r": ("BY_Verzeichnis_2014_B73516", "surname_first", "year", 0.5),
}
# 2014-register names corrected against the 2026 Mandatsträger (same Gemeinde, first
# Amtsantritt and birth year; checked 12.10.2026): an OCR "rn" read as "m"
# (Vilgertshofen), and a register entry printing the names swapped (Kirchensittenbach).
# Names the parser splits wrongly are pinned the same way (Tüßling prints only an
# initial; two double or particle surnames), and one entry is withheld: Pforzen 2014
# prints the 2008 mayor with the 2014 result, while the 2026 file, the workbook
# (Amtsantritt 1.5.2014) and the municipality name another person from 2014.
# The pins name register entries, whose redistribution the LfStat declined
# (16.10.2026), so they live in a gitignored file next to the scans:
# (ags, printed surname, printed given name) -> (surname, given name); empty = withheld.
NAME_PINS_FILE = os.path.join(SRC, "name_pins_restricted.csv")


def load_name_pins(kind):
    if not os.path.exists(NAME_PINS_FILE):
        sys.exit(f"{NAME_PINS_FILE} not found: it is gitignored and travels with the register scans")
    with open(NAME_PINS_FILE, encoding="utf-8", newline="") as f:
        return [r for r in csv.DictReader(f) if r["kind"] == kind]


NAME_PINS_2014 = {(r["ags"], r["last_raw"], r["first_raw"]): (r["last_fixed"], r["first_fixed"])
                  for r in load_name_pins("name_pin_2014")}
assert len(NAME_PINS_2014) == 6, f"{NAME_PINS_FILE}: expected 6 name pins"


def apply_reviewed_entry_corrections(entries):
    """Repair two missing OCR fields before matching, from checkpoint 417's scan review.

    Original: BY_Verzeichnis_2014_B73516.pdf, SHA256
    878f1f54d9c8c9c6e612b4cc7de0cd468aee4577e669d2689b3dc476e1949dda.
    Page, name, election, birth year and printed accession must all agree. These
    are entry corrections, not person-ID or service-calendar overrides. Pforzen's
    separate wrong-predecessor exclusion below remains in force.
    """
    pins = []
    for p in load_name_pins("entry_fix_2014"):     # gitignored, see NAME_PINS_FILE
        fixes = {}
        if p["schl_nr"]:
            fixes["schl_nr"] = p["schl_nr"]
        if p["share"]:
            fixes["share"] = float(p["share"])
        pins.append((int(p["pdf_page"]), p["name_raw"], p["kreis_code"], int(p["birth_year"]), fixes))
    assert len(pins) == 2, f"{NAME_PINS_FILE}: expected 2 entry corrections"
    for page, name, kreis, year, fixes in pins:
        hits = [r for r in entries if r["volume"] == "2014r"
                and r["pdf_page"] == page and r["name_raw"] == name]
        if len(hits) != 1:
            raise ValueError(f"Reviewed 2014 entry no longer unique: page {page}, {name}")
        r = hits[0]
        if not (r["kreis_code"] == kreis and r["birth_year"] == year
                and r["wahltag"] == dt.date(2014, 3, 16)
                and r["amtsantritt_printed"] == dt.date(2008, 5, 1)):
            raise ValueError(f"Reviewed 2014 entry anchor changed: {name}")
        for field, value in fixes.items():
            if r[field] not in (None, "", value):
                raise ValueError(f"Reviewed 2014 correction conflicts with {name}: {field}")
            r[field] = value


# Heft 502 is a contaminated binder: only pages printed for the 1996 edition
# (Einwohner am 31.12.1994) belong to it. Its 2002/2008 pages duplicate those
# volumes and are parsed under the edition they were printed for.
EDITION_BY_EW = {"31.12.1994": "1996", "31.12.2000": "2002", "31.12.2006": "2008"}

DATE_RE = re.compile(r"(?<![\d])(\d{1,2})\s?[.,]\s?(\d{1,2})\s?[.,]\s?((?:18|19|20)\d\d)(?![\d])")
SHARE_RE = re.compile(r"(?<![\d.,])(\d{1,3})\s?[,.]\s?(\d)(?![\d])")
YEAR_RE = re.compile(r"(?<![\d.,])((?:18|19)\d\d)(?![\d.,])")
STAR_RE = re.compile(r"^[\*•\+×]{1,2}$")
TITLES = {"dr.", "dr", "prof.", "prof", "dipl.-ing.", "ing.", "mag.", "dres."}
SUFFIXES = {"sen.", "jun.", "sen", "jun", "jr.", "sr."}
PARTICLES = {"von", "vom", "zu", "zum", "zur", "van", "de", "der", "den", "di", "del"}
NOISE_RE = re.compile(r"^[|!¦'’”\"`.,:;_\-—=~»«()\[\]{}\\/]+$")


# --------------------------------------------------------------------------- utils
def norm(s):
    s = (s or "").lower()
    for a, b in (("ä", "ae"), ("ö", "oe"), ("ü", "ue"), ("ß", "ss")):
        s = s.replace(a, b)
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    s = re.sub(r"\b(st|m|gkst|stadt|markt|krfr|lkr|landkreis)\b\.?", " ", s)
    return re.sub(r"[^a-z]", "", s)


def sim(a, b):
    """Name similarity; a register name that is the start of the workbook's longer
    official name ("München" / "München, Landeshauptstadt") counts as equal."""
    ra, rb = (a or "").strip(), (b or "").strip()
    a, b = norm(a), norm(b)
    if not a or not b:
        return 0.0
    if a == b:
        return 1.0
    short, long_ = (ra, rb) if len(ra) <= len(rb) else (rb, ra)
    if len(norm(short)) >= 4 and long_.lower().startswith(short.lower().rstrip(" ,")) and \
            long_[len(short.rstrip(" ,")):].lstrip().startswith(","):
        return 1.0
    return difflib.SequenceMatcher(None, a, b).ratio()


def mkdate(d, m, y):
    try:
        return dt.date(int(y), int(m), int(d))
    except ValueError:
        return None


# --------------------------------------------------------------------------- source
def load_source():
    """Every round of the raw Bayern workbook with the winner's share and Amtsantritt."""
    wb = openpyxl.load_workbook(WORKBOOK, read_only=True)
    ws = wb["20251114_bewerberRBZ1-7"]
    rows = []
    for i, r in enumerate(ws.iter_rows(values_only=True)):
        if i == 0 or r[0] is None or r[2] is None:
            continue
        valid, wvotes = r[9], r[12]
        share = None
        if isinstance(valid, (int, float)) and valid and isinstance(wvotes, (int, float)):
            share = 100.0 * wvotes / valid
        amt = r[10].date() if isinstance(r[10], dt.datetime) else None
        rows.append({
            "ags": "09" + str(int(r[0])).zfill(6),
            "gemeinde": r[1] or "",
            "date": r[2].date(),
            "wahlart": r[3] or "",
            "amtstitel": r[4] or "",
            "amtsantritt": amt,
            "share": share,
            "winner_wv": r[11] or "",
            # The workbook's first-Amtsantritt is GERDA's Bayern person key, but 151
            # rows carry a date long AFTER their own election (Hohenthann 2002/2008
            # read 2014-05-01, the next mayor's). Such a key cannot identify the
            # winner of that row, so it neither receives nor passes on a name.
            "amt_ok": amt is not None and (amt - r[2].date()).days <= 200,
        })
    by_date = defaultdict(list)
    for s in rows:
        by_date[s["date"]].append(s)
    return rows, by_date


# --------------------------------------------------------------------------- OCR
def load_lines(stem, page):
    path = os.path.join(CACHE, stem, f"p{page:04d}.tsv")
    if not os.path.exists(path):
        return None
    words = []
    with open(path, encoding="utf-8") as fh:
        for r in csv.DictReader(fh, delimiter="\t", quoting=csv.QUOTE_NONE):
            if r["level"] != "5" or not (r["text"] or "").strip():
                continue
            x, y = int(r["left"]), int(r["top"])
            t = r["text"].strip().replace("—", "-").replace("–", "-")
            if x < 200 or x > 2450 or NOISE_RE.match(t):
                continue
            words.append({"x": x, "y": y, "h": int(r["height"]), "t": t,
                          "k": (r["block_num"], r["par_num"], r["line_num"])})
    groups = defaultdict(list)
    for w in words:
        groups[w["k"]].append(w)
    lines = []
    for ws_ in groups.values():
        ws_.sort(key=lambda w: w["x"])
        lines.append({"y": min(w["y"] for w in ws_), "w": ws_,
                      "text": " ".join(w["t"] for w in ws_)})
    lines.sort(key=lambda l: l["y"])
    return lines


HEADER_RE = re.compile(r"(Postleitzahl|Stimmenanteil|Geburts(datum|jahr)|Wahlvorschlag|"
                       r"Einwohner am|Beruf vor|im Amt seit|^Gemeinde\b|Gde\.|Kreisangehörige Gemeinde)")
FOOTER_RE = re.compile(r"(Berufsm\S*iger|Berufsm\.\s*Bgrm|Oberb\S*rgerm\S*ister\.?$|^#\s*=|=\s*Stichwahl|"
                       r"^N[rR]\.?\s*\d{1,2}\s+\d{2}/\d{2})")
# Register sections are told apart by the page title. Front tables reuse the same
# words ("1. Oberbürgermeister ... und Landräte ... nach Wahlvorschlägen", "7. Frauen
# als erste Bürgermeisterinnen") and must not be read as register pages.
TABLE_TITLE_RE = re.compile(r"\bTabelle\b|Bürgermeisterinnen|nach\s+(Wahlvorschl|Regierungsbez|Größenkl|dem\s+Dienst)|"
                            r"Älteste|jüngste|Frauen\s+als|Anzahl\s+der|Jahr\s+des|"
                            r"Schaubild|Verwaltungsgemeinschaft|Inhalt", re.I)
SECTION_RE = [
    ("BM", re.compile(r"Erste\s+B\S*rgerm\S*ister\S*\s+(in\s+den|der)\s+kreisan", re.I)),
    ("OB", re.compile(r"Oberb\S*rgerm\S*ister\s+(in\s+den|der)\s+kreisfreien", re.I)),
    ("LR", re.compile(r"\b\d[.,']?\s*Landr\S*te\b|^\s*(noch:?\s*)?Landr\S*te\b", re.I)),
]
KREIS_RE = [
    re.compile(r"Landkreis\s+(\d{3})\s+([A-ZÄÖÜ][^\d]{2,40})"),
    re.compile(r"Landkreis\s+([A-ZÄÖÜ].{2,45}?)\s*\((\d{3})\)\s*$"),
]
REGBEZ = {"oberbayern": "1", "niederbayern": "2", "oberpfalz": "3", "oberfranken": "4",
          "mittelfranken": "5", "unterfranken": "6", "schwaben": "7"}


def page_geometry(lines):
    """Column edges inferred from the page's own shares, dates and birth years."""
    share_x, date_x, year_x = [], [], []
    for l in lines:
        for w in l["w"]:
            t = w["t"]
            if re.fullmatch(r"\d{1,3},\d", t):
                share_x.append(w["x"])
            elif DATE_RE.fullmatch(t.strip("(),")):
                date_x.append(w["x"])
            elif re.fullmatch(r"(18|19)\d\d,?", t):
                year_x.append(w["x"])
    if len(share_x) < 2:
        return None
    c3 = statistics.median(share_x)
    # birth tokens sit left of the Wahlvorschlag column but right of the Gemeinde column
    birth = [x for x in date_x + year_x if 700 < x < c3 - 250]
    if not birth:
        return None
    c2 = statistics.median(birth)
    return {"c2": c2, "c3": c3}


def content_edge(lines, g):
    """Left edge of the printed table: Schl.-Nr. column if present, else the Gemeinde
    column. Punch holes and binder shadows left of it OCR as stray letters."""
    schl = [w["x"] for l in lines for w in l["w"] if re.fullmatch(r"\d{3}", w["t"]) and w["x"] < g["c2"] - 300]
    einw = [l["w"][0]["x"] for l in lines
            if l["w"] and any(DATE_RE.fullmatch(w["t"]) and w["x"] >= g["c3"] - 60 for w in l["w"])
            and l["w"][0]["x"] < g["c2"] - 200]
    xs = []
    if len(schl) >= 3:
        xs.append(statistics.median(schl))
    if len(einw) >= 3:
        xs.append(statistics.median(einw))
    return (min(xs) - 70) if xs else 0


def column(w, g):
    if w["x"] >= g["c3"] - 60:
        return 3
    if w["x"] >= g["c2"] - 130:
        return 2
    return 1


def split_entries(lines, g, given_first):
    """Group the page's body lines into entries.

    An entry ends with its Wahltag; a new one starts at the next line that opens a
    Gemeinde (a Schl.-Nr. or a capitalised Gemeinde word in column 1, or a name in
    column 2 after a large vertical gap).
    """
    entries, cur = [], None
    prev_y = None
    gaps = [b["y"] - a["y"] for a, b in zip(lines, lines[1:]) if 15 < b["y"] - a["y"] < 200]
    small = statistics.median(gaps) if gaps else 45
    for l in lines:
        cols = defaultdict(list)
        for w in l["w"]:
            cols[column(w, g)].append(w)
        c1 = " ".join(w["t"] for w in cols[1])
        c3 = " ".join(w["t"] for w in cols[3])
        opens = bool(cols[1]) and re.match(r"^(\d{2,3}\s+)?[A-ZÄÖÜ]", c1) is not None
        if cols[1] and re.match(r"^\d{3}\b", c1) and not given_first:
            opens = True
        big_gap = prev_y is not None and (l["y"] - prev_y) > 1.7 * small
        start = cur is None or (cur["has_date"] and (opens or (big_gap and (cols[1] or cols[2]))))
        if start:
            cur = {"lines": [], "has_date": False, "kreis": l.get("kreis")}
            entries.append(cur)
        cur["lines"].append(cols)
        if DATE_RE.search(c3):
            cur["has_date"] = True
        prev_y = l["y"]
    return entries


def name_like(t):
    """A token that can be part of a printed name (drops OCR debris such as 'EEE', 'H', ':*')."""
    if t.lower() in TITLES or t.lower() in SUFFIXES or t.lower() in PARTICLES:
        return True
    letters = re.sub(r"[^A-Za-zÄÖÜäöüß]", "", t)
    if len(letters) < 2 or (letters.isupper() and len(letters) > 1):
        return False
    if t.lower() in ("vg", "vg|"):   # the VG marker in OCR case variants ("vG Muster")
        return False
    return re.fullmatch(r"[A-ZÄÖÜa-zäöüß][A-Za-zÄÖÜäöüß'.\-]*", t) is not None


# Words that are never a surname in these registers: office titles and the
# professions that a lost name line leaves in the name slot.
NOT_A_NAME = re.compile(r"^(Bürgermeister(in)?|Oberbürgermeister(in)?|Erste[r]?|Landr(at|ätin)|Rentner(in)?|"
                        r"Hausfrau|Pensionär|Dipl\b.*|Verw\b.*|Techn\b.*|Univ\.?)$|"
                        r"^.{6,}(meister|beamter|beamtin|angestellter|angestellte|unternehmer|arbeiter)$",
                        re.I)


def parse_name(tokens, given_first):
    toks = [t for t in tokens if not STAR_RE.match(t) and t not in ("VG", "VG|", "|")]
    toks = [t.rstrip(",;:") for t in toks if not re.fullmatch(r"\d{3}\|?", t)]
    # OCR reads a final "l" as "!" or "|" (a surname ending in "l" printed as "...!");
    # the clean-up below would otherwise cut the letter off
    toks = [re.sub(r"(?<=[a-zäöüß])[!|]$", "l", t) for t in toks]
    toks = [t if t.lower() in TITLES else re.sub(r"[^A-Za-zÄÖÜäöüß'.\-]", "", t).strip("'.-") +
            ("." if t.endswith(".") and t.lower().rstrip(".") in {s.rstrip(".") for s in SUFFIXES} else "")
            for t in toks]
    toks = [re.sub(r"^II(?=[a-zäöü])", "Il", t) for t in toks]
    # OCR lower-cases or doubles a damaged initial ("wörle", "wild", "wWilfert"):
    # repair it rather than drop the token, which would shift the given name into
    # the surname slot. Particles stay lower-case.
    toks = [t[1:] if len(t) > 2 and t[0].islower() and t[1].isupper() else t for t in toks]
    toks = [t[0].upper() + t[1:] if t and t[0].islower() and t.lower() not in PARTICLES
            and t.lower() not in SUFFIXES and t.lower() not in ("vg", "vg|") and len(t) >= 3 else t
            for t in toks]
    # typewriter "ß" read as "B" between lower-case letters ("SpieBl")
    toks = [re.sub(r"(?<=[a-zäöü])B(?=[a-zäöü]|$)", "ß", t) for t in toks]
    toks = [t for t in toks if t and name_like(t)]
    # a given name glued onto the surname and printed again ("FahrErwin Erwin", "Hahningo Ingo")
    if len(toks) >= 2:
        for k in range(1, len(toks)):
            g = toks[k].rstrip(".")
            if len(g) >= 3 and len(toks[0]) > len(g) + 2 and toks[0].lower().endswith(g.lower()):
                toks[0] = toks[0][:-len(g)]
                break
    title = [t for t in toks if t.lower() in TITLES]
    suffix = [t for t in toks if t.lower() in SUFFIXES]
    core = [t for t in toks if t.lower() not in TITLES and t.lower() not in SUFFIXES]
    if not core:
        return "", "", " ".join(title)
    if given_first:
        i = len(core) - 1
        while i > 0 and core[i - 1].lower() in PARTICLES:
            i -= 1
        last, first = core[i:], core[:i]
    else:
        j = 1
        while j < len(core) and core[j - 1].lower() in PARTICLES:
            j += 1
        # infix particle: "Surname van Surname Given" -> surname runs through it
        while j + 1 < len(core) and core[j].lower() in PARTICLES:
            j += 2
            while j < len(core) and core[j - 1].lower() in PARTICLES:
                j += 1
        last, first = core[:j], core[j:]
        # The registers print "Schnurbein Bernd von", "Ingelheim Albrecht Graf von":
        # trailing particles (and a rank right before them) belong to the surname.
        moved = []
        while first and (first[-1].lower() in PARTICLES or
                         (moved and first[-1].lower() in {"graf", "gräfin", "freiherr", "freifrau",
                                                          "frhr.", "fürst", "prinz"})):
            moved.insert(0, first.pop())
        last = moved + last
    first = first + suffix
    return " ".join(last), " ".join(first), " ".join(title)


def parse_entry(e, given_first):
    c1, c2, c3 = [], [], []
    for cols in e["lines"]:
        c1.append(cols[1]); c2.append(cols[2]); c3.append(cols[3])
    star = 0
    for ws_ in c1 + c2:
        for w in ws_:
            if STAR_RE.match(w["t"]):
                star = max(star, len(w["t"]))
    # column 1: Schl.-Nr., Gemeinde name lines (until the PLZ), VG marker
    HDR_TOK = re.compile(r"^(Schl|Schi|Sch1|VGe\w*|Nr|Gde|Gemeinde)\.?,?$")
    c1_text = [" ".join(w["t"] for w in ws_ if not HDR_TOK.match(w["t"])) for ws_ in c1]
    schl, gem, vg = "", [], ""
    for i, t in enumerate(c1_text):
        if not t:
            continue
        m = re.match(r"^(\d{3})\s+(.*)$", t) if i == 0 else None
        if m:
            schl, t = m.group(1), m.group(2)
        if re.search(r"\(?\d{4,5}\)?", t) and not re.search(r"[A-Za-zäöü]{3}", t.split("(")[0]):
            break
        mv = re.search(r"\s+(VG\|?|\d{3})$", t)
        if mv:
            vg, t = mv.group(1), t[:mv.start()]
        gem.append(t)
    gemeinde = re.sub(r"\s+", " ", " ".join(gem)).strip(" ,")
    gemeinde = re.split(r"\s\(?\d{4,5}\b", gemeinde)[0].strip(" ,")   # PLZ / Sitz des Landratsamts
    gemeinde = re.sub(r"(^|\s)\d{3}(?=\s|$)", " ", gemeinde).strip(" ,")
    if not schl and c1_text:
        m3 = re.search(r"(?<!\d)(\d{3})(?!\d)", c1_text[0])
        if m3:
            schl = m3.group(1)
            gemeinde = re.sub(r"^\W*\w{0,3}\W*" + schl + r"\s*", "", gemeinde).strip() or gemeinde
    # column 2: name, profession, birth
    c2_lines = [[w["t"] for w in ws_] for ws_ in c2 if ws_]
    name_toks = c2_lines[0] if c2_lines else []
    if name_toks and name_toks[0] in ("VG", "VG|"):
        name_toks = name_toks[1:]
        vg = "VG"
    last, first, title = parse_name(name_toks, given_first)
    beruf, birth_date, birth_year, birthplace = [], None, None, ""
    for i, toks in enumerate(c2_lines[1:], 1):
        t = " ".join(toks)
        m = DATE_RE.search(t)
        y = YEAR_RE.search(t)
        if birth_date is None and birth_year is None and m and 1870 <= int(m.group(3)) <= 2000:
            birth_date = mkdate(m.group(1), m.group(2), m.group(3))
            birth_year = int(m.group(3))
            rest = t[m.end():].strip(" ,")
            birthplace = rest
            continue
        if birth_date is None and birth_year is None and y and re.fullmatch(r"\s*(18|19)\d\d\s*,?", t):
            birth_year = int(y.group(1))
            continue
        if birth_year is not None:
            if birth_date is not None and t:     # 1978: "Almerswind/" + "Thür.", "Höchstadt" + "a.d.Aisch"
                sep = "" if birthplace.endswith(("-", "/")) or t[0].islower() and birthplace.endswith("-") else " "
                birthplace = (birthplace + sep + t).strip()
            continue
        beruf.append(t)
    # column 3: Wahlvorschlag, share, Wahltag, Stichwahl, Amtsantritt
    c3_text = [" ".join(w["t"] for w in ws_) for ws_ in c3 if ws_]
    wv, share, wahltag, amt, stich = [], None, None, None, False
    for t in c3_text:
        if re.search(r"Stichwahl|#", t):
            stich = True
        dates = list(DATE_RE.finditer(t))
        if share is None and not dates:
            m = SHARE_RE.search(t)
            if m and int(m.group(1)) <= 100 and not re.search(r"[A-Za-zäöüÄÖÜ]{4}", t[:m.start()]):
                share = float(f"{m.group(1)}.{m.group(2)}")
                continue
        if dates:
            for m in dates:
                d = mkdate(m.group(1), m.group(2), m.group(3))
                paren = t[max(0, m.start() - 1):m.start()] == "("
                if wahltag is None and not paren:
                    wahltag = d
                    if share is None:
                        ms = SHARE_RE.search(t[:m.start()])
                        if ms and int(ms.group(1)) <= 100:
                            share = float(f"{ms.group(1)}.{ms.group(2)}")
                elif amt is None:
                    amt = d
            continue
        if share is None:
            wv.append(t)
    wvs = ""
    for t in wv:
        wvs = wvs[:-1] + t if wvs.endswith("-") else (wvs + " " + t).strip()
    return {
        "schl_nr": schl, "gemeinde_raw": gemeinde, "vg": vg, "star": star,
        "name_raw": " ".join(t for t in name_toks if not STAR_RE.match(t)),
        "last_name": last, "first_name": first, "title": title,
        "beruf_raw": re.sub(r"^(VG\|?|\d{3})\s+", "", " ".join(beruf)).replace(":", ".").strip(" |,"),
        "birth_date": birth_date, "birth_year": birth_year,
        "birthplace": birthplace.strip(" ,|"), "wahlvorschlag_raw": wvs, "share": share,
        "wahltag": wahltag, "stichwahl": stich, "amtsantritt_printed": amt,
    }


def parse_volume(vol):
    stem, order, _, _ = VOLUMES[vol]
    given_first = order == "given_first"
    pages = sorted(int(f[1:5]) for f in os.listdir(os.path.join(CACHE, stem)) if f.endswith(".tsv"))
    section, kreis, out = None, None, []
    for p in pages:
        lines = load_lines(stem, p)
        if not lines:
            continue
        is_register = any(re.search(r"Wahltag", l["text"]) for l in lines[:14]) and \
            any(re.search(r"\bName\b|Beruf", l["text"]) for l in lines[:14])
        titles = [l["text"] for l in lines[:6]]
        if any(TABLE_TITLE_RE.search(x) for x in titles):
            section = None          # a front table ("Tabelle 6. Frauen als Bürgermeister"),
            continue                # even where its column header looks like the register's
        if not is_register:
            continue
        hit = None
        for x in titles:
            hit = next((sec for sec, rx in SECTION_RE if rx.search(x)), None)
            if hit:
                break
        if hit is None:
            # No legible title: the column header names the unit instead.
            hdr = " ".join(l["text"] for l in lines[:14])
            if re.search(r"Landratsam|Sitz\s+des\s+Landrat", hdr):
                hit = "LR"
            elif re.search(r"Kreisfreie\s+Stadt|kreisfreie\s+Stadt", hdr):
                hit = "OB"
            elif re.search(r"Gemeinde", hdr):
                hit = "BM"
        section = hit or section
        ew = re.search(r"Einwohner\s*a\S*\s*([0-9.]{8,10})", " ".join(l["text"] for l in lines[:14]))
        edition = vol
        if vol == "1996":
            edition = EDITION_BY_EW.get(ew.group(1).rstrip(".") if ew else "", "")
            if edition != "1996":
                continue      # 2002/2008 pages filed into the 1996 binder, or unknown
        g = page_geometry(lines)
        if g is None or section is None:
            continue
        edge = content_edge(lines, g)
        for l in lines:
            l["w"] = [w for w in l["w"] if w["x"] >= edge]
            l["text"] = " ".join(w["t"] for w in l["w"])
        lines = [l for l in lines if l["w"]]
        body = []
        header_seen = False
        for l in lines:
            if HEADER_RE.search(l["text"]):
                header_seen = True
                continue
            if not header_seen:
                continue
            if FOOTER_RE.search(l["text"]):
                continue
            # Kreis heading: "Landkreis 774 Günzburg" / "Landkreis Günzburg (774)", with
            # OCR variants of the word and trailing debris tolerated.
            if len(l["w"]) <= 8 and re.search(r"\b[LI1l]\w{2,4}kr\w{2,5}\b", l["text"]) and \
                    not any(DATE_RE.search(w["t"]) for w in l["w"]):
                kc = re.search(r"(?<!\d)(\d{3})(?!\d)", l["text"])
                if kc:
                    kreis = kc.group(1)
                    continue
            # The 2014 scan's OCR sometimes drops the word itself ("177 Erding"): a
            # short line of a valid Kreis code and a name, nothing else.
            # Only in the Gemeinde section, and only when the name is that Kreis's
            # own (the first line of an OB/Landrat entry, "163 Rosenheim Bauer
            # Gabriele CSU", and a Gemeinde line "190 Waigolshausen ..." look alike).
            if vol == "2014r" and section == "BM" and len(l["w"]) <= 5:
                kc = re.match(r"^(?:Noch:?\s+)?(\d{3})\s+([A-ZÄÖÜ][A-Za-zÄÖÜäöüß.\- ]+?)[^A-Za-zÄÖÜäöüß.]*$",
                              l["text"].strip())
                if kc and kc.group(1) in KREIS_CODES and \
                        sim(kc.group(2), KREIS_NAMES.get(kc.group(1), "")) >= 0.85:
                    kreis = kc.group(1)
                    continue
            low = l["text"].lower()
            if len(l["w"]) <= 4 and any(rb in low for rb in REGBEZ):
                continue        # Regierungsbezirk heading ("noch VI. Unterfranken")
            l["kreis"] = kreis
            body.append(l)
        # 1978 prints "First Last" in the Gemeinde register only; its OB and Landrat
        # pages, like every later volume, print "Last First".
        gf = given_first and section == "BM"
        for e in split_entries(body, g, gf):
            rec = parse_entry(e, gf)
            if rec["wahltag"] is None and not rec["last_name"]:
                continue
            rec.update({"volume": vol, "pdf": stem, "pdf_page": p, "section": section,
                        "kreis_code": (e["kreis"] or "") if section == "BM" else ""})
            out.append(rec)
    return out


# --------------------------------------------------------------------------- matching
def office_of(src):
    t = src["amtstitel"]
    if t.startswith("Landr"):
        return "LR"
    if t.startswith("Oberb"):
        return "OB"
    return "BM"


def match(rec, by_date):
    """Find the source round this entry reports. Returns (row, status, score).

    Candidates are all source rounds within 120 days of the printed Wahltag (OCR
    misreads dates, and the register occasionally dates a round differently from the
    workbook). Score: winner share (to 0.1), Gemeinde name, Kreis heading, Schl.-Nr.,
    date distance and office. A match needs a clear best score.
    """
    d = rec["wahltag"]
    if d is None:
        return None, "no_wahltag", 0
    cands = []
    for k in range(-120, 121):
        cands += by_date.get(d + dt.timedelta(days=k), [])
    sec = rec["section"]
    scored = []
    for s in cands:
        if s["wahlart"].endswith("ungültig"):
            continue
        off = office_of(s)
        kreis_ok = s["ags"][2:5] == rec["kreis_code"]
        if sec == "BM":
            schl_ok = bool(rec["schl_nr"]) and s["ags"][5:8] == rec["schl_nr"] and kreis_ok
        else:
            schl_ok = bool(rec["schl_nr"]) and s["ags"][2:5] == rec["schl_nr"]
        share_ok = rec["share"] is not None and s["share"] is not None and \
            abs(rec["share"] - round(s["share"], 1)) <= 0.15
        if not (kreis_ok or schl_ok or share_ok):
            continue
        sc = 0.0
        if rec["share"] is not None and s["share"] is not None:
            diff = abs(rec["share"] - round(s["share"], 1))
            sc += 3 if diff < 0.05 else (1 if diff <= 0.15 else -2)
        ns = sim(rec["gemeinde_raw"], s["gemeinde"])
        legible = len(norm(rec["gemeinde_raw"])) >= 4
        sc += 3 * ns if ns >= 0.6 else (-1 if legible else 0)
        sc += 1.5 if (sec == "BM" and kreis_ok) else 0
        # a round in ANOTHER Kreis than the entry's heading needs a near-exact name
        # (Stiefenhofen, Lkr Lindau, had matched Osterhofen, Lkr Deggendorf)
        if sec == "BM" and rec["kreis_code"] and not kreis_ok and ns < 0.9:
            sc -= 2
        sc += 2 if schl_ok else 0
        gap = abs((s["date"] - d).days)
        sc += 2 if gap == 0 else (1 if gap <= 14 else 0)
        if sec in ("OB", "LR") and off != sec:
            sc -= 3
        if sec == "BM" and off == "LR":
            sc -= 3
        scored.append((sc, s))
    if not scored:
        return None, "no_candidate", 0
    scored.sort(key=lambda z: -z[0])
    best = scored[0]
    if best[0] < 6 and sec in ("OB", "LR"):
        # ~96 units, one per Kreis code: code + name + date identify the round even
        # where the printed share differs (Landrat shares sometimes do).
        s = best[1]
        if (rec["schl_nr"] and s["ags"][2:5] == rec["schl_nr"] and office_of(s) == sec
                and sim(rec["gemeinde_raw"], s["gemeinde"]) >= 0.8 and abs((s["date"] - d).days) <= 14):
            return s, "matched_code_name_date", best[0]
    if best[0] < 6:
        return rescue(rec, best[0])
    # The runner-up must trail by more than the Kreis bonus: an exact share on the
    # same day in the entry's own Kreis beats the same share elsewhere in Bayern.
    if len(scored) > 1 and scored[1][0] > best[0] - 1.5 and scored[1][1]["ags"] != best[1]["ags"]:
        return None, "ambiguous", best[0]
    return best[1], "matched", best[0]



SRC_BY_AGS = defaultdict(list)
KREIS_CODES = set()   # filled in main() from the workbook
KREIS_NAMES = {}      # Kreis code -> name of its Landrat unit, from the workbook


def rescue(rec, score):
    """Second pass for an entry whose printed Wahltag is garbled: the same exact share
    (to 0.1) in a municipality the entry names unambiguously, within the volume's
    term window. Exact share + municipality is as specific as date + share."""
    if rec["share"] is None:
        return None, "low_score", score
    vy = int(rec["volume"][:4])
    pool = []
    for ags, rows in SRC_BY_AGS.items():
        if rec["section"] == "BM" and rec["kreis_code"] and ags[2:5] != rec["kreis_code"]:
            continue
        if rec["section"] in ("OB", "LR") and not ags.endswith("000"):
            continue
        nm = sim(rec["gemeinde_raw"], rows[0]["gemeinde"])
        if nm < 0.85:
            continue
        for s in rows:
            if s["share"] is None or not (vy - 13 <= s["date"].year <= vy + 6):
                continue
            if abs(rec["share"] - round(s["share"], 1)) < 0.05 and not s["wahlart"].endswith("ungültig"):
                pool.append(s)
    if len({s["ags"] for s in pool}) == 1 and len(pool) == 1:
        return pool[0], "matched_share_only", score
    return None, "low_score", score


# --------------------------------------------------------------------------- persons
def canonical(entries, field):
    """Spelling from the most legible volume (ties: most frequent)."""
    best = sorted(entries, key=lambda r: (-VOLUMES[r["volume"]][3], r["pdf_page"]))
    for r in best:
        if r.get(field):
            return r[field]
    return ""


def name_key(r):
    return norm(r["last_name"])


def build_persons(entries, src_rows):
    """One row per (ags, source Amtsantritt); report conflicts instead of guessing."""
    groups = defaultdict(list)
    for r in entries:
        if r["match_status"].startswith("matched") and r["source_amtsantritt"] and r["source_amt_ok"]:
            groups[(r["ags"], r["source_amtsantritt"])].append(r)
    persons, conflicts = [], []
    for (ags, amt), es in groups.items():
        # Name order: the 1978 Gemeinde register prints "First Last", but its pasted
        # update slips sometimes print "Last First". Where another entry for the same
        # key fixes the surname, an entry whose given name matches it is swapped.
        ref = [norm(r["last_name"]) for r in es if r["volume"] != "1978" and len(norm(r["last_name"])) >= 3
               and r["first_name"].strip()]
        if ref:
            anchor = Counter(ref).most_common(1)[0][0]
            for r in es:
                if r["volume"] != "1978":
                    continue
                fl = norm(r["first_name"].split(" ")[-1] if r["first_name"] else "")
                if (fl and difflib.SequenceMatcher(None, fl, anchor).ratio() >= 0.8 and
                        difflib.SequenceMatcher(None, norm(r["last_name"]), anchor).ratio() < 0.7):
                    r["last_name"], r["first_name"] = r["first_name"].split(" ")[-1], \
                        " ".join(r["first_name"].split(" ")[:-1] + [r["last_name"]]).strip()
        # Surnames must agree across volumes. Spellings are clustered (OCR noise
        # tolerated; a double name contains the single one); two clusters mean two
        # different people under one source key.
        clusters = []
        for r in sorted(es, key=lambda r: -VOLUMES[r["volume"]][3]):
            k = name_key(r)
            if len(k) < 3 or not r["first_name"].strip() or NOT_A_NAME.match(r["last_name"]):
                continue        # no legible name: cannot testify to who holds the key
            for c in clusters:
                if difflib.SequenceMatcher(None, k, c[0]).ratio() >= 0.7 or k in c[0] or c[0] in k:
                    c[1].append(r)
                    break
            else:
                clusters.append((k, [r]))
        # Two spellings are two people only if the birth years say so too; with one
        # birth year throughout, the minority spelling is OCR loss (e.g. a surname
        # that wrapped onto the next line in 1978).
        disagree = len(clusters) > 1 and len({r["birth_year"] for r in es if r["birth_year"]}) > 1
        if clusters:
            es_named = clusters[0][1] if not disagree else es
        else:
            es_named = es
        years = Counter(r["birth_year"] for r in es if r["birth_year"])
        byear, byear_conflict = None, False
        if years:
            # Published only with a strict majority across volumes: typewriter OCR
            # confuses 3/8, 0/9 and 5/6, and a father and son can share one key.
            top, n_top = years.most_common(1)[0]
            byear_conflict = len(years) > 1
            byear = top if n_top > sum(years.values()) - n_top else None
        if byear and amt.year - byear < 18:
            byear, byear_conflict = None, True   # e.g. Neuching 1978 "1955" for an office held since 1972
        # An exact date only where the year is settled and the date agrees with it
        # (one 1978/1984 pair, OCR "...1958" vs "...1938", leaves both out).
        dates = [r["birth_date"] for r in es if r["birth_date"] and byear and r["birth_date"].year == byear]
        bdate = Counter(dates).most_common(1)[0][0] if dates else None
        if bdate and len(set(dates)) > 1 and Counter(dates).most_common(2)[1][1] == Counter(dates)[bdate]:
            bdate = None        # tie between two different days of the same year
        prof = ""
        for r in sorted(es, key=lambda r: (r["volume"], -VOLUMES[r["volume"]][3])):
            b = r["beruf_raw"].strip(" ,.")
            if b and not OFFICE_TITLE_RE.search(b):
                prof = b
                break
        # A printed name always has a given name. Without one, the parsed "surname" is
        # a profession or a lone given name from a lost name line ("Zimmerermeister",
        # "Hans"): such a person keeps the birth data but is not named.
        named = [r for r in es_named if r["first_name"].strip() and r["last_name"].strip()
                 and not NOT_A_NAME.match(r["last_name"]) and not NOT_A_NAME.match(r["first_name"].split()[0])]
        es_named = named
        rec = {
            "ags": ags, "amtsantritt": amt, "office": canonical(es, "office"),
            "last_name": canonical(es_named, "last_name"), "first_name": canonical(es_named, "first_name"),
            "title": canonical(es_named, "title"), "birth_year": byear, "profession": prof,
            "birth_date": bdate, "birthplace": canonical([r for r in es if r["volume"] == "1978"], "birthplace"),
            "volumes": ";".join(sorted({r["volume"] for r in es})), "n_entries": len(es),
            "printed_start": min((r["amtsantritt_printed"] for r in es if r["amtsantritt_printed"]),
                                 default=None),
            "flag_name_conflict": bool(disagree), "flag_birth_year_conflict": byear_conflict,
            "birth_values_seen": ";".join(sorted({(r["birth_date"].isoformat() if r["birth_date"] else str(r["birth_year"]))
                                                  + "@" + r["volume"] for r in es if r["birth_year"]})),
            "gemeinde": es[0]["source_gemeinde"],
        }
        if disagree:
            conflicts.append((rec, sorted({r["last_name"] for r in es})))
        persons.append(rec)
    return persons, conflicts


def decisive(s):
    """Did this workbook round seat the mayor? A Stichwahl did; a Hauptwahl did unless
    a Stichwahl followed within 60 days, or it lacked a majority and a repeat
    Hauptwahl followed within 250 days (the flag_superseded rule of 01/01b)."""
    if s["wahlart"].endswith("ungültig"):
        return False
    if s["wahlart"] == "Stichwahl":
        return True
    later = [o for o in SRC_BY_AGS[s["ags"]] if o is not s and 0 < (o["date"] - s["date"]).days]
    if any(o["wahlart"] == "Stichwahl" and (o["date"] - s["date"]).days <= 60 for o in later):
        return False
    if s["share"] is not None and s["share"] < 50 and any(
            o["wahlart"] != "Stichwahl" and (o["date"] - s["date"]).days <= 250 for o in later):
        return False
    return True


def build_elections(persons, entries, src_rows):
    """Every source round whose winner is a register person (same ags + Amtsantritt)."""
    """Every source round whose winner the registers name.

    - A round reported by a register entry takes that person directly.
    - Any other round inherits the name through the source's own person key (ags,
      first Amtsantritt), but only where that key is not in conflict.
    """
    by_key = {(p["ags"], p["amtsantritt"]): p for p in persons}
    direct = defaultdict(list)
    for r in entries:
        if r["match_status"].startswith("matched") and len(name_key(r)) >= 3 and r["first_name"].strip() \
                and not NOT_A_NAME.match(r["last_name"]) and not NOT_A_NAME.match(r["first_name"].split()[0]):
            direct[(r["ags"], r["wahltag_source"])].append(r)
    out = []
    n_skip_printed = 0
    for s in src_rows:
        # Only rounds that seated someone: a Hauptwahl led by the later runoff loser
        # would otherwise name a losing candidacy in this public file.
        if not decisive(s):
            continue
        p = by_key.get((s["ags"], s["amtsantritt"])) if s["amt_ok"] else None
        if p is not None and not p["last_name"]:
            p = None          # register person with no legible name
        hits = direct.get((s["ags"], s["date"]), [])
        if p is not None and not p["flag_name_conflict"] and not hits and p["birth_year"] and \
                s["date"].year - p["birth_year"] < 18:
            p = None          # the key runs back to a predecessor; never name a minor
        if p is not None and not hits and p["printed_start"] and \
                (p["printed_start"] - s["date"]).days > 365:
            p = None          # more than a year before the register's own "im Amt seit"
            n_skip_printed += 1
        if p is not None and not p["flag_name_conflict"]:
            last, first, by, prof, vols = (p["last_name"], p["first_name"], p["birth_year"],
                                           p["profession"], p["volumes"])
            method = "register_entry" if hits else "register_person_key"
        elif hits:
            best = sorted(hits, key=lambda r: -VOLUMES[r["volume"]][3])[0]
            last, first, by, vols = best["last_name"], best["first_name"], best["birth_year"], best["volume"]
            prof = "" if OFFICE_TITLE_RE.search(best["beruf_raw"]) else best["beruf_raw"]
            method = "register_entry"
        else:
            continue
        out.append({"ags": s["ags"], "election_date": s["date"], "wahlart": s["wahlart"],
                    "office": office_of(s), "amtsantritt": s["amtsantritt"],
                    "candidate_name": f"{last}, {first}".strip(", "),
                    "candidate_last_name": last, "candidate_first_name": first,
                    "candidate_birth_year": by or "", "candidate_profession": prof,
                    "fill_method": method, "register_volumes": vols,
                    "workbook_winner_wv": s["winner_wv"],
                    "flag_key_conflict": bool(p is not None and p["flag_name_conflict"])})
    print(f"propagated names withheld for predating the printed 'im Amt seit': {n_skip_printed}")
    return out


def check_2026(persons):
    """Agreement with the 2026 Mandatsträger file for persons still in office in 2026."""
    if not os.path.exists(BY2026):
        return
    m26 = {}
    with open(BY2026, encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            if r["is_winner"] == "TRUE" and r["amtsantritt"] and r["candidate_name"]:
                m26[(r["ags"], r["amtsantritt"])] = r
    n = same_name = same_year = years = 0
    for p in persons:
        r = m26.get((p["ags"], p["amtsantritt"].isoformat()))
        if not r:
            continue
        n += 1
        last26 = r["candidate_name"].split(" ")[0]
        same_name += sim(last26, p["last_name"]) >= 0.8
        if r["candidate_birth_year"] and p["birth_year"]:
            years += 1
            same_year += int(r["candidate_birth_year"]) == p["birth_year"]
    print(f"2026 Mandatsträger overlap: {n} persons; surname agrees {same_name}/{n}; "
          f"birth year agrees {same_year}/{years}")


# --------------------------------------------------------------------------- main
def main():
    src_rows, by_date = load_source()
    for s in src_rows:
        SRC_BY_AGS[s["ags"]].append(s)
        KREIS_CODES.add(s["ags"][2:5])
        if s["ags"].endswith("000") and office_of(s) == "LR":
            KREIS_NAMES[s["ags"][2:5]] = re.sub(r"^(Landkreis|Lkr\.?)\s+", "", s["gemeinde"])
    print(f"source rounds: {len(src_rows)}")
    entries = []
    for vol in VOLUMES:
        recs = parse_volume(vol)
        print(f"{vol}: {len(recs)} entries parsed")
        entries += recs
    ocr_correct(entries)
    apply_reviewed_entry_corrections(entries)
    for r in entries:
        # an academic title printed after the given name ("Brigitte Dr.jur",
        # "Mario Dr.rer.soc") belongs in the title, not the given name
        toks = r["first_name"].split()
        tt = [t for t in toks if re.match(r"^(Dr|Prof)\.\S", t)]
        if tt and len(tt) < len(toks):
            r["first_name"] = " ".join(t for t in toks if t not in tt)
            r["title"] = " ".join(x for x in [r["title"]] + tt if x)
        # the 2014 scan's OCR reads a capital I + l as "Ii" ("Iise" = Ilse); no
        # given name starts with "Ii"
        if r["volume"] == "2014r":
            r["first_name"] = re.sub(r"\bIi(?=[a-zäöüß])", "Il", r["first_name"])
    status = Counter()
    for r in entries:
        s, st, sc = match(r, by_date)
        r["match_status"], r["match_score"] = st, round(sc, 2)
        r["ags"] = s["ags"] if s else ""
        r["source_gemeinde"] = s["gemeinde"] if s else ""
        r["source_amtsantritt"] = s["amtsantritt"] if s else None
        r["source_amt_ok"] = bool(s and s["amt_ok"])
        r["source_share"] = round(s["share"], 1) if s and s["share"] is not None else None
        r["office"] = office_of(s) if s else r["section"]
        r["wahltag_source"] = s["date"] if s else None
        status[(r["volume"], st)] += 1
    n_pin = 0
    for r in entries:
        k = (r["ags"], r["last_name"], r["first_name"])
        if r["volume"] == "2014r" and k in NAME_PINS_2014:
            r["last_name"], r["first_name"] = NAME_PINS_2014[k]
            n_pin += 1
    assert n_pin == len(NAME_PINS_2014), f"NAME_PINS_2014: {n_pin} of {len(NAME_PINS_2014)} applied"
    # A round reported by two entries of one volume: keep the better-scoring one
    # (Gutenstetten 1978 was claimed by its own entry and by Störnstein's).
    best = {}
    for r in entries:
        if r["match_status"].startswith("matched"):
            k = (r["volume"], r["ags"], r["wahltag_source"])
            if k not in best or r["match_score"] > best[k]["match_score"]:
                best[k] = r
    n_dup = 0
    for r in entries:
        if r["match_status"].startswith("matched") and best[(r["volume"], r["ags"], r["wahltag_source"])] is not r:
            r["match_status"], n_dup = "duplicate_round", n_dup + 1
            status[(r["volume"], "duplicate_round")] += 1
    print(f"  entries demoted for claiming a round another entry of the same volume claims: {n_dup}")
    for k in sorted(status):
        print("  ", k, status[k])
    write_entries(entries)
    persons, conflicts = build_persons(entries, src_rows)
    print(f"persons: {len(persons)}; surname conflicts: {len(conflicts)}; "
          f"birth-year conflicts: {sum(p['flag_birth_year_conflict'] for p in persons)}")
    for rec, names in conflicts[:15]:
        print("   conflict", rec["ags"], rec["gemeinde"], rec["amtsantritt"], names)
    check_2026(persons)
    elections = build_elections(persons, entries, src_rows)
    print(f"election rows filled: {len(elections)} "
          f"({Counter(e['fill_method'] for e in elections)})")
    # Public person table: a key that holds two people (flag_name_conflict) gets no
    # name or birth year here; 01b names those rounds only from their own entries.
    public = [dict(p, **({"last_name": "", "first_name": "", "title": "", "birth_year": None,
                          "profession": ""} if p["flag_name_conflict"] else {})) for p in persons]
    write_csv(OUT_PERSONS, public, ["ags", "amtsantritt", "office", "gemeinde", "last_name", "first_name",
                                     "title", "birth_year", "profession", "volumes", "n_entries",
                                     "flag_name_conflict", "flag_birth_year_conflict"])
    write_csv(OUT_PERSONS_R, persons, ["ags", "amtsantritt", "office", "gemeinde", "last_name", "first_name",
                                       "title", "birth_year", "birth_date", "birthplace", "profession",
                                       "volumes", "n_entries", "flag_name_conflict",
                                       "flag_birth_year_conflict", "birth_values_seen"])
    write_csv(OUT_ELECTIONS, elections, ["ags", "election_date", "wahlart", "office", "amtsantritt",
                                         "candidate_name", "candidate_last_name", "candidate_first_name",
                                         "candidate_birth_year", "candidate_profession", "fill_method",
                                         "register_volumes", "workbook_winner_wv", "flag_key_conflict"])
    return entries


def write_csv(path, rows, cols):
    rows = sorted(rows, key=lambda r: tuple(str(r.get(c, "")) for c in cols[:2]))
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow({k: ("" if r.get(k) is None else r.get(k)) for k in cols})


def edit1(a, b):
    """True if a and b differ by exactly one substitution, insertion or deletion."""
    if a == b or abs(len(a) - len(b)) > 1:
        return False
    if len(a) == len(b):
        return sum(x != y for x, y in zip(a, b)) == 1
    s, l = (a, b) if len(a) < len(b) else (b, a)
    return any(l[:i] + l[i + 1:] == s for i in range(len(l)))


def ocr_correct(entries):
    """Correct given-name and profession tokens of the typewritten volumes against
    the vocabulary of the clean printed ones (2002/2008/2008b/2014, plus the 2026
    Mandatsträger first names): a token absent from that vocabulary that is ONE
    edit away from exactly one frequent vocabulary token takes its spelling
    ("Ermst" -> "Ernst", "Polizeibeanter" -> "Polizeibeamter"). Surnames are not
    touched: they are too diverse for a vocabulary to arbitrate."""
    clean = {"2002", "2008", "2008b", "2014"}
    fn_vocab, pr_vocab = Counter(), Counter()
    for r in entries:
        if r["volume"] in clean:
            fn_vocab.update(r["first_name"].split())
            pr_vocab.update(re.findall(r"[A-Za-zÄÖÜäöüß][\w.\-äöüß]+", r["beruf_raw"]))
    if os.path.exists(BY2026):
        with open(BY2026, encoding="utf-8") as fh:
            for r in csv.DictReader(fh):
                if r["candidate_name"]:
                    fn_vocab.update(r["candidate_name"].split()[1:])

    def fix(tok, vocab, min_n):
        if tok in vocab or len(tok) < 4:
            return tok
        cands = [v for v, n in vocab.items() if n >= min_n and edit1(tok, v)]
        return cands[0] if len(cands) == 1 else tok

    n_fn = n_pr = 0
    for r in entries:
        if r["volume"] in clean:
            continue
        fn = " ".join(fix(x, fn_vocab, 5) for x in r["first_name"].split())
        n_fn += fn != r["first_name"]
        r["first_name"] = fn
        pr = re.sub(r"[A-Za-zÄÖÜäöüß][\w.\-äöüß]+", lambda m: fix(m.group(0), pr_vocab, 3), r["beruf_raw"])
        # systematic typewriter m/n and l/i confusions inside compound professions
        for rx, rep in ((r"beanter\b", "beamter"), (r"beantin\b", "beamtin"), (r"antmann\b", "amtmann"),
                        (r"\bVerwaitung", "Verwaltung"), (r"emmeister\b", "enmeister"),
                        (r"Angesteliter", "Angestellter")):
            pr = re.sub(rx, rep, pr)
        n_pr += pr != r["beruf_raw"]
        r["beruf_raw"] = pr
    print(f"OCR vocabulary corrections: {n_fn} given names, {n_pr} professions")


def write_entries(entries):
    cols = ["volume", "pdf", "pdf_page", "section", "kreis_code", "schl_nr", "gemeinde_raw", "vg",
            "star", "name_raw", "last_name", "first_name", "title", "beruf_raw", "birth_date",
            "birth_year", "birthplace", "wahlvorschlag_raw", "share", "wahltag", "stichwahl",
            "amtsantritt_printed", "match_status", "match_score", "ags", "source_gemeinde",
            "source_share", "source_amtsantritt", "source_amt_ok", "office"]
    with open(OUT_ENTRIES_R, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        for r in entries:
            w.writerow({k: ("" if r.get(k) is None else r.get(k)) for k in cols})


if __name__ == "__main__":
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--output-dir", help="Stage derived CSVs here without replacing current outputs")
    args = cli.parse_args()
    if args.output_dir:
        os.makedirs(args.output_dir, exist_ok=True)
        OUT_PERSONS = os.path.join(args.output_dir, os.path.basename(OUT_PERSONS))
        OUT_PERSONS_R = os.path.join(args.output_dir, os.path.basename(OUT_PERSONS_R))
        OUT_ENTRIES_R = os.path.join(args.output_dir, os.path.basename(OUT_ENTRIES_R))
        OUT_ELECTIONS = os.path.join(args.output_dir, os.path.basename(OUT_ELECTIONS))
    main()
