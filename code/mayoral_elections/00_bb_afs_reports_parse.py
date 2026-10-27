#!/usr/bin/env python3
"""
Stage 0 — Brandenburg Bürgermeisterwahlen 1993-2024 from the Statistische
Berichte B VII 3-6 of the LDS / Amt für Statistik Berlin-Brandenburg.

WHY
    GERDA's Brandenburg mayoral data came only from the Landeswahlleiter
    workbook (00_bb_lwl_parse.py): hauptamtliche Bürgermeister and the four
    Oberbürgermeister, 2010-2026, incomplete before 2014. The statistical
    reports add (a) hauptamtliche elections 1993-2008 and (b) the
    ehrenamtliche Bürgermeister of the amtsangehörige Gemeinden elected on the
    general Kommunalwahl dates 1998, 2003, 2008, 2014, 2019 and 2024, which the
    workbook never covered.

SOURCES  (data/mayoral_elections/raw/brandenburg/afs_bvii36_reports/, README there)
    Text layer, parsed here directly:
      SB_B7-3-6_5j-03_BB.pdf      2003: tables 8/9 (OB Brandenburg a. d. H., both
                                  rounds, all candidates) and table 10 (the mayor
                                  of every Gemeinde in office after 16.11.2003,
                                  with type H/E, Wahltag and Stichwahl date).
      SB_B7-3-6_j05-08_BB.xls     2008: table 3, the 270 mayors elected on
                                  28.09./12.10.2008 (winners only).
      SB_B07-03-06_2014j05_BB.xlsx, ..._2019j05_BB-k.xlsx, ..._2024j05_BB.xlsx
                                  2014/2019/2024: table 3, winners only (CC BY 3.0 DE).
    Scans, read from transcriptions in transcribed/ (gitignored; they name
    losing candidates). Each list that the report prints twice was transcribed
    twice from the two printings and reconciled (see reconcile_double()):
      t93hw_*      1993,1: 22 town sheets of 05.12.1993 (preliminary, all candidates)
      t93sw_*      1993,2: runoff sheets of 19.12.1993 (preliminary) + summary table
      t98_t5/t6    1998: table 5 (by Kreis) and table 6 (by party) of the ~1 230
                   mayors elected 27.09./11.10.1998; t98_potsdam_ob_* (table 4,
                   all candidates); t98_t7 (single candidates NOT elected)
      t02_t7/t8    2001/02: tables 7 (by Kreis) and 8 (by party), the 66 elected
                   OB/hauptamtliche BM; t02_t1 (dates per Gemeinde);
                   t02_ob_* (tables 5/6, OB of three cities, both rounds)

WHAT IS LEFT OUT (reported, never silent)
    * "späterer Wahltermin" (Amt Neuhausen/Spree, 15 Gemeinden, Verfassungs-
      gericht 17.10.2003);
    * every election the Landeswahlleiter workbook already holds (same AGS, a
      round within 60 days): the workbook has full results;
    * every election the 2004-2013 result reports from the Landeswahlleiter's
      archive hold (bb_lwl_scans_parsed.csv, same rule; full results there),
      after checking that both name the same elected person;
    * the 10 runoffs of 19.12.1993 without results at press time.

REPRESENTATION
    One row per candidate per round, the schema of bb_lwl_parsed.csv plus
      office            "H" hauptamtlich / "E" ehrenamtlich
      winner_only       TRUE where only the elected person is known (no votes,
                        no other candidates): 01b sets n_candidates to NA
      flag_decisive_round_missing  TRUE where the rounds we hold seated nobody
                        (1993 runoffs that missed the quorum, 1998 single
                        candidates rejected); 01b leaves is_winner NA
      name_restricted   TRUE for every named person who did not win the cycle:
                        published names are the elected persons' only (same
                        rule as the Hessen losers)
      preliminary       TRUE for the 1993 reports (vorläufige Ergebnisse); 01b
                        does not carry it (see the final README)
      elected_by_council  TRUE on the cycles where the Gemeindevertretung chose
                        the mayor (2003: 5 after a failed election and 15 former
                        Amtsdirektoren; 2008: 2; 2024: 4). They seat nobody by
                        popular vote (flag_decisive_round_missing) but end the
                        previous mayor's term in the panel.
      first_round_no_result  TRUE on the first-round row of a winner-only runoff
                        cycle: the reports print its date but no result, so the
                        row repeats the elected person without votes and 01
                        gives that round no winner. The cycle is then filed
                        under its first round like every other runoff.
    The name_restricted test compares surname and first given name, so a losing
    candidate who shares the winner's surname is not published.

AGS
    Each report prints the AGS of its own year (1993/1998 before, 2002/2003
    during, 2008-2024 after the Gemeindegebietsreform); rows keep them, and each
    is checked against data/crosswalks/final/ags_crosswalks.csv for the election
    year. The 1993 sheets print no AGS: the town is looked up by name within its
    Landkreis in the 1993 crosswalk year. (The Landeswahlleiter rows sit on the
    2026 Gebietsstand instead; see 00_bb_lwl_parse.py.)

Usage:  python3 code/mayoral_elections/00_bb_afs_reports_parse.py [--root DIR]
        Run after 00_bb_lwl_parse.py and 00_bb_lwl_scans_parse.py (it reads
        bb_lwl_parsed.csv and bb_lwl_scans_parsed.csv).
"""

import argparse
import csv
import difflib
import datetime as dt
import importlib.util
import os
import re
import subprocess
import sys
import unicodedata
from collections import Counter, defaultdict

try:
    import openpyxl
    import xlrd
except ImportError:
    sys.exit("openpyxl and xlrd are required:  pip3 install openpyxl xlrd")

HERE = os.path.dirname(os.path.abspath(__file__))

ap = argparse.ArgumentParser()
ap.add_argument("--root", default=os.path.abspath(os.path.join(HERE, "..", "..")),
                help="repository root (default: two levels above this script)")
ARGS = ap.parse_args()
ROOT = ARGS.root

RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "brandenburg")
SRC_DIR = os.path.join(RAW_DIR, "afs_bvii36_reports")
TR_DIR = os.path.join(SRC_DIR, "transcribed")
LWL_CSV = os.path.join(RAW_DIR, "bb_lwl_parsed.csv")
SCAN_CSV = os.path.join(RAW_DIR, "bb_lwl_scans_parsed.csv")   # 00_bb_lwl_scans_parse.py
CW_CSV = os.path.join(ROOT, "data", "crosswalks", "final", "ags_crosswalks.csv")
OUT_CSV = os.path.join(RAW_DIR, "bb_afs_parsed.csv")

STATE, STATE_NAME = "12", "Brandenburg"
KREISFREIE = {"12051000", "12052000", "12053000", "12054000"}

# Landkreis name -> 5-digit Kreis code (unchanged since the Kreisreform of
# 06.12.1993; the 1993 reports already use the new Landkreise).
KREIS_CODE = {
    "Brandenburg an der Havel": "12051", "Cottbus": "12052",
    "Frankfurt (Oder)": "12053", "Potsdam": "12054",
    "Barnim": "12060", "Dahme-Spreewald": "12061", "Elbe-Elster": "12062",
    "Havelland": "12063", "Märkisch-Oderland": "12064", "Oberhavel": "12065",
    "Oberspreewald-Lausitz": "12066", "Oder-Spree": "12067",
    "Ostprignitz-Ruppin": "12068", "Potsdam-Mittelmark": "12069",
    "Prignitz": "12070", "Spree-Neiße": "12071", "Teltow-Fläming": "12072",
    "Uckermark": "12073",
}

# split_name() of the workbook parser, so one person is split the same way in
# both Brandenburg sources ("Prof. Dr.-Ing. Schmidt, Helmut" -> title "Prof. Dr.-Ing.").
_spec = importlib.util.spec_from_file_location(
    "bb_lwl_parse", os.path.join(HERE, "00_bb_lwl_parse.py"))
_lwl = importlib.util.module_from_spec(_spec)
sys.argv, _argv = [sys.argv[0]], sys.argv
_spec.loader.exec_module(_lwl)
sys.argv = _argv
split_name = _lwl.split_name
norm_ws = _lwl.norm_ws

PROBLEMS = defaultdict(list)


def note(kind, msg):
    PROBLEMS[kind].append(msg)


# Misprinted dates, settled from the row's own context.
#   Müllrose 12067336, 2003 tables 10 and 11: "26.20.2003", runoff 16.11.2003,
#   so the Hauptwahl is the general election day 26.10.2003.
#   Panketal 12060181, 2003 table 10: runoff "16.11.2033" after the Hauptwahl
#   of 26.10.2003, i.e. the general runoff day 16.11.2003.
DATE_FIXES = {"26.20.2003": "26.10.2003", "16.11.2033": "16.11.2003"}

# Wrong years in 2003 table 10 (and 11), for elections the 2002 report prints
# with their true dates (tables 1 and 7): both printed dates are Mondays.
T10_DATE_FIXES = {
    ("12064380", "17.03.2003"): "17.03.2002",   # Petershagen/Eggersdorf ("Petersdorf/" in t10)
    ("12064472", "17.03.2003"): "17.03.2002",   # Strausberg
    ("12066320", "18.11.2002"): "18.11.2001",   # Vetschau/Spreewald
}

# General election days of the winner lists (table 3, 2008-2024). Seventeen
# 2014 runoff rows leave the Hauptwahl date blank; it is the general day.
GENERAL_DAY = {2008: dt.date(2008, 9, 28), 2014: dt.date(2014, 5, 25),
               2019: dt.date(2019, 5, 26), 2024: dt.date(2024, 6, 9)}

# List entries printed without the comma between surname and given name
# (2019 table 3: "Zimmer Daniel"), keyed (year, AGS, printed text).
LIST_NAME_FIXES = {(2019, "12061470", "Zimmer Daniel"): "Zimmer, Daniel"}

# A 1993 sheet whose town name resolves to the wrong Gemeinde: the runoff sheet
# "Neuenhagen" (Märkisch-Oderland, 8 782 Wahlberechtigte) is Neuenhagen b. Berlin
# (12064336, 11 210 inhabitants), not the village Neuenhagen (12064332, 1 030
# inhabitants, now part of Bad Freienwalde), whose base name matches exactly.
AGS_PINS_1993 = {("Neuenhagen", "Märkisch-Oderland"): "12064336"}


def ddate(s):
    """'26.10.2003' -> date."""
    s = s.strip()
    if s in DATE_FIXES:
        note("date_misprint_fixed", f"{s} -> {DATE_FIXES[s]}")
        s = DATE_FIXES[s]
    return dt.datetime.strptime(s, "%d.%m.%Y").date()


SALUTATION_RE = re.compile(r"^(Herrn?|Frau)\s+")
FOOTNOTE_RE = re.compile(r"[¹²³⁴⁵]+$")


def clean_person(name):
    """'Hoffmann, Herr Andreas ' -> 'Hoffmann, Andreas' (2014 list);
    'Schulze, Reinhard.' -> 'Schulze, Reinhard' (2003 table 10)."""
    name = norm_ws(name)
    if "," not in name and SALUTATION_RE.match(name):
        # 1998 table 5 prints a few mayors as "Herr Lehmann", "Herr Dr. Reibig"
        note("salutation_stripped", name)
        name = SALUTATION_RE.sub("", name)
    if "," in name:
        last, first = [x.strip() for x in name.split(",", 1)]
        first2 = SALUTATION_RE.sub("", first)
        if first2 != first:
            note("salutation_stripped", name)
        name = f"{last}, {first2}"
    if re.search(r"[a-zäöüß]{2}\.$", name):
        note("trailing_period_stripped", name)
        name = name[:-1]
    return name


def norm_party(p):
    p = norm_ws(p)
    # Orthography only: the same party, spelled "F.D.P." until 2001, and one
    # misprint on a 1993 sheet. List types stay as printed ("Einzelwahlvorschlag"
    # in 1993, "EB" later, all three forms in the Landeswahlleiter workbook); the
    # panel treats them as one label when it compares parties.
    return {"F.D.P.": "FDP", "Einzelwahlvolschlag": "Einzelwahlvorschlag"}.get(p, p)


def fold(s):
    s = unicodedata.normalize("NFKD", s or "").encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


def town_key(name):
    return fold(re.sub(r"/.*$|\(.*?\)", "", name))


# ---------------------------------------------------------------------------
# reference data
# ---------------------------------------------------------------------------

CW_POP = defaultdict(dict)    # {year: {ags: inhabitants}} filled by load_crosswalk()


def load_crosswalk():
    """{year: {ags: name}} for Brandenburg."""
    cw = defaultdict(dict)
    with open(CW_CSV, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            a = r["ags"].strip().zfill(8)
            if a.startswith(STATE):
                cw[int(r["year"])][a] = r["ags_name"]
                try:
                    CW_POP[int(r["year"])][a] = float(r["population"]) * 1000
                except ValueError:
                    pass
    return cw


def load_lwl():
    rows = list(csv.DictReader(open(LWL_CSV, encoding="utf-8-sig")))
    rounds = defaultdict(set)
    for r in rows:
        rounds[r["ags"]].add(dt.date.fromisoformat(r["election_date"]))
    return rows, rounds


# ---------------------------------------------------------------------------
# election objects
# ---------------------------------------------------------------------------

class Election:
    """One election cycle: one or two rounds, each with its candidates."""

    def __init__(self, ags, ags_name, office, source, ref, prio,
                 winner_only=False, preliminary=False):
        self.ags, self.ags_name = ags, norm_ws(ags_name)
        self.office, self.source, self.ref, self.prio = office, source, ref, prio
        self.winner_only, self.preliminary = winner_only, preliminary
        self.flag_missing = False
        self.hw_shell = None      # first-round date of a winner-only runoff cycle
        self.council = False      # the Gemeindevertretung chose the mayor
        self.rounds = []          # dicts: round, date, eligible, voters, valid, invalid, cands

    @property
    def election_type(self):
        return "Oberbürgermeisterwahl" if self.ags in KREISFREIE else "Bürgermeisterwahl"

    def add_round(self, rnd, date, cands, eligible=None, voters=None,
                  valid=None, invalid=None):
        self.rounds.append(dict(round=rnd, date=date, cands=cands, eligible=eligible,
                                voters=voters, valid=valid, invalid=invalid))
        return self

    @property
    def decisive(self):
        return max(self.rounds, key=lambda r: r["date"])

    @property
    def winner(self):
        w = [c for c in self.decisive["cands"] if c.get("is_winner")]
        return w[0] if w else None

    def key_dates(self):
        return {r["date"] for r in self.rounds}


def winner_only(ags, name_as_printed, office, party, hw, sw, source, ref, prio,
                ags_name):
    """A winner-only election: one row, on the runoff date if there was one."""
    e = Election(ags, ags_name, office, source, ref, prio, winner_only=True)
    cand = dict(name=clean_person(name_as_printed), party=norm_party(party), votes=None,
                is_winner=True)
    if sw:
        e.add_round("stichwahl", sw, [cand])
        # The source prints the first round's date too: finish() writes that round
        # (without a result) so the cycle is filed under it, as for every other
        # runoff (a December/January pair otherwise lands in the runoff's year).
        e.hw_shell = hw if hw and hw < sw else None
    else:
        e.add_round("hauptwahl", hw, [cand])
    return e


# ---------------------------------------------------------------------------
# 2003 report (text layer)
# ---------------------------------------------------------------------------

def pdftext(path):
    try:
        return subprocess.run(["pdftotext", "-layout", path, "-"], check=True,
                              capture_output=True, text=True).stdout.split("\n")
    except FileNotFoundError:
        sys.exit("pdftotext (poppler) is required:  brew install poppler")


FIELD_RE = re.compile(r"\S+(?: \S+)*")
DATE_RE = re.compile(r"^\d\d\.\d\d\.\d{4}$")
ROW_RE = re.compile(r"^\s*(120\s?\d\d\s?\d{3})\b")


def fields(line):
    return [(m.start(), m.end(), m.group()) for m in FIELD_RE.finditer(line)]


def parse_2003_t10(lines):
    """Table 10: one record per Gemeinde, wrapped cells re-attached by column."""
    start = [i for i, l in enumerate(lines)
             if re.match(r"\s*10\. Gewählte Bürgermeister im Land Brandenburg nach Regionalkreisen\s*$", l)]
    end = [i for i, l in enumerate(lines)
           if re.match(r"\s*11\. Gewählte Bürgermeister im Land Brandenburg nach Wahlvorschl", l)]
    s, e = start[-1], end[-1]
    recs = []
    for i in range(s, e):
        line = lines[i]
        if not ROW_RE.match(line):
            continue
        f = fields(line)
        ags = re.sub(r"\s", "", f[0][2])
        rest = f[1:]
        amt = ""
        col_start = f[0][1]                 # the Gemeinde column starts after AGS/Amt
        if rest and re.fullmatch(r"\d\d", rest[0][2]):
            amt = rest[0][2]
            col_start = rest[0][1]
            rest = rest[1:]
        art_i = next(k for k, x in enumerate(rest) if x[2] in ("H", "E"))
        gem_f = rest[:art_i]
        art = rest[art_i][2]
        tail = rest[art_i + 1:]
        if tail and tail[0][2].startswith("späterer Wahltermin"):
            recs.append(dict(ags=ags, amt=amt, gemeinde=" ".join(x[2] for x in gem_f),
                             art=art, status="later", line=i))
            continue
        dates = [x for x in tail if DATE_RE.match(x[2])]
        nondate = [x for x in tail if not DATE_RE.match(x[2])]
        name = nondate[0][2]
        party_f = nondate[1:]
        # wrapped cells on the neighbouring lines (no AGS, not blank)
        gem_lo = col_start
        gem_hi = rest[art_i][0]
        party_lo = nondate[0][1]
        party_hi = dates[0][0] if dates else len(line)
        gem_extra, party_extra = {}, {}
        for j in (i - 1, i + 1):
            nb = lines[j] if 0 <= j < len(lines) else ""
            if not nb.strip() or ROW_RE.match(nb):
                continue
            for (a, b, txt) in fields(nb):
                if gem_lo <= a < gem_hi and not gem_f:
                    gem_extra[j] = gem_extra.get(j, "") + txt
                elif party_lo <= a < party_hi and not party_f:
                    party_extra[j] = party_extra.get(j, "") + txt
        gemeinde = " ".join(x[2] for x in gem_f)
        if not gemeinde and gem_extra:
            parts = [gem_extra[j] for j in sorted(gem_extra)]
            gemeinde = "".join(parts) if parts[0].endswith("-") else " ".join(parts)
        party = " ".join(x[2] for x in party_f)
        if not party and party_extra:
            party = " ".join(party_extra[j] for j in sorted(party_extra))
        gemeinde = re.sub(r"\d\)$", "", gemeinde).strip()
        if not gemeinde:
            sys.exit(f"FATAL: 2003 table 10 line {i}: no Gemeinde name for {ags}")
        ds = []
        for d in dates:
            txt = d[2]
            if (ags, txt) in T10_DATE_FIXES:
                note("date_misprint_fixed", f"2003 t10 {ags} {gemeinde}: {txt} -> {T10_DATE_FIXES[(ags, txt)]}")
                txt = T10_DATE_FIXES[(ags, txt)]
            ds.append(ddate(txt))
        recs.append(dict(ags=ags, amt=amt, gemeinde=gemeinde, art=art,
                         name=clean_person(name), party=norm_ws(party),
                         hw=ds[0] if ds else None, sw=ds[1] if len(ds) > 1 else None,
                         status="ok", line=i))
    return recs


def parse_2003_ob(lines):
    """Tables 8/9: OB Brandenburg an der Havel, Hauptwahl and Stichwahl."""
    out = []
    heads = [i for i, l in enumerate(lines)
             if re.match(r"\s*(8|9)\. Ergebnis der Oberbürgermeister", l)]
    for h, (rnd, date) in zip(heads[-2:], [("hauptwahl", dt.date(2003, 10, 26)),
                                           ("stichwahl", dt.date(2003, 11, 16))]):
        block = lines[h:h + 60]
        def num(label):
            for l in block:
                m = re.match(r"\s*" + label + r"\s+([\d ]+?)\s{2,}", l + "  ")
                if m:
                    return int(m.group(1).replace(" ", ""))
        eligible, voters = num("Wahlberechtigte"), num(r"Wähler / Wahlbeteiligung")
        invalid, valid = num("ungültige Stimmen"), num("gültige Stimmen")
        cands = []
        for l in block:
            m = re.match(r"\s*(\d)\s{2,}(.+?)\s{2,}(\S.*?)\s{2,}([\d ]+?)\s{2,}(\d+,\d\d)\s*$", l)
            if m:
                cands.append(dict(name=norm_ws(m.group(2)), party=norm_party(m.group(3)),
                                  votes=int(m.group(4).replace(" ", "")), is_winner=False))
        out.append(dict(round=rnd, date=date, eligible=eligible, voters=voters,
                        valid=valid, invalid=invalid, cands=cands))
    return out


# ---------------------------------------------------------------------------
# 2008-2024 winner lists (Excel)
# ---------------------------------------------------------------------------

def to_date(v):
    if v is None or v == "":
        return None
    if isinstance(v, dt.datetime):
        return v.date()
    if isinstance(v, float):     # xlrd serial
        return xlrd.xldate.xldate_as_datetime(v, 0).date()
    s = str(v).strip()
    return ddate(s) if re.match(r"\d\d\.\d\d\.\d{4}$", s) else None


def load_list(fname, year):
    path = os.path.join(SRC_DIR, fname)
    if fname.endswith(".xls"):
        sh = xlrd.open_workbook(path).sheet_by_name("Tabelle3")
        rows = [[sh.cell_value(r, c) for c in range(sh.ncols)] for r in range(sh.nrows)]
    else:
        rows = [list(r) for r in
                openpyxl.load_workbook(path, read_only=True, data_only=True)["3"].iter_rows(values_only=True)]
    # Footnotes printed under the table (2024): "1 Keine der Bewerbenden hat die
    # erforderliche Stimmenzahl erhalten ... Wahl ... durch die
    # Stadtverordnetenversammlung bzw. Gemeindevertretung" and "2 infolge
    # Nichtannahme des Mandats erfolgt eine Wiederholungswahl". The marker sits
    # on the Gemeinde name as a superscript.
    footnote = {}
    for r in rows:
        t = norm_ws(r[0]) if isinstance(r[0], str) else ""
        m = re.match(r"^([1-9])\s+(.*)$", t)
        if m and not t[:2].isdigit():
            if "Vertretung" in t or "Gemeindevertretung" in t:
                footnote[m.group(1)] = "council"
            elif "Nichtannahme" in t:
                footnote[m.group(1)] = "declined"
    out = []
    for r in rows:
        raw = str(r[0]).split(".")[0].strip() if r[0] is not None else ""
        if not raw.startswith("12"):
            continue
        sup = re.search(r"[¹²³]", str(r[2] or ""))
        status = footnote.get({"¹": "1", "²": "2", "³": "3"}[sup.group()]) if sup else None
        amt = None
        if len(raw) == 12:                     # ARS (2024)
            ags = raw[:5] + raw[9:12]
            amt = "00" if raw[5:9] == "0" + raw[9:12] else raw[5:9]
        elif len(raw) == 10:                   # AGS + Amt (2014)
            ags, amt = raw[:8], raw[8:]
        else:
            ags = raw.zfill(8)
        hw, sw = to_date(r[5]), to_date(r[6])
        council = "Vertretung" in norm_ws(r[3]) or status == "council"
        if council:
            note("council_elected_list", f"{fname}: {ags} {norm_ws(r[2])} {norm_ws(r[3])} "
                 f"({to_date(r[5])})")
            if hw is None:
                hw = GENERAL_DAY[year]      # Neiße-Malxetal 2024 prints no date
            out.append(dict(ags=ags, amt=amt, gemeinde=FOOTNOTE_RE.sub("", norm_ws(r[2])),
                            declined=False, council=True,
                            name="" if "Vertretung" in norm_ws(r[3]) else clean_person(r[3]),
                            party="", hw=hw, sw=None, year=year, ref=f"{fname}!3:{ags}"))
            continue
        if hw is None and sw is not None:
            hw = GENERAL_DAY[year]
            note("list_hw_date_blank_filled", f"{year} {ags} {norm_ws(r[2])}")
        if hw is None:
            sys.exit(f"FATAL: {fname}: row without any date: {r}")
        printed = norm_ws(r[3])
        if (year, ags, printed) in LIST_NAME_FIXES:
            note("list_name_comma_fixed", f"{year} {ags} '{printed}' -> '{LIST_NAME_FIXES[(year, ags, printed)]}'")
            r = list(r)
            r[3] = LIST_NAME_FIXES[(year, ags, printed)]
        if status == "declined":
            note("list_elected_declined", f"{fname}: {ags} {norm_ws(r[2])} {norm_ws(r[3])}")
        out.append(dict(ags=ags, amt=amt, gemeinde=FOOTNOTE_RE.sub("", norm_ws(r[2])),
                        declined=(status == "declined"), council=False,
                        name=clean_person(r[3]),
                        party=norm_ws(r[4]), hw=hw, sw=sw, year=year,
                        ref=f"{fname}!3:{ags}"))
    return out


# ---------------------------------------------------------------------------
# transcriptions of the scanned reports
# ---------------------------------------------------------------------------

def read_tr(name):
    path = os.path.join(TR_DIR, name)
    if not os.path.exists(path):
        sys.exit(f"FATAL: transcription missing: {path}")
    return list(csv.DictReader(open(path, encoding="utf-8-sig")))


def ival(x):
    x = (x or "").strip().replace(" ", "")
    return int(x) if x.isdigit() else None


def reconcile_double(a_rows, b_rows, label, fixes):
    """Two printings of one list (by Kreis / by party), transcribed separately.

    Joined on AGS; any field that differs is reported. `fixes` holds the
    readings settled by inspecting the scan: {(ags, field): value}.
    """
    b_by = defaultdict(list)
    for r in b_rows:
        b_by[r["ags"]].append(r)
    out = []
    for r in a_rows:
        r = dict(r)
        for fld in ("name", "party", "marker", "gemeinde"):
            if (r["ags"], fld) in fixes:
                r[fld] = fixes[(r["ags"], fld)]
        mates = b_by.get(r["ags"], [])
        if len(mates) != 1:
            note(f"{label}_unmatched", f"{r['ags']} {r['gemeinde']} {r['name']}: "
                 f"{len(mates)} rows in the second printing")
        else:
            m = mates[0]
            for fld in ("name", "party"):
                va, vb = norm_ws(r[fld]), norm_ws(m[fld])
                if (r["ags"], fld) in fixes:
                    continue
                if va != vb:
                    note(f"{label}_differs", f"{r['ags']} {r['gemeinde']} {fld}: "
                         f"'{va}' vs '{vb}'")
        out.append(r)
    extra = set(b_by) - {r["ags"] for r in a_rows}
    for a in sorted(extra):
        note(f"{label}_only_in_second", f"{a} {b_by[a][0]['gemeinde']} {b_by[a][0]['name']}")
    return out


# Readings settled by looking at the scan where the two printings disagreed,
# keyed (ags, field). 1998: table 6 is the poorer print and fuses "rn" into "m";
# table 5 reads clearly (checked at 400 dpi, 2026-10-09). Three entries print
# surname and given name in the wrong order in BOTH tables; they are put in
# "Surname, Given" order, the spelling is untouched. Printed misspellings
# ("Wolfgng", "Frriedhelm", "Egebert") are kept as printed.
FIXES_1998 = {
    ("12063076", "name"): "Börner, Horst",           # t6 "Bömer"
    ("12064539", "name"): "Dr. Börnert, Gerd",       # t6 "Bömert"
    ("12069060", "name"): "Sternberg, Werner",       # t6 "Stemberg"
    ("12069188", "name"): "Sternberg, Axel",         # t6 "Stemberg"
    ("12069660", "name"): "Dr. Pleßke, Ulrich",      # t6 "Pießke"
    ("12063236", "name"): "Meyer, Jörg",             # printed "Jörg, Meyer"
    ("12068392", "name"): "Schönberg, Helmut",       # printed "Helmut Schönberg"
    ("12068428", "name"): "Balcerzak, Dieter",       # printed "Dieter Balcerzak"
}
FIXES_2002 = {}


def sheets_to_elections(sheet_rows, cand_rows, report, rnd, date, cw_year, cw,
                        prio, source):
    """1993 result sheets -> elections (all candidates, preliminary)."""
    by_page = defaultdict(list)
    for c in cand_rows:
        by_page[c["page"]].append(c)
    out = []
    for s in sheet_rows:
        kreis = norm_ws(s["landkreis"])
        stadt = norm_ws(re.sub(r"-\s+", "-", s["stadt"]))
        for (rep_, st_, fld), val in SHEET_FIXES.items():
            if rep_ == report and st_ == stadt:
                note("sheet_figure_fixed", f"{report} {stadt} {fld}: {s[fld]} -> {val}")
                s = dict(s, **{fld: val})
        ags = AGS_PINS_1993.get((stadt, kreis)) or lookup_ags(stadt, kreis, cw[cw_year])
        if ags is None:
            note("ags_unresolved", f"{report} p.{s['page']}: {stadt} ({kreis})")
            continue
        # A town name can resolve to a namesake: the electorate must fit the
        # Gemeinde's population (it is 0.44-1.04 of it on every other sheet).
        pop, elig = CW_POP[cw_year].get(ags), ival(s["eligible"])
        if pop and elig and not 0.4 <= elig / pop <= 1.1:
            sys.exit(f"FATAL: {report} p.{s['page']} {stadt} -> {ags}: {elig} Wahlberechtigte "
                     f"for {pop:.0f} inhabitants; wrong Gemeinde? Pin it in AGS_PINS_1993")
        cands = []
        for c in sorted(by_page[s["page"]], key=lambda c: int(c["lfd"])):
            if not norm_ws(c["name"]):
                note("sheet_blank_candidate_row", f"{report} p.{s['page']} {stadt}")
                continue
            if ival(c["votes"]) is None:
                # "Verzicht": the candidate withdrew before the runoff
                note("sheet_candidate_withdrew", f"{report} p.{s['page']} {stadt}: {c['name']}")
                continue
            cands.append(dict(name=clean_person(c["name"]), party=norm_party(c["party"]),
                              votes=ival(c["votes"]), is_winner=False))
        valid = ival(s["valid"])
        tot = sum(c["votes"] for c in cands)
        if valid is not None and tot != valid:
            if len(cands) == 1 and tot < valid:
                pass       # a sole candidate: the votes printed are Ja-Stimmen
            elif (report, stadt) in VOTE_ANOMALIES:
                note("votes_ne_valid_source", f"{report} p.{s['page']} {stadt}: "
                     f"{tot} vs {valid} ({VOTE_ANOMALIES[(report, stadt)]})")
            else:
                note("votes_ne_valid", f"{report} p.{s['page']} {stadt}: {tot} vs {valid}")
        outcome = s["outcome"].strip()
        e = Election(ags, stadt, None, source, f"{report} p.{s['page']}", prio,
                     preliminary=True)
        if outcome == "elected":
            nm = fold(split_name(s["elected_name"])[1])
            hit = [c for c in cands if fold(split_name(c["name"])[1]) == nm]
            if len(hit) != 1:
                note("elected_name_unmatched", f"{report} p.{s['page']} {stadt}: "
                     f"'{s['elected_name']}'")
                hit = [max(cands, key=lambda c: c["votes"] or 0)]
            hit[0]["is_winner"] = True
        elif outcome == "failed":
            e.flag_missing = True
        elif outcome == "runoff":
            pass      # decided on 19.12.1993; the runoff report carries it
        else:
            note("sheet_outcome_unclear", f"{report} p.{s['page']} {stadt}: {outcome}")
            continue
        e.add_round(rnd, date, cands, ival(s["eligible"]), ival(s["voters"]),
                    valid, ival(s["invalid"]))
        out.append(e)
    return out


# Sheet figures contradicted by the report's own summary table (1993,2 p. 4):
# Bestensee prints 2 397 Wahlberechtigte and 78,22 % turnout on its sheet, the
# summary 3 897 and 48,11 %; both fit its 1 875 voters, but only 3 897 makes the
# Gemeinde rows add up to the printed state total (655 765).
SHEET_FIXES = {
    ("Stichwahl_BM_u_OB-19121993_BB.pdf", "Bestensee", "eligible"): "3897",
}

# Sheets whose candidate votes do not add up to the printed valid votes, checked
# against the scan (both figures are printed as transcribed).
VOTE_ANOMALIES = {
    ("Stichwahl_BM_u_OB-19121993_BB.pdf", "Hörlitz"):
        "135 + 159 = 294, gültige printed 296; the shares use 296",
}


def lookup_ags(stadt, kreis, cw_year_map):
    """Town name + Landkreis (as printed, misspellings included) -> AGS."""
    for city, code in KREIS_CODE.items():
        if code in ("12051", "12052", "12053", "12054") and fold(stadt) == fold(city):
            return code + "000"
    if kreis not in KREIS_CODE:
        close = difflib.get_close_matches(kreis, KREIS_CODE, n=1, cutoff=0.85)
        if not close:
            return None
        note("landkreis_misprint", f"'{kreis}' -> '{close[0]}' ({stadt})")
        kreis = close[0]
    code = KREIS_CODE[kreis]
    base = lambda n: fold(re.sub(r",.*$|\(.*?\)|/.*$", "", n))
    want = base(stadt)
    cands = {a: base(n) for a, n in cw_year_map.items() if a.startswith(code)}
    hits = [a for a, n in cands.items() if n == want]
    if len(hits) == 1:
        return hits[0]
    hits = [a for a, n in cands.items() if n.startswith(want) or want.startswith(n + " ")]
    if len(hits) == 1:
        return hits[0]
    close = difflib.get_close_matches(want, list(cands.values()), n=2, cutoff=0.88)
    if len(close) == 1:
        hit = [a for a, n in cands.items() if n == close[0]]
        if len(hit) == 1:
            note("town_name_fuzzy", f"'{stadt}' -> {hit[0]} {cw_year_map[hit[0]]}")
            return hit[0]
    return None


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main():
    for p in (SRC_DIR, LWL_CSV, CW_CSV):
        if not os.path.exists(p):
            sys.exit(f"FATAL: input not found: {p}")
    cw = load_crosswalk()
    lwl_rows, lwl_rounds = load_lwl()
    elections = []

    # ---- 2003 -------------------------------------------------------------
    lines = pdftext(os.path.join(SRC_DIR, "SB_B7-3-6_5j-03_BB.pdf"))
    t10 = parse_2003_t10(lines)
    n_later = n_council = n_amtsdir = 0
    h_ags_2003 = set()
    for r in t10:
        if r["status"] == "later":
            n_later += 1
            continue
        council = None
        if re.fullmatch(r"\d\)", r["party"]):
            n_council += 1
            council = "council_elected_2003"
        elif r["art"] == "H" and not r["party"]:
            n_amtsdir += 1
            council = "amtsdirektor_2003"
        if council:
            # Not a direct election: the cycle seats nobody by popular vote, but
            # it ends the previous mayor's term (elected_by_council). The person
            # is kept like a declined winner: restricted twin only.
            note(council, f"{r['ags']} {r['gemeinde']} {r['name']} ({r['hw']})")
            e = winner_only(r["ags"], r["name"], r["art"], "", r["hw"], None, "afs_2003_t10",
                            f"SB_B7-3-6_5j-03_BB.pdf t10 line {r['line']}", 3, r["gemeinde"])
            e.flag_missing = e.council = True
            e.rounds[0]["cands"][0]["is_winner"] = False
            elections.append(e)
            continue
        if r["art"] == "H":
            h_ags_2003.add(r["ags"])
        elections.append(winner_only(
            r["ags"], r["name"], r["art"], r["party"], r["hw"], r["sw"],
            "afs_2003_t10", f"SB_B7-3-6_5j-03_BB.pdf t10 line {r['line']}", 3, r["gemeinde"]))
    if (n_later, n_council, n_amtsdir) != (15, 5, 15):
        sys.exit(f"FATAL: 2003 table 10 exclusions {n_later}/{n_council}/{n_amtsdir} "
                 "!= 15/5/15 stated in the Vorbemerkungen")
    ob = parse_2003_ob(lines)
    e = Election("12051000", "Brandenburg an der Havel", "H", "afs_2003_t8t9",
                 "SB_B7-3-6_5j-03_BB.pdf t8/t9", 1)
    for r in ob:
        if sum(c["votes"] for c in r["cands"]) != r["valid"] or r["valid"] + r["invalid"] != r["voters"]:
            sys.exit("FATAL: 2003 OB table does not add up")
        e.add_round(r["round"], r["date"], r["cands"], r["eligible"], r["voters"],
                    r["valid"], r["invalid"])
    top = max(e.decisive["cands"], key=lambda c: c["votes"])
    top["is_winner"] = True
    elections.append(e)

    # ---- 2008-2024 lists --------------------------------------------------
    lists = (load_list("SB_B7-3-6_j05-08_BB.xls", 2008)
             + load_list("SB_B07-03-06_2014j05_BB.xlsx", 2014)
             + load_list("SB_B07-03-06_2019j05_BB-k.xlsx", 2019)
             + load_list("SB_B07-03-06_2024j05_BB.xlsx", 2024))
    lwl_h = {r["ags"] for r in lwl_rows} - {"12073430"}   # Oberuckersee is ehrenamtlich
    for r in lists:
        # The Amt / Gemeindeverband code decides where the list prints one (2014,
        # 2024): an amtsangehörige or Verbandsgemeinde member has an
        # ehrenamtlichen mayor even if the Gemeinde had a full-time one before
        # (Bad Liebenwerda, Falkenberg/Elster, Uebigau-Wahrenbrück joined the
        # Verbandsgemeinde Liebenwerda in 2020). Otherwise a Gemeinde is H if
        # the workbook holds full-time elections for it, or (2008 only) if it
        # had a full-time mayor in 2003. From 2014 the workbook is complete for
        # full-time mayors, so 2003 status alone does not make a 2019 row H
        # (Steinhöfel and Niederer Fläming sit in an Amt/Verbandsgemeinde by 2024).
        if r["ags"] in KREISFREIE:
            office = "H"
        elif r["amt"] is not None:
            office = "H" if r["amt"] == "00" else "E"
            if office == "E" and (r["ags"] in lwl_h or r["ags"] in h_ags_2003):
                note("list_office_by_amt_code", f"{r['year']} {r['ags']} {r['gemeinde']}: "
                     f"Gemeindeverband {r['amt']} -> E (H elsewhere)")
        else:
            office = "H" if (r["ags"] in lwl_h or (r["year"] < 2010 and r["ags"] in h_ags_2003)) else "E"
        if office == "H" and r["year"] >= 2014:
            note("list_h_row", f"{r['year']} {r['ags']} {r['gemeinde']} {r['name']} "
                 "(full-time; dropped below if the Landeswahlleiter workbook holds it)")
        e = winner_only(r["ags"], r["name"], office, r["party"], r["hw"], r["sw"],
                        f"afs_{r['year']}_t3", r["ref"], 3, r["gemeinde"])
        if r["declined"] or r["council"]:
            # declined: the elected person did not accept the office; council:
            # the Gemeindevertretung chose the mayor. Either way the direct
            # election seated nobody.
            e.flag_missing = True
            e.council = r["council"]
            e.rounds[0]["cands"][0]["is_winner"] = False
        elections.append(e)

    # ---- scans (transcriptions) ------------------------------------------
    if os.path.isdir(TR_DIR):
        elections += load_scans(cw, h_ags_2003)
    else:
        note("no_transcriptions", f"{TR_DIR} missing: 1993/1998/2002 not included")

    finish(elections, cw, lwl_rounds)


def load_scans(cw, h_ags_2003):
    out = []
    # 1998: tables 5 and 6, reconciled; table 4 (Potsdam OB); table 7
    t5 = read_tr("t98_t5.csv")
    t6 = read_tr("t98_t6.csv")
    t5 = reconcile_double(t5, t6, "t98", FIXES_1998)
    d_hw, d_sw = dt.date(1998, 9, 27), dt.date(1998, 10, 11)
    # Table 1 of the report: 1 150 elected in the Hauptwahl, 44 in the runoff
    # (45 runoffs, Neuhardenberg elected nobody), plus the Potsdam OB.
    n_sw98 = sum("#" in r.get("marker", "") for r in t5 if r["ags"] != "12054000")
    n_hw98 = sum(1 for r in t5 if r["ags"] != "12054000") - n_sw98
    if (n_hw98, n_sw98) != (1150, 44):
        sys.exit(f"FATAL: 1998 table 5 has {n_hw98} + {n_sw98} winners, report says 1150 + 44")
    h98 = set()
    for r in t5:
        if r["ags"] == "12054000":
            continue                 # Potsdam: full result from table 4
        marker = r.get("marker", "")
        office = "H" if "haupt" in marker else "E"
        if office == "H":
            h98.add(r["ags"])
        out.append(winner_only(r["ags"], r["name"], office, r["party"], d_hw,
                               d_sw if "#" in marker else None, "afs_1998_t5",
                               f"SB_B7-3-6_5j-98_BBa.pdf p.{r['page']} row {r['row']}", 2,
                               r["gemeinde"]))
    sh = read_tr("t98_potsdam_ob_sheet.csv")[0]
    cands = [dict(name=norm_ws(c["name"]), party=norm_party(c["party"]),
                  votes=ival(c["votes"]), is_winner=False)
             for c in read_tr("t98_potsdam_ob_cands.csv")]
    max(cands, key=lambda c: c["votes"])["is_winner"] = True
    out.append(Election("12054000", "Potsdam", "H", "afs_1998_t4",
                        "SB_B7-3-6_5j-98_BBa.pdf p.12", 1).add_round(
        "hauptwahl", d_hw, cands, ival(sh["eligible"]), ival(sh["voters"]),
        ival(sh["valid"]), ival(sh["invalid"])))
    # table 7: the single candidate was rejected (Ja/Nein); "nachrichtlich": the
    # one runoff of 11.10.1998 that elected nobody (Neuhardenberg). Both seat
    # nobody, so the cycle is flagged and every name is a loser's.
    t7_by = defaultdict(list)
    for r in read_tr("t98_t7.csv"):
        if r["ags"]:
            t7_by[(r["ags"], "Stichwahl" in r["note"])].append(r)
    n_t7 = Counter()
    for (ags, is_sw), rs in t7_by.items():
        e = Election(ags, rs[0]["gemeinde"], "E", "afs_1998_t7",
                     f"SB_B7-3-6_5j-98_BBa.pdf p.54 rows {','.join(r['row'] for r in rs)}", 2,
                     winner_only=True)
        e.flag_missing = True
        e.add_round("stichwahl" if is_sw else "hauptwahl", d_sw if is_sw else d_hw,
                    [dict(name=norm_ws(r["name"]), party=norm_party(r["party"]),
                          votes=None, is_winner=False) for r in rs])
        n_t7[is_sw] += 1
        out.append(e)
    # table 1/2 of the report: 32 rejected single candidates, 1 failed runoff
    if (n_t7[False], n_t7[True]) != (32, 1):
        sys.exit(f"FATAL: 1998 table 7 gives {n_t7[False]}/{n_t7[True]}, report says 32/1")

    # 2001/02: tables 7 and 8, dates from table 1; OB tables 5/6
    t7 = reconcile_double(read_tr("t02_t7.csv"), read_tr("t02_t8.csv"), "t02", FIXES_2002)
    if (len(t7), sum(bool(r["runoff_marker"].strip()) for r in t7)) != (66, 27):
        sys.exit("FATAL: 2002 table 7 should list 66 mayors, 27 elected in a runoff")
    t1 = read_tr("t02_t1.csv")
    dates_by_name = {}
    for r in t1:
        hw = [d for col, d in (("hw_20011111", dt.date(2001, 11, 11)),
                               ("hw_20011118", dt.date(2001, 11, 18)),
                               ("hw_20020224", dt.date(2002, 2, 24))) if r[col].strip() == "1"]
        sw = [d for col, d in (("sw_20011202", dt.date(2001, 12, 2)),
                               ("sw_20020317", dt.date(2002, 3, 17))) if r[col].strip() == "1"]
        if len(hw) != 1 or len(sw) > 1:
            note("t02_t1_dates", f"{r['gemeinde']}: hw {hw} sw {sw}")
            continue
        dates_by_name[town_key(r["gemeinde"])] = (hw[0], sw[0] if sw else None)
    ob_cities = {"12051000", "12052000", "12053000"}
    for r in t7:
        if r["ags"] in ob_cities:
            continue
        key = town_key(r["gemeinde"])     # table 1 prints "Vetschau", table 7 "Vetschau/Spreewald"
        if key not in dates_by_name:
            note("t02_no_dates", f"{r['ags']} {r['gemeinde']}")
            continue
        hw, sw = dates_by_name[key]
        if bool(sw) != bool(r["runoff_marker"].strip()):
            note("t02_runoff_marker_vs_t1", f"{r['ags']} {r['gemeinde']}")
        out.append(winner_only(r["ags"], r["name"], "H", r["party"], hw, sw, "afs_2002_t7",
                               f"SB_B7-3-6_5j-02_BB.pdf p.{r['page']} row {r['row']}", 2,
                               r["gemeinde"]))
    obs = read_tr("t02_ob_sheets.csv")
    obc = read_tr("t02_ob_cands.csv")
    city_ags = {"Brandenburg an der Havel": "12051000", "Cottbus": "12052000",
                "Frankfurt (Oder)": "12053000"}
    for city, ags in city_ags.items():
        e = Election(ags, city, "H", "afs_2002_t5t6", "SB_B7-3-6_5j-02_BB.pdf t5/t6", 1)
        for rnd, date in (("hauptwahl", dt.date(2002, 2, 24)), ("stichwahl", dt.date(2002, 3, 17))):
            s = [x for x in obs if norm_ws(x["stadt"]) == city and x["round"].lower() == rnd]
            if len(s) != 1:
                sys.exit(f"FATAL: 2002 OB sheet {city} {rnd}: {len(s)} rows")
            s = s[0]
            cands = [dict(name=norm_ws(c["name"]), party=norm_party(c["party"]),
                          votes=ival(c["votes"]), is_winner=False)
                     for c in obc if norm_ws(c["stadt"]) == city and c["round"].lower() == rnd]
            if sum(c["votes"] for c in cands) != ival(s["valid"]):
                sys.exit(f"FATAL: 2002 OB {city} {rnd}: votes do not add up")
            e.add_round(rnd, date, cands, ival(s["eligible"]), ival(s["voters"]),
                        ival(s["valid"]), ival(s["invalid"]))
        max(e.decisive["cands"], key=lambda c: c["votes"])["is_winner"] = True
        out.append(e)

    # 1993: first-round town sheets (05.12.) and runoff sheets (19.12.)
    out += sheets_to_elections(read_tr("t93hw_sheets.csv"), read_tr("t93hw_cands.csv"),
                               "BM_OB_5.12.1993.pdf", "hauptwahl", dt.date(1993, 12, 5),
                               1993, cw, 1, "afs_1993_hw")
    out += sheets_to_elections(read_tr("t93sw_sheets.csv"), read_tr("t93sw_cands.csv"),
                               "Stichwahl_BM_u_OB-19121993_BB.pdf", "stichwahl",
                               dt.date(1993, 12, 19), 1993, cw, 1, "afs_1993_sw")
    # 1993 office: hauptamtlich where the Gemeinde elected a hauptamtlichen
    # mayor in 1998/2001-02/2003 (same AGS), else ehrenamtlich. The 22 towns of
    # 05.12.1993 are hauptamtlich by the report's own title.
    h_any = h98 | h_ags_2003 | {r["ags"] for r in t7}
    for e in out:
        if e.source == "afs_1993_hw":
            e.office = "H"
        elif e.source == "afs_1993_sw":
            e.office = "H" if (e.ags in h_any or e.ags in KREISFREIE) else "E"
    return out


def load_scan_cycles():
    """{ags: [(dates, winner key or None)]} from bb_lwl_scans_parsed.csv, the
    2004-2013 result reports (00_bb_lwl_scans_parse.py): full results, so they
    supersede the winner-only rows here. Empty if that parser has not run."""
    out = defaultdict(list)
    if not os.path.exists(SCAN_CSV):
        note("no_scan_file", f"{SCAN_CSV} missing: elections it holds are kept here")
        return out
    rows = list(csv.DictReader(open(SCAN_CSV, encoding="utf-8-sig")))
    # one cycle = an AGS's rounds within 42 days (a Hauptwahl and its runoff)
    for r in sorted(rows, key=lambda r: (r["ags"], r["election_date"])):
        d = dt.date.fromisoformat(r["election_date"])
        cs = out[r["ags"]]
        if not cs or (d - max(cs[-1]["dates"])).days > 42:
            cs.append({"dates": set(), "winner": None, "missing": False})
        c = cs[-1]
        c["dates"].add(d)
        c["missing"] |= r["flag_decisive_round_missing"] == "TRUE"
        if r["name_restricted"] == "FALSE" and r["flag_decisive_round_missing"] != "TRUE":
            _, last, first, _ = split_name(r["candidate_name"])
            c["winner"] = (fold(last), (fold(first).split() or [""])[0])
    return out


def finish(elections, cw, lwl_rounds):
    # ---- 1. drop what the Landeswahlleiter workbook already holds ---------
    keep = []
    n_lwl = n_scan = 0
    scan = load_scan_cycles()
    for e in elections:
        lwl_d = lwl_rounds.get(e.ags, set())
        if any(abs((d - x).days) <= 60 for d in e.key_dates() for x in lwl_d):
            n_lwl += 1
            continue
        # ... and what the 2004-2013 result reports hold, after checking that
        # both name the same elected person (surname + first given name)
        sc = [c for c in scan.get(e.ags, [])
              if any(abs((d - x).days) <= 60 for d in e.key_dates() for x in c["dates"])]
        if sc:
            n_scan += 1
            w = e.winner
            wk = None
            if w and not e.flag_missing:
                _, last, first, _ = split_name(w["name"])
                wk = (fold(last), (fold(first).split() or [""])[0])
            if wk != sc[0]["winner"]:
                note("scan_winner_differs", f"{e.ags} {e.ags_name} {e.decisive['date']} ({e.source}): "
                     f"report and result report name different elected persons")
            continue
        keep.append(e)
    print(f"Dropped {n_lwl} elections the Landeswahlleiter workbook already holds")
    print(f"Dropped {n_scan} elections the 2004-2013 result reports hold (full results there)")

    # ---- 2. one election in two reports: keep the richer one -------------
    # Same decisive date, same Landkreis and the same elected person (surname
    # and first given name); the AGS itself can differ, because 2002 prints the
    # AGS at election time and 2003 the merged Gemeinde's. Only elections from
    # DIFFERENT reports are merged: within one report two Gemeinden can elect
    # namesakes on the same day.
    groups = defaultdict(list)
    for e in keep:
        w = e.winner
        if w:
            _, last, first, _ = split_name(w["name"])
            k = (e.decisive["date"], e.ags[:5], fold(last), (fold(first).split() or [""])[0])
        else:
            k = (e.decisive["date"], e.ags, e.source)
        groups[k].append(e)
    final = []
    for k, es in groups.items():
        if len(es) == 1 or len({e.source for e in es}) < len(es):
            if len(es) > 1:
                note("same_person_same_day_one_report",
                     "; ".join(f"{e.ags} {e.ags_name} {e.source}" for e in es))
            final.extend(es)
            continue
        es.sort(key=lambda e: (e.prio, e.decisive["date"]))
        best = es[0]
        for o in es[1:]:
            wb, wo = best.winner, o.winner
            if wb and wo and norm_ws(wb["name"]) != norm_ws(wo["name"]):
                note("name_variant_between_reports",
                     f"{best.ags}/{o.ags} {k[0]}: '{wb['name']}' ({best.source}) vs "
                     f"'{wo['name']}' ({o.source}) - kept the first")
            if best.ags != o.ags:
                note("ags_differs_between_reports",
                     f"{best.ags} ({best.source}) vs {o.ags} ({o.source}) {k[0]}")
        final.append(best)
    print(f"Merged {len(keep) - len(final)} duplicate elections across reports")

    # same AGS, same day, different persons: never expected
    seen = defaultdict(list)
    for e in final:
        for r in e.rounds:
            seen[(e.ags, r["date"], r["round"])].append(e)
    clash = {k: v for k, v in seen.items() if len(v) > 1}
    for k, v in clash.items():
        note("ags_date_clash", f"{k}: " + "; ".join(f"{x.source} {x.winner and x.winner['name']}" for x in v))

    # ---- 3. weekday dates (all report dates are Sundays unless misprinted) -
    for e in final:
        for r in e.rounds:
            if r["date"].weekday() != 6:
                note("non_sunday", f"{e.ags} {e.ags_name} {r['date']} ({r['date'].strftime('%A')}) "
                     f"{r['round']} ({e.source})")

    # ---- 3b. AGS valid for the election year -----------------------------
    for e in final:
        y = e.decisive["date"].year
        if e.ags not in cw.get(min(y, 2020), {}):
            alt = [yy for yy in (y - 1, y + 1) if e.ags in cw.get(min(yy, 2020), {})]
            note("ags_not_in_crosswalk_year" if not alt else "ags_in_adjacent_year_only",
                 f"{e.ags} {e.ags_name} {e.decisive['date']} ({e.source})")

    # ---- 4. rows ----------------------------------------------------------
    def person_key(name):
        _, last, first, _ = split_name(name)
        return fold(last), (fold(first).split() or [""])[0]

    rows = []
    n_shell = 0
    for e in sorted(final, key=lambda e: (e.ags, e.decisive["date"])):
        # surname AND first given name: a loser sharing the winner's surname
        # must not be published as the winner
        winners = {person_key(c["name"]) for r in e.rounds for c in r["cands"] if c.get("is_winner")}
        if not e.flag_missing and not winners:
            note("no_winner", f"{e.ags} {e.ags_name} {e.decisive['date']} ({e.source})")
        rounds = sorted(e.rounds, key=lambda r: r["date"])
        if e.hw_shell and [r["round"] for r in rounds] == ["stichwahl"]:
            gap = (rounds[0]["date"] - e.hw_shell).days
            note("runoff_gap_days", f"{gap}")
            if not 7 <= gap <= 42:
                sys.exit(f"FATAL: {e.ags} {e.ags_name}: first round {e.hw_shell}, runoff {rounds[0]['date']}")
            rounds = [dict(round="hauptwahl", date=e.hw_shell, eligible=None, voters=None,
                           valid=None, invalid=None, shell=True,
                           cands=[dict(c, votes=None, is_winner=False) for c in rounds[0]["cands"]])] + rounds
            n_shell += 1
        for r in rounds:
            for c in r["cands"]:
                full, last, first, title = split_name(c["name"])
                valid = r["valid"]
                share = (c["votes"] / valid) if (c["votes"] is not None and valid) else None
                turnout = (r["voters"] / r["eligible"]) if (r["voters"] and r["eligible"]) else None
                won_cycle = person_key(c["name"]) in winners and not e.flag_missing
                rows.append({
                    "ags": e.ags, "ags_name": e.ags_name, "state": STATE,
                    "state_name": STATE_NAME, "election_year": r["date"].year,
                    "election_date": r["date"].isoformat(),
                    "election_type": e.election_type, "round": r["round"],
                    "eligible_voters": r["eligible"] if r["eligible"] is not None else "",
                    "number_voters": r["voters"] if r["voters"] is not None else "",
                    "valid_votes": valid if valid is not None else "",
                    "invalid_votes": r["invalid"] if r["invalid"] is not None else "",
                    "turnout": turnout if turnout is not None else "",
                    "candidate_name": full, "candidate_last_name": last,
                    "candidate_first_name": first, "candidate_title": title,
                    "candidate_party": c["party"], "wahlvorschlag_art": "",
                    "candidate_votes": c["votes"] if c["votes"] is not None else "",
                    "candidate_voteshare": share if share is not None else "",
                    "is_winner": ("" if e.flag_missing else ("TRUE" if c.get("is_winner") else "FALSE")),
                    "source": e.source, "source_url": e.ref,
                    "office": e.office,
                    "winner_only": "TRUE" if e.winner_only else "FALSE",
                    "flag_decisive_round_missing": "TRUE" if e.flag_missing else "FALSE",
                    "name_restricted": "FALSE" if won_cycle else "TRUE",
                    "preliminary": "TRUE" if e.preliminary else "FALSE",
                    "first_round_no_result": "TRUE" if r.get("shell") else "FALSE",
                    "elected_by_council": "TRUE" if e.council else "FALSE",
                })

    print("\n--- source defects, exclusions & anomalies ---")
    for k in sorted(PROBLEMS):
        print(f"  [{k}] {len(PROBLEMS[k])}")
        for msg in PROBLEMS[k][:60]:
            print(f"      {msg}")
    fatal = [k for k in ("votes_ne_valid", "ags_date_clash", "no_winner",
                         "ags_unresolved", "elected_name_unmatched",
                         "scan_winner_differs") if PROBLEMS.get(k)]
    if fatal:
        sys.exit(f"\nFATAL: {fatal} — fix the transcription or pin the case before publishing.")

    with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\nWrote {len(rows)} candidate rows -> {OUT_CSV}")
    print(f"  {n_shell} winner-only runoff cycles given their first round (no result)")
    print(f"  {len(final)} elections, {len({e.ags for e in final})} AGS")
    print("  by source:", dict(sorted(Counter(e.source for e in final).items())))
    print("  by office:", dict(Counter(e.office for e in final)))
    print("  by year:", dict(sorted(Counter(e.decisive['date'].year for e in final).items())))


if __name__ == "__main__":
    main()
