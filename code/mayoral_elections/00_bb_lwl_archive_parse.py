#!/usr/bin/env python3
"""
Stage 0 — Brandenburg Bürgermeisterwahlen from the Landeswahlleiter's old
result pages, 2001-2017, as archived by the Internet Archive.

SOURCE
    data/mayoral_elections/raw/brandenburg/lwl_bmwahlen_archive/
        pages/<AGS10>_<h|s>_<capture>.html   raw captures ("id_" mode), gitignored:
                                               they name every Bewerber
        _capture_manifest.csv                 tracked: file, archive URL, capture, sha256
        cdx_bmwahlen.json                     tracked: the Internet Archive's CDX listing
    From 2008 until the move to the current portal (2018) the Landeswahlleiter
    published each Gemeinde's LATEST Bürgermeisterwahl at
    wahlen.brandenburg.de/bmwahlen/<AGS10>/h (Hauptwahl) and .../s (Stichwahl),
    hauptamtliche and ehrenamtliche alike. Each page is the LWL's MAIS form, the
    same form as the Gemeinden's result reports read by 00_bb_lwl_scans_parse.py:
    Wahlberechtigte, Wähler, ungültige and gültige Stimmen, the Nein-Stimmen of a
    single-candidate ballot, the two thresholds of § 72 Abs. 2 BbgKWahlG, every
    Bewerber with Wahlvorschlagsträger and votes, and the Wahlausschuss's finding.
    984 captures (2008-2017, 414 Gemeinden) were fetched by
    00_bb_lwl_archive_fetch.py. A page shows elections from 2001 on: whatever
    was the Gemeinde's latest election when it was captured.

WHAT IS TAKEN, election by election (a Hauptwahl and the Stichwahl 14-35 days later)
    * held by the workbook (bb_lwl_parsed.csv), the result reports
      (bb_lwl_scans_parsed.csv) or a B VII 3-6 report with counts
      (bb_afs_parsed.csv): every count and vote is compared and the election is
      left out. A difference stops the run unless it is pinned (OVERLAP_PINS).
    * held by a B VII 3-6 report WINNER-ONLY: replaced. The archived rounds bring
      the counts and every Bewerber. A round the archive lacks (a runoff page
      overwritten before it was captured, or the result-less first round of a
      winner-only runoff cycle) is carried over from bb_afs_parsed.csv into this
      file, so each election comes from one file and its two rounds pair by
      pair_key. The elected person must be the one the report names (fatal
      otherwise). The report's rounds are listed in bb_lwl_archive_supersedes.csv,
      which 01/01b drop.
    * held by nobody: added. A Hauptwahl that went to a runoff the archive does
      not hold seats nobody (flag_decisive_round_missing); a runoff whose first
      round is not archived stays a runoff-only cycle.
    * a page whose counts equal a GERDA round of the same Gemeinde on ANOTHER
      date carries the site's wrong date and is left out (pinned in MISDATED).

CHECKS (as 00_bb_lwl_scans_parse.py)
    per round: Sunday; gültige + ungültige = Wähler; Wähler <= Wahlberechtigte;
    votes (+ Nein on a single-candidate ballot) = gültige; printed percentages;
    printed thresholds; the finding against the votes, incl. the 15 % quorum;
    two captures of one round must agree in every field;
    per election: runoff 2-5 weeks after the Hauptwahl; the runoff Bewerber are
    the two the Hauptwahl admitted; similar electorates in both rounds;
    the AGS is a Gemeinde of that name in the election year (crosswalk); an
    added election must not exist under another code (same day, same name).
    A source defect that is understood is pinned in ANOMALIES; a pin that no
    longer fires stops the run.

OUTPUT
    data/mayoral_elections/raw/brandenburg/bb_lwl_archive_parsed.csv (gitignored)
        the schema of bb_lwl_scans_parsed.csv; source "lwl_archive" (or the
        B VII 3-6 source of a carried-over round); name_restricted FALSE only on
        the elected person's own rows; office from the B VII 3-6 report it
        replaces, else from GERDA's other elections in that Gemeinde.
    data/mayoral_elections/raw/brandenburg/bb_lwl_archive_supersedes.csv (gitignored)
        ags, election_date, round of every bb_afs_parsed.csv round replaced.

Run order:  00_bb_lwl_parse.py -> 00_bb_lwl_scans_parse.py ->
            00_bb_afs_reports_parse.py -> 00_bb_lwl_archive_parse.py -> 01 / 01b
Usage:      python3 code/mayoral_elections/00_bb_lwl_archive_parse.py [--root DIR]
"""

import argparse
import csv
import datetime as dt
import hashlib
import html
import importlib.util
import math
import os
import re
import sys
import unicodedata
from collections import Counter, defaultdict
from html.parser import HTMLParser

HERE = os.path.dirname(os.path.abspath(__file__))
ap = argparse.ArgumentParser()
ap.add_argument("--root", default=os.path.abspath(os.path.join(HERE, "..", "..")),
                help="repository root (default: two levels above this script)")
ROOT = ap.parse_args().root

RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "brandenburg")
ARC_DIR = os.path.join(RAW_DIR, "lwl_bmwahlen_archive")
PAGES = os.path.join(ARC_DIR, "pages")
MANIFEST = os.path.join(ARC_DIR, "_capture_manifest.csv")
LWL_CSV = os.path.join(RAW_DIR, "bb_lwl_parsed.csv")
SCAN_CSV = os.path.join(RAW_DIR, "bb_lwl_scans_parsed.csv")
AFS_CSV = os.path.join(RAW_DIR, "bb_afs_parsed.csv")
CW_CSV = os.path.join(ROOT, "data", "crosswalks", "final", "ags_crosswalks.csv")
CW25_CSV = os.path.join(ROOT, "data", "crosswalks", "final", "ags_1990_to_2025_crosswalk.csv")
OUT_CSV = os.path.join(RAW_DIR, "bb_lwl_archive_parsed.csv")
SUP_CSV = os.path.join(RAW_DIR, "bb_lwl_archive_supersedes.csv")

STATE, STATE_NAME = "12", "Brandenburg"
KREISFREIE = {"12051000", "12052000", "12053000", "12054000"}

# split_name()/norm_ws() of the workbook parser, so one person is split the same
# way in every Brandenburg source (as in 00_bb_lwl_scans_parse.py)
_spec = importlib.util.spec_from_file_location(
    "bb_lwl_parse", os.path.join(HERE, "00_bb_lwl_parse.py"))
_lwl = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_lwl)
split_name = _lwl.split_name
norm_ws = _lwl.norm_ws

# ---------------------------------------------------------------------------
# pins (each reported on every run; a pin that no longer fires stops the run)
# ---------------------------------------------------------------------------

# Captures without a result (no table, or a form without counts and Bewerber):
# (AGS10, page, capture) -> what the page is
EMPTY_PAGES = {
    ("1206002000", "s", "20100103121922"): "Biesenthal runoff page, no result table",
    ("1206316109", "h", "20130926112817"): "Kleßen-Görne 25.01.2011 (a Tuesday): a form with a named person but "
                                           "no counts and no Bewerber; not an election result",
    ("1206752801", "h", "20151130040715"): "Gosen-Neu Zittau Hauptwahl page, no result table",
}

# Pages that carry the site's wrong date: their counts equal a GERDA round of the
# same Gemeinde on another date. (ags, site date, round) -> (GERDA date, why)
_MOL_2009 = ("the result report and the eight-year cycle 2001/2009/2017 date it 27.09.2009; "
             "the captures show the result first in October 2009")
MISDATED = {
    ("12064044", "2008-09-28", "hauptwahl"): ("2009-09-27", "Bad Freienwalde (Oder): " + _MOL_2009),
    ("12064448", "2008-09-28", "hauptwahl"): ("2009-09-27", "Seelow: " + _MOL_2009),
    ("12064512", "2008-09-28", "hauptwahl"): ("2009-09-27", "Wriezen: " + _MOL_2009),
    ("12067120", "2017-09-24", "stichwahl"): ("2017-10-08", "Eisenhüttenstadt: the runoff page carries the "
                                                            "Hauptwahl's date; the workbook dates it 08.10.2017"),
    ("12072312", "2017-09-24", "stichwahl"): ("2017-10-15", "Nuthe-Urstromtal: the runoff page carries the "
                                                            "Hauptwahl's date; the workbook dates it 15.10.2017"),
}

# Differences between an archived round and the GERDA round it duplicates:
# (ags, date, round, field, archive value, GERDA value) -> explanation
OVERLAP_PINS = {
    ("12069020", "2008-09-28", "hauptwahl", "eligible", "5779", "9779"):
        "Belzig: the site repeats the Gemeinde's misprint; GERDA's 9 779 is the corrected figure "
        "(00_bb_lwl_scans_parse.py ELIGIBLE_FIXES, AfS SB B VII 3-5 5j/08 p. 108)",
}

# Bewerber names the site misprints, corrected to the spelling of an independent
# official source; applied to the page's table and finding alike.
# (ags, date, printed name) -> corrected name
NAME_FIXES = {
    ("12063165", "2008-09-28", "Ossenbühl, Maries"): "Ossenbühl, Marlies",     # B VII 3-6 2008, table 3
    ("12072014", "2002-05-26", "IIK, Peter"): "Ilk, Peter",                    # B VII 3-6 2002/2003
    ("12072014", "2002-06-16", "IIk, Peter"): "Ilk, Peter",                    # B VII 3-6 2002/2003
    ("12064128", "2008-09-28", "Fandrich, Bernhard"): "Fahndrich, Bernhard",   # its runoff page; B VII 3-6 2008
    ("12063244", "2016-04-10", "Trebling, Ralf"): "Tebling, Ralf",             # its runoff page; the workbook
}

# Understood source anomalies: (ags, date, round, check) -> (message fragment, why)
ANOMALIES = {
    # thresholds the Gemeinde computed by hand, off by one; no outcome depends on them
    ("12066304", "2006-11-12", "stichwahl", "R6"): ("printed half 3733|printed erforderlich 3733",
                                                    "Senftenberg: half of 7 463 is 3 732; the winner has 5 493"),
    ("12072002", "2007-10-28", "stichwahl", "R6"): ("printed 15 % 886", "Am Mellensee: 0.15 * 5 900 = 885 exactly; the winner has 1 580 (also on the result report)"),
    ("12072340", "2003-11-16", "stichwahl", "R6"): ("printed 15 % 1101", "Rangsdorf: 15 % rounded down; the winner has 2 406"),
    ("12072477", "2003-11-16", "stichwahl", "R6"): ("printed 15 % 2068", "Zossen: 15 % rounded down; the winner has 2 663"),
    ("12073384", "2002-04-21", "hauptwahl", "R6"): ("printed 15 % 460", "Lychen: 0.15 * 3 060 = 459 exactly; nobody reached the majority anyway"),
    # percentages the page prints wrongly; the counts are consistent
    ("12064084", "2014-05-25", "hauptwahl", "R5"): ("printed 224,61", "Buckow (Märkische Schweiz): the single candidate's share "
                                                                       "is printed as 224,61 %, which no count on the page gives; Ja 575 + Nein 116 = gültige 691"),
    ("12066041", "2008-09-28", "hauptwahl", "R5"): (",00 !=", "Bronkow: the page prints every percentage as a whole number"),
    # electorates that differ between the two rounds; each page is consistent and
    # the winner clears the 15 % quorum on either figure
    ("12060181", "2003-11-16", "stichwahl", "C5"): ("electorate 15008 -> 14409", "Panketal: winner 2 753 votes, 15 % = 2 252 / 2 162"),
    ("12061112", "2009-10-11", "stichwahl", "C5"): ("electorate 4923 -> 5270", "Eichwalde: kept as printed (00_bb_lwl_scans_parse.py, result reports)"),
    ("12067180", "2008-10-12", "stichwahl", "C5"): ("electorate 1538 -> 1597", "Groß Lindow: winner 450 votes, 15 % = 231 / 240"),
    ("12070316", "2009-10-11", "stichwahl", "C5"): ("electorate 12377 -> 11162", "Pritzwalk: kept as printed (00_bb_lwl_scans_parse.py, result reports)"),
}

PROBLEMS = defaultdict(list)
PIN_USED = Counter()


def note(kind, msg):
    PROBLEMS[kind].append(msg)


def fail(key, check, msg):
    """A failed check: pinned anomalies are reported, anything else is fatal."""
    k = (key[0], str(key[1]), key[2], check)
    if k in ANOMALIES and any(fr in msg for fr in ANOMALIES[k][0].split("|")):
        PIN_USED[("anomaly",) + k] += 1
        note(f"anomaly_{check}", f"{' '.join(map(str, k[:3]))}: {msg} [{ANOMALIES[k][1]}]")
    else:
        note(f"FAIL_{check}", f"{' '.join(map(str, key))}: {msg}")


# ---------------------------------------------------------------------------
# helpers (same rules as 00_bb_lwl_scans_parse.py)
# ---------------------------------------------------------------------------

def fold(s):
    s = (s or "").replace("ß", "ss")
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode().lower()
    s = re.sub(r"\b(stadt|gemeinde)\b", " ", s)
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


def person_key(name):
    _, last, first, _ = split_name(name)
    return fold(last), (fold(first).split() or [""])[0]


def ival(x):
    x = (x or "").strip().replace(".", "").replace(" ", "")
    return int(x) if re.fullmatch(r"\d+", x) else None


def pct(x):
    x = (x or "").strip().replace(",", ".")
    try:
        return float(x), (len(x.split(".")[1]) if "." in x else 0)
    except ValueError:
        return None, 0


def tokens(s):
    s = re.sub(r"\b(Dr|Prof|Dipl|Ing|med|phil|rer|nat|oec|habil)\b\.?(-\w+\.?)?", " ", s or "")
    return set(fold(s).split())


def clean_name(s):
    """The 2014 pages put a salutation before the given name ("Muster, Herr Max",
    invented); one page writes "Muster , Max". Both are removed, as no other
    source prints them."""
    s = re.sub(r"\b(Herrn?|Frau)\b\.?\s*", " ", s or "")
    return norm_ws(re.sub(r"\s+,", ",", s))


def given_first(name):
    toks = name.split()
    return f"{toks[-1]}, {' '.join(toks[:-1])}" if len(toks) > 1 else name


def match_finding(part, cands):
    """The one Bewerber whose name tokens all occur in a finding entry
    ('Muster, Max (SPD)', invented); None if not unique."""
    t = tokens(re.sub(r"\(.*$", "", part)) or tokens(part)
    hit = [c for c in cands if tokens(c["name"]) and tokens(c["name"]) <= tokens(part)]
    if len(hit) != 1:
        hit = [c for c in cands if tokens(c["name"]) & t and fold(split_name(c["name"])[1]) in t]
    return hit[0] if len(hit) == 1 else None


def elected_rule(votes, valid, eligible):
    """§ 72 Abs. 2 BbgKWahlG: more than half of the gültige Stimmen AND at least
    15 % of the Wahlberechtigte."""
    return votes is not None and votes * 2 > valid and votes >= math.ceil(0.15 * eligible - 1e-9)


def sha256(path):
    return hashlib.sha256(open(path, "rb").read()).hexdigest()


# ---------------------------------------------------------------------------
# pages
# ---------------------------------------------------------------------------

class _Tables(HTMLParser):
    def __init__(self):
        super().__init__()
        self.tables, self.row, self.cell = [], None, None

    def handle_starttag(self, tag, attrs):
        if tag == "table":
            self.tables.append([])
        elif tag == "tr" and self.tables:
            self.row = []
        elif tag in ("td", "th") and self.row is not None:
            self.cell = []

    def handle_endtag(self, tag):
        if tag in ("td", "th") and self.cell is not None:
            self.row.append(" ".join("".join(self.cell).split()))
            self.cell = None
        elif tag == "tr" and self.row is not None:
            self.tables[-1].append(self.row)
            self.row = None

    def handle_data(self, data):
        if self.cell is not None:
            self.cell.append(data)


def read_page(path):
    """One capture -> a round dict, or None for a page without a result table."""
    raw = open(path, encoding="utf-8", errors="replace").read()
    p = _Tables()
    p.feed(raw)
    tabs = [t for t in p.tables if t]
    # page text with cell/paragraph boundaries kept as " | "
    txt = re.sub(r"(?s)<script.*?</script>|<style.*?</style>", " ", raw)
    txt = html.unescape(re.sub(r"<[^>]+>", " | ", txt))
    txt = re.sub(r"(\s*\|\s*)+", " | ", re.sub(r"[ \t\r\n]+", " ", txt))
    if len(tabs) != 3:
        return None
    head = {r[0]: r[1] for r in tabs[0] if len(r) >= 2}
    cnt = {}
    for r in tabs[1]:
        if len(r) >= 2 and r[0]:
            cnt[r[0]] = r[1:]
    def count(prefix):
        hits = [v for k, v in cnt.items() if k.startswith(prefix)]
        return hits[0] if hits else ["", ""]
    cands = []
    for r in tabs[2]:
        if len(r) == 5 and re.fullmatch(r"\d+", r[0]):
            name = clean_name(r[1])
            if "," not in name:          # "Max Muster" (invented): Vorname first
                name = given_first(name)
                note("given_name_first", os.path.basename(path))
            cands.append(dict(lfd=int(r[0]), name=name, party=norm_ws(r[2]),
                              votes=ival(r[3]), pct=r[4]))
    m = re.search(r"Ergebnis der (\S+) in (.+?) \| Wahltag", txt)
    f = re.search(r"stellte fest, dass \|?(.*?)\| (?:nach oben|Seite drucken)", txt)
    finding = clean_name((f.group(1) if f else "").strip(" |")).replace(" | ", "|").replace("| ", "|").replace(" |", "|")
    parts = [x.strip() for x in finding.split("|") if x.strip()]
    outcome, elected, runoff = "", "", []
    if re.search(r"zum neuen (Ober)?[Bb]ürgermeister gewählt worden ist", finding):
        outcome = "elected"
        i = next((k for k, x in enumerate(parts) if x.startswith("die/der Bewerber")), None)
        rest = finding.split("die/der Bewerber", 1)[1] if "die/der Bewerber" in finding else ""
        elected = norm_ws(rest.split("die erforderliche Stimmenzahl")[0].replace("|", " "))
    elif "Für die Stichwahl" in finding:
        outcome = "runoff"
        after = finding.split("zugelassen:", 1)[1] if "zugelassen:" in finding else ""
        runoff = [norm_ws(x) for x in after.split("|") if x.strip()]
    elif re.search(r"Wahl des (Ober)?[Bb]ürgermeisters durch die", finding):
        outcome = "council"
    return dict(
        title=m.group(1) if m else "", gemeinde_title=m.group(2) if m else "",
        wahltag=head.get("Wahltag", ""), landkreis=head.get("Landkreis", ""),
        amt=head.get("Amt", ""), gemeinde=head.get("Gemeinde") or head.get("Kreisfreie Stadt", ""),
        eligible=ival(count("Wahlberechtigte")[0]), voters=ival(count("Wähler")[0]),
        voters_pct=count("Wähler")[1] if len(count("Wähler")) > 1 else "",
        invalid=ival(count("Ungültige")[0]), invalid_pct=count("Ungültige")[1] if len(count("Ungültige")) > 1 else "",
        valid=ival(count("Gültige")[0]), valid_pct=count("Gültige")[1] if len(count("Gültige")) > 1 else "",
        nein=ival(count("darunter Nein")[0]),
        half=ival(count("Stimmenzahl, die mehr als")[0]), q15=ival(count("Stimmenzahl, die 15")[0]),
        required=ival(count("Erforderliche")[0]),
        cands=cands, outcome=outcome, elected_text=elected, runoff_list=runoff)


def reconciles(r):
    single = len(r["cands"]) == 1
    tot = sum(c["votes"] or 0 for c in r["cands"]) + ((r["nein"] or 0) if single else 0)
    return r["valid"] is not None and tot == r["valid"]


def load_pages():
    """{(ags, date, round): round} from every capture; two captures of one round
    must agree in every field."""
    man = {r["file"]: r for r in csv.DictReader(open(MANIFEST, encoding="utf-8"))}
    files = sorted(f for f in os.listdir(PAGES) if f.endswith(".html"))
    if set(files) != set(man):
        sys.exit(f"FATAL: pages and _capture_manifest.csv differ: "
                 f"{sorted(set(files) ^ set(man))[:5]}")
    rounds, empty = {}, 0
    for f in files:
        if sha256(os.path.join(PAGES, f)) != man[f]["sha256"]:
            sys.exit(f"FATAL: {f} does not match its sha256 in the manifest")
        ags10, page, cap = man[f]["ags10"], man[f]["page"], man[f]["capture"]
        r = read_page(os.path.join(PAGES, f))
        if r is not None and (not r["cands"] or None in (r["eligible"], r["voters"], r["valid"])):
            r = None                      # a form without counts or Bewerber
        if r is None:
            k = (ags10, page, cap)
            if k in EMPTY_PAGES:
                PIN_USED[("empty",) + k] += 1
                note("empty_page_skipped", f"{f} [{EMPTY_PAGES[k]}]")
            else:
                note("FAIL_page", f"{f}: no result (no table, counts or Bewerber)")
            empty += 1
            continue
        rnd = "stichwahl" if "stichwahl" in r["title"].lower() else "hauptwahl"
        if (page == "s") != (rnd == "stichwahl"):
            note("FAIL_page", f"{f}: page '{page}' titled '{r['title']}'")
        try:
            d = dt.datetime.strptime(r["wahltag"], "%d.%m.%Y").date()
        except ValueError:
            note("FAIL_page", f"{f}: Wahltag '{r['wahltag']}'")
            continue
        r.update(ags10=ags10, ags=ags10[:8], date=d, round=rnd, capture=cap, file=f,
                 url=f"wahlen.brandenburg.de/bmwahlen/{ags10}/{page}, Internet Archive {cap}")
        for c in r["cands"]:
            k = (r["ags"], d.isoformat(), c["name"])
            if k in NAME_FIXES:
                PIN_USED[("name",) + k] += 1
                fixed = NAME_FIXES[k]
                r["elected_text"] = r["elected_text"].replace(c["name"], fixed)
                r["runoff_list"] = [x.replace(c["name"], fixed) for x in r["runoff_list"]]
                c["name"] = fixed
        key = (r["ags"], d, rnd)
        if key in rounds:
            a, b = rounds[key], r
            same = all(a[k] == b[k] for k in ("eligible", "voters", "invalid", "valid", "nein",
                                              "outcome", "elected_text", "runoff_list")) and \
                [(c["lfd"], c["name"], c["party"], c["votes"]) for c in a["cands"]] == \
                [(c["lfd"], c["name"], c["party"], c["votes"]) for c in b["cands"]]
            if not same:
                # a capture taken while the page was incomplete: keep the one whose
                # votes add up to the gültige Stimmen, if exactly one does
                ok = [x for x in (a, b) if reconciles(x)]
                if len(ok) == 1:
                    note("capture_incomplete_superseded", f"{key[0]} {d} {rnd}: "
                         f"{(b if ok[0] is a else a)['file']} superseded by {ok[0]['file']}")
                    rounds[key] = ok[0]
                else:
                    note("FAIL_captures_differ", f"{key[0]} {d} {rnd}: {a['file']} vs {f}")
            continue                      # otherwise keep the earliest capture
        rounds[key] = r
    return rounds, empty


# ---------------------------------------------------------------------------
# reference data
# ---------------------------------------------------------------------------

def load_crosswalks():
    names = defaultdict(dict)
    with open(CW_CSV, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            a = r["ags"].strip().zfill(8)
            if a.startswith(STATE):
                names[int(r["year"])][a] = r["ags_name"]
    to25, best = {}, {}
    with open(CW25_CSV, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            a = r["ags"].strip().zfill(8)
            if not a.startswith(STATE):
                continue
            k = (int(r["year"]), a)
            w = float(r["pop_cw"] or 0)
            if k not in best or w > best[k]:
                best[k], to25[k] = w, r["ags_25"].strip().zfill(8)
    return names, to25


def load_gerda(path, src):
    """{(ags, date, round): [rows]} of a Brandenburg Stage-0 file."""
    out = defaultdict(list)
    if not os.path.exists(path):
        return out
    with open(path, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            r["_src"] = src
            out[(r["ags"], dt.date.fromisoformat(r["election_date"]), r["round"])].append(r)
    return out


def num(x):
    x = (x or "").strip()
    return int(float(x)) if x not in ("", "NA") else None


# ---------------------------------------------------------------------------
# checks
# ---------------------------------------------------------------------------

def check_round(r):
    key = (r["ags"], r["date"], r["round"])
    el, vo, inv, va, cs = r["eligible"], r["voters"], r["invalid"], r["valid"], r["cands"]
    if None in (el, vo, inv, va) or not cs:
        fail(key, "R0", "a count or the Bewerber list is missing")
        return
    if r["date"].weekday() != 6:
        fail(key, "R1", f"{r['date']} is a {r['date'].strftime('%A')}")
    if va + inv != vo:
        fail(key, "R2", f"gültige {va} + ungültige {inv} != Wähler {vo}")
    if vo > el:
        fail(key, "R3", f"Wähler {vo} > Wahlberechtigte {el}")
    single = len(cs) == 1
    tot = sum(c["votes"] or 0 for c in cs) + ((r["nein"] or 0) if single else 0)
    if any(c["votes"] is None for c in cs) or tot != va:
        fail(key, "R4", f"votes sum {tot} != gültige {va}")
    for label, n, d, t in (("turnout", vo, el, r["voters_pct"]), ("ungültige", inv, vo, r["invalid_pct"]),
                           ("gültige", va, vo, r["valid_pct"])):
        p, dp = pct(t)
        if p is not None and d and abs(round(100 * n / d, dp) - p) > 10 ** -dp / 2 + 1e-9:
            fail(key, "R5", f"{label} printed {t} != {100 * n / d:.3f}")
    for c in cs:
        p, dp = pct(c["pct"])
        if p is not None and c["votes"] is not None and va and \
                abs(round(100 * c["votes"] / va, dp) - p) > 10 ** -dp / 2 + 1e-9:
            fail(key, "R5", f"lfd {c['lfd']} printed {c['pct']} != {100 * c['votes'] / va:.3f}")
    half, q15 = va // 2 + 1, math.ceil(0.15 * el - 1e-9)
    if r["half"] is not None and r["half"] != half:
        fail(key, "R6", f"printed half {r['half']} != {half}")
    if r["q15"] is not None and r["q15"] != q15:
        fail(key, "R6", f"printed 15 % {r['q15']} != {q15}")
    if r["required"] is not None and r["required"] != max(half, q15):
        fail(key, "R6", f"printed erforderlich {r['required']} != {max(half, q15)}")
    top = max(cs, key=lambda c: c["votes"] or -1)
    ranked = sorted(cs, key=lambda c: -(c["votes"] or -1))
    meets = elected_rule(top["votes"], va, el)
    if r["outcome"] == "elected":
        hit = match_finding(r["elected_text"], cs)
        if hit is None:
            fail(key, "R7", "the elected person of the finding is not on the list")
        elif hit is not top or not meets:
            fail(key, "R7", "the elected person is not the top Bewerber above both thresholds")
    elif r["outcome"] == "runoff":
        named = [match_finding(x, cs) for x in r["runoff_list"]]
        if r["round"] != "hauptwahl" or meets:
            fail(key, "R7", "a runoff although the leader meets both thresholds, or after a runoff")
        if None in named or {c["lfd"] for c in named} != {c["lfd"] for c in ranked[:2]}:
            fail(key, "R7", "the runoff Bewerber are not the top two")
    elif r["outcome"] == "council":
        if meets:
            fail(key, "R7", "the council elects although the leader meets both thresholds")
    else:
        fail(key, "R7", "no finding")


def check_ags(r, names):
    y = min(r["date"].year, 2020)
    cw = names[y]
    key = (r["ags"], r["date"], r["round"])
    if r["ags"] not in cw:
        fail(key, "AGS", f"{r['ags']} is not a Gemeinde in {y}")
    elif fold(r["gemeinde"]) not in fold(cw[r["ags"]]) and fold(cw[r["ags"]]) not in fold(r["gemeinde"]):
        fail(key, "AGS", f"{r['ags']} is '{cw[r['ags']]}' in {y}, the page says '{r['gemeinde']}'")


def build_cycles(rounds):
    by_ags = defaultdict(list)
    for r in rounds:
        by_ags[r["ags"]].append(r)
    cycles = []
    for a, rs in sorted(by_ags.items()):
        rs.sort(key=lambda r: (r["date"], r["round"]))
        for r in rs:
            if r["round"] == "stichwahl" and cycles and cycles[-1]["ags"] == a and \
                    cycles[-1]["rounds"][-1]["round"] == "hauptwahl" and \
                    0 < (r["date"] - cycles[-1]["rounds"][-1]["date"]).days <= 42:
                cycles[-1]["rounds"].append(r)
            else:
                cycles.append(dict(ags=a, rounds=[r]))
    return cycles


def check_cycle(c):
    hw = [r for r in c["rounds"] if r["round"] == "hauptwahl"]
    sw = [r for r in c["rounds"] if r["round"] == "stichwahl"]
    if hw and sw:
        h, s = hw[0], sw[0]
        key = (c["ags"], s["date"], "stichwahl")
        gap = (s["date"] - h["date"]).days
        if gap not in (14, 21, 28, 35):
            fail(key, "C1", f"runoff {gap} days after the Hauptwahl")
        elif gap > 21:
            note("runoff_after_4_or_5_weeks", f"{c['ags']} {h['date']}: {gap} days")
        if h["outcome"] != "runoff":
            fail(key, "C2", f"a runoff after a Hauptwahl with outcome '{h['outcome']}'")
        named = [match_finding(x, s["cands"]) for x in h["runoff_list"]]
        if None in named or len({x["lfd"] for x in named}) != len(s["cands"]):
            fail(key, "C2", "the runoff Bewerber differ from those the Hauptwahl admitted")
        if abs(s["eligible"] - h["eligible"]) > 0.03 * h["eligible"]:
            fail(key, "C5", f"electorate {h['eligible']} -> {s['eligible']}")


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main():
    for p in (MANIFEST, LWL_CSV, AFS_CSV, CW_CSV, CW25_CSV):
        if not os.path.exists(p):
            sys.exit(f"FATAL: missing input {p}")
    names, to25 = load_crosswalks()
    rounds, n_empty = load_pages()
    print(f"Captures: {len(os.listdir(PAGES))} pages, {len(rounds)} distinct rounds, {n_empty} without a result")
    for r in rounds.values():
        check_round(r)
        check_ags(r, names)

    lwl = load_gerda(LWL_CSV, "workbook")
    scan = load_gerda(SCAN_CSV, "scans")
    afs = load_gerda(AFS_CSV, "afs")
    # the workbook is on the 2026 Gebietsstand: compare it through the crosswalk
    def a25(ags, d):
        return to25.get((min(d.year, 2020), ags), ags)
    full = defaultdict(dict)            # {ags: {(date, round): rows}} rounds with counts
    for src in (scan, afs):
        for (a, d, rnd), rows in src.items():
            if rows[0]["valid_votes"] not in ("", "NA"):
                full[a][(d, rnd)] = rows
    lwl25 = defaultdict(dict)
    for (a, d, rnd), rows in lwl.items():
        lwl25[a][(d, rnd)] = rows

    def gerda_full(r):
        """GERDA rounds with counts of this Gemeinde: {(date, round): rows}."""
        out = dict(full.get(r["ags"], {}))
        out.update(lwl25.get(a25(r["ags"], r["date"]), {}))
        return out

    # misdated pages: counts equal a GERDA round of the same Gemeinde on another date
    for key in sorted(rounds):
        r = rounds[key]
        fp = (r["eligible"], r["voters"], r["valid"])
        for (d, rnd), rows in gerda_full(r).items():
            g = rows[0]
            if d != r["date"] and (num(g["eligible_voters"]), num(g["number_voters"]), num(g["valid_votes"])) == fp:
                k = (r["ags"], r["date"].isoformat(), r["round"])
                if k in MISDATED and MISDATED[k][0] == d.isoformat():
                    PIN_USED[("misdated",) + k] += 1
                    note("site_misdated_left_out", f"{' '.join(k)} = GERDA {d} {rnd} [{MISDATED[k][1]}]")
                else:
                    note("FAIL_misdated", f"{' '.join(k)}: counts equal GERDA {d} {rnd} ({g['_src']})")
                r["misdated"] = True
                break
    cycles = build_cycles([r for r in rounds.values() if not r.get("misdated")])
    for c in cycles:
        check_cycle(c)

    # GERDA cycles of the B VII 3-6 file, for the replacement
    afs_by_ags = defaultdict(list)
    for (a, d, rnd), rows in afs.items():
        afs_by_ags[a].append((d, rnd, rows))
    def near(c, dates):
        return any(abs((r["date"] - d).days) <= 42 for r in c["rounds"] for d in dates)

    out_rows, supersede = [], []
    stats = Counter()
    for c in cycles:
        a, rs = c["ags"], c["rounds"]
        tag = f"{a} {rs[0]['date']}"
        # 1. held with counts by the workbook, the result reports or a B VII 3-6 table
        gf = {}
        for r in rs:
            gf.update(gerda_full(r))
        if near(c, [d for (d, _r) in gf]):
            for r in rs:
                g = gf.get((r["date"], r["round"]))
                key = (a, r["date"], r["round"])
                if not g:
                    note("FAIL_overlap", f"{tag}: GERDA holds this election, but not its {r['round']} of {r['date']}")
                    continue
                g0 = g[0]
                for f_arc, f_g in (("eligible", "eligible_voters"), ("voters", "number_voters"),
                                   ("valid", "valid_votes"), ("invalid", "invalid_votes")):
                    if r[f_arc] != num(g0[f_g]):
                        k = (a, r["date"].isoformat(), r["round"], f_arc, str(r[f_arc]), str(num(g0[f_g])))
                        if k in OVERLAP_PINS:
                            PIN_USED[("overlap",) + k] += 1
                            note("overlap_pinned", f"{' '.join(k)} [{OVERLAP_PINS[k]}]")
                        else:
                            note("FAIL_overlap", f"{' '.join(k[:4])}: archive {k[4]}, GERDA {k[5]} ({g0['_src']})")
                if sorted(x["votes"] for x in r["cands"]) != sorted(num(x["candidate_votes"]) for x in g if x["candidate_votes"] not in ("", "NA")):
                    note("FAIL_overlap", f"{tag} {r['round']}: Bewerber votes differ from GERDA ({g0['_src']})")
            stats["held_with_counts"] += 1
            continue
        # 2. held winner-only by a B VII 3-6 report: replace
        ac = [(d, rnd, rows) for (d, rnd, rows) in afs_by_ags.get(a, []) if near(c, [d])]
        office = ""
        carry = []
        afs_council = False
        if ac:
            rows_ac = [x for (_d, _r, rows) in ac for x in rows]
            if not all(x["winner_only"] == "TRUE" for x in rows_ac):
                note("FAIL_afs_mixed", f"{tag}: the B VII 3-6 election has counts in part only")
                continue
            offices = {x["office"] for x in rows_ac}
            if len(offices) != 1:
                note("FAIL_afs_office", f"{tag}: offices {offices}")
                continue
            office = offices.pop()
            if any(x["elected_by_council"] == "TRUE" for x in rows_ac):
                # the report records the Gemeindevertretung's choice: the archived
                # popular vote must have seated nobody (its runoff is not archived,
                # or failed); the rounds are kept without a winner, as in the
                # result reports, and the council record is replaced
                if not all(x["elected_by_council"] == "TRUE" for x in rows_ac) or rs[-1]["outcome"] == "elected":
                    note("FAIL_afs_council", f"{tag}: B VII 3-6 records a council choice, the archive an elected person")
                    continue
                afs_council = True
                for (d, rnd, rows) in ac:
                    supersede.append(dict(ags=a, election_date=d.isoformat(), round=rnd, source=rows[0]["source"]))
                note("council_after_failed_vote", f"{tag}: B VII 3-6 records the council's choice "
                                                  f"({rows_ac[0]['source']}); last archived round: {rs[-1]['round']} ({rs[-1]['outcome']})")
                stats["council_after_failed_vote"] += 1
                ac = []
        if afs_council:
            pass
        elif ac:
            winners = [x for x in rows_ac if x["is_winner"] == "TRUE"]
            wk = {person_key(x["candidate_name"]) for x in winners}
            if len(wk) != 1:
                note("FAIL_afs_winner", f"{tag}: the B VII 3-6 report names {len(wk)} elected persons")
                continue
            afs_winner = winners[0]
            last = rs[-1]
            if last["outcome"] == "elected":
                arc_w = match_finding(last["elected_text"], last["cands"])
                ok = arc_w is not None and person_key(arc_w["name"]) in wk
            elif last["outcome"] == "runoff":          # the runoff page is not archived
                q = [match_finding(x, last["cands"]) for x in last["runoff_list"]]
                ok = any(x is not None and person_key(x["name"]) in wk for x in q)
            else:
                ok = False
            if not ok:
                note("FAIL_winner_differs", f"{tag}: the archive's elected person is not the one B VII 3-6 names")
                continue
            arc_dates = {r["date"] for r in rs}
            for (d, rnd, rows) in ac:
                supersede.append(dict(ags=a, election_date=d.isoformat(), round=rnd, source=rows[0]["source"]))
                # a round the archive does not hold is carried over (a runoff not
                # captured; the result-less first round of a winner-only runoff cycle)
                if d not in arc_dates:
                    carry.append((d, rnd, rows))
                elif rnd != next(r["round"] for r in rs if r["date"] == d):
                    note("afs_round_relabelled", f"{tag}: B VII 3-6 files its {rnd} of {d}; the archive "
                                                 f"labels that day {next(r['round'] for r in rs if r['date'] == d)}")
            stats["replaces_winner_only"] += 1
            if carry:
                stats["with_carried_round"] += 1
        else:
            # 3. new: not in GERDA. Not under another code either (same day and name)
            for (k, rows) in list(afs.items()) + list(scan.items()):
                if k[0] != a and k[1] in {r["date"] for r in rs} and \
                        fold(rows[0]["ags_name"]) == fold(rs[0]["gemeinde"]):
                    note("FAIL_new_elsewhere", f"{tag}: GERDA has {rows[0]['ags_name']} on {k[1]} under {k[0]}")
            # office: that of the Gemeinde's latest earlier election in GERDA (the
            # workbook holds hauptamtliche only), else of its next one. An office
            # changes only with the Gemeinde's administration (Falkenberg/Elster:
            # hauptamtlich 2009, ehrenamtlich in 2024), so the nearest date alone
            # would be wrong.
            offs = sorted({(k[1], rows[0].get("office") or "H") for src in (afs, scan)
                           for (k, rows) in src.items() if k[0] == a and not rows[0]["first_round_no_result"] == "TRUE"}
                          | {(d, "H") for (d, _r) in lwl25.get(a25(a, rs[0]["date"]), {})})
            before = [o for (d, o) in offs if d < rs[0]["date"]]
            after = [o for (d, o) in offs if d > rs[-1]["date"]]
            if a in KREISFREIE:
                office = "H"
            elif before or after:
                office = before[-1] if before else after[0]
                note("office_of_added_election", f"{tag}: {office} (from GERDA's "
                     f"{'previous' if before else 'next'} election in this Gemeinde)")
            else:
                note("FAIL_office", f"{tag}: no other GERDA election of this Gemeinde gives the office")
                continue
            stats["added"] += 1

        # ---- rows of the election ------------------------------------------
        hw = [r for r in rs if r["round"] == "hauptwahl"]
        sw = [r for r in rs if r["round"] == "stichwahl"]
        last = rs[-1]
        flag_missing = last["round"] == "hauptwahl" and last["outcome"] == "runoff" and \
            not any(rnd == "stichwahl" for (_d, rnd, _rows) in carry)
        council = last["outcome"] == "council" or afs_council
        if flag_missing and not council:
            note("seated_nobody_runoff_not_archived", f"{tag}")
        if council:
            note("council_elects", f"{tag} ({last['round']})")
        if sw and not hw and not carry:
            note("runoff_only_cycle", f"{tag}: the first round is not archived and not in GERDA")
        # person ids: the Hauptwahl's lfd; a runoff Bewerber gets the lfd of the
        # same person's Hauptwahl row (the Hauptwahl finding's list of qualifiers)
        pid_of = {}
        base = f"{a}_{(min([r['date'] for r in rs] + [d for (d, _r, _x) in carry])).isoformat()}_"
        if hw:
            for x in hw[0]["cands"]:
                pid_of[id(x)] = base + str(x["lfd"])
            if sw:
                for part in hw[0]["runoff_list"]:
                    h_, s_ = match_finding(part, hw[0]["cands"]), match_finding(part, sw[0]["cands"])
                    if h_ is not None and s_ is not None:
                        pid_of[id(s_)] = base + str(h_["lfd"])
                        if norm_ws(h_["name"]) != norm_ws(s_["name"]):
                            note("name_differs_between_rounds", f"{tag} lfd {h_['lfd']}/{s_['lfd']} (paired by pair_key)")
        elif sw:
            for x in sw[0]["cands"]:
                pid_of[id(x)] = base + "s" + str(x["lfd"])
        # the elected person (decisive round), by row
        winner = None
        if not flag_missing and not council:
            if last["outcome"] == "elected":
                winner = match_finding(last["elected_text"], last["cands"])
            elif carry and ac:                      # decisive runoff carried over from B VII 3-6
                q = [match_finding(x, last["cands"]) for x in last["runoff_list"]]
                winner = next((x for x in q if x is not None and person_key(x["name"]) in wk), None)
        win_pid = pid_of.get(id(winner)) if winner is not None else None
        filing = min([r["date"] for r in rs] + [d for (d, _r, _x) in carry])
        for r in rs:
            top = max(r["cands"], key=lambda x: x["votes"])
            for x in r["cands"]:
                full_n, last_n, first_n, title = split_name(x["name"] if "," in x["name"] else given_first(x["name"]))
                pk = pid_of.get(id(x), "")
                mine = (winner is not None and pk and pk == win_pid) or (x is winner)
                out_rows.append({
                    "ags": a, "ags_name": rs[0]["gemeinde"],
                    "state": STATE, "state_name": STATE_NAME,
                    "election_year": r["date"].year, "election_date": r["date"].isoformat(),
                    "election_type": "Oberbürgermeisterwahl" if "Ober" in r["title"] else "Bürgermeisterwahl",
                    "round": r["round"],
                    "eligible_voters": r["eligible"], "number_voters": r["voters"],
                    "valid_votes": r["valid"], "invalid_votes": r["invalid"],
                    "turnout": r["voters"] / r["eligible"],
                    "candidate_name": full_n, "candidate_last_name": last_n,
                    "candidate_first_name": first_n, "candidate_title": title,
                    "candidate_party": x["party"], "wahlvorschlag_art": "",
                    "candidate_votes": x["votes"], "candidate_voteshare": x["votes"] / r["valid"],
                    # the top Bewerber of a round that elected someone; a Hauptwahl
                    # that went to a runoff elected nobody (with a winner-only runoff
                    # carried over, its leader has no runoff row to overrule it)
                    "is_winner": "" if (flag_missing or council) else
                                 ("TRUE" if (x is top and r["outcome"] == "elected") else "FALSE"),
                    "source": "lwl_archive", "source_url": r["url"],
                    "office": office, "winner_only": "FALSE",
                    "flag_decisive_round_missing": "TRUE" if (flag_missing or council) else "FALSE",
                    "name_restricted": "FALSE" if mine else "TRUE",
                    "preliminary": "FALSE", "first_round_no_result": "FALSE",
                    "elected_by_council": "TRUE" if (council and r["date"] == filing) else "FALSE",
                    "pair_key": pk if (hw and sw) or carry else "",
                })
        for (d, rnd, rows) in carry:                # winner-only rounds from B VII 3-6
            for x in rows:
                row = {k: v for k, v in x.items() if not k.startswith("_")}
                row["pair_key"] = win_pid or ""
                out_rows.append(row)
            note("afs_round_carried_over", f"{tag}: {rnd} {d} ({rows[0]['source']}, winner-only)")
        for (d, rnd, _rows) in carry:                # C1 for a carried-over round
            other = [r["date"] for r in rs if r["round"] != rnd]
            gaps = [abs((d - o).days) for o in other]
            if not gaps or min(gaps) not in (14, 21, 28, 35):
                fail((a, d, rnd), "C1", f"carried-over {rnd} {d} is {gaps} days from the archived round")
            elif min(gaps) > 21:
                note("runoff_after_4_or_5_weeks", f"{tag}: {min(gaps)} days (carried-over {rnd})")
        if carry and not win_pid:
            note("FAIL_carry_unpaired", f"{tag}: a carried-over round cannot be linked to the archived rounds")

    stale = ([k for k in ANOMALIES if not PIN_USED[("anomaly",) + k]]
             + [k for k in MISDATED if not PIN_USED[("misdated",) + k]]
             + [k for k in OVERLAP_PINS if not PIN_USED[("overlap",) + k]]
             + [k for k in EMPTY_PAGES if not PIN_USED[("empty",) + k]]
             + [k for k in NAME_FIXES if not PIN_USED[("name",) + k]])
    for k in stale:
        note("FAIL_stale_pin", f"{k}: the pin no longer fires; review it")
    print("\n--- source defects, exclusions & anomalies ---")
    for k in sorted(PROBLEMS):
        print(f"  [{k}] {len(PROBLEMS[k])}")
        for msg in PROBLEMS[k][:60]:
            print(f"      {msg}")
    print("\nelections:", dict(stats))
    fatal = sorted(k for k in PROBLEMS if k.startswith("FAIL_"))
    if fatal:
        sys.exit(f"\nFATAL: {fatal} — investigate, then fix or pin the case before publishing.")

    cols = list(out_rows[0].keys())
    with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        w.writerows(out_rows)
    with open(SUP_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=["ags", "election_date", "round", "source"])
        w.writeheader()
        w.writerows(sorted(supersede, key=lambda x: (x["ags"], x["election_date"], x["round"])))
    n_r = len({(r["ags"], r["election_date"], r["round"]) for r in out_rows})
    print(f"\nWrote {len(out_rows)} candidate rows ({n_r} rounds) -> {OUT_CSV}")
    print(f"  {len(supersede)} B VII 3-6 rounds replaced -> {SUP_CSV}")
    print("  by year of the first round:", dict(sorted(Counter(
        int(r["election_date"][:4]) for r in out_rows if r["source"] == "lwl_archive").items())))


if __name__ == "__main__":
    main()
