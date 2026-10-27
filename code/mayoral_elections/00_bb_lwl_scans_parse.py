#!/usr/bin/env python3
"""
Stage 0 — Brandenburg hauptamtliche Bürgermeister-/Oberbürgermeisterwahlen
2004-2013 from the Gemeinden's result reports in the Landeswahlleiter's archive.

WHY
    The Landeswahlleiter workbook (00_bb_lwl_parse.py) starts in 2010 and is
    incomplete before 2014, when digital reporting became compulsory; the
    Statistische Berichte B VII 3-6 (00_bb_afs_reports_parse.py) cover 1993-2003
    and the general election days only. The hauptamtliche elections of 2004-2009
    were therefore missing, apart from four of 28.09.2008. On 2026-10-15 the
    Geschäftsstelle des Landeswahlleiters (C. Sokol, "hh* 2025/1211", Teil 1-4)
    sent scans of the paper result reports (Ergebnismeldungen) the Gemeinden had
    filed with it, one PDF per Landkreis / for the four kreisfreie Städte.

SOURCE  (data/mayoral_elections/raw/brandenburg/lwl_ergebnismeldungen_2004_2013/,
         README and _source_manifest.csv there; the PDFs are gitignored)
    15 PDFs, 225 A4 pages, one round per page, in two layouts: the LWL's
    electronic form ("Landeswahlleiter Brandenburg - Ergebnis der
    Bürgermeisterwahl", with AGS and Vorgangsnummer, from 2006) and the older
    spreadsheet form ("Ergebnis der Bürgermeisterwahl am ...", Landkreis / Amt /
    Gemeinde, no AGS). Both print Wahlberechtigte, Wähler, ungültige and gültige
    Stimmen, the two thresholds of § 72 Abs. 2 BbgKWahlG, every Bewerber with
    Wahlvorschlagsträger and votes, and the Wahlausschuss's finding (who was
    elected, or who goes to the runoff).

TRANSCRIPTION  (transcribed/, gitignored: it names losing candidates)
    bb_lwl_scans_rounds.csv      one row per page (round)
    bb_lwl_scans_candidates.csv  one row per Bewerber per page
    The scans are 144-dpi JPEGs with an OmniPage OCR layer. Two independent
    reads were made: an automatic parse of the OCR layer, and a visual read of
    every page from zoomed crops. Every field on which they disagreed, and every
    page the OCR layer could not be parsed on, was settled against the image
    (`read_status`). A blind third read of 71 pages (all disputed and pinned
    pages plus a random sample) reproduced every count, vote, date, AGS, name and
    finding (`third_read`). The figures below are then re-derived from the
    page's own printed subtotals; a transcription slip aborts the script.
    One round is not on the scans: the Hauptwahl of Schöneiche bei Berlin,
    22.04.2012, transcribed from the Landeswahlleiter's own result page as
    archived by the Internet Archive (`source_ref`; the HTML is kept in
    wayback/, gitignored).

CHECKS  (each one fails the script; known source anomalies are pinned below)
    per round
      R1  the date is a Sunday
      R2  gültige + ungültige = Wähler
      R3  Wähler <= Wahlberechtigte
      R4  the Bewerber's votes sum to the gültige Stimmen (Ja/Nein ballot of a
          single Bewerber: Ja + Nein = gültige)
      R5  printed percentages (turnout, ungültige, gültige, each Bewerber) equal
          the recomputed ones to the printed precision
      R6  the printed thresholds equal floor(gültige / 2) + 1 and
          ceil(0.15 * Wahlberechtigte), and "erforderlich" is the larger; the
          printed "Summe Stimmenanzahl" equals the gültige Stimmen
      R7  the finding agrees with the votes: an elected Bewerber is the top of
          the round with > 50 % of the gültige Stimmen AND >= 15 % of the
          Wahlberechtigte (§ 72 Abs. 2 BbgKWahlG); a runoff names the top two
    per cycle
      C1  a Stichwahl follows its Hauptwahl by 2-5 weeks at the same AGS (§ 72
          Abs. 2 BbgKWahlG 2001/2004: "frühestens am zweiten und spätestens am
          fünften Sonntag"), on the date the Hauptwahl finding prints
      C2  the runoff Bewerber are the two the Hauptwahl finding admitted
      C3  a Hauptwahl without an elected Bewerber has its runoff here, in the
          workbook, or carries flag_decisive_round_missing
      C4  a decisive round that elects nobody (a runoff leader below the 15 %
          quorum) seats nobody by popular vote: flag_decisive_round_missing on
          every row of the cycle. After a failed runoff the Gemeindevertretung
          elects (§ 72 Abs. 4 BbgKWahlG; the rule the Landrat pipeline applies),
          so elected_by_council marks the cycle, on its first-round rows only:
          01b files the wide cycle under that date, and 01 keeps both real
          rounds (it drops only winner-only council records, i.e. those of the
          B VII 3-6 reports). No such cycle in the delivery; the path is tested
          by mutation.
      C5  the electorate of a runoff is within 3 % of its Hauptwahl's

DEDUP AND VALIDATION
    * Landeswahlleiter workbook (bb_lwl_parsed.csv, on the 2026 Gebietsstand):
      an election with a round within 60 days at the same Gemeinde (the scan's
      AGS taken to its 2025 code, data/crosswalks/final/
      ags_1990_to_2025_crosswalk.csv) is left out; the workbook keeps it. Each
      such round is compared field by field (Wahlberechtigte, Wähler, gültige,
      ungültige, every Bewerber's votes); a round the workbook lacks, or a
      differing count or vote, FAILS the script unless pinned in OVERLAP_PINS
      (the workbook would otherwise silently replace a fuller or different
      record). Spelling differences of names are reported only. All findings go
      to _work/overlap_findings.csv.
    * B VII 3-6 reports (bb_afs_parsed.csv): those rows are winner-only, so the
      full results here supersede them. 00_bb_afs_reports_parse.py reads this
      file and drops its own copy of an election held here (run order below);
      it checks that both name the same elected person.

AGS
    The AGS valid on the election day: the electronic form prints it (the first
    8 digits of the printed 10-digit key; the 2018 form prints the 12-digit
    Regionalschlüssel); the older form prints only the Gemeinde, which is looked
    up by name within its Landkreis in data/crosswalks/final/ags_crosswalks.csv
    for the election year (it must be unique). Every printed AGS is checked against the
    crosswalk name, so a misread digit cannot move an election to another
    Gemeinde. All 2004-2013 rows lie after the 2003 Gemeindegebietsreform.

OUTPUT
    data/mayoral_elections/raw/brandenburg/bb_lwl_scans_parsed.csv (gitignored)
    One row per Bewerber per round, the schema of bb_afs_parsed.csv:
    office "H"; winner_only FALSE (TRUE on a result-less first-round row, whose
    field is unknown); is_winner TRUE for the top-voted Bewerber of
    each round (the workbook's convention; 01b takes the runoff winner where a
    runoff exists) and NA on a cycle that seated nobody; name_restricted FALSE
    only on the elected person's own rows (the top row of the decisive round, its
    paired first-round row and the result-less first-round copy), TRUE for every
    other Bewerber (restricted twin only).

Run order:  00_bb_lwl_parse.py -> 00_bb_lwl_scans_parse.py ->
            00_bb_afs_reports_parse.py -> 01 / 01b
Usage:      python3 code/mayoral_elections/00_bb_lwl_scans_parse.py [--root DIR]
"""

import argparse
import csv
import datetime as dt
import importlib.util
import math
import os
import re
import sys
import unicodedata
from collections import Counter, defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))

ap = argparse.ArgumentParser()
ap.add_argument("--root", default=os.path.abspath(os.path.join(HERE, "..", "..")),
                help="repository root (default: two levels above this script)")
ap.add_argument("--transcribed", default=None,
                help="read the transcription from DIR and write nothing (calibration runs)")
ARGS = ap.parse_args()
ROOT = ARGS.root

RAW_DIR = os.path.join(ROOT, "data", "mayoral_elections", "raw", "brandenburg")
SRC_DIR = os.path.join(RAW_DIR, "lwl_ergebnismeldungen_2004_2013")
TR_DIR = ARGS.transcribed or os.path.join(SRC_DIR, "transcribed")
ROUNDS_CSV = os.path.join(TR_DIR, "bb_lwl_scans_rounds.csv")
CANDS_CSV = os.path.join(TR_DIR, "bb_lwl_scans_candidates.csv")
WORK_DIR = os.path.join(SRC_DIR, "_work")
LWL_CSV = os.path.join(RAW_DIR, "bb_lwl_parsed.csv")
CW_CSV = os.path.join(ROOT, "data", "crosswalks", "final", "ags_crosswalks.csv")
CW25_CSV = os.path.join(ROOT, "data", "crosswalks", "final", "ags_1990_to_2025_crosswalk.csv")
OUT_CSV = os.path.join(RAW_DIR, "bb_lwl_scans_parsed.csv")

STATE, STATE_NAME = "12", "Brandenburg"
KREISFREIE = {"12051000", "12052000", "12053000", "12054000"}

# page-id prefix -> (PDF, Kreis code); the page id is PREFIX_NN (1-based page)
DOCS = {
    "KFS": ("Ergebnisse Potsdam, Cottbus, BB, FFO - 2004-2013 - kompromiert.pdf", None),
    "BAR": ("LK Barnim 2004-2013 - komprimiert.pdf", "12060"),
    "LDS": ("LK Dahme-Spreewald 2004-2013 - komprimiert.pdf", "12061"),
    "EE": ("LK Elbe-Elster 2004-2013 - komprimiert.pdf", "12062"),
    "HVL": ("LK Havelland 2004-2013 - komprimiert.pdf", "12063"),
    "MOL": ("LK Märkisch-Oderland 2004-2013 - komprimiert.pdf", "12064"),
    "OHV": ("LK Oberhavel 2004-2013 - komprimiert.pdf", "12065"),
    "OSL": ("LK Oberspreewald-Lausitz 2004-2013 - komprimiert.pdf", "12066"),
    "LOS": ("LK Oder-Spree 2004-2013 - komprimiert.pdf", "12067"),
    "OPR": ("LK Ostprignitz-Ruppin 2004-2013 - komprimiert.pdf", "12068"),
    "PM": ("LK Potsdam-Mittelmark 2004-2013 - komprimiert.pdf", "12069"),
    "PR": ("LK Prignitz - 2004-2013 - komprimiert.pdf", "12070"),
    "SPN": ("LK Spree-Neiße 2004-2013 - komprimiert.pdf", "12071"),
    "TF": ("LK Teltow-Fläming 2004-2013 - komprimiert.pdf", "12072"),
    "UM": ("LK Uckermark 2004-2013 - komprimiert.pdf", "12073"),
}

# A second scan of a page already in the file (same Vorgangsnummer); skipped
# after checking that every field and Bewerber row is identical.
DUPLICATE_PAGES = {"LOS_09": "LOS_08"}

# A Bewerber printed without a comma is "Given Surname" (Stahnsdorf 2008 prints
# every name that way, Wittenberge 2008 one of three) and is turned into
# "Surname, Given" (given_first()). Findings are matched to the list by name
# tokens, so their word order does not matter (match_finding()).

# A surname misprinted in the table, keyed (page, lfd, as printed): the elected
# person's own finding and every later source spell it otherwise. Public office
# holders only (an elected person's name is published).
NAME_FIXES = {
    ("PR_01", 2, "Dr. Oliver Herrmann"): "Hermann, Dr. Oliver",   # finding + workbook 2016/2024: Hermann
}

# A round mislabelled on its form. Schwarzheide (OSL_04, keyed into the LWL
# system in 2021, Vorgangsnummer 2021...) prints "Hauptwahl" for 12.10.2008, the
# general runoff day, with two Bewerber; the B VII 3-6 report of 2008 (table 3)
# dates that election 28.09.2008 with its Stichwahl on 12.10.2008, so the page is
# the runoff. Its first-round page is not in the delivery.
WAHLART_FIXES = {"OSL_04": ("Hauptwahl", "Stichwahl")}

# Runoffs whose first-round page is missing but whose first-round date is known
# from another official source: the cycle is filed under that date with a
# result-less first-round row (first_round_no_result), as 00_bb_afs_reports_parse.py
# does for its winner-only runoffs.
HW_SHELLS = {"OSL_04": "28.09.2008"}   # B VII 3-6 2008, table 3
HW_SHELLS_SOURCE = "SB_B7-3-6_j05-08_BB.xls t3 (first-round date; result not in the delivery)"

# Misprinted Wahlvorschlagsträger labels (orthography only, as in the B VII 3-6
# parser's norm_party()).
PARTY_FIXES = {"Gemeinbsame Liste SPD/PDS": "Gemeinsame Liste SPD/PDS"}

# Wahlberechtigte misprinted on the Gemeinde's form, settled from an independent
# official count of the same voter register. The checks run on the page as
# printed (it is internally consistent); the corrected value is published.
#   Belzig 28.09.2008 (PM_18): the page prints 5 779 Wahlberechtigte and 5 607
#   Wähler, a turnout of 97.02 %. The Gemeindevertretung was elected the same day
#   from the same Wählerverzeichnis: 9 779 Wahlberechtigte, 5 606 Wähler, 57.3 %
#   (AfS, Statistischer Bericht B VII 3-5 - 5j/08, table 2, p. 108; GERDA
#   municipal_unharm); the Kreistag the same day: 9 790 / 5 499 (GERDA
#   county_elec_unharm). One digit (5/9); the 15 % row (867) was computed from
#   the misprint. The winner clears the quorum either way (3 153 >= 1 467). The
#   town's own result notice was not found (SLB Potsdam holds only the Landkreis
#   Amtsblatt); confirmation asked of the Landeswahlleiter.
ELIGIBLE_FIXES = {"PM_18": (5779, 9779)}

# Known, verified source anomalies: (page id, check) -> (the exact failure
# message it excuses, explanation). Only a failure with that message is
# reported instead of failing, so a new defect on the same page still aborts;
# a pin that no longer fires aborts too (stale pin).
ANOMALIES = {
    # thresholds the Gemeinde computed by hand on the older forms
    ("OPR_07", "R6"): ("printed 15 % 3943 != 3944",
                       "15 % row = floor(0.15 * 26289); the law rounds up. Irrelevant: the leader (5 223) misses the majority (5 245)"),
    ("PR_06", "R6"): ("printed 15 % 1631 != 1632",
                      "15 % row = floor(0.15 * 10875); the law rounds up. Irrelevant: the leader has 1 519 of 6 385"),
    ("PR_07", "R6"): ("printed half 2245 != 2244|printed erforderlich 2245 != 2244",
                      "half row 2245, floor(4487 / 2) + 1 = 2244; the winner (2 289) clears both"),
    ("TF_21", "R6"): ("printed 15 % 886 != 885",
                      "15 % row 886, 0.15 * 5900 = 885 exactly; the winner (1 580) clears both"),
    # a stale runoff date in the finding of a reused form (Neuruppin 16.01.2005)
    ("OPR_08", "C1"): ("runoff dated 2005-02-06, Hauptwahl finding says 10.10.2004",
                       "the Hauptwahl finding (OPR_07) prints the runoff date 10.10.2004, before the "
                       "Hauptwahl itself; the runoff page says 06.02.2005, 21 days later"),
    # the electorate printed for the two rounds differs by more than 3 %; each
    # page is internally consistent, and the Landeswahlleiter's own result pages
    # (Internet Archive) print the same figures. The other counts of those years
    # fit the runoff's figure, not the Hauptwahl's, but no document gives the
    # Hauptwahl's true electorate, so both stay as printed (asked of the
    # Landeswahlleiter). Neither changes an outcome: both Hauptwahlen went to a runoff.
    ("LDS_18", "C5"): ("electorate 4923 -> 5270",
                       "Eichwalde 2009; Landtag 27.09.2009 5 060 (Germans only), council 2008 5 300, 2014 5 406"),
    ("PR_03", "C5"): ("electorate 12377 -> 11162",
                      "Pritzwalk 2009; Landtag 27.09.2009 11 181, council 2008 11 351, 2014 10 631"),
}

# Rounds also held by the Landeswahlleiter workbook whose figures differ, pinned
# with the evidence: (page, field, scan value, workbook value) -> explanation.
# The workbook keeps the election either way; an unpinned difference aborts.
OVERLAP_PINS = {
    # Liebenwalde 11.09.2011: the Gemeinde's form prints 3 704 and is consistent
    # with its own turnout (44,49 %) and 15 % row (556); the council electorates
    # (2008: 3 843, 2014: 3 682) and the Landtag's (2009: 3 778) fit 3 704. The
    # LWL's own result page (Internet Archive, 2015-2019) and its workbook print
    # 3 407: a 7/4 transposition on the LWL's side. Reported, not changed.
    ("OHV_10", "wahlberechtigte", "3704", "3407"): "workbook transposition (3704 -> 3407), reported",
}

# split_name()/norm_ws() of the workbook parser, so one person is split the same
# way in every Brandenburg source.
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


PIN_USED = Counter()


def fail(pid, check, msg):
    pin = ANOMALIES.get((pid, check))
    if pin and any(m in msg for m in pin[0].split("|")):
        PIN_USED[("anomaly", pid, check)] += 1
        note("anomaly_pinned", f"{pid} {check}: {msg} [{pin[1]}]")
    else:
        note("FAIL_" + check, f"{pid}: {msg}")


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


def ddate(s):
    return dt.datetime.strptime(s.strip(), "%d.%m.%Y").date()


# ---------------------------------------------------------------------------
# reference data
# ---------------------------------------------------------------------------

def load_crosswalks():
    names = defaultdict(dict)                  # {year: {ags: name}}
    with open(CW_CSV, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            a = r["ags"].strip().zfill(8)
            if a.startswith(STATE):
                names[int(r["year"])][a] = r["ags_name"]
    to25 = {}                                  # {(year, ags): ags_25 of the largest pop share}
    best = {}
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


def load_lwl():
    rounds = defaultdict(list)                 # {(ags, date, round): [rows]}
    with open(LWL_CSV, encoding="utf-8-sig") as fh:
        for r in csv.DictReader(fh):
            rounds[(r["ags"], dt.date.fromisoformat(r["election_date"]), r["round"])].append(r)
    by_ags = defaultdict(set)
    for (a, d, _rn) in rounds:
        by_ags[a].add(d)
    return rounds, by_ags


# ---------------------------------------------------------------------------
# transcription -> rounds
# ---------------------------------------------------------------------------

def resolve_ags(r, names):
    pid = r["pid"]
    y = min(r["date"].year, 2020)
    cw = names[y]
    code = re.sub(r"\D", "", r["ags_printed"])
    if len(code) == 12:                         # Regionalschlüssel (2018 form)
        code = code[:5] + code[9:12]
    prefix = pid.split("_")[0]
    kreis = DOCS[prefix][1]
    gem = r["gemeinde"] or r["kreis"]
    if code:
        a = code[:8]
        if a not in cw:
            fail(pid, "AGS", f"printed AGS {code} not a Gemeinde in {y}")
            return a, "printed?"
        if kreis and not a.startswith(kreis):
            fail(pid, "AGS", f"printed AGS {a} outside Landkreis {kreis}")
        if fold(gem) not in fold(cw[a]) and fold(cw[a]) not in fold(gem):
            fail(pid, "AGS", f"printed AGS {a} is '{cw[a]}', page says '{gem}'")
        return a, "printed"
    pool = {a: n for a, n in cw.items()
            if (a.startswith(kreis) if kreis else a in KREISFREIE)}
    hits = [a for a, n in pool.items() if fold(n) == fold(gem)]
    if len(hits) != 1:
        fail(pid, "AGS", f"Gemeinde '{gem}' matches {len(hits)} codes in {y}")
        return (hits[0] if hits else ""), "name?"
    return hits[0], "name"


def given_first(name):
    """'Prof. Dr. Erika Muster' -> 'Muster, Prof. Dr. Erika' (invented example)."""
    toks = name.split()
    return f"{toks[-1]}, {' '.join(toks[:-1])}" if len(toks) > 1 else name


def tokens(s):
    """Name tokens without titles, order-free: 'Max, Muster' = 'Muster, Max'."""
    s = re.sub(r"\b(Dr|Prof|Dipl|Ing|med|phil|rer|nat|oec|habil)\b\.?(-\w+\.?)?", " ", s or "")
    return set(fold(s).split())


def match_finding(part, cands):
    """The one Bewerber whose name tokens all occur in a finding entry such as
    'Muster, Max (SPD)' or 'Erika Beispiel CDU' (invented); None if not unique."""
    t = tokens(re.sub(r"\(.*$", "", part)) or tokens(part)
    hit = [c for c in cands if tokens(c["name"]) and tokens(c["name"]) <= tokens(part)]
    if len(hit) != 1:
        hit = [c for c in cands if tokens(c["name"]) & t and fold(split_name(c["name"])[1]) in t]
    return hit[0] if len(hit) == 1 else None


def finding_parts(text):
    return [x for x in re.split(r"\s*;\s*", text or "") if x.strip()]


def finding_labels(r):
    """{lfd: Träger label} as printed in the page's finding, which repeats the
    label in full where the table cell cuts it off ("Name, Vorname (Label)")."""
    out = {}
    for part in finding_parts(r["elected_text"]) + finding_parts(r["runoff_text"]):
        m = re.match(r"^(.*?)\s*\((.*)$", part)
        c = match_finding(part, r["cands"])
        if m and m.group(2) and c:
            lab = m.group(2).strip()
            if lab.endswith(")") and lab.count(")") > lab.count("("):
                lab = lab[:-1]          # the bracket that closes "(Label)"
            out[c["lfd"]] = norm_ws(lab)
    return out


def load_transcription(names):
    rounds = []
    cands = defaultdict(list)
    with open(CANDS_CSV, encoding="utf-8-sig") as fh:
        for c in csv.DictReader(fh):
            name = norm_ws(c["name"])
            key = (c["pid"], int(c["lfd"]), name)
            if key in NAME_FIXES:
                PIN_USED[("name",) + key] += 1
                note("name_misprint_fixed", f"{c['pid']} lfd {c['lfd']}")
                name = NAME_FIXES[key]
            elif "," not in name:
                note("given_name_first", f"{c['pid']} lfd {c['lfd']}")
                name = given_first(name)
            party = norm_ws(c["traeger"])
            if party in PARTY_FIXES:
                PIN_USED[("party", party)] += 1
                note("party_misprint_fixed", f"{c['pid']} lfd {c['lfd']}: {party} -> {PARTY_FIXES[party]}")
                party = PARTY_FIXES[party]
            cands[c["pid"]].append(dict(lfd=int(c["lfd"]), name=name, party=party,
                                        votes=ival(c["votes"]), pct=c["pct"]))
    raw = list(csv.DictReader(open(ROUNDS_CSV, encoding="utf-8-sig")))
    by_pid = {r["pid"]: r for r in raw}
    for dup, orig in DUPLICATE_PAGES.items():
        a, b = dict(by_pid[dup]), dict(by_pid[orig])
        for k in ("pid", "duplicate_of", "read_status", "third_read", "reader_uncertain", "reader_notes"):
            a.pop(k, None), b.pop(k, None)
        if a != b or cands[dup] != cands[orig]:
            sys.exit(f"FATAL: {dup} is pinned as a second scan of {orig} but differs")
        note("duplicate_scan_skipped", f"{dup} = {orig}")
        cands.pop(dup)
    for r in raw:
        if r["pid"] in DUPLICATE_PAGES:
            continue
        r = {k: (v or "").strip() for k, v in r.items()}
        r["date"] = ddate(r["wahltag"])
        if r["pid"] in WAHLART_FIXES:
            printed, fixed = WAHLART_FIXES[r["pid"]]
            if r["wahlart"] != printed:
                sys.exit(f"FATAL: {r['pid']} no longer reads '{printed}'; review WAHLART_FIXES")
            note("wahlart_misprint_fixed", f"{r['pid']} {r['gemeinde']} {r['date']}: {printed} -> {fixed}")
            r["wahlart"] = fixed
        r["round"] = {"Hauptwahl": "hauptwahl", "Stichwahl": "stichwahl"}[r["wahlart"]]
        for k in ("wahlberechtigte", "waehler", "ungueltige", "gueltige", "nein_stimmen",
                  "mehr_als_haelfte", "quorum15", "erforderlich", "summe"):
            r[k] = ival(r[k])
        r["cands"] = sorted(cands.pop(r["pid"], []), key=lambda c: c["lfd"])
        # a Träger cut off by its table cell, printed in full in the finding
        full = finding_labels(r)
        for c in r["cands"]:
            f = full.get(c["lfd"])
            if f and f != c["party"] and f.startswith(c["party"].rstrip(".")):
                note("traeger_completed_from_finding", f"{r['pid']} lfd {c['lfd']}: cell cut off, finding prints it in full")
                c["party"] = f
        r["ags"], r["ags_how"] = resolve_ags(r, names)
        rounds.append(r)
    if cands:
        sys.exit(f"FATAL: candidate rows without a page: {sorted(cands)}")
    return rounds


# ---------------------------------------------------------------------------
# checks
# ---------------------------------------------------------------------------

def elected_rule(votes, valid, eligible):
    """§ 72 Abs. 2 BbgKWahlG: more than half the gültige Stimmen AND at least
    15 % of the Wahlberechtigte."""
    return votes is not None and votes * 2 > valid and votes >= math.ceil(0.15 * eligible - 1e-9)


def check_round(r):
    pid = r["pid"]
    el, vo, inv, va = r["wahlberechtigte"], r["waehler"], r["ungueltige"], r["gueltige"]
    cs = r["cands"]
    if None in (el, vo, inv, va) or not cs:
        sys.exit(f"FATAL: {pid}: a count or the candidate list is missing in the transcription")
    if r["date"].weekday() != 6:
        fail(pid, "R1", f"{r['date']} is a {r['date'].strftime('%A')}")
    if va + inv != vo:
        fail(pid, "R2", f"gültige {va} + ungültige {inv} = {va + inv} != Wähler {vo}")
    if vo > el:
        fail(pid, "R3", f"Wähler {vo} > Wahlberechtigte {el}")
    single = len(cs) == 1
    tot = sum(c["votes"] or 0 for c in cs) + ((r["nein_stimmen"] or 0) if single else 0)
    if any(c["votes"] is None for c in cs) or tot != va:
        fail(pid, "R4", f"votes sum {tot} != gültige {va}")
    for label, num, den, txt in (("turnout", vo, el, r["waehler_pct"]),
                                 ("ungültige", inv, vo, r["ungueltige_pct"]),
                                 ("gültige", va, vo, r["gueltige_pct"])):
        p, dp = pct(txt)
        if p is not None and den and abs(round(100 * num / den, dp) - p) > 10 ** -dp / 2 + 1e-9:
            fail(pid, "R5", f"{label} printed {txt} != {100 * num / den:.3f}")
    for c in cs:
        p, dp = pct(c["pct"])
        if p is not None and c["votes"] is not None and va and \
                abs(round(100 * c["votes"] / va, dp) - p) > 10 ** -dp / 2 + 1e-9:
            fail(pid, "R5", f"lfd {c['lfd']} printed {c['pct']} != {100 * c['votes'] / va:.3f}")
    half, q15 = va // 2 + 1, math.ceil(0.15 * el - 1e-9)
    if r["mehr_als_haelfte"] is not None and r["mehr_als_haelfte"] != half:
        fail(pid, "R6", f"printed half {r['mehr_als_haelfte']} != {half}")
    if r["quorum15"] is not None and r["quorum15"] != q15:
        fail(pid, "R6", f"printed 15 % {r['quorum15']} != {q15}")
    if r["erforderlich"] is not None and r["erforderlich"] != max(half, q15):
        fail(pid, "R6", f"printed erforderlich {r['erforderlich']} != {max(half, q15)}")
    if r["summe"] is not None and not single and r["summe"] != va:
        fail(pid, "R6", f"printed Summe {r['summe']} != gültige {va}")
    # R7 finding vs votes
    top = max(cs, key=lambda c: c["votes"] or -1)
    ranked = sorted(cs, key=lambda c: -(c["votes"] or -1))
    meets = elected_rule(top["votes"], va, el) if not single else \
        (top["votes"] * 2 > va and top["votes"] >= q15)
    out = r["outcome"]
    if out == "elected":
        hit = match_finding(r["elected_text"], cs)
        if hit is None:
            fail(pid, "R7", "the elected person of the finding is not on the list")
        elif hit is not top or not meets:
            fail(pid, "R7", "the elected person is not the top Bewerber above both thresholds")
    elif out == "runoff":
        if r["round"] != "hauptwahl" or meets:
            fail(pid, "R7", "a runoff although the leader meets both thresholds, or after a runoff")
        named = [match_finding(x, cs) for x in finding_parts(r["runoff_text"])]
        if None in named or {c["lfd"] for c in named} != {c["lfd"] for c in ranked[:2]}:
            fail(pid, "R7", "the runoff Bewerber are not the top two")
    elif out == "none":
        if meets:
            fail(pid, "R7", "nobody elected although the leader meets both thresholds")
    else:
        fail(pid, "R7", f"outcome '{out}'")


# ---------------------------------------------------------------------------
# cycles
# ---------------------------------------------------------------------------

def build_cycles(rounds):
    by_ags = defaultdict(list)
    for r in rounds:
        by_ags[r["ags"]].append(r)
    cycles = []
    for a, rs in by_ags.items():
        rs.sort(key=lambda r: (r["date"], r["round"]))
        for r in rs:
            if r["round"] == "stichwahl" and cycles and cycles[-1]["ags"] == a and \
                    cycles[-1]["rounds"][-1]["round"] == "hauptwahl" and \
                    0 < (r["date"] - cycles[-1]["rounds"][-1]["date"]).days <= 42:
                cycles[-1]["rounds"].append(r)
            else:
                cycles.append(dict(ags=a, rounds=[r]))
    return cycles


def pair_runoff(c):
    """{id(SW Bewerber): lfd of the same person in the Hauptwahl}. The Hauptwahl
    finding names exactly who reached the runoff; each entry is matched to the
    Hauptwahl list and to the runoff list by name tokens, so a name spelled
    differently in the two rounds (a second given name dropped) or a relabelled
    Träger cannot split one person into two rows in 01b's wide pivot (the
    Brandenburg workbook defect class, see 00_bb_lwl_parse.py reconcile_rounds())."""
    hw = [r for r in c["rounds"] if r["round"] == "hauptwahl"]
    sw = [r for r in c["rounds"] if r["round"] == "stichwahl"]
    if not (hw and sw):
        return {}
    out = {}
    for part in finding_parts(hw[0]["runoff_text"]):
        h, s_ = match_finding(part, hw[0]["cands"]), match_finding(part, sw[0]["cands"])
        if h is None or s_ is None or id(s_) in out:
            fail(sw[0]["pid"], "C2", f"{c['ags']} {hw[0]['date']}: a runoff Bewerber cannot be paired with the Hauptwahl")
            continue
        out[id(s_)] = h["lfd"]
        if norm_ws(h["name"]) != norm_ws(s_["name"]):
            note("name_differs_between_rounds", f"{hw[0]['pid']}/{sw[0]['pid']} lfd {h['lfd']}/{s_['lfd']} (paired by pair_key)")
    if len(out) != len(sw[0]["cands"]):
        fail(sw[0]["pid"], "C2", f"{c['ags']} {hw[0]['date']}: {len(out)} of {len(sw[0]['cands'])} runoff Bewerber paired")
    return out


def pair_key(c, r, x, pairs, shell):
    base = f"{c['ags']}_{c['rounds'][0]['date'].isoformat()}_"
    if shell or (r["round"] == "stichwahl" and c.get("hw_shell")):
        return base + str(x["lfd"])          # result-less first round = the runoff's list
    if r["round"] == "hauptwahl" and pairs:
        return base + str(x["lfd"])
    if r["round"] == "stichwahl" and id(x) in pairs:
        return base + str(pairs[id(x)])
    return ""


def check_cycle(c, lwl_by_ags25, to25):
    rs = c["rounds"]
    hw = [r for r in rs if r["round"] == "hauptwahl"]
    sw = [r for r in rs if r["round"] == "stichwahl"]
    tag = f"{c['ags']} {rs[0]['date']}"
    c["flag_missing"] = False
    c["council"] = False
    if sw and not hw:
        if sw[0]["pid"] in HW_SHELLS:
            c["hw_shell"] = ddate(HW_SHELLS[sw[0]["pid"]])
            gap = (sw[0]["date"] - c["hw_shell"]).days
            if gap not in (14, 21, 28, 35):
                fail(sw[0]["pid"], "C1", f"{tag}: pinned first round {c['hw_shell']} is {gap} days earlier")
            note("runoff_filed_under_known_first_round", f"{sw[0]['pid']} {tag}: first round {c['hw_shell']} (no result)")
        else:
            fail(sw[0]["pid"], "C1", f"{tag}: runoff without its Hauptwahl")
    if hw and sw:
        h, s = hw[0], sw[0]
        gap = (s["date"] - h["date"]).days
        # § 72 Abs. 2 BbgKWahlG (2001/2004): "frühestens am zweiten und spätestens
        # am fünften Sonntag nach der Wahl"
        if gap not in (14, 21, 28, 35):
            fail(s["pid"], "C1", f"{tag}: runoff {gap} days after the Hauptwahl")
        elif gap > 21:
            note("runoff_after_4_or_5_weeks", f"{s['pid']} {tag}: {gap} days")
        if h["outcome"] != "runoff":
            fail(s["pid"], "C2", f"{tag}: a runoff after a Hauptwahl with outcome '{h['outcome']}'")
        named = [match_finding(x, s["cands"]) for x in finding_parts(h["runoff_text"])]
        if None in named or len({x["lfd"] for x in named}) != len(s["cands"]):
            fail(s["pid"], "C2", f"{tag}: runoff Bewerber differ from those the Hauptwahl admitted")
        if h["runoff_date"] and ddate(h["runoff_date"]) != s["date"]:
            fail(s["pid"], "C1", f"{tag}: runoff dated {s['date']}, Hauptwahl finding says {h['runoff_date']}")
        if abs(s["wahlberechtigte"] - h["wahlberechtigte"]) > 0.03 * h["wahlberechtigte"]:
            fail(s["pid"], "C5", f"{tag}: electorate {h['wahlberechtigte']} -> {s['wahlberechtigte']}")
    last = rs[-1]
    if last["outcome"] == "elected":
        return
    if last["round"] == "hauptwahl" and last["outcome"] == "runoff":
        # C3: the runoff may sit in the workbook (it is compared there)
        a25 = to25.get((min(last["date"].year, 2020), c["ags"]), c["ags"])
        if any(0 < (d - last["date"]).days <= 35 for d in lwl_by_ags25.get(a25, ())):
            note("runoff_in_workbook_only", f"{last['pid']} {tag}")
            return
        c["flag_missing"] = True
        fail(last["pid"], "C3", f"{tag}: Hauptwahl without its runoff")
        return
    # C4: nobody elected in the decisive round -> the Gemeindevertretung elects
    c["flag_missing"] = True
    c["council"] = last["round"] == "stichwahl"
    note("seated_nobody", f"{last['pid']} {tag} ({last['round']}, council={c['council']})")


# ---------------------------------------------------------------------------
# workbook overlap
# ---------------------------------------------------------------------------

def compare_with_workbook(c, lwl_rounds, lwl_by_ags25, to25, findings):
    """True if the workbook holds this election (then it is left out here)."""
    first = c["rounds"][0]
    a25 = to25.get((min(first["date"].year, 2020), c["ags"]), c["ags"])
    dates = lwl_by_ags25.get(a25, set())
    if not any(abs((d - r["date"]).days) <= 60 for r in c["rounds"] for d in dates):
        return False
    for r in c["rounds"]:
        rows = lwl_rounds.get((a25, r["date"], r["round"]))
        if not rows:
            findings.append(dict(pid=r["pid"], ags=c["ags"], ags_2025=a25, date=r["date"],
                                 round=r["round"], field="round", scan="present", workbook="absent"))
            continue
        w = rows[0]
        for f_scan, f_wb in (("wahlberechtigte", "eligible_voters"), ("waehler", "number_voters"),
                             ("gueltige", "valid_votes"), ("ungueltige", "invalid_votes")):
            if str(r[f_scan]) != (w[f_wb] or "").split(".")[0]:
                findings.append(dict(pid=r["pid"], ags=c["ags"], ags_2025=a25, date=r["date"],
                                     round=r["round"], field=f_scan, scan=r[f_scan], workbook=w[f_wb]))
        sv = sorted(x["votes"] for x in r["cands"])
        wv = sorted(int(float(x["candidate_votes"])) for x in rows if x["candidate_votes"])
        if sv != wv:
            findings.append(dict(pid=r["pid"], ags=c["ags"], ags_2025=a25, date=r["date"],
                                 round=r["round"], field="candidate_votes",
                                 scan=" ".join(map(str, sv)), workbook=" ".join(map(str, wv))))
        # the elected person (surname + first given name; spelling noted only)
        sk = {person_key(x["name"]) for x in r["cands"]}
        wk = {person_key(x["candidate_name"]) for x in rows}
        if sk != wk:
            findings.append(dict(pid=r["pid"], ags=c["ags"], ags_2025=a25, date=r["date"],
                                 round=r["round"], field="candidate_names",
                                 scan=f"{len(sk - wk)} differ", workbook=f"{len(wk - sk)} differ"))
    return True


# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------

def main():
    for p in (ROUNDS_CSV, CANDS_CSV, LWL_CSV, CW_CSV, CW25_CSV):
        if not os.path.exists(p):
            sys.exit(f"FATAL: missing input {p}")
    names, to25 = load_crosswalks()
    lwl_rounds, lwl_by_ags = load_lwl()
    rounds = load_transcription(names)
    print(f"Transcription: {len(rounds)} rounds, {sum(len(r['cands']) for r in rounds)} Bewerber rows")
    for p in sorted({r["pid"].split("_")[0] for r in rounds if not r["source_ref"]}):
        if not os.path.exists(os.path.join(SRC_DIR, DOCS[p][0])):
            sys.exit(f"FATAL: source PDF missing for {p}")
    for r in rounds:                      # rounds from an archived LWL result page
        if r["source_ref"]:
            m = re.search(r"bmwahlen/(\d{10})/([hs]), Internet Archive (\d{14})", r["source_ref"])
            f = m and os.path.join(SRC_DIR, "wayback", f"{m.group(1)}_{m.group(2)}_{m.group(3)}.html")
            if not (f and os.path.exists(f)):
                sys.exit(f"FATAL: {r['pid']}: archived source page missing ({r['source_ref']})")
            note("round_from_archived_lwl_page", f"{r['pid']} {r['gemeinde']} {r['date']} {r['round']}")
    for r in rounds:
        check_round(r)
    # pinned corrections of misprinted electorates, after the as-printed checks;
    # the decisive Bewerber must still clear the 15 % quorum on the corrected count
    for r in rounds:
        if r["pid"] in ELIGIBLE_FIXES:
            printed, fixed = ELIGIBLE_FIXES[r["pid"]]
            if r["wahlberechtigte"] != printed:
                sys.exit(f"FATAL: {r['pid']} no longer prints {printed} Wahlberechtigte; review the pin")
            r["wahlberechtigte"] = fixed
            top = max(r["cands"], key=lambda c: c["votes"])
            if r["outcome"] == "elected" and not elected_rule(top["votes"], r["gueltige"], fixed):
                sys.exit(f"FATAL: {r['pid']}: the corrected electorate changes the outcome")
            note("eligible_misprint_fixed", f"{r['pid']} {r['gemeinde']} {r['date']}: {printed} -> {fixed}")
    cycles = build_cycles(rounds)
    for c in cycles:
        check_cycle(c, lwl_by_ags, to25)

    findings, keep, n_wb = [], [], 0
    for c in cycles:
        if compare_with_workbook(c, lwl_rounds, lwl_by_ags, to25, findings):
            n_wb += 1
        else:
            keep.append(c)
    print(f"{len(cycles)} elections; {n_wb} held by the workbook (compared, left out); {len(keep)} kept")
    if not ARGS.transcribed:
        os.makedirs(WORK_DIR, exist_ok=True)
        with open(os.path.join(WORK_DIR, "overlap_findings.csv"), "w", newline="", encoding="utf-8") as fh:
            w = csv.DictWriter(fh, fieldnames=["pid", "ags", "ags_2025", "date", "round", "field", "scan", "workbook"])
            w.writeheader()
            w.writerows(findings)
    print(f"Workbook comparison: {len(findings)} differing fields "
          f"({len({(f['pid']) for f in findings})} rounds) -> _work/overlap_findings.csv")
    for f in findings:
        k = (f["pid"], f["field"], str(f["scan"]), str(f["workbook"]))
        if f["field"] == "candidate_names":
            note("overlap_name_spelling", f"{f['pid']} {f['ags']} {f['date']} {f['round']} (workbook keeps its spelling)")
        elif k in OVERLAP_PINS:
            PIN_USED[("overlap",) + k] += 1
            note("overlap_pinned", f"{f['pid']} {f['ags']} {f['date']} {f['round']}: {f['field']} "
                                   f"scan={f['scan']} workbook={f['workbook']} [{OVERLAP_PINS[k]}]")
        else:
            note("FAIL_overlap", f"{f['pid']} {f['ags']} {f['date']} {f['round']}: {f['field']} "
                                 f"scan={f['scan']} workbook={f['workbook']} (the workbook would replace this round)")

    rows = []
    for c in sorted(keep, key=lambda c: (c["ags"], c["rounds"][0]["date"])):
        pairs = pair_runoff(c)
        rounds_out = list(c["rounds"])
        if c.get("hw_shell"):
            s0 = c["rounds"][0]
            rounds_out.insert(0, dict(s0, date=c["hw_shell"], round="hauptwahl", shell=True))
        # the elected person's own rows, by row (not by name, so a namesake or a
        # relative with the same first given name is never published): the top
        # row of the decisive round, the same person's paired first-round row,
        # and the result-less first-round copy of the runoff list
        win_rows = set()
        if not c["flag_missing"]:
            last = c["rounds"][-1]
            top = max(last["cands"], key=lambda x: x["votes"])
            win_rows.add((last["date"], top["lfd"]))
            if last["round"] == "stichwahl" and id(top) in pairs:
                win_rows.add((c["rounds"][0]["date"], pairs[id(top)]))
            if c.get("hw_shell"):
                win_rows.add((c["hw_shell"], top["lfd"]))
        filing_date = rounds_out[0]["date"]
        for r in rounds_out:
            shell = r.get("shell", False)
            top = max(r["cands"], key=lambda x: x["votes"])
            va, el, vo = r["gueltige"], r["wahlberechtigte"], r["waehler"]
            for x in r["cands"]:
                full, last_n, first_n, title = split_name(x["name"])
                rows.append({
                    "ags": c["ags"], "ags_name": re.sub(r"^Stadt\s+", "", r["gemeinde"] or r["kreis"]),
                    "state": STATE, "state_name": STATE_NAME,
                    "election_year": r["date"].year, "election_date": r["date"].isoformat(),
                    "election_type": "Oberbürgermeisterwahl" if c["ags"] in KREISFREIE else "Bürgermeisterwahl",
                    "round": r["round"],
                    "eligible_voters": "" if shell else el, "number_voters": "" if shell else vo,
                    "valid_votes": "" if shell else va, "invalid_votes": "" if shell else r["ungueltige"],
                    "turnout": "" if shell else vo / el,
                    "candidate_name": full, "candidate_last_name": last_n,
                    "candidate_first_name": first_n, "candidate_title": title,
                    "candidate_party": x["party"], "wahlvorschlag_art": "",
                    "candidate_votes": "" if shell else x["votes"],
                    "candidate_voteshare": "" if shell else x["votes"] / va,
                    "is_winner": "" if c["flag_missing"] else ("TRUE" if (x is top and not shell) else "FALSE"),
                    "source": "lwl_scan",
                    "source_url": (HW_SHELLS_SOURCE if shell else r["source_ref"] or
                                   f"{DOCS[r['pid'].split('_')[0]][0]} p.{int(r['pid'].split('_')[1])}"),
                    # the result-less first round of a runoff cycle lists only the two
                    # Bewerber who reached the runoff, not the whole field: winner_only
                    # makes 01b report n_candidates NA for that round (as for the
                    # B VII 3-6 shells) instead of a fabricated 2
                    "office": "H", "winner_only": "TRUE" if shell else "FALSE",
                    "flag_decisive_round_missing": "TRUE" if c["flag_missing"] else "FALSE",
                    "name_restricted": "FALSE" if (r["date"], x["lfd"]) in win_rows else "TRUE",
                    "preliminary": "FALSE", "first_round_no_result": "TRUE" if shell else "FALSE",
                    "elected_by_council": "TRUE" if (c["council"] and r["date"] == filing_date) else "FALSE",
                    # 01b's explicit HW/SW link: Hauptwahl row lfd, runoff row the
                    # lfd of the same person's Hauptwahl row; empty without a runoff
                    "pair_key": pair_key(c, r, x, pairs, shell),
                })

    stale = ([k for k in ANOMALIES if not PIN_USED[("anomaly",) + k]]
             + [k for k in NAME_FIXES if not PIN_USED[("name",) + k]]
             + [k for k in PARTY_FIXES if not PIN_USED[("party", k)]]
             + [k for k in OVERLAP_PINS if not PIN_USED[("overlap",) + k]])
    for k in stale:
        note("FAIL_stale_pin", f"{k}: the pin no longer fires; review it")
    print("\n--- source defects, exclusions & anomalies ---")
    for k in sorted(PROBLEMS):
        print(f"  [{k}] {len(PROBLEMS[k])}")
        for msg in PROBLEMS[k][:80]:
            print(f"      {msg}")
    fatal = sorted(k for k in PROBLEMS if k.startswith("FAIL_"))
    if fatal:
        sys.exit(f"\nFATAL: {fatal} — fix the transcription or pin the case before publishing.")

    if ARGS.transcribed:
        print("calibration run: nothing written")
        return
    with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\nWrote {len(rows)} candidate rows -> {OUT_CSV}")
    print(f"  {len(keep)} elections, {len({c['ags'] for c in keep})} AGS, "
          f"{sum(len(c['rounds']) for c in keep)} rounds")
    print("  by year:", dict(sorted(Counter(c["rounds"][0]["date"].year for c in keep).items())))
    print("  runoff cycles filed under a known first round without result:", sum(bool(c.get("hw_shell")) for c in keep))
    print("  seated nobody:", sum(c["flag_missing"] for c in keep),
          " council elects:", sum(c["council"] for c in keep))
    print("  AGS from:", dict(Counter(r["ags_how"] for c in keep for r in c["rounds"])))


if __name__ == "__main__":
    main()
