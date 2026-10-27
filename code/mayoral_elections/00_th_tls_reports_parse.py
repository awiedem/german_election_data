#!/usr/bin/env python3
"""
Stage 0 — Thüringen Bürgermeister: names and missing elections from the TLS
Kommunalwahl reports 2000, 2004 and 2010.

Sources (data/mayoral_elections/raw/thueringen/tls_wahlberichte_2000_2004/,
sent by the Thüringer Landesamt für Statistik by e-mail on 2026-10-08; see the
README there; PDFs gitignored):
  20421_2004_01.pdf  Kommunalwahlen 2004, Endgültige Ergebnisse, Wahlen der
                     Bürgermeister (text layer).
                     Table 3.5 = per-Gemeinde results of the 820 Gemeinden that
                     voted on 27.06.2004 (runoffs 11.07.2004), every candidate
                     NAMED. Table 3.4 = every Bürgermeister in office, with the
                     date of their own election where it differs.
  29924_2000_01.pdf  Kommunalwahlen 2000, Vorläufige Ergebnisse, Stichwahlen
                     (28.05.2000) of Landräte, Oberbürgermeister, Bürgermeister.
                     A scan: transcribed visually into
                     th_tls2000_runoffs_transcribed.csv (same folder,
                     gitignored because it names losing candidates).
  29926_2010_01.pdf  Kommunalwahlen 2010, Endgültige Ergebnisse, Wahlen der
                     Bürgermeister (tls_wahlberichte_2010/, from the public
                     Statistische Bibliothek; text layer, two Gemeinde columns
                     per page). Table 3.3 = the 689 elections of 06.06.2010,
                     every candidate named; table 3.2 = Kreis totals incl. the
                     votes and Mandate of CDU, DIE LINKE, SPD, FDP, GRÜNE.

Why
---
The Landesamt's database (wahlen.thueringen.de, scraped by 00_th_scrape.py)
names candidates only from 2020 on (§ 50 ThürKWO) and lists only TODAY's
Gemeinden, so it holds 439 of the 820 Bürgermeisterwahlen of 27.06.2004 and
389 of the 689 of 06.06.2010. The reports name every candidate and cover all.

What this script does
---------------------
1. Parses table 3.5 and checks it against the report's own subtotals (Kreis and
   state totals of table 3.2 must match exactly).
2. Where the database holds the round, it checks that both agree exactly
   (electorate, voters, invalid and valid votes, candidate votes) and GRAFTS the
   report's names onto the database rows (column tls_name). The database keeps
   its own labels and figures.
3. Where the database lacks the round, it ADDS the round from the report, in the
   database's own convention: list elections carry every Wahlvorschlag; in an
   election "ohne Bindung an zugelassene Wahlvorschläge" (at most one
   Wahlvorschlag; voters may write in any eligible person) the first-listed
   person and any person with a Träger stay separate and every other write-in is
   pooled as "Weitere Personen". That rule reproduces the database for the
   overlapping rounds (rate printed and asserted below).
4. Grafts the names of table 3.4 (mayors elected on another date: 1999-2004)
   and of the 2000 runoffs onto the database / OB rows, and adds the 2000
   runoffs the database lacks.
5. Links each runoff candidate to their first-round row with pair_key, so 01b
   cannot split one person into two rows.
6. Marks the four 2004 write-in elections that seated nobody ("Wiederholungswahl
   erforderlich") with flag_decisive_round_missing.
7. Does the same for 2010 (process_2010): table 3.3 must reproduce table 3.2
   for every Kreis (electorate, voters, valid votes, each party's votes and
   Mandate); the 389 database rounds must agree exactly and get the names; the
   300 others are added in the database's convention (8 on their 2009 code: the
   Gemeinden merged later in 2010); the 2 added "Neuwahl erforderlich" are
   flagged. check_2010_winners() then asserts that the majority rule seats the
   report's elected person in all 684 accepted elections.
   Table 4.1 of the same report (every mayor in office at the end of 2010;
   process_41_2010): its undated rows must be 3.3's elected persons; its 264
   dated rows (elections of 2005-2010 on other dates) name 168 database rounds
   and add 95 winner-only elections, filed as runoffs where the election
   calendar says so (round_41). It also breaks Bad Berka's tied runoff of
   21.05.2006 (is_winner_src).
8. Applies the majority rule to every Thüringen cycle (decide_majority_rule):
   first rounds without a majority or runoff seat nobody, except in the
   2009-2010 window without runoffs, where the top NAMED candidate won
   (TLS 29926; is_winner_src) unless the election was never accepted.

Every name taken from these reports is restricted, the elected persons' too.
The TLS consented to publishing the results "unter Löschung sämtlicher
personenbezogenen Daten zu Bewerbern und gewählten Kandidaten" (§ 50 Abs. 2
ThürKWO; e-mail of 12.10.2026, Vorgang 1167/2026). 01b marks these rows
name_restricted and the Landrat combine withholds the three Landrat runoffs'
names; both keep the names in the restricted twin only. The intermediate CSVs
written here name candidates and are gitignored. The same e-mail settled the
Tautenburg spelling, two misprinted dates and three runoff dates of table 3.4
(TAUTENBURG, DATES_34, RUNOFF_DATES_34).

Output
------
  data/mayoral_elections/raw/thueringen_bm/th_mayoral_tls_augmented.csv
    = th_bm_scraped.csv + th_ob_parsed.csv, unchanged, plus the columns
      tls_name, candidate_title, pair_key, flag_decisive_round_missing,
      name_source; plus the added rounds (source starts with "TLS").
    01/01b read it instead of the two inputs when it exists.
  data/landrat_elections/raw/thueringen_tls2000_runoffs.csv
    = the three Landrat runoffs of 28.05.2000 (no Landrat data before 2006
      otherwise), for code/landrat_elections/00_th_parse.R.

Run order: 00_th_scrape.py, 00_th_mayoral_parse.py, then this script, then
01/01b (and the Landrat parser + combine).
"""

import collections
import csv
import os
import re
import subprocess
import sys
import unicodedata

import pandas as pd

RAW = "data/mayoral_elections/raw/thueringen/tls_wahlberichte_2000_2004"
PDF_2004 = f"{RAW}/20421_2004_01.pdf"
TRANSCRIPT_2000 = f"{RAW}/th_tls2000_runoffs_transcribed.csv"
PDF_2010 = "data/mayoral_elections/raw/thueringen/tls_wahlberichte_2010/29926_2010_01.pdf"
DB_FILE = "data/mayoral_elections/raw/thueringen_bm/th_bm_scraped.csv"
OB_FILE = "data/mayoral_elections/raw/thueringen/th_ob_parsed.csv"
CW_FILE = "data/crosswalks/final/ags_crosswalks.csv"
OUT = "data/mayoral_elections/raw/thueringen_bm/th_mayoral_tls_augmented.csv"
OUT_LR = "data/landrat_elections/raw/thueringen_tls2000_runoffs.csv"

SRC_2004 = "TLS 20421 Kommunalwahlen 2004 (endgültig), Tab. 3.5"
SRC_2004_34 = "TLS 20421 Kommunalwahlen 2004 (endgültig), Tab. 3.4"
SRC_2000 = "TLS 29924 Stichwahlen 28.05.2000 (vorläufig)"
SRC_2010 = "TLS 29926 Kommunalwahlen 2010 (endgültig), Tab. 3.3"

KREIS = {"Eichsfeld": "16061", "Nordhausen": "16062", "Wartburgkreis": "16063",
         "Unstrut-Hainich-Kreis": "16064", "Kyffhäuserkreis": "16065",
         "Schmalkalden-Meiningen": "16066", "Gotha": "16067", "Sömmerda": "16068",
         "Hildburghausen": "16069", "Ilm-Kreis": "16070", "Weimarer Land": "16071",
         "Sonneberg": "16072", "Saalfeld-Rudolstadt": "16073",
         "Saale-Holzland-Kreis": "16074", "Saale-Orla-Kreis": "16075",
         "Greiz": "16076", "Altenburger Land": "16077"}

# Gemeinde names the crosswalk cannot resolve (Eichsfeld has two Bernterodes;
# the 2004 report abbreviates the name).
AGS_PINS = {("Eichsfeld", "Bernterode (b. Heilb. Heiligenst.)"): "16061004",
            ("Eichsfeld", "Bernterode (bei Heiligenstadt)"): "16061004",
            ("Eichsfeld", "Heiligenstadt, Stadt"): "16061045"}      # Heilbad Heiligenstadt

# Names the report prints inconsistently, as {printed form: canonical form}. Kept
# in a gitignored CSV next to the reports, because no name from these reports may
# be published (TLS, 12.10.2026). Berlingerode, Römhild and Volkmannsdorf were
# decided by the report's own three mentions of the person (candidate line in 3.5,
# elected line in 3.5, table 3.4), majority wins; Tautenburg, where all three
# differ, by the TLS's e-mail of 12.10.2026 (Vorgang 1167/2026). A missing comma
# between surname and given name is handled in split_label.
PINS_FILE = f"{RAW}/name_pins_restricted.csv"


def load_name_pins():
    with open(PINS_FILE, encoding="utf-8") as fh:
        pins = {r["printed"]: r["canonical"] for r in csv.DictReader(fh)}
    assert len(pins) == 5, f"{PINS_FILE}: expected 5 pins, got {len(pins)}"
    return pins


NAME_FIXES = load_name_pins() if os.path.exists(PINS_FILE) else None

# Table 3.4 dates the TLS corrected (e-mail of 12.10.2026). Both printed dates
# fall on weekdays; with them the Leutenberg winner missed the database's own
# round of 15.06.2003 and was added a second time as a winner-only election.
DATES_34 = {("Königsee, Stadt", "27.09.2003"): "07.09.2003",
            ("Leutenberg, Stadt", "17.06.2003"): "15.06.2003"}
# Runoffs that only table 3.4 records: it prints the first round's date
# (confirmed by the TLS, 12.10.2026); the runoff dates are the TLS's.
RUNOFF_DATES_34 = {("Andenhausen", "26.09.2004"): "10.10.2004",
                   ("Berga/Elster, Stadt", "12.01.2003"): "26.01.2003",
                   ("Teichwolframsdorf", "13.06.1999"): "27.06.1999"}

# ---------------------------------------------------------------------------
# text layer
# ---------------------------------------------------------------------------
# Some lines of 20421 use a font whose glyphs sit 29 code points below their
# letters ("GDYRQ HQW¿HOHQ DXI" = "davon entfielen auf"); its space is \x03 and
# a few letters have their own codes. Parentheses in such lines are lost and
# come out as blanks.
SHIFT_SPECIAL = {"\x81": "ü", "l": "ä", "|": "ö", "¿": "fi", "À": "fl", "}": "ü"}


def decode(line):
    """Decode the shifted-font segments of a line. A line can mix both fonts
    ("Wähler   9RQ\x03GHQ..."), so a segment between layout gaps is decoded
    unless it is plain text: no shifted-font code (control character, \x81)
    and a lowercase letter the shifted font never produces. The shifted font
    writes every letter at or below "]" (z), so "[" alone is an "x"."""
    if "\x03" not in line:
        return line
    out = []
    for seg in re.split(r"( {2,})", line):
        if seg.strip() == "" or (not re.search(r"[\x00-\x1f\x81]", seg)
                                 and re.search(r"[a-km-zßäöü]", seg)):
            out.append(seg)
            continue
        for c in seg:
            if c == " ":
                out.append(" ")
            elif c in SHIFT_SPECIAL:
                out.append(SHIFT_SPECIAL[c])
            elif ord(c) < 0x7F:
                out.append(chr(ord(c) + 29))
            else:
                raise ValueError(f"undecodable glyph {c!r} in {line!r}")
    return "".join(out)


def pdf_lines(pdf):
    txt = subprocess.run(["pdftotext", "-layout", pdf, "-"], capture_output=True,
                         text=True, check=True).stdout
    return [decode(l).replace("\x0c", "") for l in txt.split("\n")]


NUM = r"(?:\d{1,3}(?: \d{3})*|-)"
PCT = r"(?:\d{1,3},\d|x|-)"
ROW = re.compile(r"^(?P<label>.*?)\s{2,}(?P<n1>" + NUM + r")\s+(?P<p1>" + PCT +
                 r")(?:\s+(?P<n2>" + NUM + r")\s+(?P<p2>" + PCT + r"))?\s*$")
STATS = ["Wahlberechtigte", "Wähler/Wahlbeteiligung", "Ungültige Stimmen", "Gültige Stimmen"]


def num(x):
    return None if x in (None, "-") else int(x.replace(" ", ""))


def parse_35(lines):
    s = [i for i, l in enumerate(lines) if re.match(r"^\s*3\.5\s+Ergebnisse nach Gemeinden\s*$", l)][0]
    e = [i for i, l in enumerate(lines) if re.match(r"^\s*4\.\s+Gemeinderegister\s*$", l)][0]
    blocks, cur, kreis = [], None, None
    for raw in lines[s:e]:
        l = raw.rstrip()
        st = l.strip()
        if not st:
            continue
        m = re.match(r"^(?:Noch: )?Landkreis:? (.+)$", st)
        if m:
            kreis = m.group(1).strip()
            continue
        if (st.startswith(("Thüringer Landesamt", "Wahlen der Bürgermeister", "3.", "Gemeinde ",
                           "Stimmverhalten", "——", "Wähler ", "Hauptwahl"))
                or re.match(r"^\d+(\s+Thüringer Landesamt.*)?$", st)):
            continue
        if "Wahlberechtigte" in l:
            name = l.split("Wahlberechtigte")[0].strip()
            cur = {"kreis": kreis, "gemeinde": name, "stats": {}, "cands": [],
                   "ohne_bindung": False, "elected": None, "amt": None}
            blocks.append(cur)
            m = ROW.match(" " * len(l.split("Wahlberechtigte")[0]) + "Wahlberechtigte" + l.split("Wahlberechtigte", 1)[1])
            cur["stats"]["Wahlberechtigte"] = (num(m["n1"]), num(m["n2"]))
            continue
        if cur is None:
            continue
        if st.replace(" ", "") == "davonentfielenauf":
            continue
        if "Wahl ohne Bindung" in st:
            cur["ohne_bindung"] = True
            continue
        m = re.match(r"^Bürgermeister/in\s*\(?(ehrenamtlich|hauptamtlich)\)?\s*:?\s*(.*)$", st)
        if m:
            cur["amt"], cur["elected"] = m.group(1), m.group(2).strip()   # keep "  " = lost brackets
            continue
        m = ROW.match(l)
        if not m:
            raise ValueError(f"unparsed line in {cur['gemeinde']}: {st!r}")
        lab = m["label"].strip()
        if lab in STATS:
            cur["stats"][lab] = (num(m["n1"]), num(m["n2"]))
        else:
            cur["cands"].append({"label": lab, "hw": num(m["n1"]), "sw": num(m["n2"])})
    for b in blocks:
        assert len(b["stats"]) == 4 and b["elected"], b
    return blocks


def parse_32(lines):
    """Table 3.2: Wahlberechtigte, Wähler, Ungültige, Gültige per Landkreis."""
    s = [i for i, l in enumerate(lines) if re.match(r"^\s*3\.2\s+Ergebnisse nach Landkreisen\s*$", l)][0]
    ref = {}
    for l in lines[s:s + 80]:
        m = re.match(r"^\s+(\S.*?)\s+Anzahl\s+(.*)$", l)
        if m:
            v = re.findall(r"\d{1,3}(?: \d{3})*", m.group(2))[:4]
            ref[m.group(1).strip()] = [int(x.replace(" ", "")) for x in v]
    assert len(ref) == 18, ref.keys()
    return ref


def parse_34_dated(lines):
    """Table 3.4 rows that carry their own election date (not 27.06.2004)."""
    s = [i for i, l in enumerate(lines) if re.match(r"^\s*3\.4\s+Übersicht der gewählten Bürgermeister\s*$", l)][0]
    e = [i for i, l in enumerate(lines) if re.match(r"^\s*3\.5\s+Ergebnisse nach Gemeinden\s*$", l)][0]
    rows, kreis = [], None
    pat = re.compile(r"^\s*(?P<gem>\S.*?)\s{2,}(?P<date>\d\d\.\d\d\.\d{4})\s+(?P<rest>\S.*?)\s+"
                     r"(?:(?P<ha>\*)\s+)?(?P<votes>\d{1,3}(?: \d{3})*)\s+(?P<pct>\d{1,3},\d)"
                     r"(?:\s+(?P<sw>\*))?\s*$")
    for l in lines[s:e]:
        st = l.strip()
        m = re.match(r"^(?:Noch: )?(?:Landkreis:? )?(.+)$", st)
        if m and m.group(1).strip() in KREIS:
            kreis = m.group(1).strip()
            continue
        if not re.search(r"\d\d\.\d\d\.\d{4}", l) or "Stand" in l:
            continue
        m = pat.match(l)
        if not m:
            raise ValueError(f"unparsed 3.4 row: {l!r}")
        parts = re.split(r"\s{2,}", m["rest"].strip())
        if len(parts) == 1:
            # no gap: a party may still trail the name ("Surname, Given SPD")
            mm = re.match(r"^(.*, \S+(?:-\S+)?)\s+([A-ZÄÖÜ]{2,}\S*)$", parts[0])
            parts = [mm.group(1), mm.group(2)] if mm else parts
        name, party = parts[0], (" ".join(parts[1:]) if len(parts) > 1 else "")
        date = DATES_34.get((m["gem"].strip(), m["date"]), m["date"])
        rows.append({"kreis": kreis, "gemeinde": m["gem"].strip(), "date": date,
                     "name": name.strip(), "party": party.strip(), "hauptamtlich": bool(m["ha"]),
                     "votes": int(m["votes"].replace(" ", "")), "pct": float(m["pct"].replace(",", ".")),
                     "runoff": bool(m["sw"])})
    fixed = {(r["gemeinde"], r["date"]) for r in rows}
    assert all((g, d) in fixed for (g, _), d in DATES_34.items()), "a DATES_34 entry no longer matches"
    return rows


# ---------------------------------------------------------------------------
# names
# ---------------------------------------------------------------------------
TITLE_RE = re.compile(r"^((?:Prof\.|Dr\.|DR\.|Dipl\.-[A-Za-zäöü]+\.)\s*)+")


def split_label(label):
    """'Dr. Surname, Given-Name (FWG)' -> (name, title, party). Party None if no bracket.
    A decoded shifted-font line lost its brackets: 'Surname, Given  FWG'."""
    lab = re.sub(r"\s+$", "", label)
    party = None
    # a degree in brackets after the name ("Surname, Given (Dipl.-Ing.) (Träger)",
    # 2010) is a title, not the Wahlvorschlag
    m = re.search(r"\s*\((Dipl\.-[^()]*)\)", lab)
    degree = m.group(1).strip() if m else None
    if m:
        lab = (lab[:m.start()] + lab[m.end():]).strip()
    m = re.match(r"^(.*?)\s*\((.+)\)$", lab)
    if m:
        lab, party = m.group(1), m.group(2).strip()
    else:
        m = re.match(r"^(.*?,\s*\S.*?)\s{2,}(\S.*)$", lab)
        if m:
            lab, party = m.group(1), m.group(2).strip()
    lab = re.sub(r"\s+", " ", lab).strip()
    lab = re.sub(r"^Dr, ", "Dr. ", lab)      # Wasserthaleben prints "Dr, Surname, Given"
    lab = NAME_FIXES.get(lab, lab)
    title = None
    m = TITLE_RE.match(lab)
    if m:
        title = m.group(0).strip().replace("DR.", "Dr.")
        lab = lab[m.end():].strip()
    if degree:
        title = f"{title} {degree}" if title else degree
    if "," not in lab:
        # the report drops the comma now and then ("Surname Given"); a write-in
        # can be a bare surname ("Dr. Surname")
        tok = lab.split()
        if len(tok) == 1:
            return tok[0], title, party
        if len(tok) != 2:
            raise ValueError(f"name without comma: {label!r}")
        lab = f"{tok[0]}, {tok[1]}"
    last, first = [x.strip() for x in lab.split(",", 1)]
    if party and (norm_label(last).startswith(norm_label(party)) or norm_label(party).startswith(norm_label(last))):
        # a Wahlvorschlag named after its own candidate ("Surname, Given (Surname)",
        # also truncated) is what the database calls an Einzelbewerber
        party = None
    return f"{last}, {first}", title, party


def strip_own_name(label, name):
    """'BWV Surname' -> 'BWV' for a candidate called Surname, also for a truncated
    surname ('BJV Hoffm' for a Hoffmann). Empty when nothing else remains."""
    if not label or not name:
        return label
    sur = norm_label(name.split(",")[0].split()[-1] if "," not in name else name.split(",")[0])
    keep = [t for t in label.split()
            if not (len(norm_label(t)) >= 4 and (sur.startswith(norm_label(t)) or norm_label(t).startswith(sur)))]
    return " ".join(keep)


def is_sonstige(label):
    # "sonstige", and Herpf's "11 weitere" (eleven further write-ins, 13 votes)
    return bool(re.fullmatch(r"sonstige|\d+ weitere", label.strip().lower()))


# ---------------------------------------------------------------------------
# AGS
# ---------------------------------------------------------------------------
def norm_name(s):
    s = s.lower()
    s = re.sub(r",\s*(stadt|kurort|moorbad).*$", "", s)
    s = s.replace("st.", "sankt ").replace("b.", "bei ").replace("v.", "vor ").replace("a.", "am ")
    s = unicodedata.normalize("NFKD", s)
    return "".join(c for c in s if c.isalnum())


def ags_lookup(cw, year):
    t = cw[(cw.year == str(year)) & cw.ags.str.startswith("16")].drop_duplicates("ags").copy()
    t["k"] = t.ags.str[:5]
    t["n"] = t.ags_name.map(norm_name)
    return t


def find_ags(tab, kreis, gemeinde):
    if (kreis, gemeinde) in AGS_PINS:
        return AGS_PINS[(kreis, gemeinde)]
    cand = tab[tab.k == KREIS[kreis]]
    nb = norm_name(gemeinde)
    hit = cand[cand.n == nb]
    if len(hit) != 1:
        hit = cand[cand.n.map(lambda x: x.startswith(nb) or nb.startswith(x))]
    if len(hit) != 1:
        raise ValueError(f"no unique AGS for {kreis} / {gemeinde}: {hit.ags_name.tolist()}")
    return hit.ags.iloc[0]


# ---------------------------------------------------------------------------
# helpers on the database frame
# ---------------------------------------------------------------------------
def norm_label(x):
    return re.sub(r"[^a-z0-9äöüß]", "", str(x).lower())


def match_votes(rows, wanted):
    """Assign each wanted vote count to one row index with that count (rows: list of
    (idx, votes)). Returns {wanted position: idx}; raises if a count is missing."""
    pool = list(rows)
    out = {}
    for pos, v in wanted:
        hits = [r for r in pool if r[1] == v]
        if not hits:
            return None
        out[pos] = hits[0][0]
        pool.remove(hits[0])
    return out


def main():
    for f in (PDF_2004, PDF_2010, DB_FILE, OB_FILE, CW_FILE, PINS_FILE):
        if not os.path.exists(f):
            sys.exit(f"missing input: {f}")

    db = pd.read_csv(DB_FILE, dtype=str, keep_default_na=False)
    ob = pd.read_csv(OB_FILE, dtype=str, keep_default_na=False)
    n_db, n_ob = len(db), len(ob)
    base = pd.concat([db, ob], ignore_index=True)
    for c in ("tls_name", "candidate_title", "pair_key", "name_source"):
        base[c] = ""
    base["flag_decisive_round_missing"] = False
    base["_votes"] = pd.to_numeric(base.candidate_votes, errors="coerce")
    cw = pd.read_csv(CW_FILE, dtype=str)
    cw["ags"] = cw.ags.str.zfill(8)
    tab04 = ags_lookup(cw, 2004)

    lines = pdf_lines(PDF_2004)
    blocks = parse_35(lines)
    ref32 = parse_32(lines)

    # ---- check 1: table 3.5 against the report's own table 3.2 -------------
    assert len(blocks) == 820, len(blocks)
    fields = STATS
    for k, v in ref32.items():
        bs = blocks if k == "Thüringen" else [b for b in blocks if b["kreis"] == k]
        got = [sum(b["stats"][f][0] or 0 for b in bs) for f in fields]
        assert got == v, (k, got, v)
    for b in blocks:
        for r in (0, 1):
            g, u, w = (b["stats"]["Gültige Stimmen"][r], b["stats"]["Ungültige Stimmen"][r],
                       b["stats"]["Wähler/Wahlbeteiligung"][r])
            if g is None and w is None:
                continue
            assert (g or 0) + (u or 0) == w, (b["gemeinde"], r)
            assert sum((c["hw"] if r == 0 else c["sw"]) or 0 for c in b["cands"]) == g, (b["gemeinde"], r)
    print(f"2004 table 3.5: {len(blocks)} Gemeinden, "
          f"{sum(b['stats']['Gültige Stimmen'][1] is not None for b in blocks)} runoffs; "
          f"reconciles with table 3.2 for all 17 Kreise and the state")

    # ---- names, elected person, Wiederholungswahl --------------------------
    repeat_needed = []
    for b in blocks:
        b["ags"] = find_ags(tab04, b["kreis"], b["gemeinde"])
        for c in b["cands"]:
            if is_sonstige(c["label"]):
                c["name"] = c["title"] = c["party"] = None
            else:
                c["name"], c["title"], c["party"] = split_label(c["label"])
        if b["elected"] == "Wiederholungswahl erforderlich":
            b["winner"] = None
            repeat_needed.append(b)
            continue
        en, _, _ = split_label(b["elected"])
        r = 1 if b["stats"]["Gültige Stimmen"][1] is not None else 0
        top = max((c for c in b["cands"] if (c["sw"] if r else c["hw"]) is not None),
                  key=lambda c: c["sw"] if r else c["hw"])
        assert top["name"] == en, (b["gemeinde"], top["label"], b["elected"])
        b["winner"] = top
    assert len({b["ags"] for b in blocks}) == 820
    assert sorted(b["gemeinde"] for b in repeat_needed) == ["Andenhausen", "Kauern", "Metebach", "St.Bernhard"]
    print(f"  elected person = top candidate of the decisive round in all {820 - len(repeat_needed)} "
          f"decided elections; {len(repeat_needed)} need a repeat election")

    # ---- match to the database ----------------------------------------------
    is_db = base.source == "wahlen.thueringen.de"
    added = []
    n_graft = n_new = 0
    agree = collections.Counter()
    for b in blocks:
        rounds = [(0, "2004-06-27", "hauptwahl")]
        if b["stats"]["Gültige Stimmen"][1] is not None:
            rounds.append((1, "2004-07-11", "stichwahl"))
        sel = {r: base.index[is_db & (base.ags == b["ags"]) & (base.election_date == d)].tolist()
               for r, d, _ in rounds}
        in_db = [bool(sel[r]) for r, _, _ in rounds]
        assert all(in_db) or not any(in_db), (b["gemeinde"], in_db)
        key = "hw", "sw"
        if any(in_db):
            for r, d, _ in rounds:
                idx = sel[r]
                row0 = base.loc[idx[0]]
                st = b["stats"]
                rep = tuple((st[f][r] or 0) for f in fields)
                dbv = (int(row0.eligible_voters), int(row0.number_voters), int(row0.invalid_votes or 0),
                       int(row0.valid_votes))
                assert rep == dbv, (b["gemeinde"], d, rep, dbv)
                lines_r = [(i, c) for i, c in enumerate(b["cands"]) if c[key[r]] is not None]
                dbrows = [(j, int(base.at[j, "_votes"])) for j in idx
                          if base.at[j, "candidate_party"] != "Weitere Personen"]
                named = [(i, c[key[r]]) for i, c in lines_r if c["name"]]
                # named report lines -> DB rows of the same vote count, top-down;
                # write-ins pooled as "Weitere Personen" in the DB find no row
                pool = list(dbrows)
                for i, v in sorted(named, key=lambda t: -t[1]):
                    hits = [p for p in pool if p[1] == v]
                    if hits:
                        j = hits[0][0]
                        pool.remove(hits[0])
                        c = b["cands"][i]
                        base.at[j, "tls_name"] = c["name"]
                        base.at[j, "candidate_title"] = c["title"] or ""
                        base.at[j, "name_source"] = SRC_2004
                        if b["stats"]["Gültige Stimmen"][1] is not None and c["sw"] is not None:
                            base.at[j, "pair_key"] = f"th2004:{b['ags']}:{i}"
                        n_graft += 1
                if b["winner"] is None:
                    base.loc[idx, "flag_decisive_round_missing"] = True
            # the DB names the elected person in the decisive round
            dr = 1 if len(rounds) == 2 else 0
            if b["winner"] is not None:
                w = [j for j in sel[dr] if base.at[j, "tls_name"] == b["winner"]["name"]]
                assert len(w) == 1, (b["gemeinde"], "winner not grafted")
            agree["db"] += 1
            # how well would the rule below have reproduced this round?
            sim = build_rows(b)[0]
            simv = sorted((r["candidate_party"] == "Weitere Personen", r["candidate_votes"]) for r in sim)
            dbr = base.loc[sel[0]]
            dbv2 = sorted((p == "Weitere Personen", int(v)) for p, v in zip(dbr.candidate_party, dbr._votes))
            agree["rule_ok" if simv == dbv2 else "rule_differs"] += 1
        else:
            for rr in build_rows(b)[0] + build_rows(b)[1]:
                added.append(rr)
            n_new += 1
    # a grafted name sits on a row whose label agrees with the report's Träger
    g = base[(base.name_source == SRC_2004)]
    lab_diff = 0
    for b in blocks:
        for c in b["cands"]:
            if not c["name"] or not c["party"]:
                continue
            rows = g[(g.ags == b["ags"]) & (g.tls_name == c["name"])]
            for lab in rows.candidate_party:
                if norm_label(lab) != norm_label(c["party"]):
                    lab_diff += 1
                    print(f"   label differs: {b['gemeinde']}: report ({c['party']}) vs database {lab!r}")
    assert lab_diff == 0, lab_diff
    print(f"  {agree['db']} Gemeinden also in the database: all counts identical; "
          f"{n_graft} database rows named")
    print(f"  added from the report: {n_new} Gemeinden "
          f"({sum(1 for r in added if r['round'] == 'stichwahl') // 2} runoffs)")
    rate = agree["rule_ok"] / agree["db"]
    print(f"  the write-in pooling rule reproduces the database in {agree['rule_ok']}/{agree['db']} "
          f"overlapping Hauptwahlen ({rate:.1%})")
    assert rate > 0.95

    # ---- 2010: the elections of 6 June 2010 (TLS 29926) ------------------------
    added2010, blocks2010 = process_2010(base, cw)
    added += added2010

    # ---- 2000 runoffs ---------------------------------------------------------
    added2000, lr2000 = graft_2000(base, cw)
    a2 = pd.DataFrame(added2000).astype(str)
    a2["_votes"] = pd.to_numeric(a2.candidate_votes)
    a2["flag_decisive_round_missing"] = False
    base = pd.concat([base, a2], ignore_index=True)

    # ---- table 3.4: mayors elected on another date ---------------------------
    dated = parse_34_dated(lines)
    assert len(dated) == 176, len(dated)
    _, missing34 = graft_34(base, dated, tab04, cw)
    added += winner_only_rows(missing34, tab04, cw)

    out = pd.concat([base.drop(columns="_votes"), pd.DataFrame(added)], ignore_index=True)
    out = out.fillna("")
    # ---- 2010 table 4.1: mayors elected on other dates (2005-2010) -------------
    out = process_41_2010(out, blocks2010, cw)
    # A Wahlvorschlag named after its candidate ("BWV <Surname>", or a truncated
    # surname) would publish the name: on the rows added from the reports, drop
    # the label token that is the candidate's surname or a 4+-letter start of it.
    # (The database's own labels on its rows are left as the database prints them.)
    added_rows = out.source.str.startswith("TLS") & (out.candidate_name != "")
    stripped = out.loc[added_rows].apply(lambda r: strip_own_name(r.candidate_party, r.candidate_name), axis=1)
    n_strip = int((stripped != out.loc[added_rows, "candidate_party"]).sum())
    out.loc[added_rows, "candidate_party"] = stripped
    print(f"  candidate names removed from {n_strip} Wahlvorschlag labels of added rows")
    assert n_strip == 2, n_strip
    for c in ("flag_decisive_round_missing", "winner_only"):
        out[c] = out[c].map(lambda x: "TRUE" if x is True or x == "True" else "FALSE")
    decide_majority_rule(out)
    check_2010_winners(out, blocks2010)
    assert (out.source == "wahlen.thueringen.de").sum() == n_db
    out.to_csv(OUT, index=False)
    print(f"wrote {OUT}: {len(out)} rows ({n_db + n_ob} from the inputs, {len(out) - n_db - n_ob} added); "
          f"{(out.tls_name != '').sum()} input rows named")
    if lr2000 is not None:
        lr2000.to_csv(OUT_LR, index=False)
        print(f"wrote {OUT_LR}: {len(lr2000)} rows")


# ---------------------------------------------------------------------------
# Who was elected: the majority rule over time
# ---------------------------------------------------------------------------
# A Thüringen Bürgermeister needs an absolute majority. Without one, a list
# election goes to a runoff between the top two, and an election "ohne Bindung
# an Wahlvorschläge" (write-ins) is repeated; the database and the TLS reports
# show a repeat election within months in nearly every such case. The exception
# is the period in which the runoff was abolished: on 6 June 2010 "genügte eine
# einfache Mehrheit" (TLS 29926, Vorbemerkungen; the 2010 amendment restoring the
# runoff did not yet apply). The database holds no runoff for any election held
# between 19 April 2009 and 22 August 2010, and seven plurality winners of 2009 and
# early 2010 appear as mayors in office in the 2010 report's table 4.1 (Schmölln
# 30.8 %, Krauthausen, Altenbeuthen, Leutenberg, Kauern). The window below
# brackets that period; decide_majority_rule() asserts that no runoff falls in it.
NO_RUNOFF_WINDOW = ("2009-05-01", "2010-07-31")
# Window elections whose plurality winner never took office: "Neuwahl
# erforderlich" in TLS 29926 (the elected person declined) for Lehesten,
# Renthendorf and Mittelpöllnitz; Bruchstedt held a Neuwahl on 15.08.2010, whose
# winner table 4.1 lists as the mayor in office. Keyed on the printed valid
# votes, so a changed source stops the run.
NOT_SEATED_IN_WINDOW = {("16073046", "2010-06-06"): 914, ("16074077", "2010-06-06"): 163,
                        ("16075066", "2010-06-06"): 109, ("16064009", "2010-02-21"): 106}
# Write-ins pooled by the database (see build_rows): not a candidate, so never
# the winner of a plurality election.
POOLED = "Weitere Personen"


def decide_majority_rule(out):
    """Flag first rounds that seated nobody, and in the no-runoff window make the
    top NAMED candidate the winner (column is_winner_src, read by 01/01b).

    Outside the window: a Hauptwahl with no majority (largest share, pooled
    write-ins included, below 50 %) and no runoff within five weeks decided
    nothing; the repeat election that followed is its own cycle in the data.
    Inside the window: the candidate with the most votes was elected, unless the
    pin above says the election was not accepted."""
    if "is_winner_src" not in out:
        out["is_winner_src"] = ""
    mayor = out.election_type.isin(["Bürgermeisterwahl", "Oberbürgermeisterwahl"]) & (out.winner_only != "TRUE")
    v = pd.to_numeric(out.candidate_votes, errors="coerce")
    valid = pd.to_numeric(out.valid_votes, errors="coerce")
    d = pd.to_datetime(out.election_date)
    in_win = (d >= NO_RUNOFF_WINDOW[0]) & (d <= NO_RUNOFF_WINDOW[1])
    sw_keys = out[mayor & (out["round"] == "stichwahl")][["ags"]].assign(d=d)
    assert not (in_win & mayor & (out["round"] == "stichwahl")).any(), "a runoff inside the no-runoff window"
    failed, not_seated, renamed = [], [], []
    hw = out[mayor & (out["round"] == "hauptwahl") & valid.gt(0)]
    for (ags, date), g in hw.groupby(["ags", "election_date"]):
        idx = g.index
        if (out.loc[idx, "flag_decisive_round_missing"] == "TRUE").any():
            continue
        dd = pd.Timestamp(date)
        s = sw_keys[(sw_keys.ags == ags) & (sw_keys.d > dd) & (sw_keys.d <= dd + pd.Timedelta(days=35))]
        top = (v[idx] / valid[idx]).max()
        if NO_RUNOFF_WINDOW[0] <= date <= NO_RUNOFF_WINDOW[1]:
            if (ags, date) in NOT_SEATED_IN_WINDOW:
                assert valid[idx].iloc[0] == NOT_SEATED_IN_WINDOW[(ags, date)], (ags, date)
                out.loc[idx, "flag_decisive_round_missing"] = "TRUE"
                not_seated.append((ags, date))
                continue
            named = [j for j in idx if out.at[j, "candidate_party"] != POOLED]
            top_named = max(named, key=lambda j: v[j])
            if v[top_named] < v[idx].max():          # the pooled write-ins lead
                assert sum(v[j] == v[top_named] for j in named) == 1, (ags, date, "tie")
                out.loc[idx, "is_winner_src"] = "FALSE"
                out.at[top_named, "is_winner_src"] = "TRUE"
                renamed.append((ags, date))
        elif len(s) == 0 and top < 0.5:
            out.loc[idx, "flag_decisive_round_missing"] = "TRUE"
            failed.append((ags, date))
    assert len(not_seated) == len(NOT_SEATED_IN_WINDOW), not_seated
    print(f"majority rule: {len(failed)} first rounds without a majority or runoff seated nobody "
          f"({', '.join(sorted({x[1][:4] for x in failed}))}); {len(not_seated)} window elections not "
          f"accepted; {len(renamed)} window elections won by the top named candidate, not the pooled "
          f"write-ins")
    assert len(failed) == 14 and len(renamed) == 8, (failed, renamed)


def build_rows(b):
    """Rows for a 2004 election the database lacks, in the database's convention.
    Returns (hauptwahl rows, stichwahl rows)."""
    st = b["stats"]
    has_sw = st["Gültige Stimmen"][1] is not None
    out = ([], [])
    for r, date, rnd in ((0, "2004-06-27", "hauptwahl"), (1, "2004-07-11", "stichwahl")):
        if r == 1 and not has_sw:
            break
        key = "hw" if r == 0 else "sw"
        cands = [(i, c) for i, c in enumerate(b["cands"]) if c[key] is not None]
        keep, pooled = [], 0
        for pos, (i, c) in enumerate(cands):
            if c["name"] is None:                          # "sonstige"
                if b["ohne_bindung"]:
                    pooled += c[key]
                else:
                    keep.append((i, c, "Sonstige"))
                continue
            separate = (not b["ohne_bindung"] or pos == 0 or c["party"] is not None
                        or c["sw"] is not None or c is b["winner"])
            if separate:
                keep.append((i, c, c["party"] or "Einzelbewerber"))
            else:
                pooled += c[key]
        g = st["Gültige Stimmen"][r]
        common = {
            "ags": b["ags"], "ags_name": b["gemeinde"], "state": "16", "state_name": "Thüringen",
            "election_year": "2004", "election_date": date, "election_type": "Bürgermeisterwahl",
            "round": rnd, "amt": b["amt"], "eligible_voters": st["Wahlberechtigte"][r],
            "number_voters": st["Wähler/Wahlbeteiligung"][r],
            "valid_votes": g, "invalid_votes": st["Ungültige Stimmen"][r] or 0,
            "turnout": round(st["Wähler/Wahlbeteiligung"][r] / st["Wahlberechtigte"][r], 4),
            "winner_name_raw": "", "gemnr": str(int(b["ags"][2:])), "source": SRC_2004,
            "flag_decisive_round_missing": b["winner"] is None,
        }
        for i, c, lab in keep:
            out[r].append({**common, "candidate_name": c["name"] or "", "tls_name": "",
                           "candidate_title": c["title"] or "", "candidate_party": lab,
                           "candidate_votes": c[key], "candidate_voteshare": round(c[key] / g, 4),
                           "pair_key": f"th2004:{b['ags']}:{i}" if has_sw and c["sw"] is not None else "",
                           "name_source": SRC_2004 if c["name"] else ""})
        if pooled:
            out[r].append({**common, "candidate_name": "", "tls_name": "", "candidate_title": "",
                           "candidate_party": "Weitere Personen", "candidate_votes": pooled,
                           "candidate_voteshare": round(pooled / g, 4), "pair_key": "", "name_source": ""})
        assert sum(x["candidate_votes"] for x in out[r]) == g
    return out


def pair_partner(base, hw_idx, sw_row):
    """The Hauptwahl row of the person on Stichwahl row sw_row: the only HW row
    with the same Wahlvorschlag label. None if the label is not unique, which is
    exactly when 01b's own party-key pairing would fail too."""
    lab = norm_label(base.at[sw_row, "candidate_party"])
    hits = [j for j in hw_idx if norm_label(base.at[j, "candidate_party"]) == lab]
    return hits[0] if len(hits) == 1 else None


def name_row(base, j, name, title, src):
    if base.at[j, "tls_name"] and base.at[j, "tls_name"] != name:
        raise ValueError(f"row {j} already named {base.at[j, 'tls_name']!r}, not {name!r}")
    base.at[j, "tls_name"] = name
    base.at[j, "candidate_title"] = title or ""
    base.at[j, "name_source"] = src


def graft_34(base, dated, tab04, cw):
    """Names of mayors elected on another date than 27.06.2004 (table 3.4)."""
    srch = base.source.isin(["wahlen.thueringen.de", SRC_2000])
    stats = collections.Counter()
    missing, party_diff = [], []
    for r in dated:
        d = pd.to_datetime(r["date"], dayfirst=True)
        # The first "Landkreis Sömmerda" heading of 3.4 is missing, so the Kreis
        # can be wrong: try the printed Kreis first, then all others.
        cands = []
        for k in [r["kreis"]] + [k for k in KREIS if k != r["kreis"]]:
            try:
                cands.append(find_ags(tab04, k, r["gemeinde"]))
            except ValueError:
                continue
        r["ags04"] = cands[0] if cands else None

        def rounds_of(a):
            sub = base[srch & (base.ags == a)]
            sd = pd.to_datetime(sub.election_date)
            return sub[(sd >= d) & (sd <= d + pd.Timedelta(days=35))]

        def holds(a):
            """a round of AGS a in the window whose WINNER has the printed votes
            and share"""
            rr = rounds_of(a)
            for _, g in rr.groupby(["election_date", "round"]):
                top = g._votes.max()
                v = pd.to_numeric(g.valid_votes.iloc[0], errors="coerce")
                if top == r["votes"] and (pd.isna(v) or abs(top / v * 100 - r["pct"]) < 0.06):
                    return True
            return False
        hit = [a for a in cands if holds(a)]
        if not hit:
            # filed under a later AGS? search the Kreis by date, winner votes and share
            for a in cands:
                pool = base[srch & base.ags.str.startswith(a[:5]) & ~base.ags.isin(cands)]
                hit += [x for x in sorted(set(pool.ags)) if holds(x)]
            hit = sorted(set(hit))
            if len(hit) == 1:
                stats["other_ags"] += 1
        if len(hit) != 1:
            assert not hit, (r, hit)
            missing.append(r)
            continue
        ags = hit[0]
        name, title, _ = split_label(r["name"])
        rr = rounds_of(ags)
        hw = rr.index[(rr.election_date == d.strftime("%Y-%m-%d")) & (rr["round"] == "hauptwahl")].tolist()
        sw_dates = sorted(set(rr.election_date[(rr["round"] == "stichwahl")]))
        assert len(sw_dates) <= 1, (r, sw_dates)
        if sw_dates and pd.Timestamp(sw_dates[0]) == d:
            # 3.4 printed the runoff's own date (Uhlstädt-Kirchhasel 06.10.2002): the
            # first round is the latest Hauptwahl of the AGS within five weeks before.
            # Taking the runoff's own rows for it left the winner's first round
            # unpaired, split into two rows by 01b.
            sub = base[srch & (base.ags == ags) & (base["round"] == "hauptwahl")]
            sd = pd.to_datetime(sub.election_date)
            prev = sub[(sd < d) & (sd >= d - pd.Timedelta(days=35))]
            hw = prev.index[prev.election_date == prev.election_date.max()].tolist() if len(prev) else []
        # The 3.4 runoff marker is not reliable (Gotha 2000 went to a runoff on
        # 28.05.2000, p. 15 of TLS 29924, and carries none), so the decisive
        # round is the runoff whenever one follows within five weeks.
        if sw_dates:
            dec = rr.index[rr.election_date == sw_dates[0]].tolist()
            stats["runoff_unmarked"] += not r["runoff"]
            r["runoff"] = True
            stats[f"gap_{(pd.Timestamp(sw_dates[0]) - d).days}"] += 1
        else:
            dec = hw
        w = [j for j in dec if base.at[j, "_votes"] == r["votes"]]
        if len(w) != 1:
            raise ValueError(f"3.4 winner votes not unique/absent in the decisive round: {r}")
        j = w[0]
        if base.at[j, "valid_votes"]:
            assert abs(r["votes"] / float(base.at[j, "valid_votes"]) * 100 - r["pct"]) < 0.06, r
        assert base.at[j, "_votes"] == base.loc[dec, "_votes"].max(), r
        if r["party"] and norm_label(r["party"]) != norm_label(base.at[j, "candidate_party"]):
            party_diff.append((r["gemeinde"], r["date"], r["party"], base.at[j, "candidate_party"]))
        name_row(base, j, name, title, base.at[j, "name_source"] or SRC_2004_34)
        stats["named"] += 1
        if r["runoff"] and hw:
            p = pair_partner(base, hw, j)
            if base.at[j, "pair_key"]:
                stats["runoff_paired"] += 1          # linked already by the 2000 report
            elif p is not None:
                name_row(base, p, name, title, SRC_2004_34)
                base.at[j, "pair_key"] = base.at[p, "pair_key"] = f"th34:{ags}:{r['date']}"
                stats["runoff_paired"] += 1
            else:
                stats["runoff_unpaired"] += 1
    print(f"2004 table 3.4: {len(dated)} mayors elected on another date; {stats['named']} found in the "
          f"inputs ({stats['other_ags']} under a later AGS; {stats['runoff_paired']} runoff winners linked "
          f"to their first round, {stats['runoff_unpaired']} not linkable; {stats['runoff_unmarked']} "
          f"runoffs unmarked in 3.4); {len(missing)} elections in no other source")
    if party_diff:
        print(f"   {len(party_diff)} with a differently written Wahlvorschlag (3.4 vs database):", party_diff)
    # For a runoff 3.4 prints the decisive round's votes but usually the FIRST
    # round's date (22 of 23 runoffs another source dates) and once the runoff's
    # own (Uhlstädt-Kirchhasel 06.10.2002). So a runoff only 3.4 records cannot
    # be dated: winner_only_rows() leaves those out.
    gaps = {k: v for k, v in stats.items() if k.startswith("gap_")}
    assert gaps == {"gap_14": 22, "gap_0": 1}, gaps
    return stats, missing


def winner_only_rows(missing, tab04, cw):
    """Elections that only table 3.4 records: the elected person, their votes and
    share in the decisive round, full-/part-time. Nothing else is known, so
    counts are empty and winner_only = TRUE (01b leaves n_candidates NA).

    A runoff gets two rows, linked by pair_key: the first round on the date 3.4
    prints (no result known) and the runoff on the TLS's date with the printed
    votes. Filed as a runoff without its first round, Andenhausen's runoff of
    10.10.2004 would fall within 01b's pairing window of the failed write-in
    election of 27.06.2004 and be folded into it."""
    out, undated, runoffs = [], [], 0
    for r in missing:
        d = pd.to_datetime(r["date"], dayfirst=True)
        sw = None
        if r["runoff"]:
            sw = RUNOFF_DATES_34.get((r["gemeinde"], r["date"]))
            if sw is None:
                undated.append(f"{r['gemeinde']} {r['date']}")
                continue
            sw = pd.to_datetime(sw, dayfirst=True)
            assert pd.Timedelta(days=7) <= sw - d <= pd.Timedelta(days=21), (r, sw)
            runoffs += 1
        ags = r["ags04"]
        assert ags, r
        tab = ags_lookup(cw, d.year)
        assert (tab.ags == ags).any(), (r, "AGS not valid in the election year")
        name, title, _ = split_label(r["name"])
        row = {
            "ags": ags, "ags_name": r["gemeinde"], "state": "16", "state_name": "Thüringen",
            "election_year": str(d.year), "election_date": d.strftime("%Y-%m-%d"),
            "election_type": "Bürgermeisterwahl",
            "round": "hauptwahl", "amt": "hauptamtlich" if r["hauptamtlich"] else "ehrenamtlich",
            "eligible_voters": "", "number_voters": "", "valid_votes": "", "invalid_votes": "",
            "turnout": "", "candidate_name": name, "tls_name": "", "candidate_title": title or "",
            "candidate_party": r["party"] or "", "candidate_votes": r["votes"],
            "candidate_voteshare": round(r["pct"] / 100, 4), "winner_name_raw": "",
            "gemnr": str(int(ags[2:])), "source": SRC_2004_34, "pair_key": "",
            "name_source": SRC_2004_34, "flag_decisive_round_missing": False, "winner_only": True}
        if sw is None:
            out.append(row)
            continue
        key = f"th34:{ags}:{r['date']}"
        out.append({**row, "candidate_votes": "", "candidate_voteshare": "", "pair_key": key})
        out.append({**row, "election_date": sw.strftime("%Y-%m-%d"), "round": "stichwahl",
                    "pair_key": key})
    print(f"  added {len(out) - runoffs} winner-only elections from table 3.4 ({runoffs} runoffs dated "
          f"by the TLS, first round without a result); left out {len(undated)} runoffs whose date "
          f"3.4 leaves ambiguous: {', '.join(undated)}")
    assert not undated and runoffs == len(RUNOFF_DATES_34), (undated, runoffs)
    return out


def split_label_2000(label):
    """The 2000 report writes 'Last, First[, Title] (Party)', once 'Dr. Last, First
    (Party)', and in Westenfeld 'First Last'. A label cut at the page edge loses
    its closing bracket ('(FWG Pro')."""
    lab = label.strip()
    party = None
    m = re.match(r"^(.*?)\s*\(([^()]*)\)?$", lab)
    if m and "(" in lab:
        lab, party = m.group(1).strip(), m.group(2).strip()
    parts = [x.strip() for x in lab.split(",")]
    title = None
    if len(parts) == 3:
        title = parts[2]
        parts = parts[:2]
    if len(parts) == 1:                      # "Given Surname" (one Gemeinde prints it so)
        tok = parts[0].split()
        assert len(tok) == 2, label
        parts = [tok[1], tok[0]]
    m = TITLE_RE.match(parts[0])
    if m:
        title = m.group(0).strip()
        parts[0] = parts[0][m.end():].strip()
    return f"{parts[0]}, {parts[1]}", title, party


def close(a, b):
    """Preliminary (2000 report) vs final (database) figures: a few votes apart
    at most (Suhl OB 2000: 10 160 vs 10 162)."""
    return abs(a - b) <= max(10, 0.005 * max(a, b))


def find_round(base, mask, kreis5, date, votes):
    """Index list of the round on `date` whose candidate votes match `votes`
    (rank by rank, within `close`), first under the expected AGS (mask), else
    anywhere in the Kreis: the database files some pre-merger elections under
    the merged Gemeinde's current AGS."""
    want = sorted(votes, reverse=True)
    for m in (mask, base.ags.str.startswith(kreis5)):
        sub = base[m & (base.election_date == date)]
        hits = []
        for _, g in sub.groupby("ags"):
            have = sorted(g._votes.dropna().astype(int), reverse=True)
            if len(have) == len(want) and all(close(a, b) for a, b in zip(have, want)):
                hits.append(g.sort_values("_votes", ascending=False).index.tolist())
        if len(hits) == 1:
            return hits[0]
        assert len(hits) == 0, (date, votes, hits)
    return None


def graft_2000(base, cw):
    """Names of the runoffs of 28.05.2000 (TLS 29924, transcribed) and of
    Arnstadt's first round (an insert in the same report). Returns the rounds the
    inputs lack (Bürgermeister) and the Landrat runoffs."""
    t = pd.read_csv(TRANSCRIPT_2000, dtype=str, keep_default_na=False)
    hw_ins = pd.read_csv(TRANSCRIPT_2000.replace("runoffs", "arnstadt_hw"), dtype=str, keep_default_na=False)
    for c in ("eligible_voters", "number_voters", "invalid_votes", "valid_votes", "cand_votes"):
        t[c] = t[c].astype(int)
        if c in hw_ins:
            hw_ins[c] = hw_ins[c].astype(int)
    # the transcription's own checks, repeated
    for _, g in t.groupby("pdf_page"):
        assert g.cand_votes.sum() == g.valid_votes.iloc[0]
        assert g.invalid_votes.iloc[0] + g.valid_votes.iloc[0] == g.number_voters.iloc[0]
    assert t.groupby("level").area_code.nunique().to_dict() == {"BM": 25, "LR": 3, "OB": 3}
    assert hw_ins.cand_votes.sum() == hw_ins.valid_votes.iloc[0] == 10301

    srch = base.source.isin(["wahlen.thueringen.de", "ThLfS Daten HTML"])
    added, lr_rows = [], []
    st = collections.Counter()
    for (lvl, code), g in t.groupby(["level", "area_code"], sort=False):
        g = g.sort_values("cand_nr")
        names = [split_label_2000(x) for x in g.cand_label]
        votes = g.cand_votes.tolist()
        if lvl == "LR":
            ags = f"16{code}000"
            for (nm, ti, pa), (_, r) in zip(names, g.iterrows()):
                lr_rows.append({
                    "candidate_name": r.cand_label.rsplit(" (", 1)[0], "candidate_party": pa,
                    "candidate_votes": r.cand_votes, "ags": ags,
                    "ags_name": "Landkreis " + r.area_name, "state": "16", "state_name": "Thüringen",
                    "election_year": 2000, "election_date": "2000-05-28", "election_type": "Landratswahl",
                    "eligible_voters": r.eligible_voters, "number_voters": r.number_voters,
                    "valid_votes": r.valid_votes, "invalid_votes": r.invalid_votes,
                    "turnout": round(r.number_voters / r.eligible_voters, 6),
                    "candidate_voteshare": r.cand_votes / r.valid_votes, "round": "stichwahl",
                    "source_file": os.path.basename(TRANSCRIPT_2000),
                    "election_date_stand": "2000-05-28"})
            continue
        ags = f"16{code}000" if lvl == "OB" else f"160{code}"
        sw = find_round(base, srch & (base.ags == ags), ags[:5], "2000-05-28", votes)
        if sw is None:
            assert lvl == "BM", code
            # not in the database: add the runoff (preliminary figures; its first
            # round of 14.05.2000 is in no source we hold)
            r0 = g.iloc[0]
            tab = ags_lookup(cw, 2000)
            nm0 = tab.loc[tab.ags == ags, "ags_name"]
            assert len(nm0) == 1, (code, "AGS not in the 2000 crosswalk")
            for k, ((nm, ti, pa), (_, r)) in enumerate(zip(names, g.iterrows())):
                added.append({
                    "ags": ags, "ags_name": r.area_name, "state": "16", "state_name": "Thüringen",
                    "election_year": "2000", "election_date": "2000-05-28",
                    "election_type": "Bürgermeisterwahl", "round": "stichwahl", "amt": r.amt,
                    "eligible_voters": r.eligible_voters, "number_voters": r.number_voters,
                    "valid_votes": r.valid_votes, "invalid_votes": r.invalid_votes,
                    "turnout": round(r.number_voters / r.eligible_voters, 4),
                    "candidate_name": nm, "tls_name": "", "candidate_title": ti or "",
                    "candidate_party": pa or "Einzelbewerber", "candidate_votes": r.cand_votes,
                    "candidate_voteshare": round(r.cand_votes / r.valid_votes, 4),
                    "winner_name_raw": "", "gemnr": str(int(ags[2:])), "source": SRC_2000,
                    "pair_key": "", "name_source": SRC_2000, "flag_decisive_round_missing": False})
            st["added"] += 1
            continue
        st["named_" + lvl] += 1
        sw_ags = base.at[sw[0], "ags"]
        if sw_ags != ags:
            st["other_ags"] += 1
        hw = base.index[srch & (base.ags == sw_ags) & (base.election_date == "2000-05-14")].tolist()
        assert hw, (code, "no first round")
        open_sw = []
        exact = sorted(votes) == sorted(int(base.at[x, "_votes"]) for x in sw)
        st["exact" if exact else "preliminary_differs"] += 1
        order = sorted(range(len(votes)), key=lambda k: -votes[k])     # sw is sorted by votes
        assert len(set(votes)) == len(votes), code
        for rank, k in enumerate(order):
            (nm, ti, pa), j = names[k], sw[rank]
            lab = base.at[j, "candidate_party"]
            assert pa is None or norm_label(pa) == norm_label(lab) or lab in ("Einzelbewerber", ""), \
                (code, nm, pa, lab)
            name_row(base, j, nm, ti, SRC_2000)
            if code == "70004":
                # Arnstadt: the insert names the first round, match by votes
                h = hw_ins[hw_ins.cand_label.map(lambda x: split_label_2000(x)[0]) == nm]
                assert len(h) == 1, nm
                p = [x for x in hw if base.at[x, "_votes"] == h.cand_votes.iloc[0]]
                p = p[0] if len(p) == 1 else None
            else:
                p = pair_partner(base, hw, j)
            if p is not None:
                name_row(base, p, nm, ti, SRC_2000)
                base.at[j, "pair_key"] = base.at[p, "pair_key"] = f"th2000:{sw_ags}:{nm}"
                st["paired"] += 1
            else:
                open_sw.append((j, nm, ti))
        # Elimination: the runoff is between the first round's top two (§ 24
        # ThürKWG). With one runoff candidate linked, the other is the remaining
        # top-two row, even when the label repeats ("Einzelbewerber" twice in
        # Schleusegrund) or changed (Ellrich: Einzelbewerber, then F.D.P.).
        top2 = sorted(hw, key=lambda x: -base.at[x, "_votes"])[:2]
        rest = [x for x in top2 if not base.at[x, "pair_key"]]
        if len(open_sw) == 1 and len(rest) == 1 and len(sw) == 2:
            (j, nm, ti), p = open_sw[0], rest[0]
            name_row(base, p, nm, ti, SRC_2000)
            base.at[j, "pair_key"] = base.at[p, "pair_key"] = f"th2000:{sw_ags}:{nm}"
            st["paired_by_elimination"] += 1
        else:
            st["unpaired"] += len(open_sw)
        if code == "70004":
            # the three Arnstadt candidates who did not reach the runoff
            for _, h in hw_ins.iterrows():
                nm, ti, pa = split_label_2000(h.cand_label)
                p = [x for x in hw if base.at[x, "_votes"] == h.cand_votes]
                assert len(p) == 1, nm
                if not base.at[p[0], "tls_name"]:
                    name_row(base, p[0], nm, ti, SRC_2000)
    print(f"2000 runoffs (TLS 29924): {st['named_OB']} OB + {st['named_BM']} Bürgermeister runoffs named "
          f"in the inputs ({st['other_ags']} filed under a later AGS), {st['paired']} runoff candidates "
          f"linked to their first round by label, {st['paired_by_elimination']} by elimination, "
          f"{st['unpaired']} not linkable; {st['added']} Bürgermeister "
          f"runoffs added; {len(lr_rows) // 2} Landrat runoffs for the Landrat pipeline; "
          f"{st['preliminary_differs']} of the named runoffs differ by a few votes from the final count")
    return added, pd.DataFrame(lr_rows)


# ---------------------------------------------------------------------------
# 2010: TLS 29926, table 3.3 (the 689 Bürgermeisterwahlen of 6 June 2010)
# ---------------------------------------------------------------------------
# The database holds 389 of them (today's Gemeinden only). No runoff was held
# (see NO_RUNOFF_WINDOW): the elected person is the top named candidate, which
# the report's "Bürgermeister/in (...): <name>" line confirms for every election.
DATE_2010 = "2010-06-06"
PARTIES_32_2010 = ["CDU", "DIE LINKE", "SPD", "FDP", "GRÜNE"]       # + "sonstige"
HEAD_2010 = re.compile(r"^(Gemeinde\s+Stimmen|–—|Wähler\s+An-|Stimmverhalten|3\. Ergebnisse|"
                       r"3\.3 Ergebnisse|ahlen am 6\. Juni 2010|Thüringer Landesamt|"
                       r"ahlen in Thüringen|ahlen der |\d{1,3}$)")
STAT_2010 = re.compile(r"^(Wahlberechtigte|Wähler/Wahlbeteiligung|Ungültige Stimmen|Gültige Stimmen)"
                       r"\s+(" + NUM + r")\s+(\d{1,3},\d|x|-)$")
CAND_2010 = re.compile(r"^(?P<lab>.*?\S)\s+(?P<num>\d[\d ]*|-)\s+(?P<pct>\d{1,3},\d|-)$")


def stream_33_2010(pdf):
    """Table 3.3 prints two Gemeinde columns per page: read each half on its own
    (pdftotext interleaves them)."""
    import pdfplumber
    out = []
    with pdfplumber.open(pdf) as doc:
        for p in doc.pages:
            t = p.extract_text() or ""
            if "3.3 Ergebnisse nach Gemeinden" not in t or "Wahlberechtigte" not in t:
                continue
            for box in ((0, 0, p.width / 2, p.height), (p.width / 2, 0, p.width, p.height)):
                out += [l.strip() for l in p.crop(box).extract_text(layout=True).split("\n") if l.strip()]
    return out


def votes_2010(lab, numtxt, pct, valid):
    """A label can end in a number that runs into the votes ('BI Musterdorf 94 120'):
    take the split whose votes fit the printed share."""
    toks = numtxt.split()
    opts = [(k, int("".join(toks[k:]))) for k in range(len(toks))
            if re.fullmatch(r"\d{1,3}", toks[k]) and all(re.fullmatch(r"\d{3}", t) for t in toks[k + 1:])]
    if not opts:
        return lab, num(numtxt)
    k, v = (opts[0] if pct is None or not valid
            else min(opts, key=lambda kv: abs(kv[1] - pct * valid / 100)))
    return (lab + " " + " ".join(toks[:k])).strip(), v


def parse_33_2010(stream):
    blocks, cur, kreis, pend, state = [], None, None, "", None
    lines = [l for l in stream if not HEAD_2010.match(l)]
    for k, l in enumerate(lines):
        nxt = lines[k + 1] if k + 1 < len(lines) else ""
        m = re.match(r"^(?:Noch: )?Landkreis (.+)$", l)
        if m:
            kreis = m.group(1).strip()
            continue
        if nxt.startswith("Wahlberechtigte") and not STAT_2010.match(l):
            cur = {"kreis": kreis, "gemeinde": l, "stats": {}, "cands": [], "elected": None,
                   "amt": None, "neuwahl": False}
            blocks.append(cur)
            state, pend = "stats", ""
            continue
        if cur is None:
            raise ValueError(f"2010 table 3.3: text before the first Gemeinde: {l!r}")
        m = STAT_2010.match(l)
        if m:
            cur["stats"][m.group(1)] = num(m.group(2))
            continue
        if l == "davon entfielen auf":
            state = "cands"
            continue
        m = re.match(r"^Bürgermeister/in \((ehrenamtlich|hauptamtlich)\):\s*(.*)$", l)
        if m:
            cur["amt"], cur["elected"], state = m.group(1), m.group(2).strip(), "elected"
            continue
        if l == "Neuwahl erforderlich":
            cur["neuwahl"], state = True, "elected"
            continue
        if state == "cands":
            m = CAND_2010.match(l)
            if m:
                pct = None if m["pct"] == "-" else float(m["pct"].replace(",", "."))
                lab, v = votes_2010((pend + " " + m["lab"]).strip(), m["num"], pct,
                                    cur["stats"].get("Gültige Stimmen"))
                cur["cands"].append({"label": re.sub(r"\s+", " ", lab), "votes": v})
                pend = ""
            else:                                    # a label wrapped onto two lines
                pend = (pend + " " + l).strip()
            continue
        if state == "elected" and (l.startswith("(") or not re.search(r"\d", l)):
            # the Träger wrapped onto the next line; a bracket at the crop line
            # can be lost ("Name, Given" + "WG Musterdorf)")
            if l.endswith(")") and "(" not in l and "(" not in (cur["elected"] or ""):
                l = "(" + l
            cur["elected"] = ((cur["elected"] or "") + " " + l).strip()
            continue
        raise ValueError(f"2010 table 3.3: unparsed line in {cur['gemeinde']}: {l!r}")
    return blocks


def parse_32_2010(lines):
    """Table 3.2 per Landkreis and in total ('Gesamt'): Wahlberechtigte, Wähler,
    Gültige, the votes of CDU, DIE LINKE, SPD, FDP, GRÜNE and sonstige, and the
    same parties' Mandate (one per election, also where a Neuwahl followed)."""
    s = [i for i, l in enumerate(lines) if l.strip() == "3.2 Ergebnisse nach Landkreisen"][0]
    cells = lambda x: [None if t == "-" else int(t.replace(" ", "")) for t in re.split(r"\s{2,}", x.strip())]
    ref, cur = {}, None
    for l in lines[s:s + 80]:
        m = re.match(r"^(\S.*?)\s+Anzahl\s+(.*)$", l)
        if m:
            cur = m.group(1).strip()
            ref[cur] = {"n": cells(m.group(2))}
            continue
        m = re.match(r"^\s+Mandate\s+x\s+x\s+(.*)$", l)
        if m:
            ref[cur]["m"] = cells(m.group(1))
    assert len(ref) == 18 and all(len(v["n"]) == 9 and len(v["m"]) == 7 for v in ref.values()), ref
    return ref


def build_rows_2010(b):
    """Rows for a 2010 election the database lacks, in the database's convention:
    a list election (no 'sonstige' line) carries every Wahlvorschlag; in a write-in
    election the first-listed person, every person with a Träger and the elected
    person stay separate, and the other write-ins are pooled as 'Weitere Personen'."""
    st = b["stats"]
    g = st["Gültige Stimmen"]
    write_in = any(c["name"] is None for c in b["cands"])
    keep, pooled = [], 0
    for pos, c in enumerate(b["cands"]):
        if c["name"] is not None and (not write_in or pos == 0 or c["party"] is not None or c is b["top"]):
            keep.append(c)
        else:
            pooled += c["votes"] or 0
    common = {
        "ags": b["ags"], "ags_name": b["gemeinde"], "state": "16", "state_name": "Thüringen",
        "election_year": "2010", "election_date": DATE_2010, "election_type": "Bürgermeisterwahl",
        "round": "hauptwahl", "amt": b["amt"] or "", "eligible_voters": st["Wahlberechtigte"],
        "number_voters": st["Wähler/Wahlbeteiligung"], "valid_votes": g,
        "invalid_votes": st["Ungültige Stimmen"] or 0,
        "turnout": round(st["Wähler/Wahlbeteiligung"] / st["Wahlberechtigte"], 4),
        "winner_name_raw": "", "gemnr": str(int(b["ags"][2:])), "source": SRC_2010,
        "flag_decisive_round_missing": b["neuwahl"],
    }
    rows = [{**common, "candidate_name": c["name"], "tls_name": "", "candidate_title": c["title"] or "",
             "candidate_party": c["party"] or "Einzelbewerber", "candidate_votes": c["votes"] or 0,
             "candidate_voteshare": round((c["votes"] or 0) / g, 4), "pair_key": "",
             "name_source": SRC_2010} for c in keep]
    if pooled:
        rows.append({**common, "candidate_name": "", "tls_name": "", "candidate_title": "",
                     "candidate_party": POOLED, "candidate_votes": pooled,
                     "candidate_voteshare": round(pooled / g, 4), "pair_key": "", "name_source": ""})
    assert sum(r["candidate_votes"] for r in rows) == g, b["gemeinde"]
    return rows


def process_2010(base, cw):
    """Parse and check table 3.3, graft its names onto the database's rounds of
    6 June 2010 and return (rows for the elections the database lacks, blocks)."""
    blocks = parse_33_2010(stream_33_2010(PDF_2010))
    txt = subprocess.run(["pdftotext", "-layout", PDF_2010, "-"], capture_output=True,
                         text=True, check=True).stdout
    ref = parse_32_2010(txt.split("\n"))

    # ---- check 1: each block is complete and adds up --------------------------
    assert len(blocks) == 689, len(blocks)
    for b in blocks:
        st = b["stats"]
        assert len(st) == 4, (b["gemeinde"], st)
        assert (st["Ungültige Stimmen"] or 0) + st["Gültige Stimmen"] == st["Wähler/Wahlbeteiligung"], b["gemeinde"]
        assert sum(c["votes"] or 0 for c in b["cands"]) == st["Gültige Stimmen"], b["gemeinde"]
        assert bool(b["elected"]) != b["neuwahl"], b["gemeinde"]
        for c in b["cands"]:
            c["name"], c["title"], c["party"] = ((None, None, None) if is_sonstige(c["label"])
                                                 else split_label(c["label"]))
        named = [c for c in b["cands"] if c["name"]]
        b["top"] = max(named, key=lambda c: c["votes"] or 0)
        assert sum(c["votes"] == b["top"]["votes"] for c in named) == 1, (b["gemeinde"], "tie")
        b["winner"] = None if b["neuwahl"] else b["top"]
        if b["winner"]:
            # the elected person is the top named candidate (plurality, no runoff)
            assert split_label(b["elected"])[0] == b["top"]["name"], (b["gemeinde"], "elected")

    # ---- check 2: table 3.3 against the report's own table 3.2 ----------------
    # electorate, voters, valid votes, every party's votes, and every party's
    # Mandate counted as the top named candidate's Träger
    for k, v in ref.items():
        bs = blocks if k == "Gesamt" else [b for b in blocks if b["kreis"] == k]
        votes, seats = collections.Counter(), collections.Counter()
        for b in bs:
            for c in b["cands"]:
                votes[c["party"] if c["party"] in PARTIES_32_2010 else "sonstige"] += c["votes"] or 0
            seats[b["top"]["party"] if b["top"]["party"] in PARTIES_32_2010 else "sonstige"] += 1
        got = [sum(b["stats"][f] for b in bs) for f in ("Wahlberechtigte", "Wähler/Wahlbeteiligung",
                                                         "Gültige Stimmen")]
        got += [votes[p] or None for p in PARTIES_32_2010 + ["sonstige"]]
        assert got == v["n"], (k, got, v["n"])
        got_m = [len(bs)] + [seats[p] or None for p in PARTIES_32_2010 + ["sonstige"]]
        assert got_m == v["m"], (k, got_m, v["m"])
    neuwahl = sorted(b["gemeinde"] for b in blocks if b["neuwahl"])
    print(f"2010 table 3.3: {len(blocks)} Gemeinden; reconciles with table 3.2 for all 17 Kreise and "
          f"the state (electorate, voters, valid votes, party votes and Mandate); "
          f"elected person = top named candidate in all {len(blocks) - len(neuwahl)} accepted "
          f"elections; {len(neuwahl)} need a Neuwahl")

    # ---- AGS: the 2010 crosswalk, else 2009 (Gemeinden merged later in 2010) ---
    t10, t09 = ags_lookup(cw, 2010), ags_lookup(cw, 2009)
    n09 = 0
    for b in blocks:
        try:
            b["ags"] = find_ags(t10, b["kreis"], b["gemeinde"])
        except ValueError:
            b["ags"] = find_ags(t09, b["kreis"], b["gemeinde"])
            n09 += 1
    assert len({b["ags"] for b in blocks}) == 689
    assert n09 == 8, n09

    # ---- match to the database ------------------------------------------------
    is_db = base.source == "wahlen.thueringen.de"
    db10 = base[is_db & (base.election_date == DATE_2010)]
    assert set(db10.ags) <= {b["ags"] for b in blocks}, "a database election of 6.6.2010 not in the report"
    added, n_graft, agree = [], 0, collections.Counter()
    for b in blocks:
        idx = db10.index[db10.ags == b["ags"]].tolist()
        if not idx:
            added += build_rows_2010(b)
            continue
        st, row0 = b["stats"], base.loc[idx[0]]
        rep = tuple(st[f] or 0 for f in STATS)
        dbv = (int(row0.eligible_voters), int(row0.number_voters), int(row0.invalid_votes or 0),
               int(row0.valid_votes))
        assert rep == dbv, (b["gemeinde"], rep, dbv)
        pool = [(j, int(base.at[j, "_votes"])) for j in idx if base.at[j, "candidate_party"] != POOLED]
        for c in sorted((c for c in b["cands"] if c["name"]), key=lambda c: -(c["votes"] or 0)):
            hits = [p for p in pool if p[1] == c["votes"]]
            if hits:
                pool.remove(hits[0])
                name_row(base, hits[0][0], c["name"], c["title"], SRC_2010)
                n_graft += 1
        w = [j for j in idx if base.at[j, "tls_name"] == b["top"]["name"]]
        assert len(w) == 1, (b["gemeinde"], "elected person not on a database row")
        sim = sorted((r["candidate_party"] == POOLED, r["candidate_votes"]) for r in build_rows_2010(b))
        dbr = sorted((base.at[j, "candidate_party"] == POOLED, int(base.at[j, "_votes"])) for j in idx)
        agree["ok" if sim == dbr else "differs"] += 1
    n_db = agree["ok"] + agree["differs"]
    assert n_db == db10.ags.nunique() == 389, (n_db, db10.ags.nunique())
    # the report's Neuwahlen among the database's elections are the pinned ones
    pinned = {a for a, d in NOT_SEATED_IN_WINDOW if d == DATE_2010}
    assert {b["ags"] for b in blocks if b["neuwahl"] and b["ags"] in set(db10.ags)} == pinned
    print(f"  {n_db} Gemeinden also in the database: all counts identical; {n_graft} database rows "
          f"named; the write-in pooling rule reproduces the database in {agree['ok']}/{n_db}")
    assert agree["ok"] / n_db > 0.95
    n_new = len({r["ags"] for r in added})
    print(f"  added from the report: {n_new} Gemeinden ({len(added)} rows; {n09} on their 2009 code, "
          f"merged later in 2010)")
    assert n_new == 300, n_new
    return added, blocks


def check_2010_winners(out, blocks):
    """After decide_majority_rule: each accepted 2010 election seats exactly the
    report's elected person, and each Neuwahl seats nobody."""
    sel = out[(out.election_date == DATE_2010) & (out.election_type == "Bürgermeisterwahl")]
    v = pd.to_numeric(sel.candidate_votes, errors="coerce")
    for b in blocks:
        g = sel[sel.ags == b["ags"]]
        flagged = (g.flag_decisive_round_missing == "TRUE").all()
        if b["neuwahl"]:
            assert flagged, (b["gemeinde"], "Neuwahl not flagged")
            continue
        assert not (g.flag_decisive_round_missing == "TRUE").any(), (b["gemeinde"], "flagged")
        w = g[g.is_winner_src == "TRUE"] if (g.is_winner_src == "TRUE").any() else g[v[g.index] == v[g.index].max()]
        assert len(w) == 1, (b["gemeinde"], len(w))
        # database rows carry the report's name in tls_name (their candidate_name is
        # the scraped Wahlvorschlag string); added rows in candidate_name
        name = w.tls_name.iloc[0] if w.source.iloc[0] == "wahlen.thueringen.de" else w.candidate_name.iloc[0]
        assert name == b["winner"]["name"], (b["gemeinde"], "wrong winner")
    print(f"  2010: the rule seats the report's elected person in all "
          f"{sum(not b['neuwahl'] for b in blocks)} accepted elections")


# ---------------------------------------------------------------------------
# 2010: TLS 29926, table 4.1 (every mayor in office at the end of 2010)
# ---------------------------------------------------------------------------
# One row per Gemeinde: the mayor, Träger, hauptamtlich, write-in election
# ("Wahl ohne Bindung an zugelassene Wahlvorschläge"), votes and share. A date
# after the Gemeinde marks an election not held on 06.06.2010 (2005-2010, incl.
# the Neuwahlen of August-November 2010). The table has no runoff marker: for a
# runoff it prints the runoff's own date (22 of the 25 runoffs the database
# holds) or the first round's (3), always with the decisive round's votes.
SRC_2010_41 = "TLS 29926 Kommunalwahlen 2010 (endgültig), Tab. 4.1"
NUMS_41 = re.compile(r"(\d{1,3}(?: \d{3})*)\s+(\d{1,3},\d)")


def table_41_2010(pdf):
    """Rows of table 4.1, read by x-position: Gemeinde < 205 pt, name < 305,
    Träger < 440, the two 'x' columns (hauptamtlich at ~445, write-in at ~479),
    then votes and share. A wrapped entry starts at the Gemeinde column (57 pt)
    and carries its figures on its last line."""
    import pdfplumber
    rows, kreis = [], None
    with pdfplumber.open(pdf) as doc:
        for p in doc.pages:
            t = p.extract_text() or ""
            if "4.1 Übersicht der Thüringer" not in t or "Bürgermeister/in" not in t:
                continue
            ws = sorted((w for w in p.extract_words() if w["top"] > 140), key=lambda w: w["top"])
            lines, cur, last = [], [], None
            for w in ws:
                if last is not None and w["top"] - last > 3:
                    lines.append(sorted(cur, key=lambda w: w["x0"]))
                    cur = []
                cur.append(w)
                last = w["top"]
            if cur:
                lines.append(sorted(cur, key=lambda w: w["x0"]))
            entry = []
            for ln in lines:
                txt = " ".join(w["text"] for w in ln)
                if re.match(r"^\d{1,3}$|^Thüringer Landesamt für Statistik", txt):
                    continue
                m = re.match(r"^(?:Noch: )?Landkreis (.+)$", txt)
                if m:
                    assert not entry, txt
                    kreis = m.group(1).strip()
                    continue
                starts = min(w["x0"] for w in ln) < 60
                if starts == bool(entry):
                    raise ValueError(f"table 4.1: unexpected line {txt[:40]!r}")
                entry.append(ln)
                nums = " ".join(w["text"] for w in ln if w["x0"] >= 488)
                m = NUMS_41.fullmatch(nums)
                if not m:
                    continue

                def band(lo, hi):
                    # join the entry's lines; a hyphen at a line end joins the word
                    out = ""
                    for l in entry:
                        s = " ".join(w["text"] for w in l if lo <= w["x0"] < hi).strip()
                        if not s:
                            continue
                        if out.endswith("-") and s[0].islower():
                            out = out[:-1] + s
                        elif out.endswith("-"):
                            out += s
                        else:
                            out = f"{out} {s}".strip()
                    return out
                gem = band(0, 205)
                md = re.match(r"^(.*?)\s*\((\d\d\.\d\d\.\d{4})\)$", gem)
                rows.append({"kreis": kreis, "gemeinde": md.group(1) if md else gem,
                             "date": md.group(2) if md else None, "name": band(205, 305),
                             "party": band(305, 440),
                             "hauptamtlich": any(w["text"] == "x" and 440 <= w["x0"] < 460 for l in entry for w in l),
                             "write_in": any(w["text"] == "x" and 470 <= w["x0"] < 488 for l in entry for w in l),
                             "votes": int(m.group(1).replace(" ", "")),
                             "pct": float(m.group(2).replace(",", "."))})
                entry = []
            assert not entry
    return rows


def round_41(r, out):
    """Round of a 4.1 election in no other source. A write-in election and any
    election in the no-runoff window had no runoff; otherwise a printed date on
    which the database holds runoffs but no first round elsewhere is a runoff.
    On the 168 dated rows the database holds, the rule is right in 163 and never
    calls a first round a runoff; it files 5 runoffs as first rounds."""
    ds = pd.to_datetime(r["date"], dayfirst=True).strftime("%Y-%m-%d")
    if r["write_in"] or NO_RUNOFF_WINDOW[0] <= ds <= NO_RUNOFF_WINDOW[1]:
        return "hauptwahl"
    cal = out[(out.source == "wahlen.thueringen.de") & (out.election_date == ds)]
    hw, sw = (cal["round"] == "hauptwahl").any(), (cal["round"] == "stichwahl").any()
    return "stichwahl" if sw and not hw else "hauptwahl"


def process_41_2010(out, blocks, cw):
    """Check table 4.1 against table 3.3, name the database rounds of the mayors
    elected on other dates (restricted, like every TLS name) and add the
    elections in no other source as winner-only rows (like 2004's table 3.4)."""
    rows = table_41_2010(PDF_2010)
    assert len(rows) == 951 and sum(r["kreis"] is None for r in rows) == 6, len(rows)
    # the six Oberbürgermeister (kreisfreie Städte) come from the Info files
    rows = [r for r in rows if r["kreis"] is not None]

    # ---- check: the undated rows are table 3.3's elected persons ---------------
    # 3.3 tags duplicate Gemeinde names with the Kreis ("Steinbach (EIC)"); 4.1
    # truncates one name ("Brünn/Thür.")
    def key(b):
        return b["kreis"], norm_name(re.sub(r"\s*\([A-ZÄÖÜ]{2,3}\)$", "", b["gemeinde"]))
    bk = {key(b): b for b in blocks}
    assert len(bk) == len(blocks)
    seen = set()
    for r in rows:
        k = key(r)
        if k not in bk:
            hits = [x for x in bk if x[0] == k[0] and x[1].startswith(k[1])]
            k = hits[0] if len(hits) == 1 else k
        if r["date"] is None:
            b = bk[k]
            assert split_label(r["name"])[0] == b["top"]["name"] and r["votes"] == b["top"]["votes"], r["gemeinde"]
        seen.add(k) if k in bk else None
        r["in_33"] = k in bk
    redated = sorted(r["gemeinde"] for r in rows if r["date"] and r["in_33"])
    assert seen == set(bk), set(bk) - seen
    neuwahl = {key(b) for b in blocks if b["neuwahl"]}
    assert neuwahl <= {key(r) for r in rows if r["date"] and r["in_33"]}
    print(f"2010 table 4.1: {len(rows)} mayors in the Landkreise; the {sum(r['date'] is None for r in rows)} "
          f"undated are table 3.3's elected persons (name and votes); {len(redated)} Gemeinden of 3.3 list a later "
          f"election (the {len(neuwahl)} Neuwahlen and {len(redated) - len(neuwahl)} others)")

    # ---- dated rows: the database round, or a winner-only election -------------
    out = out.copy()
    out["is_winner_src"] = ""
    v = pd.to_numeric(out.candidate_votes, errors="coerce")
    d_all = pd.to_datetime(out.election_date)
    tabs, stats, added = {}, collections.Counter(), []
    for r in (r for r in rows if r["date"]):
        d = pd.to_datetime(r["date"], dayfirst=True)
        ags = AGS_PINS.get((r["kreis"], r["gemeinde"]))
        for y in (d.year, d.year - 1, 2010):
            if ags:
                break
            tabs.setdefault(y, ags_lookup(cw, y))
            try:
                ags = find_ags(tabs[y], r["kreis"], r["gemeinde"])
            except ValueError:
                pass
        assert ags, r["gemeinde"]
        sub = out[(out.ags == ags) & (d_all >= d) & (d_all <= d + pd.Timedelta(days=35))]
        hit = sub.index[(v[sub.index] == r["votes"]) & (sub.candidate_party != POOLED)].tolist()
        name, title, _ = split_label(r["name"])
        if len(hit) > 1:
            # a tie, decided by lot (§ 24 ThürKWG): 4.1's Träger names the winner
            hit = [j for j in hit if norm_label(out.at[j, "candidate_party"]) == norm_label(r["party"])]
            assert len(hit) == 1, (r["gemeinde"], "tie not resolved by the Träger")
            tie = out.index[(out.ags == ags) & (out.election_date == out.at[hit[0], "election_date"])
                            & (out["round"] == out.at[hit[0], "round"])]
            out.loc[tie, "is_winner_src"] = "FALSE"
            out.at[hit[0], "is_winner_src"] = "TRUE"
            stats["tie"] += 1
        if hit:
            j = hit[0]
            g = sub[(sub.election_date == out.at[j, "election_date"]) & (sub["round"] == out.at[j, "round"])]
            valid = pd.to_numeric(out.at[j, "valid_votes"], errors="coerce")
            assert pd.isna(valid) or abs(r["votes"] / valid * 100 - r["pct"]) < 0.06, r["gemeinde"]
            named = g[g.candidate_party != POOLED]
            assert v[j] == v[named.index].max(), (r["gemeinde"], "not the top named candidate")
            assert out.at[j, "source"] == "wahlen.thueringen.de", r["gemeinde"]
            # the decisive round is the runoff whenever one follows (as in 2004)
            sw = sub[sub["round"] == "stichwahl"]
            assert sw.empty or (out.at[j, "round"] == "stichwahl"), (r["gemeinde"], "runoff winner off the runoff")
            if out.at[j, "round"] == "stichwahl":
                stats[f"runoff_gap_{(d_all[j] - d).days}"] += 1
                hw_d = d_all[j] - pd.Timedelta(days=14)
                hw = out.index[(out.ags == ags) & (out.election_date == hw_d.strftime("%Y-%m-%d"))
                               & (out["round"] == "hauptwahl")].tolist()
                assert hw, (r["gemeinde"], "runoff without its first round 14 days earlier")
                p = pair_partner(out, hw, j)
                if p is None:
                    # by elimination: the first round's top two, less the one whose
                    # label the other runoff candidate shares
                    top2 = sorted(hw, key=lambda x: -v[x])[:2]
                    rest = [x for x in sw.index if x != j]
                    taken = {pair_partner(out, top2, x) for x in rest}
                    left = [x for x in top2 if x not in taken]
                    p = left[0] if len(rest) == 1 and len(left) == 1 else None
                if p is None:
                    # a name on the runoff row alone would make 01b split the person
                    # into two rows (Reichstädt 2009: three Einzelbewerber)
                    stats["runoff_unpaired"] += 1
                    continue
                name_row(out, p, name, title, SRC_2010_41)
                out.at[j, "pair_key"] = out.at[p, "pair_key"] = f"th41:{ags}:{out.at[j, 'election_date']}"
                stats["runoff_paired"] += 1
            name_row(out, j, name, title, SRC_2010_41)
            stats["named"] += 1
            continue
        assert sub.empty, (r["gemeinde"], "a round in the window but not the printed winner")
        rnd = round_41(r, out)
        # a runoff row with a first round within 120 days before would be paired
        # with it by 01b (a Hauptwahl row is a cycle of its own)
        assert rnd == "hauptwahl" or not ((out.ags == ags) & (d_all < d)
                                          & (d_all >= d - pd.Timedelta(days=120))).any(), r["gemeinde"]
        party = r["party"]
        sur = norm_label(name.split(",")[0])
        if party and (norm_label(party).startswith(sur) or sur.startswith(norm_label(party))):
            party = "Einzelbewerber"         # a Träger named after its candidate
        added.append({
            "ags": ags, "ags_name": r["gemeinde"], "state": "16", "state_name": "Thüringen",
            "election_year": str(d.year), "election_date": d.strftime("%Y-%m-%d"),
            "election_type": "Bürgermeisterwahl", "round": rnd,
            "amt": "hauptamtlich" if r["hauptamtlich"] else "ehrenamtlich",
            "eligible_voters": "", "number_voters": "", "valid_votes": "", "invalid_votes": "",
            "turnout": "", "candidate_name": name, "tls_name": "", "candidate_title": title or "",
            "candidate_party": party, "candidate_votes": r["votes"],
            "candidate_voteshare": round(r["pct"] / 100, 4), "winner_name_raw": "",
            "gemnr": str(int(ags[2:])), "source": SRC_2010_41, "pair_key": "",
            "name_source": SRC_2010_41, "flag_decisive_round_missing": False, "winner_only": True})
        stats[f"added_{rnd}"] += 1
    gaps = {k: n for k, n in stats.items() if k.startswith("runoff_gap")}
    print(f"  {stats['named']} mayors elected on another date named on their database round "
          f"({stats['runoff_paired']} runoff winners linked to their first round; {stats['runoff_unpaired']} "
          f"not linkable and left unnamed; printed date = runoff date {gaps.get('runoff_gap_0', 0)}x, first round "
          f"{gaps.get('runoff_gap_14', 0)}x); {len(added)} winner-only elections added "
          f"({stats['added_stichwahl']} filed as runoffs by the election calendar)")
    assert set(gaps) <= {"runoff_gap_0", "runoff_gap_14"}, gaps
    assert stats["tie"] == 1, stats["tie"]        # Bad Berka, runoff of 21.05.2006
    assert stats["named"] + stats["runoff_unpaired"] + len(added) == sum(1 for r in rows if r["date"])
    assert stats["runoff_unpaired"] == 1 and len(added) == 95, (stats, len(added))
    return pd.concat([out, pd.DataFrame(added)], ignore_index=True).fillna("")


if __name__ == "__main__":
    main()
