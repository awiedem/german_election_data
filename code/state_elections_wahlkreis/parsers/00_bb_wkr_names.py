#!/usr/bin/env python3
"""Stage-0: BRANDENBURG Landtagswahlkreis NAMES, per election year (1990-2024).

Why this exists: the Amt fuer Statistik Berlin-Brandenburg "Downloadtabelle"
workbooks that parse_BB.R reads are Wahlbezirk-level and carry the Landtags-
wahlkreis only as a NUMBER.  parse_BB.R therefore emitted the placeholder
"Landtagswahlkreis NN" for all 8 elections.  The real names live in the
Statistischer Bericht of each election, which is already in raw/.

Two acquisition routes, both from the state's own reports:

  * 1990, 1994, 1999 -- the report PDFs are image-only scans (1994, 1999) or
    carry unusable OCR (1990: "Baeskow-", "Liibben-", "Cottbus!"), so the lists
    below were read VISUALLY off high-DPI renderings, per the project's OCR
    guidance, and are pinned here with the exact source page.
  * 2004-2024 -- the reports have clean text layers, so the names are EXTRACTED
    on every run.  Nothing is inherited between years: Brandenburg renumbered
    its Wahlkreise twice (the 1993 Kreisgebietsreform, then the Wahlkreis-
    aenderungsgesetz of 23 Oct 1998), so e.g. WK 11 is "Oranienburg I" in 1990,
    "Havelland I" in 1994/1999 and "Uckermark I" from 2004.

Output: data/state_elections/processed/wahlkreis/wkr_names/BB_wkr_names.csv
        (election_year, wkr_nr, wkr_name) -- read by parsers/parse_BB.R
Run:    python3 code/state_elections_wahlkreis/parsers/00_bb_wkr_names.py
"""

import csv
import os
import re
import collections

import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW = os.path.join(ROOT, "data", "state_elections", "raw",
                   "Landtagswahlen_Wahlkreis", "Brandenburg")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed",
                       "wahlkreis", "wkr_names")
OUT = os.path.join(OUT_DIR, "BB_wkr_names.csv")

N_WKR = 44
EXTRACT_YEARS = [2004, 2009, 2014, 2019, 2024]

# --- pinned lists (image-only / unusable-OCR scans) ---------------------------
# 1990: "Verzeichnis der Wahlkreise", page 8 of the joint report of the
#       Gemeinsames Statistisches Amt in Berlin ("WAHL ZUM LANDTAG von
#       Brandenburg am 14. Oktober 1990"), Brandenburg block.
#       NB the source misprints WK 32 as "Eilsenhuettenstadt, Land"; WK 34 on the
#       same page spells the town correctly, so the typo is corrected here.
PINNED = {
    1990: [
        "Perleberg I", "Kyritz-Perleberg II", "Pritzwalk-Wittstock", "Neuruppin",
        "Gransee-Templin II", "Templin I-Angermünde I", "Prenzlau-Angermünde II",
        "Schwedt", "Nauen I", "Oranienburg III-Nauen II", "Oranienburg I",
        "Oranienburg II", "Bernau", "Eberswalde I", "Bad Freienwalde-Eberswalde II",
        "Rathenow", "Brandenburg I", "Brandenburg II", "Brandenburg, Land-Belzig",
        "Potsdam, Land I", "Potsdam, Land II", "Potsdam I", "Potsdam II",
        "Strausberg I", "Seelow-Strausberg II", "Fürstenwalde I", "Fürstenwalde II",
        "Jüterbog-Luckenwalde II-Zossen II", "Luckenwalde I-Zossen III",
        "Zossen I-Königs-Wusterhausen II", "Königs-Wusterhausen I",
        "Beeskow-Frankfurt/Oder II-Eisenhüttenstadt, Land", "Frankfurt/Oder I",
        "Eisenhüttenstadt", "Guben-Forst I", "Cottbus I", "Cottbus II",
        "Cottbus, Land-Forst II", "Spremberg-Calau I", "Calau II-Senftenberg II",
        "Senftenberg I", "Lübben-Luckau", "Herzberg-Finsterwalde I",
        "Bad Liebenwerda-Finsterwalde II",
    ],
    # 1994: section-1 table headings "Wahlkreis: NN (Name)", report pages 10-53
    #       of "Landtagswahl 11.09.1994 - Endgueltiges Ergebnis" (LDS Brandenburg).
    1994: [
        "Prignitz I", "Prignitz II", "Ostprignitz-Ruppin I", "Ostprignitz-Ruppin II",
        "Oberhavel I", "Oberhavel II", "Oberhavel III", "Uckermark I", "Uckermark II",
        "Uckermark III", "Havelland I", "Havelland II", "Barnim I", "Barnim II",
        "Barnim III", "Märkisch-Oderland I", "Märkisch-Oderland II",
        "Märkisch-Oderland III", "Brandenburg an der Havel I",
        "Brandenburg an der Havel II", "Potsdam-Mittelmark I", "Potsdam-Mittelmark II",
        "Potsdam-Mittelmark III", "Potsdam I", "Potsdam II", "Teltow-Fläming I",
        "Teltow-Fläming II", "Dahme-Spreewald I", "Dahme-Spreewald II", "Oder-Spree I",
        "Oder-Spree II", "Oder-Spree III / Frankfurt(O.)", "Oder-Spree IV",
        "Frankfurt (Oder)", "Elbe-Elster I", "Elbe-Elster II",
        "Oberspreewald-Lausitz I", "Oberspreewald-Lausitz II",
        "Oberspreewald-Lausitz III", "Cottbus I", "Cottbus II", "Spree-Neiße I",
        "Spree-Neiße II", "Spree-Neiße III",
    ],
    # 1999: appendix "Einteilung des Landes in Wahlkreise fuer die Wahl zum
    #       Landtag Brandenburg entsprechend der Anlage zu § 15 Abs. 1 BbgLWahlG,
    #       gemaess dem Wahlkreisaenderungsgesetz (GVBl. I S. 205) vom 23. Oktober
    #       1998", report pages 156-160.
    1999: [
        "Prignitz I", "Prignitz II", "Ostprignitz-Ruppin I", "Ostprignitz-Ruppin II",
        "Oberhavel I", "Oberhavel II", "Oberhavel III", "Uckermark I", "Uckermark II",
        "Uckermark III", "Havelland I", "Havelland II", "Barnim I", "Barnim II",
        "Barnim III", "Märkisch-Oderland I", "Märkisch-Oderland II",
        "Märkisch-Oderland III", "Teltow-Fläming III / Dahme-Spreewald III",
        "Brandenburg an der Havel I",
        "Potsdam-Mittelmark I / Brandenburg an der Havel II", "Potsdam-Mittelmark II",
        "Potsdam-Mittelmark III", "Potsdam I", "Potsdam II", "Teltow-Fläming I",
        "Teltow-Fläming II", "Dahme-Spreewald I", "Dahme-Spreewald II", "Oder-Spree I",
        "Oder-Spree II", "Oder-Spree III / Frankfurt (Oder) II", "Oder-Spree IV",
        "Frankfurt (Oder) I", "Elbe-Elster I", "Elbe-Elster II",
        "Oberspreewald-Lausitz I", "Oberspreewald-Lausitz II",
        "Oberspreewald-Lausitz III", "Cottbus I", "Cottbus II", "Spree-Neiße I",
        "Spree-Neiße II", "Spree-Neiße III",
    ],
}

# A Wahlkreis name never contains an Arabic digit (the roman numerals are
# letters).  That single rule is what separates a real heading from the report's
# two-column contents lines ("... Prignitz I 11 3.23 Wahlkreis 23 Teltow-Flaeming
# I 33"), which otherwise parse as a plausible-looking name.
NAME_OK = re.compile(r"^[A-ZÄÖÜ][A-Za-zÄÖÜäöüß ./()\-]{2,58}$")
BAD_TOKENS = ("Wahlkreis", "Seite", "Tabelle")

# 2009-2024: per-Wahlkreis section heading, e.g. "3.1 Wahlkreis 01 Prignitz I"
# (the optional trailing group swallows a running page number).
HEAD_RE = re.compile(
    r"^(?:\d{1,2}\.\d{1,2}\s*)?Wahlkreis\s+(\d{1,2})\s+(.+?)(?:\s+\d{1,3})?$")
# 2004 has no "Wahlkreis NN" headings; its contents pages list "01 Prignitz I 54".
TOC2004_RE = re.compile(r"^(\d{2})\s+([A-ZÄÖÜ].*?)\s+(\d{1,3})$")
YEAR_RE = {2004: TOC2004_RE}

# Two text-layer artefacts, each repaired to what the printed page shows, and
# each keyed on the broken string so the override fails loudly rather than
# silently persisting if the source PDF is ever re-issued:
#   2004 WK 35 - the contents page renders "Frankfurt [Oder])"; report pages 43,
#                88 and 116 print "Frankfurt (Oder)".
#   2009 WK 31 - the heading's roman numeral is split into two glyph runs, so the
#                text layer yields "Oder-Spree I V" for "Oder-Spree IV" (the 2004,
#                2014, 2019 and 2024 reports all print the latter).
TEXT_LAYER_FIXES = {
    (2004, 35): ("Frankfurt [Oder])", "Frankfurt (Oder)"),
    (2009, 31): ("Märkisch-Oderland I/Oder-Spree I V",
                 "Märkisch-Oderland I/Oder-Spree IV"),
}


def clean(name):
    return re.sub(r"\s+", " ", name).strip(" .")


def usable(name):
    return (not re.search(r"[0-9]", name)
            and not any(b in name for b in BAD_TOKENS)
            and bool(NAME_OK.match(name))
            and not name.endswith(("/", ",", "-")))


def extract_year(year):
    """(nr -> name) from one report's text layer; every Wahlkreis must be
    unambiguous, so a stray match cannot quietly outvote the real heading."""
    pdf = os.path.join(RAW, f"BB_{year}_Landtagswahl_Wahlkreis_StatBericht.pdf")
    rx = YEAR_RE.get(year, HEAD_RE)
    good = collections.defaultdict(set)
    rejected = collections.defaultdict(set)
    with pdfplumber.open(pdf) as doc:
        for page in doc.pages:
            for line in (page.extract_text() or "").split("\n"):
                m = rx.match(line.strip())
                if not m:
                    continue
                nr, name = int(m.group(1)), clean(m.group(2))
                if not (1 <= nr <= N_WKR):
                    continue
                (good if usable(name) else rejected)[nr].add(name)

    out, problems = {}, []
    for nr in range(1, N_WKR + 1):
        fix = TEXT_LAYER_FIXES.get((year, nr))
        if fix is not None:
            artefact, repaired = fix
            seen = good.get(nr, set()) | rejected.get(nr, set())
            if artefact not in seen:
                problems.append(f"WK {nr}: the pinned text-layer repair for "
                                f"'{artefact}' no longer applies (got {sorted(seen)}) "
                                f"- re-check the source")
            else:
                out[nr] = repaired
            continue
        cand = good.get(nr, set())
        if len(cand) == 1:
            out[nr] = next(iter(cand))
        elif not cand:
            problems.append(f"WK {nr}: no name found (rejected: {sorted(rejected.get(nr, set()))[:3]})")
        else:
            problems.append(f"WK {nr}: ambiguous - {sorted(cand)}")
    if problems:
        for pr in problems:
            print(f"  [FAIL] BB {year} {pr}")
        raise SystemExit(1)
    return out


def main():
    years = {}
    for y, names in PINNED.items():
        if len(names) != N_WKR:
            raise SystemExit(f"BB {y}: pinned list has {len(names)} names, expected {N_WKR}")
        years[y] = {i + 1: n for i, n in enumerate(names)}
        print(f"  BB {y}: {N_WKR} names (pinned from the scanned report)")
    for y in EXTRACT_YEARS:
        years[y] = extract_year(y)
        print(f"  BB {y}: {N_WKR} names (extracted from the report text layer)")

    fails = []
    for y, mapping in sorted(years.items()):
        dupes = [n for n, c in collections.Counter(mapping.values()).items() if c > 1]
        if dupes:
            fails.append(f"BB {y}: duplicate Wahlkreis names {dupes}")
    # 2004-2024 share one Wahlkreiseinteilung; 1994/1999 differ from it, and 1990
    # differs from both. Assert that so a silent copy-paste cannot creep in.
    same = [y for y in (2009, 2014, 2019, 2024) if years[y] != years[2004]]
    if same:
        # spelling drifts ("III/Spree-Neisse" vs "III / Spree-Neisse") are fine,
        # so compare on a whitespace-insensitive key
        def key(m):
            return {k: re.sub(r"\s*/\s*", "/", v) for k, v in m.items()}
        same = [y for y in same if key(years[y]) != key(years[2004])]
    if same:
        print(f"  note: {same} differ from the 2004 Wahlkreiseinteilung (check if unexpected)")
    for a, b in ((1990, 1994), (1994, 1999)):
        if years[a] == years[b]:
            fails.append(f"BB {a} and {b} have identical names - a renumbering was missed")
    if fails:
        for f in fails:
            print("  [FAIL]", f)
        raise SystemExit(1)

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["election_year", "wkr_nr", "wkr_name"])
        for y in sorted(years):
            for nr in range(1, N_WKR + 1):
                w.writerow([y, f"{nr:02d}", years[y][nr]])
    print(f"\nWrote {len(years) * N_WKR} rows -> {os.path.relpath(OUT, ROOT)}")
    print("  sample (WK 11 across years):",
          ", ".join(f"{y}={years[y][11]}" for y in sorted(years)))


if __name__ == "__main__":
    main()
