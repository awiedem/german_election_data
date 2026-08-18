#!/usr/bin/env python3
"""Stage-0: MECKLENBURG-VORPOMMERN Landtagswahlkreis NAMES, 1994-2011.

Why this exists: the LAIV-MV Wahlbezirk workbooks that parse_MV.R reads for
1994-2011 carry the Wahlkreis only as a NUMBER, so those five elections were
published with wkr_name = NA.  (2016 and 2021 come from the newer XLSX reports,
which do carry names, and are untouched here.)

Names for 2002, 2006 and 2011 are read from section 1.3 "Uebersicht ueber die
Wahlkreise des Landes Mecklenburg-Vorpommern" of each year's own Statistischer
Bericht, already in raw/.  That section - not the result tables - is the right
source: the per-Wahlkreis result tables abbreviate long names ("Mecklenb.-Stre
I/Muer II") and wrap them across two lines.  Extraction is coordinate-based: the
"Gebiet des Wahlkreises" column's left edge is located from the continuation
lines, and the name is whatever sits left of it.

1994 and 1998 have no name anywhere in the published material.  They are given
the 2002 names, but only after PROVING the Wahlkreise are the same units: every
Gemeinde that appears in both years' official "nach Gemeinden" workbook must
carry the same Wahlkreis number.  It does - 1994 vs 2002: 960/960 Gemeinden,
1998 vs 2002: 961/961, 1994 vs 1998: 1068/1068, zero disagreements - so the
2002 Wahlkreiseinteilung is literally the 1994/1998 one.  The check runs on
every invocation and the script aborts if a single Gemeinde ever disagrees.

Output: data/state_elections/processed/wahlkreis/wkr_names/MV_wkr_names.csv
        (election_year, wkr_nr, wkr_name, name_source) -- read by parse_MV.R
Run:    python3 code/state_elections_wahlkreis/parsers/00_mv_wkr_names.py
"""

import collections
import csv
import os
import re

import pandas as pd
import pdfplumber

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(HERE)))
RAW = os.path.join(ROOT, "data", "state_elections", "raw",
                   "Landtagswahlen_Wahlkreis", "Mecklenburg-Vorpommern")
OUT_DIR = os.path.join(ROOT, "data", "state_elections", "processed",
                       "wahlkreis", "wkr_names")
OUT = os.path.join(OUT_DIR, "MV_wkr_names.csv")

N_WKR = 36
PDF_YEARS = [2002, 2006, 2011]
INHERIT_YEARS = [1994, 1998]          # take 2002's names, gated on the check below
SECTION = "bersicht über die Wahlkreise"
GEM_XLS = {y: os.path.join(RAW, f"MV_{y}_Landtagswahl_nach_Gemeinden.xls")
           for y in (1994, 1998, 2002)}
NAME_OK = re.compile(r"^[A-ZÄÖÜ][A-Za-zÄÖÜäöüß ./\-]{2,45}$")


def page_lines(page, ytol=3.0):
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


def names_from_report(year):
    """{nr: name} from section 1.3 of the year's Statistischer Bericht."""
    pdf = os.path.join(RAW, f"MV_{year}_Landtagswahl_Wahlkreis.pdf")
    found = {}
    with pdfplumber.open(pdf) as doc:
        for page in doc.pages[:30]:
            if SECTION not in (page.extract_text() or ""):
                continue
            lines = page_lines(page)
            # the table of contents also names the section; only accept pages
            # where the heading IS the page heading (first two line groups)
            head = " ".join(w["text"] for grp in lines[:2] for w in grp)
            if SECTION not in head:
                continue
            # The Gebiet column's left edge is where the wrapped description
            # lines start.  Take the MODE of those starts, not the minimum: the
            # page footer and the "1)" / "______" footnotes also follow a
            # numbered row and begin at the far-left margin, and using the
            # minimum let them collapse the name column to nothing (2002 lost
            # 15 of 36 Wahlkreise that way).
            seen_row, starts = False, collections.Counter()
            for grp in lines:
                if re.fullmatch(r"\d{1,2}", grp[0]["text"]):
                    seen_row = True
                elif seen_row:
                    starts[round(grp[0]["x0"])] += 1
            if not starts:
                continue
            gebiet_x = starts.most_common(1)[0][0]
            if not 120 < gebiet_x < page.width:
                raise SystemExit(f"MV {year} p{page.page_number}: implausible Gebiet "
                                 f"column edge {gebiet_x}")
            for grp in lines:
                if not re.fullmatch(r"\d{1,2}", grp[0]["text"]):
                    continue
                nr = int(grp[0]["text"])
                if not 1 <= nr <= N_WKR:
                    continue
                name = " ".join(w["text"] for w in grp[1:] if w["x1"] < gebiet_x - 1)
                name = re.sub(r"\s+", " ", name).strip(" ,;:")
                if name and nr not in found:
                    found[nr] = name
    missing = [n for n in range(1, N_WKR + 1) if n not in found]
    if missing:
        raise SystemExit(f"MV {year}: no name for Wahlkreis {missing}")
    bad = {n: v for n, v in found.items() if not NAME_OK.match(v)}
    if bad:
        raise SystemExit(f"MV {year}: implausible names {bad}")
    if len(set(found.values())) != N_WKR:
        raise SystemExit(f"MV {year}: duplicate Wahlkreis names")
    return found


def gemeinde_wahlkreis_map(year):
    """{Gemeindenummer: Wahlkreisnummer} from the official 'nach Gemeinden' XLS."""
    df = pd.read_excel(GEM_XLS[year], sheet_name=-1, header=None, engine="xlrd")
    wk = pd.to_numeric(df.iloc[:, 0], errors="coerce")
    gem = pd.to_numeric(df.iloc[:, 1], errors="coerce")
    ok = wk.notna() & gem.notna() & wk.between(1, N_WKR)
    return dict(zip(gem[ok].astype("int64"), wk[ok].astype(int)))


def main():
    print("Reading the Statistische Berichte ...")
    years = {y: names_from_report(y) for y in PDF_YEARS}
    for y in PDF_YEARS:
        print(f"  MV {y}: {N_WKR} names from section 1.3 of its own report")

    print("\nChecking that 1994/1998 use the 2002 Wahlkreiseinteilung ...")
    maps = {y: gemeinde_wahlkreis_map(y) for y in GEM_XLS}
    ok = True
    for a, b in ((1994, 2002), (1998, 2002), (1994, 1998)):
        shared = set(maps[a]) & set(maps[b])
        diff = [g for g in shared if maps[a][g] != maps[b][g]]
        print(f"  {a} vs {b}: {len(shared)} shared Gemeinden, {len(diff)} in a different Wahlkreis")
        if diff:
            ok = False
            print("     e.g.", [(g, maps[a][g], maps[b][g]) for g in sorted(diff)[:5]])
    if not ok:
        raise SystemExit("MV: the Wahlkreiseinteilung is NOT stable across 1994/1998/2002 - "
                         "the 2002 names may not be inherited; find a per-year source.")
    for y in INHERIT_YEARS:
        years[y] = dict(years[2002])

    source = {y: ("Statistischer Bericht %d, Abschnitt 1.3" % y) if y in PDF_YEARS
              else "2002 Wahlkreiseinteilung (verified identical by Gemeinde-Wahlkreis map)"
              for y in years}

    os.makedirs(OUT_DIR, exist_ok=True)
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.writer(fh)
        w.writerow(["election_year", "wkr_nr", "wkr_name", "name_source"])
        for y in sorted(years):
            for nr in range(1, N_WKR + 1):
                w.writerow([y, f"{nr:02d}", years[y][nr], source[y]])
    print(f"\nWrote {len(years) * N_WKR} rows -> {os.path.relpath(OUT, ROOT)}")
    for y in sorted(years):
        print(f"  {y}: {years[y][1]} / {years[y][21]} / {years[y][25]} / {years[y][36]}")


if __name__ == "__main__":
    main()
