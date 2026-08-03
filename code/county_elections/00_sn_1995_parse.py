#!/usr/bin/env python3
"""Parse the Sachsen Kreistagswahl of 3 December 1995 into a tidy CSV.

The 1995 election is the re-run the Sächsisches Verfassungsgericht forced after
it annulled the 1994 Kreisreform (see 00_sn_1994_parse.py). Table KT95_TAB1
carries it at KREIS level, which is what makes it ingestible at all: the
Gemeinde tables (KT95TAB7-10) print names only, with no AGS, and the audit
worklist assumed those were the only source and that ~32 names would need
hand-adjudication. They are not: table 1 is Kreis-level and needs no name
matching whatsoever.

Table 1 has FIVE sections, and they are not all the same kind of thing:

  * three full Kreistagswahlen in newly formed Kreise, exported here --
      Vogtlandkreis                 -> 14178 (2021: Vogtlandkreis 14523)
      Meißen-Radebeul               -> 14280 (2021: mostly Meißen 14627)
      Westlausitz-Dresdner Land     -> 14292 (2021: mostly Bautzen 14625)
    "Meißen-Radebeul" and "Westlausitz-Dresdner Land" are the provisional names
    the new Kreise carried in 1995; the register knows them as Meißen and
    Kamenz.
  * two SINGLE-GEMEINDE ballots, deliberately NOT exported -- Uhyst (1,015
    electors) voting to join the Niederschlesischer Oberlausitzkreis and
    Schönfeld-Weißig (6,350) joining Sächsische Schweiz. Both Kreise already
    held a full Kreistagswahl in 1994, so emitting these as 1995 Kreis rows
    would publish about one per cent of a county as though it were the county.

Output: data/county_elections/raw/Kreistagswahlen/Sachsen/sn_1995_parsed.csv
"""
import csv
import os
import re
import sys

import xlrd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
SRC = os.path.join(ROOT, "data", "county_elections", "raw", "Kreistagswahlen",
                   "Sachsen", "Sachsen_1995_Kreistagswahl", "KT95_TAB1.XLS")
OUT = os.path.join(ROOT, "data", "county_elections", "raw", "Kreistagswahlen",
                   "Sachsen", "sn_1995_parsed.csv")

ELECTION_DATE = "1995-12-03"

# Section heading in the report -> Kreis code of the 1995 vintage.
KREIS = {
    "Vogtlandkreis": "14178",
    "Meißen-Radebeul": "14280",
    "Westlausitz-Dresdner Land": "14292",
}
# Single-Gemeinde ballots: recorded so their omission is explicit, not silent.
PARTIAL = ["Niederschlesischer Oberlausitzkreis", "Sächsische Schweiz"]

# FORUM stood only in Meißen-Radebeul (2,156 votes); omitting it left that
# Kreis 2,156 short of its own printed gültige Stimmen, which is exactly what
# the reconciliation assertion below is for. "Sonstige" is "x" in all three
# 1995 sections but is listed so a future value cannot go unnoticed.
PARTIES = ["CDU", "SPD", "PDS", "GRÜNE", "F.D.P.", "DSU", "FORUM",
           "Wählervereinigungen", "Sonstige"]

MEASURES = {
    "Wahlberechtigte": "eligible_voters",
    "Wähler": "number_voters",
    "Ungültige Stimmzettel": "invalid_votes",
    "Gültige Stimmzettel": "valid_votes",
    "Gültige Stimmen": "valid_vote_total",
}


def num(x):
    if isinstance(x, float):
        return x
    x = str(x).strip()
    if not x or x in ("x", "-", "."):
        return None
    try:
        return float(x.replace(",", "."))
    except ValueError:
        return None


def main():
    sheet = xlrd.open_workbook(SRC).sheet_by_index(0)

    def label(r):
        return re.sub(r"\s+", " ", str(sheet.cell_value(r, 0))).strip()

    def heading(r):
        return re.sub(r"\s+", " ", str(sheet.cell_value(r, 1))).strip()

    # Section starts: a row with an empty col 0 and a name in col 1.
    starts = []
    for r in range(sheet.nrows):
        h = heading(r)
        if not label(r) and h and not num(h) and h != "Anzahl":
            starts.append((r, h))
    if not starts:
        sys.exit("SN 1995: no section headings found in KT95_TAB1")

    rows, skipped = [], []
    for i, (r0, head) in enumerate(starts):
        r1 = starts[i + 1][0] if i + 1 < len(starts) else sheet.nrows
        # Headings are printed across two cells for the long names.
        full = head
        for k in (r0 + 1,):
            nxt = heading(k)
            if nxt and not label(k) and not num(nxt):
                full = (full + " " + nxt).strip()
        match = next((k for k in KREIS if full.startswith(k[:14])), None)
        part = next((p for p in PARTIAL if full.startswith(p[:14])), None)
        if match is None:
            if part is not None:
                skipped.append(part)
            continue

        rec = {"ags": KREIS[match] + "000", "ags_name": match,
               "election_date": ELECTION_DATE}
        for r in range(r0, r1):
            lab = label(r)
            for key, col in MEASURES.items():
                if lab.startswith(key[:16]) and col not in rec:
                    rec[col] = num(sheet.cell_value(r, 1))
            for p in PARTIES:
                if lab.rstrip().startswith(p) and p not in rec:
                    rec[p] = num(sheet.cell_value(r, 1))
        for p in PARTIES:
            rec.setdefault(p, None)
        missing = [c for c in MEASURES.values() if rec.get(c) is None]
        if missing:
            sys.exit(f"SN 1995: {match} is missing {missing}")

        got = sum(rec[p] or 0 for p in PARTIES)
        if got != rec["valid_vote_total"]:
            sys.exit(f"SN 1995: {match} party votes {got:.0f} != gültige Stimmen "
                     f"{rec['valid_vote_total']:.0f}")
        if not (rec["valid_votes"] < rec["number_voters"] <= rec["eligible_voters"]):
            sys.exit(f"SN 1995: {match} counts are not ordered")
        rows.append(rec)

    if len(rows) != len(KREIS):
        sys.exit(f"SN 1995: parsed {len(rows)} Kreise, expected {len(KREIS)}")

    cols = ["ags", "ags_name", "election_date", "eligible_voters", "number_voters",
            "invalid_votes", "valid_votes", "valid_vote_total"] + PARTIES
    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        for rec in sorted(rows, key=lambda z: z["ags"]):
            w.writerow({c: ("" if rec.get(c) is None else
                            (int(rec[c]) if isinstance(rec[c], float) else rec[c]))
                        for c in cols})

    print(f"SN 1995: {len(rows)} Kreise -> {OUT}")
    for rec in rows:
        print(f"  {rec['ags']} {rec['ags_name']:26} eligible {rec['eligible_voters']:.0f}"
              f"  gültige Stimmen {rec['valid_vote_total']:.0f}")
    print("  single-Gemeinde ballots deliberately excluded: " + ", ".join(skipped))


if __name__ == "__main__":
    main()
