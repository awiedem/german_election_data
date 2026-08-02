#!/usr/bin/env python3
"""Parse the Sachsen Kreistagswahl of 12 June 1994 into a tidy CSV for Stage 1.

The 1994 tables are legacy BIFF .xls that readxl cannot open, which is why the
Saxon county series began at 1999. They are also KREIS-LEVEL only -- the
Landesamt published no Gemeinde breakdown -- so these rows populate
county_elec_harm_21_cty but not _muni, the same treatment NRW 2025 gets.

Sources (Sachsen_1994_Kreistagswahl/):
  KT94_SN_01.XLS  statewide Kreistagswahl totals, used here only as fixtures
  KT94_SN_03.XLS  Wahlberechtigte and Wähler per Kreis, absolute
  KT94_SN_04.XLS  valid-ballot share per Kreis, one decimal
  KT94_SN_05.XLS  party votes per Kreis, absolute
  KT94_SN_06.XLS  the same as shares; supplies the party legend

valid_votes holds valid BALLOTS, as it does for Saxony from 1999 onward. 1994
prints that figure only as the percentage in table 4, so it is reconstructed as
pct/100 * Wähler. Table 1 shows the relation is exact -- 1 559 982 * 94.22782 %
= 1 469 937, the printed statewide Gültige Stimmzettel -- and rounding to one
decimal bounds the per-Kreis error at about +/-0.05 % of ballots. Letting the
column mean valid VOTES for one year instead would repeat the semantic flip the
July-2026 audit found in Sachsen 2019/2024. Party shares are computed against
the exact three-vote total in table 5 and carry no such error.

Two groups are absent from the output by design, both consequences of the
Sächsisches Verfassungsgericht ruling on the 1994 Kreisreform:
  * Meißen, Kamenz, Dresden-Land and Hoyerswerda (footnote 1) held no valid
    1994 election and re-ran in 1995; the source prints "-" for them.
  * Elstertalkreis and Göltzschtalkreis (footnote 2) polled 336 198 votes but
    were then never constituted, so they have no Kreis code in any vintage of
    cty_crosswalks and cannot be harmonised. The statewide figures in table 1
    do include them, which is why the 19 exported Kreise sum to 3 737 081
    rather than the printed 4 073 279.

Output: data/county_elections/raw/Kreistagswahlen/Sachsen/sn_1994_parsed.csv
"""
import csv
import os
import re
import sys

import xlrd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
SRC = os.path.join(ROOT, "data", "county_elections", "raw", "Kreistagswahlen",
                   "Sachsen", "Sachsen_1994_Kreistagswahl")
OUT = os.path.join(ROOT, "data", "county_elections", "raw", "Kreistagswahlen",
                   "Sachsen", "sn_1994_parsed.csv")

ELECTION_DATE = "1994-06-12"

# Kreis label as printed -> Kreis code of the 1994 vintage in cty_crosswalks.
KREIS = {
    "Annaberg": "14071", "Bautzen": "14072", "Chemnitzer Land": "14073",
    "Delitzsch": "14074", "Döbeln": "14075", "Freiberg": "14077",
    "Leipziger Land": "14079", "Mittlerer Erzgebirgskreis": "14081",
    "Mittweida": "14082", "Muldentalkreis": "14083",
    "Niederschlesischer Oberlausitzkreis": "14084",
    "Riesa-Großenhain": "14085", "Löbau-Zittau": "14086",
    "Sächsische Schweiz": "14087", "Stollberg": "14088",
    "Torgau-Oschatz": "14089", "Weißeritzkreis": "14090",
    "Aue-Schwarzenberg": "14091", "Zwickauer Land": "14092",
}
ANNULLED = ["Meißen", "Kamenz", "Dresden-Land", "Hoyerswerda"]
NEVER_FORMED = ["Elstertalkreis", "Göltzschtalkreis"]
TOTAL_ROW = "Landkreise"

# Table 5 columns 3..11, per the legend in table 6 (row 4). Column 10 prints no
# header in table 5; table 6 names it "andere Parteien", which Annaberg
# confirms: table 6 gives 45.7/12.7/7.8/5.2/7.0/x/2.2/x/19.4 and table 5's
# 69276/19306/11890/7869/10571/x/3394/x/29386 reproduce those to one decimal.
PARTIES = ["CDU", "SPD", "PDS", "GRÜNE", "F.D.P.", "REP", "DSU",
           "Andere Parteien", "Wählervereinigungen"]

# Table 1, "Kreistagswahl" block. Includes the two never-formed Kreise.
FIXTURES = {"eligible_voters": 2119178, "number_voters": 1559982,
            "valid_ballots": 1469937, "valid_vote_total": 4073279,
            "CDU": 1744462, "SPD": 815226, "PDS": 538027, "GRÜNE": 256905,
            "F.D.P.": 299574, "REP": 6123, "DSU": 154692,
            "Andere Parteien": 19037, "Wählervereinigungen": 239233}


def sheet(name):
    return xlrd.open_workbook(os.path.join(SRC, name)).sheet_by_index(0)


def txt(s, r, c):
    if r >= s.nrows or c >= s.ncols:
        return ""
    v = s.cell_value(r, c)
    if isinstance(v, float):
        return str(int(v)) if v == int(v) else str(v)
    return str(v).strip()


def num(x):
    """Numeric value, or None for the source's 'x' (not applicable) and '-'."""
    x = str(x).strip()
    if x in ("", "x", "-", "."):
        return None
    try:
        return float(x.replace(",", "."))
    except ValueError:
        return None


def label(x):
    return re.sub(r"\s+", " ", re.sub(r"\s*\d\)\s*$", "", x)).strip()


def ordered_labels(name, value_col):
    """Kreis labels of a one-row-per-Kreis table, in printed order.

    Carries the annulled Kreise through even though their cells read 'x', so
    the order can be asserted against table 5.
    """
    s = sheet(name)
    out = []
    for r in range(s.nrows):
        lab = label(txt(s, r, 0))
        if (not lab or lab.startswith("_") or lab.lower() == "außerdem:"
                or lab.lower().startswith(("noch:", "merkmal"))
                or re.match(r"^\d\)", lab) or re.match(r"^\d\.", lab)):
            continue
        if num(txt(s, r, value_col)) is None and lab not in ANNULLED:
            continue
        out.append((lab, r))
    return out


def main():
    t3 = ordered_labels("KT94_SN_03.XLS", 1)          # eligible / voters
    t4 = {lab: r for lab, r in ordered_labels("KT94_SN_04.XLS", 1)}
    s3, s4 = sheet("KT94_SN_03.XLS"), sheet("KT94_SN_04.XLS")

    s5 = sheet("KT94_SN_05.XLS")
    rows5 = [r for r in range(s5.nrows) if txt(s5, r, 1).lower() == "absolut"]

    expected = [lab for lab, _ in t3]
    if len(rows5) != len(expected):
        sys.exit(f"SN 1994: table 5 has {len(rows5)} data rows, table 3 has "
                 f"{len(expected)} Kreis labels")
    # Table 5 wraps long names over up to three rows, so its labels are not read
    # directly; alignment with table 3 is asserted instead. Both tables mark the
    # four annulled Kreise, table 3 with 'x' and table 5 with '-', and those
    # positions must coincide.
    blank5 = [i for i, r in enumerate(rows5) if num(txt(s5, r, 2)) is None]
    blank3 = [i for i, lab in enumerate(expected) if lab in ANNULLED]
    if blank5 != blank3:
        sys.exit(f"SN 1994: tables 3 and 5 disagree on which Kreise held no "
                 f"election ({blank3} vs {blank5}); order cannot be trusted")

    rows, unmapped, totals = [], [], {p: 0.0 for p in PARTIES}
    grand = 0.0
    for lab, r5 in zip(expected, rows5):
        vals = {p: num(txt(s5, r5, 3 + i)) for i, p in enumerate(PARTIES)}
        total = num(txt(s5, r5, 2))
        if lab == TOTAL_ROW:
            printed_total = total
            printed = {p: vals[p] for p in PARTIES}
            continue
        if total is None:                       # annulled: no election held
            continue
        got = sum(v for v in vals.values() if v is not None)
        if got != total:
            sys.exit(f"SN 1994: {lab} party votes {got:.0f} != gültige Stimmen "
                     f"{total:.0f}")
        grand += total
        for p in PARTIES:
            totals[p] += vals[p] or 0.0
        if lab in NEVER_FORMED:
            continue
        code = KREIS.get(lab)
        if code is None:
            unmapped.append(lab)
            continue
        r3 = dict(t3)[lab]
        voters = num(txt(s3, r3, 3))
        pct = num(txt(s4, t4[lab], 1))
        if voters is None or pct is None:
            sys.exit(f"SN 1994: {lab} lacks Wähler or valid-ballot share")
        rows.append({
            "ags": code + "000", "ags_name": lab,
            "election_date": ELECTION_DATE,
            "eligible_voters": int(num(txt(s3, r3, 1))),
            "number_voters": int(voters),
            "valid_votes": int(round(pct / 100.0 * voters)),
            "valid_vote_total": int(total),
            **{p: ("" if vals[p] is None else int(vals[p])) for p in PARTIES},
        })

    if unmapped:
        sys.exit("SN 1994: unmapped Kreis label(s): " + ", ".join(unmapped))
    if len(rows) != len(KREIS):
        sys.exit(f"SN 1994: exported {len(rows)} Kreise, expected {len(KREIS)}")

    # The printed Landkreise row must equal the 19 exported Kreise plus the two
    # that were never constituted, and must match table 1's statewide block.
    if grand != printed_total:
        sys.exit(f"SN 1994: Kreis votes {grand:.0f} != printed Landkreise total "
                 f"{printed_total:.0f}")
    for p in PARTIES:
        if totals[p] != (printed[p] or 0.0) or totals[p] != FIXTURES[p]:
            sys.exit(f"SN 1994: {p} sums to {totals[p]:.0f}, printed "
                     f"{printed[p]}, table 1 says {FIXTURES[p]}")
    if grand != FIXTURES["valid_vote_total"]:
        sys.exit(f"SN 1994: gültige Stimmen {grand:.0f} != {FIXTURES['valid_vote_total']}")

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        cols = list(rows[0].keys())
        w = csv.DictWriter(fh, fieldnames=cols)
        w.writeheader()
        for row in sorted(rows, key=lambda z: z["ags"]):
            w.writerow(row)

    exp_elig = sum(r["eligible_voters"] for r in rows)
    exp_ball = sum(r["valid_votes"] for r in rows)
    print(f"SN 1994: {len(rows)} Kreise -> {OUT}")
    print(f"  reconciles to table 1 on all {len(PARTIES)} parties and "
          f"gültige Stimmen {grand:.0f}")
    print(f"  exported eligible {exp_elig} of statewide "
          f"{FIXTURES['eligible_voters']} (rest = never-formed Kreise)")
    print(f"  derived valid ballots {exp_ball}, statewide printed "
          f"{FIXTURES['valid_ballots']} incl. never-formed")
    print("  no election held: " + ", ".join(ANNULLED)
          + " | never constituted: " + ", ".join(NEVER_FORMED))


if __name__ == "__main__":
    main()
