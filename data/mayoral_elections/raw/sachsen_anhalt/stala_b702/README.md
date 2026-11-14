# Sachsen-Anhalt: "Landräte und Bürgermeister" (StaLA, B VII 702), 2004–2011

Registers of every elected Landrat and Bürgermeister in office on a Stichtag,
published by the Statistisches Landesamt Sachsen-Anhalt.

| File | Stichtag | Pages | Source |
|---|---|---|---|
| `2V005_01.01.2004_klein_ocred_.pdf` | 01.01.2004 | 92 | StaLA scan (order no. 2V005) |
| `3B702_01.08.2007_klein_ocred_.pdf` | 01.08.2007 | 88 | StaLA scan |
| `3B702_01.01.2009_klein_ocred_.pdf` | 01.01.2009 | 106 | StaLA scan |
| `3B702_01.03.2010_klein_ocred_.pdf` | 01.03.2010 | 50 | StaLA scan |
| `6B702_2011.pdf` | 01.10.2011 | 35 | Statistische Bibliothek (public) |

Checksums are in `_source_manifest.csv`. The StaLA holds no other issues
(its e-mail of 2026-10-13). The 2004 issue says an issue with Stichtag
01.01.2002 existed. The StaLA did not list it, so it is not here.

## Provenance and terms

- Requested from the StaLA library on 2026-10-07 (Vorgang 2026_1083). The
  StaLA offered free scans on 2026-10-13. Vincent accepted on the same day and
  asked whether GERDA may publish name, party/Wahlvorschlag and birth year
  of the office holders.
- The scans arrived on 2026-11-13 as one zip (`2026_1083`, 18.8 MB, OCR text
  layer by the StaLA) through a Dataport ddatabox link that expires on
  2027-11-12.
- The e-mail of 2026-11-13 carries only the general line "Vervielfältigung und
  Verbreitung, auch auszugsweise, nur mit Quellenangabe gestattet". The imprint
  of each issue says the same. Neither answers the question about personal
  details.
- The PDFs and every table parsed from them stay out of git (`.gitignore` in
  this folder). The public files take from them the elected persons' names,
  ballot parties and dates, and the Landrat results. Professions are not used.
  **Birth years stay in the restricted twin only** (Vincent's decision of
  2026-11-13, until the StaLA answers the question above), and so do the losing
  candidates named in the results annexes.

## Content

Each issue lists, per Kreis, the elected Landräte (5-digit Kreis code) and the
Bürgermeister of every Gemeinde (8-digit AGS of the Stichtag):

- name;
- party or Wählergruppe as printed on the ballot;
- number of candidates;
- profession;
- Wahltag and, if held, the Stichwahl date;
- Ha/HaOB flag;
- birth year (2009–2011 only).

Extra lists:

| Issue | Lists |
|---|---|
| 2004 | 2.3 hauptamtliche BM (a repeat of 2.2); 2.4 BM elected after the Stichtag (to 15.03.2004) |
| 2007 | 2.3 hauptamtliche BM; annex with full results of the OB and Landrat elections |
| 2009 | 2.3 hauptamtliche BM; 2.4 BM elected in 2008 whose Gemeinde was dissolved; 2.5 BM elected after the Stichtag; the same annex |
| 2010 | 2.3 Verbandsgemeindebürgermeister; 2.4 BM elected in 2009 whose Gemeinde was dissolved |
| 2011 | 2.3 Verbandsgemeindebürgermeister |

## Parsing

`code/mayoral_elections/00_st_b702_parse.py` writes:

- `st_b702_register_restricted.csv`: one row per printed record, 4,139 rows;
- `st_b702_results_restricted.csv`: one row per candidate in the results
  annexes, 205 rows.

Traps:

- The scans are rotated by up to 0.02 pt/pt: a record's party and candidate
  count sit up to 6 pt below its AGS. The parser estimates the slope per page
  and groups words on the deskewed baseline.
- Column positions come from the page's own data (the profession lines start
  in the name column), not from the column headers, which are centred and
  shift between lists.
- The table rule left of the name column is read as a leading capital
  ("MMuster", invented) or noise ("_ Muster"). `clean_name_ocr()` removes those. Spelling
  slips inside names (I for l, ie for le) remain as printed.
- The candidate count in the 2009 issue is often missing: the OCR dropped the
  small digits at the right edge, which are visible in the scan.
- The lists of dissolved Gemeinden and Verbandsgemeinden print two-digit years
  (29.11.09).
- Footnotes ("Gemeinde bestand ... nicht mehr", "Nicht mehr im Amt; am ...
  wurde ein neuer ... gewählt") are kept in `page_footnotes` for marked records.
  Acting mayors ("amt. Bürgermeister") carry `acting = TRUE` and no Wahltag.
- In the results annex, OCR drops commas ("178" for 17,8) and adds stray glyphs
  in the runoff column. The reader joins thousands groups only while the count
  stays within the round's valid votes. It takes a runoff count only if a
  percentage follows it.

Checks:

- The record counts equal the issues' own totals: 1,162 of 1,163 Gemeinden in
  2004, 959 in 2009, 365 in 2010, 220 in 2011, plus 21/11 Landräte.
- Every annex round satisfies voters = invalid + valid, and its candidate votes
  sum to the valid votes.
- The 2007 and 2009 annexes are separate scans. They agree on all 12 elections
  they share.

## Comparison with GERDA (`code/checks/check_st_b702_register.py`, 2026-11-13, before integration)

- 3,660 of 3,815 Bürgermeister records match a GERDA election, mostly on (AGS,
  Wahltag) or on (Wahltag, Gemeinde name). The winner's surname agrees in
  3,618 of them.
- 79 elections are in the registers but not in GERDA. Most are by-elections
  of 1997–2003 that the StaLA's "BM-Wahl ab 1994" file lacks. All fall on
  Sundays, and 17 are printed identically in two or more issues.
- **Dates:**
  - Colbitz 2007: GERDA has 28.05.2007, a Monday; the registers have 22.04.2007.
  - Jerchel: GERDA has 12.11.2005, a Saturday; the registers have 13.11.2005.
  - Roxförde: GERDA has 05.11.2005, a Saturday; the registers have 05.11.2006.
  - Vienau is the reverse case: the 2004 register prints 22.06.2002, a
    Saturday, and GERDA's 22.09.2002 is right.
- **Codes:**
  - GERDA files the elections of Losse, Meseberg and Aue-Fallstein under codes
    of another Kreis.
  - GERDA mixes the two Gemeinden named Jerchel and the two named Kläden.
- **Winners:**
  - Marienborn 2002: the registers name another winner than GERDA, and GERDA's
    winner birth year (1962) is the registers' winner's.
  - Bad Kösen 2001: GERDA swaps given name and surname.
  - Wettin 2001: the surname differs.
  - Bucha 2007 and Lindstedt 2008: GERDA's winner has no name.
  - 21 elections differ in the spelling of the surname.
- **Party:** 273 GERDA elections have no party for the winner, but the register
  prints one.
- **Birth year:** 8 elections disagree on the birth year, and 27 lack one in
  GERDA.
- **Landrat annex:**
  - The runoffs of 06.05.2007 in Anhalt-Bitterfeld, Mansfeld-Südharz,
    Saalekreis, Salzlandkreis and Wittenberg have full results here. GERDA
    flags all five `flag_decisive_round_missing` and seats nobody.
  - In Börde, Burgenlandkreis, Saalekreis and Wittenberg, GERDA's first-round
    counts for 2007 (`lr07dat3.csv`) are 954–4,543 valid votes below the
    annex, with the same electorate. That pattern points to postal votes left
    out.
  - The annex adds three Landrat elections that GERDA lacks: Altmarkkreis
    Salzwedel 2001 and 2008, and Stendal 2005.
  - The OB results (Dessau-Roßlau 2007, Halle 2006, Magdeburg 2001 and 2008)
    agree exactly with GERDA.

## In the pipeline (since 2026-11-13)

The mayoral findings enter through three pin tables in `data/mayoral_elections/metadata/`, applied by `00_st_hist_parse.py` (`apply_b702()`):

- `st_b702_added_elections.csv` (79 winner-only elections) and `st_b702_fills.csv` (265 parties) are derived by `check_st_b702_register.py --write-pins`, and so are the 27 birth-year fills, which it writes to the gitignored `st_b702_birth_year_pins_restricted.csv` in this folder (plus one hand-made row, Rotta 2004). `00_st_hist_parse.py` lists every register birth year it uses (37, with the 9 of added elections) in `st_b702_birth_years_restricted.csv`; `01b_mayoral_candidates.R` keeps them in the restricted twin and blanks them in the public file (`99_audit.R` §39e). It reads the historical file as parsed before any pin (`st_stala_parsed_before_b702_restricted.csv`, written by `00_st_hist_parse.py`) and refuses to drop a pin that exists; three added elections are confirmed by hand (`MANUAL_ADD`).
- `st_b702_corrections.csv` (21 rows) is written by hand; each row cites the register issue or the source's own winner columns.

The Landrat annex enters through `st_b702_annex()` in `01_landrat_combine.R`. Professions are not used. After integration 3,779 of the 3,815 register entries match a GERDA election (`--after`); 31 entries give no Wahltag (8 of them acting mayors), and five are listed in `docs/mayoral_elections_known_issues.md` §27, which also lists what stays open. Landrat: see the Landrat README.
