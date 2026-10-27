# Brandenburg: the Gemeinden's mayoral result reports, 2004–2013 (Landeswahlleiter archive)

## Source

The Geschäftsstelle des Landeswahlleiters, Ministerium des Innern und für Kommunales Brandenburg (Christopher Sokol), answered the request of 2026-10-09 (Az. "hh* 2025/1211") on **2026-10-15** in four e-mails ("… - Teil 1" to "Teil 4"). He had found the **Ergebnismeldungen** that the Gemeinden filed with the Landeswahlleiter for their hauptamtliche Bürgermeister- and Oberbürgermeisterwahlen and scanned them, one PDF per Landkreis and one for the four kreisfreie Städte. The data were supplied for scientific research under § 25 BbgDSG in connection with Art. 89 (1) DSGVO, the same terms as the Landeswahlleiter workbook of 31.07.2026 (`../BB_Buergermeisterwahlen_ab2010_Landeswahlleiter.xlsx`).

`_source_manifest.csv` lists the 15 PDFs with their e-mail thread, size and SHA-256. They were saved unchanged on 2026-10-15 and copied here on 2026-10-16; the hashes were checked.

**The PDFs, `transcribed/`, `_work/` and the archived pages in `wayback/` are gitignored**: they name losing candidates, who are private individuals. Only this README and the two manifests are tracked.

`wayback/` holds the Landeswahlleiter's own result page for one round that is not on the scans (Schöneiche bei Berlin, Hauptwahl 22.04.2012). The page was retrieved from the Internet Archive on 2026-10-19. `wayback/_wayback_manifest.csv` gives its archive URL, capture time and SHA-256.

## What the scans hold

- **225 pages, one round per page**, 2004–2018, in all 14 Landkreise and the 4 kreisfreie Städte. One page (Oder-Spree, page 9) is a second scan of page 8.
- Two layouts:
  - the LWL's electronic form, "Landeswahlleiter Brandenburg – Ergebnis der Bürgermeisterwahl", with the 10-digit Gemeindeschlüssel (the 2018 form prints the 12-digit Regionalschlüssel), Wahltag, "Hauptwahl"/"Stichwahl" and a Vorgangsnummer. Most pages were keyed into the LWL system in 2021 (Vorgangsnummer 2021…);
  - an older spreadsheet form, "Ergebnis der Bürgermeisterwahl am …" / "…stichwahl am …", with Landkreis, Amt and Gemeinde but no AGS (40 pages, 2004–2008).
- Every page prints Wahlberechtigte, Wähler, ungültige and gültige Stimmen, the Nein-Stimmen of a single-candidate ballot, the two thresholds of § 72 Abs. 2 BbgKWahlG (more than half of the gültige Stimmen, 15 % of the Wahlberechtigte), every Bewerber with Wahlvorschlagsträger, votes and share, and the Wahlausschuss's finding: who was elected, or who goes to the runoff.
- The JPEGs are 144 dpi and carry an OmniPage OCR layer.

## Transcription

- Two independent reads:
  1. an automatic parse of the OCR layer;
  2. a visual read of every page from zoomed crops.
- The two reads agreed on every field of 208 pages. On 10 pages they differed (fax copies, a line the OCR dropped, glyphs), and every disputed field was settled against the image. On 7 pages the OCR layer could not be parsed; those were read visually and checked on the image (`read_status` in `transcribed/bb_lwl_scans_rounds.csv`).
- **Blind third read (2026-10-19).** 71 pages were read again without seeing any earlier read: every disputed, single-read and pinned page, plus 35 drawn at random. All counts, votes, percentages, dates, AGS, names, findings and round labels agree (`third_read`). The only differences are three Träger cells cut off at the cell border, where the last visible letter cannot be read either way.
- **External check (2026-10-19).** For 22 elected persons drawn at random, public sources (Amtsblätter, Wikipedia, the LWL's archived result pages) confirm the winner and the date in all 22, and the figures wherever they give them (19).
- **The LWL's own result pages (Internet Archive, 2026-10-19).** The LWL published each Gemeinde's latest result at `wahlen.brandenburg.de/bmwahlen/<AGS>/h` (and `/s` for runoffs). The Internet Archive kept 984 distinct captures of these pages from 2008–2017; all were read.
  - They show 139 of the 164 added rounds. 135 agree in every figure (Wahlberechtigte, Wähler, ungültige and gültige Stimmen), and 3 agree in every figure but carry a wrong date on the site. The one difference is Belzig's electorate (below).
  - This confirms the transcription against the LWL's own publication of the same reports, not the original count.
  - The other 25 rounds are mostly from 2004–2007, before the site existed.
- The Schöneiche bei Berlin Hauptwahl (22.04.2012) is parsed from the archived LWL page (`source_ref`). Its votes sum to the gültige Stimmen, both thresholds agree, and its finding names the two Bewerber of the scanned runoff.
- Tables in `transcribed/`:
  - `bb_lwl_scans_rounds.csv`: one row per page, as printed;
  - `bb_lwl_scans_candidates.csv`: one row per Bewerber per page.
- Parser: `code/mayoral_elections/00_bb_lwl_scans_parse.py` → `../bb_lwl_scans_parsed.csv` (gitignored). It re-derives every figure from the page's own subtotals and aborts on a slip. Each check was calibrated by mutating the transcription or the workbook (18/18 fire). The list below includes an anomaly pin that would otherwise hide a new defect on its page, stale pins, and a workbook overlap with a missing round or a differing vote:
  - per round: Sunday; gültige + ungültige = Wähler; Wähler ≤ Wahlberechtigte; votes (+ Nein) = gültige; printed percentages; printed thresholds; finding vs. votes, including the 15 % quorum;
  - per cycle: runoff 2–5 weeks after the Hauptwahl (§ 72 Abs. 2 BbgKWahlG, version of 2001/2004: "frühestens am zweiten und spätestens am fünften Sonntag"); the runoff Bewerber are the two the Hauptwahl admitted; a Hauptwahl without a winner has its runoff; similar electorates in the two rounds.
- AGS: the printed key, checked against the crosswalk name for the election year (123 of the 164 added rounds); otherwise the Gemeinde's name within its Landkreis (41). All on the AGS of the election day. No single-digit change of a printed key passes the crosswalk, Kreis and name checks.

## Coverage, against what GERDA already held

| | elections | rounds |
|---|---|---|
| on the scans | 166 | 224 |
| from the archived LWL page (Schöneiche 2012 Hauptwahl) | | 1 |
| already in the Landeswahlleiter workbook (2010–2018): compared, left out | 47 | 61 |
| **added** | **119** | **164** |
| of these, superseding a winner-only row of the B VII 3-6 report 2008 | 4 | |

Added by year of the first round: 2004 5, 2005 6, 2006 9, 2007 10, 2008 16, 2009 39, 2010 24, 2011 7, 2012 3 (115 Gemeinden). Before this delivery GERDA held 4 hauptamtliche elections of 2004–2009, all winner-only; it now holds 85, all with full results.

**Comparison with the workbook.** All 61 overlapping rounds agree exactly on every Bewerber's votes, and 60 of them on Wahlberechtigte, Wähler, gültige and ungültige Stimmen. A round missing from the workbook, or any other differing count or vote, now stops the parser unless pinned. Two findings:

- **Liebenwalde, 11.09.2011**: the scan prints 3 704 Wahlberechtigte, the workbook 3 407. The scan is consistent with its own turnout (44,49 %) and 15 % row (556). The council electorates of 2008 (3 843) and 2014 (3 682) and the Landtag electorate of 2009 (3 778) fit 3 704. The workbook figure looks like a 7/4 transposition, made at the LWL: its own result page carried 3 407 too (Internet Archive, 2015–2019). It is left as published pending the Landeswahlleiter's confirmation.
- One Bewerber's surname differs by a letter between the scan and the workbook (Heidesee 2011; the workbook spelling matches the 2003 B VII 3-6 report), and one is spelled with ss on the scan and ß in the workbook (Michendorf 2011). The workbook keeps its spellings.

## Source anomalies (pinned in the parser, each reported on every run)

- **Belzig 28.09.2008**: the form prints 5 779 Wahlberechtigte with 5 607 Wähler, a turnout of 97,02 %. The Gemeindevertretung was elected the same day from the same register: 9 779 Wahlberechtigte, 5 606 Wähler, 57,3 % (AfS, Statistischer Bericht B VII 3-5 – 5j/08, table 2, p. 108). The Kreistag the same day had 9 790 / 5 499. GERDA publishes **9 779** (one digit, 5/9). The outcome is unchanged. The LWL's own result page (Internet Archive, 2015) repeats the Gemeinde's 5 779, so it is no independent confirmation.
- **Schwarzheide 2008**: the page dated 12.10.2008 says "Hauptwahl" but shows two Bewerber on the general runoff day. The B VII 3-6 report dates the election 28.09.2008 with its Stichwahl on 12.10.2008. Filed as the runoff of a cycle whose first round (28.09.2008) has no result: its page is not in the delivery. No capture of the LWL's 2008 result page survives in the Internet Archive.
- **Schöneiche bei Berlin 2012**: the Hauptwahl page (22.04.2012) is not in the delivery, nor in the workbook. It is taken from the LWL's archived result page (above), so the cycle is complete. The incumbent placed second in the Hauptwahl and won the runoff.
- **Eichwalde 2009 and Pritzwalk 2009**: the Hauptwahl and the runoff print different electorates (Eichwalde 4 923 vs 5 270, +7 %; Pritzwalk 12 377 vs 11 162, −10 %). Each page is internally consistent, and the LWL's own result pages carry the same figures. The council and Landtag counts of those years fit the runoff figure. Both are kept as printed: no document gives the Hauptwahl's true electorate. Both Hauptwahlen went to a runoff, so no outcome depends on them.
- **Neuruppin 16.01.2005**: the Hauptwahl finding prints a stale runoff date (10.10.2004). The runoff page says 06.02.2005, 21 days later.
- **Hand-computed thresholds** on four older forms are off by one (rounded down, or 15 % taken as "more than"); no outcome depends on them.
- **Elbe-Elster, Mühlberg/Elbe 2008**: the form prints 14 Nein-Stimmen although three Bewerber stood. Their votes alone equal the gültige Stimmen, so the Nein count is ignored, as on any multi-candidate ballot.
- **Wiesenburg/Mark 2008**: the Nein-Stimmen were corrected by hand on the form (typed 504 struck through, 153 written beside it). 1 398 + 153 = 1 551 = gültige Stimmen, so 153 is used.
- **Names and labels**:
  - Stahnsdorf 2008 prints every name "Vorname Name", and one Bewerber in Wittenberge 2008 is printed the same way; these are turned round;
  - the Wittenberge 2008 winner's surname is misprinted in the table (the finding and the workbook 2016/2024 have the other spelling, which is used);
  - one Träger label is misprinted ("Gemeinbsame Liste");
  - six Träger cells cut off by the table cell are completed from the same page's finding, which prints them in full.
- **Runoffs after 4 or 5 weeks** (Letschin 2005, Senftenberg 2006, Jüterbog 2006, Perleberg 2007) are legal under the law of the time; three of the four Hauptwahl findings print the same runoff date (the Senftenberg form has no field for it).
- **Names differing between the two rounds** (4 Bewerber in 3 cycles, e.g. a second given name dropped in the runoff; Träger labels differ for 10 Bewerber in 8 cycles): the parser links the two rows of one person through the Hauptwahl's list of runoff qualifiers (`pair_key`), so 01b does not split them.
- **Bad Freienwalde (Oder), Seelow and Wriezen, 27.09.2009**: the LWL's own result pages date these elections 28.09.2008. The figures are identical to the scans'. For Bad Freienwalde the Internet Archive's captures show the 2001 result still online on 03.10.2008 and the new one first on 04.10.2009, so the scans' date is right; in all three the elections of November 2001, 2009 and 2017 form the regular eight-year cycle.

## Privacy

Elected persons are named in the public `mayoral_candidates` (user decision 2026-10-16). Every other Bewerber (295 people in the added elections) keeps votes, shares, rank and Wahlvorschlagsträger, but their name and the fields derived from it go to the restricted twin only (`name_restricted`), as for the B VII 3-6 losers. The published rows are the elected person's own: the top row of the decisive round, the paired first-round row and the result-less first-round copy. They are chosen by row, not by name, so a namesake or relative is never published. In 9 elections a losing Bewerber's own Wahlvorschlag is labelled with their surname ("Einzelwahlvorschlag …"). Since 19.10.2026 such a label is the plain list type ("Einzelwahlvorschlag", "Einzelbewerber") in the public files and stays as printed in the restricted twin (user decision 2026-10-19, superseding the "as printed" choice of 2026-10-16; `code/shared/bb_own_name_label.R`).

## To ask the Landeswahlleiter

- the Hauptwahl page of Schwarzheide (28.09.2008);
- the electorate of Belzig 28.09.2008 (9 779?), and of the Hauptwahlen of Eichwalde and Pritzwalk (27.09.2009);
- Liebenwalde 11.09.2011: 3 704 or 3 407 Wahlberechtigte (workbook).

## Re-run

`python3 code/mayoral_elections/00_bb_lwl_parse.py` → `python3 code/mayoral_elections/00_bb_lwl_scans_parse.py` → `python3 code/mayoral_elections/00_bb_afs_reports_parse.py` → 01 / 01b / 02 / `04a_build_gender_lookup.py` / 04 / 03 / Landrat combine. `99_audit.R` §34 pins the result.
