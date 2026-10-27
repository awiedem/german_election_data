# Brandenburg Bürgermeisterwahlen — Statistische Berichte B VII 3-6 (1993–2024)

Downloaded 2026-10-09 from the Statistische Bibliothek, series
`BBSerie_mods_00000746` ("Kommunalwahlen im Land Brandenburg, Bürgermeisterwahlen"):
https://www.statistischebibliothek.de/mir/receive/BBSerie_mods_00000746

The library of the Amt für Statistik Berlin-Brandenburg (Gabriele Knopf, 73 B –
Bibliothek, Bibliothek@statistik-bbb.de) pointed to this series on 2026-10-09, in reply
to our request of 2026-10-07 for historical mayor registers. No Vorgang number. The AfS
holds **no** historical Bürgermeisterverzeichnisse; this series is all it has. The files
are verbatim downloads from the Statistische Bibliothek. The e-mail itself carried only
signature logos.

**Ingested 2026-10-09**; released with the permission of the Amt für Statistik (see "Licence" below):
`code/mayoral_elections/00_bb_afs_reports_parse.py` reads these files and the
transcriptions in `transcribed/` and writes `../bb_afs_parsed.csv`, which the
Brandenburg blocks of 01/01b append to the Landeswahlleiter workbook rows.

## Files

| File | Issue | Content | Pages | Text layer | SHA-256 |
|---|---|---|---|---|---|
| `BM_OB_5.12.1993.pdf` | 1993,1 | Wahl der BM und OB am 05.12.1993, *Ergebnisse ausgewählter Städte* (preliminary). One sheet per town for 22 towns. Of the 45 "ausgewählte Städte" (≥ 20 000 inhabitants or former Kreisstädte), 30 elected a mayor that day; 22 had results at press time. Each sheet gives the electorate, voters, invalid, valid and required votes, **every candidate with Wahlvorschlag and votes**, and the elected person. Includes the Frankfurt (Oder) OB. | 23 | scan (OCR layer unusable) | `dc263ef22640decda360dcee0d315c15bafd017397027bd161eb17291172be99` |
| `Stichwahl_BM_u_OB-19121993_BB.pdf` | 1993,2 | Stichwahl der BM und OB am 19.12.1993 (preliminary). Lists the 85 Gemeinden with a runoff, 10 of them without results at press time. A summary table gives electorate, voters, turnout and the party of both runoff candidates and of the winner. Then one sheet per Gemeinde, as above. Includes the OB runoffs of Brandenburg a. d. H., Cottbus and Potsdam, and many small amtsangehörige Gemeinden. | 82 | scan (partial OCR layer) | `cb2f759dda0e15f3548748094a9299b1191da0eb217c6efa03258bde6fe51f2a` |
| `SB_B7-3-6_5j-98_BBa.pdf` | 1998 | Bürgermeisterwahlen 27.09.1998 / Stichwahl 11.10.1998, endgültiges Ergebnis: 1 223 ehrenamtliche BM, 4 hauptamtliche (Jüterbog, Löwenberger Land, Trebbin, Wahrenbrück) and the Potsdam OB (table 4: all candidates with votes). Table 5 lists every elected BM: AGS, Amt, Gemeinde, name, Wahlvorschlag. `#` = elected in the runoff; `hauptamtl.` marks full-time mayors. | 54 | scan (no text) | `91d6edabe8f2d1f419109de4d6192dba02a91f273a2d45aad58f5469dfdf463f` |
| `SB_B7-3-6_5j-02_BB.pdf` | 2002 | Bürgermeisterwahlen 2001/2002, endgültiges Ergebnis. First round 11./18.11.2001 (43 Gemeinden), runoff 02.12.2001; first round 24.02.2002 (23 Gemeinden), runoff 17.03.2002. Tables 5/6: OB Brandenburg a. d. H., Cottbus and Frankfurt (Oder), all candidates with votes in both rounds. Tables 7/8: the 66 elected OB and hauptamtliche BM (AGS, Amt, name, Wahlvorschlag, runoff marker). | 22 | scan (no text), clean | `57ca4906ac8f03bb3accce95a8e79bf968397d717297479dfda263bf9e10748e` |
| `SB_B7-3-6_5j-03_BB.pdf` | 2003 | Bürgermeisterwahlen 26.10.2003 / Stichwahl 16.11.2003. OB Brandenburg a. d. H. in tables 8/9 (all candidates, both rounds). 43 hauptamtliche and 266 ehrenamtliche BM, aggregated only (tables 2–7). **Table 10 lists the mayor of every Gemeinde in office after the election**: type (H/E), name, Wahlvorschlag, Wahltag and Stichwahl date. It includes hauptamtliche elected 1996–2003. Table 11 has the same, by party. | 47 | **yes** | `422f96da863fac498bbe14f2b7166c712450fda8df86a11da8a92257a85e793e` |
| `SB_B7-3-6_j05-08_BBa.pdf` | 2008 | Bürgermeisterwahlen 28.09.2008 / Stichwahl 12.10.2008: 266 ehrenamtliche and 4 hauptamtliche (Müncheberg, Kremmen, Schwarzheide, Belzig). Table 3: the 270 elected (AGS, name, Wahlvorschlag, dates). Winners only. | 16 | yes | `86ea2626a5c993d08b97d367ce6769e11322a764ccb642cf29915141f4fe1180` |
| `SB_B7-3-6_j05-08_BB.xls` | 2008 | Excel version of the above (sheet `Tabelle3`). | – | – | `5157dc5b6bec7236a9f5ab38bc35df22102d3795e6b1be941e03ce3a47e294df` |
| `SB_B07-03-06_2014j05_BBa.pdf` / `.xlsx` | 2014 | 25.05.2014 / Stichwahl 15.06.2014: 264 elected BM (sheet `3`), almost all ehrenamtlich. Winners only. The AGS column has 10 digits (AGS + 2). | 16 | yes | pdf `ad5bf0667311dd4fff61dbc10482d4ed4f53b2d8238278984606d5e7bbc00e6b`, xlsx `8be8470c1b29db73064f906e68adda11537bdfe7eb192548585c1800c877b817` |
| `SB_B07-03-06_2019j05_BB-k.pdf` / `.xlsx` | 2019 | 26.05.2019 / Stichwahl 16.06.2019: 264 elected BM (sheet `3`). Winners only. | 14 | yes | pdf `c4e3c1fed2c2046cfdd41d8466ef5f11ce574e5e555e23267a715c01668a270b`, xlsx `7f6df73de1d308c0cf8e51452c37508d1604fd6521c9a3df6b0b6f3ebb393900` |
| `SB_B07-03-06_2024j05_BB.pdf` / `.xlsx` | 2024 | 09.06.2024 / Stichwahl 30.06.2024: 272 elected BM (sheet `3`). Winners only. 12-digit ARS → AGS = `ARS[0:5] + ARS[9:12]`. | 14 | yes | pdf `45e6ac730e15822db08007f540c752d355e256eebba3fce3bb3710137298ddeb`, xlsx `7540816f19a593acc50f2262a0c3c7fe3e53bb5170983ad3c6ef3f518cb9c403` |

## Licence / imprint

- 1993 (both): prints of the Landeswahlleiter / LDS, with no imprint page.
- 1998: "Nachdruck, auch im Auszug, nur mit Quellenangabe gestattet", plus the usual ban on using the print for election campaigning.
- 2002 and 2003 (LDS) and 2008 (AfS): non-commercial reproduction and free distribution with a source note are allowed. **Distribution "über elektronische Systeme/Datenträger" needs prior consent**, the same clause as in the TLS and LfStat reports.
- 2014, 2019, 2024: **CC BY 3.0 DE** (stated in the `Impressum` sheet).
- **Permission (e-mail of 27.10.2026):** the Amt für Statistik Berlin-Brandenburg (Wahlstatistik, Stabsstelle 83 – Wahlen; the library had forwarded our request of 2026-10-09) allows GERDA to use the elected mayors from the reports 1993–2008, as described in the request (name, Wahlvorschlagsträger, Wahltag, votes), with a source citation. The request said that losing candidates would not be named. The AfS holds no final result of the elections of December 1993.

Gitignored (see `.gitignore`): the two 1993 files and every `SB_B7-3-6_*` file (1998–2008). They name losing candidates (1993, 1998 Potsdam, 2001/02 OB, 2003 OB) or carry the consent clause (2002, 2003, 2008); the permission covers the data GERDA takes from them, not redistributing the reports, which stay in the Statistische Bibliothek. The CC-BY files of 2014–2024 are not ignored.

## Assessment (2026-10-09, before ingestion)

GERDA's Brandenburg scope is the **hauptamtliche** BM plus the 4 OB (`00_bb_lwl_parse.py`). The ehrenamtliche BM of amtsangehörige Gemeinden are excluded. Measured against `final/` of 2026-10-08, BB mayoral has 142 AGS, covers 2010–2026 only, and has 1 051 candidate rows and 295 panel terms. By the LWL's own statement, 2010–2013 is incomplete.

- **In scope, new:**
  - Table 10 of 2003 names 147 hauptamtliche BM/OB in office in late 2003 (elected 1996: 3, 1997: 1, 1998: 5, 2000: 5, 2001: 46, 2002: 35, 2003: 52). 140 of the 147 AGS are in GERDA's BB set.
  - Full results for the OB races of 1993 (both rounds), Potsdam 1998, the 3 cities of 2001/02 and Brandenburg a. d. H. 2003.
  - The 22 town sheets of 05.12.1993 and the runoff sheets of 19.12.1993.
  - 4 named hauptamtliche winners in 1998 and 4 in 2008.
- **Spot checks:**
  - A prototype parse of table 10 (scratch only) found 42 of the 140 holders as later GERDA winners in the same AGS. All 42 have the same surname, and all but one the same given name (`Garn, Friedrich Wilhelm` vs `Wilhelm Garn`). Party labels agree (PDS → DIE LINKE).
  - The 2024 list agrees with GERDA on all 4 AGS both hold.
  - Known office-holders check out: Pohl (Frankfurt (Oder), 1993, EB 61.0 %), Rätzel (Cottbus, 2002 runoff, EB), Patzelt (Frankfurt (Oder), 2002 runoff, CDU), Jakobs (Potsdam, 22.09.2002), Tiemann (Brandenburg a. d. H., 2003), Platzeck (Potsdam, 1998).
- **Out of scope unless the scope widens:** ~1 223 (1998), 272 (2003), 266 (2008) and ~264–272 (2014/2019/2024) ehrenamtliche BM, winners only.
- **Gaps that remain:**
  - None of these reports covers the hauptamtliche elections between the general election dates (2004–2009), which includes most of the 2009/2010 re-election cycle of the 2001/02 cohort. Since 2026-10-16 they come from the Gemeinden's result reports in the Landeswahlleiter's archive (`../lwl_ergebnismeldungen_2004_2013/`).
  - 1993 covers selected towns only and is preliminary; the AfS holds no final result (27.10.2026).
- **Traps seen:**
  - Table 10 has two footnoted Gemeinden without an Amt code (Diepensee, Haidemühl), and 15 Gemeinden of Amt Neuhausen/Spree marked "späterer Wahltermin".
  - One Gemeinde name runs over two lines (Hohenselchow-/Groß Pinnow).
  - Some H rows have an empty Wahlvorschlag.
  - The AGS are those of the report year, before or after the Gemeindegebietsreform of 2001–2003, while BB `*_unharm` rows are on 01.01.2026 boundaries.
  - Winner-only historical rows followed by a 2004–2009 gap would give open-ended panel terms (the Thüringen term-cap problem).

## Transcriptions (`transcribed/`, gitignored: they name losing candidates)

Made on 2026-10-09 from the scans by reading 200-400 dpi renders; every page was
read twice. Where the report prints a list twice, the two printings were
transcribed by different readers and reconciled in the parser.

| File | Source | Rows | Checks |
|---|---|---|---|
| `t93hw_sheets.csv`, `t93hw_cands.csv`, `t93hw_cover.csv` | 1993,1 pp. 1-23 | 22 sheets, 69 candidates | votes = gültige (except Gransee, a sole candidate's Ja-Stimmen); ungültige + gültige = Wähler; shares; turnout |
| `t93sw_sheets.csv`, `t93sw_cands.csv` | 1993,2 pp. 6-81 (p. 82 blank) | 76 sheets, 152 rows | as above, and each sheet against its line of the summary table |
| `t93sw_list.csv`, `t93sw_summary.csv` | 1993,2 pp. 1, 3 (list), 4-5 (summary) | 85 Gemeinden | summary rows add up to the printed total (655 765 / 308 019) |
| `t98_t5.csv` / `t98_t6.csv` | 1998 tables 5 and 6 | 1 195 each | 1 150 + 44 + Potsdam = report table 1; the two printings agree on 1 190 names, the 5 others settled from the sharper table 5 |
| `t98_potsdam_ob_*.csv`, `t98_t7.csv`, `t98_agg.csv`, `t98_abbrev.csv` | 1998 tables 1-4, 7 | | tables 1-3 add up across Kreise and parties; table 7 = 32 + Neuhardenberg |
| `t02_t7.csv` / `t02_t8.csv`, `t02_t1.csv`, `t02_ob_*.csv`, `t02_agg.csv` | 2001/02 tables 1-8 | 66 each | t7 = t8 on all 66 rows; runoff markers = table 1; per-Kreis party counts = tables 2-4 |

## Ingestion summary (`bb_afs_parsed.csv`, 2026-10-09; revised after the audit of 2026-10-12)

- 2 807 elections, 1 494 AGS, 1993-2024, after merging 68 elections printed in two reports, dropping 5 the workbook already holds and 4 of 2008 that the 2004-2013 result reports hold with full results (the parser checks that both name the same elected person); 26 of them are council choices (below). 2 600 ehrenamtlich, 207 hauptamtlich/OB.
- By year: 1993 98, 1996-2000 8, 1998 1 230 (incl. Potsdam OB), 2001 43, 2002 30, 2003 317, 2008 268, 2014 264, 2019 263, 2024 264.
- Full results (all candidates with votes): the 22 town sheets and 76 runoff sheets of 1993, Potsdam OB 1998, the three OB races of 2002, Brandenburg an der Havel OB 2003. Everything else is winner-only.
- 36 elections seated nobody (`flag_decisive_round_missing`): 2 runoffs of 1993 (Bahnsdorf tie 94:94; Schipkau below the 15 % quorum), 32 rejected single candidates and the Neuhardenberg runoff of 1998 (table 7), Tschernitz 2024 (the elected person declined; repeat election 20.10.2024).
- Council choices (15 former Amtsdirektoren and 5 ehrenamtliche in 2003, "Wahl durch Vertretung" in 2008 (2) and 2024 (4, footnote 1)) are kept as cycles with `elected_by_council` = TRUE: no directly elected winner, the person in the restricted twin only, not in `mayoral_unharm`; they end the previous mayor's term in the panel. Left out: 15 postponed elections (Amt Neuhausen/Spree 2003), the 9 runoffs of 1993 without results.
- Winner-only elections decided in a runoff (222) get a first-round row on the date the report prints, without result (`first_round_no_result`), so the cycle is filed under its first round like every other runoff.
- 1993: the runoff sheet "Neuenhagen" is Neuenhagen b. Berlin (12064336), not the village Neuenhagen whose name matched first (`AGS_PINS_1993`); every name-resolved 1993 AGS must now have an electorate of 40-110 % of its population. List types stay as printed ("Einzelwahlvorschlag" in 1993, "EB" later); the panel compares them as one label.
- `name_restricted` compares surname and first given name, so a loser sharing the winner's surname is not published.
- Source misprints fixed and pinned in the parser: Müllrose "26.20.2003", Panketal "16.11.2033", Petershagen/Eggersdorf and Strausberg "17.03.2003" and Vetschau "18.11.2002" (2003 table 10; the 2002 report has the true dates), Bestensee's 1993 electorate (sheet 2 397, summary and state total 3 897), three name-order swaps in 1998 ("Jörg, Meyer").
- Kept as printed: misspelled given names ("Wolfgng", "Frriedhelm", "Egebert"), Hörlitz 1993 (votes 294, gültige 296), surnames without given names (1993 sheets, a few 1998 entries).
- Office: H/E from the report where it says so (1998 marker, 2003 Wahlart, 2014/2024 Amt code); otherwise by whether the workbook holds full-time elections for the Gemeinde.

## Open items

- Licence: settled on 2026-10-27 (see "Licence / imprint").
- The hauptamtliche elections of 2004-2009 (and 2010-2013, incomplete in the workbook) are in no report. A request to the Geschäftsstelle des Landeswahlleiters was sent on 2026-10-09; on 2026-10-15 it sent scans of the Gemeinden's result reports 2004-2013, ingested on 2026-10-16 (`../lwl_ergebnismeldungen_2004_2013/README.md`).
- Spelling differences of one person between reports (Horts/Horst 1998, Clauß/Claus, ...) are pinned in `data/mayoral_elections/metadata/person_name_variant_links.csv` (31 Brandenburg pairs).
- Three persons appear as elected on 09.06.2024 in two Gemeinden each in the 2024 list (identical in the xlsx and the PDF); likely namesakes or a source error, published as printed.

## Replaced by the Landeswahlleiter's archived result pages (19 October 2026)

`00_bb_lwl_archive_parse.py` (`../lwl_bmwahlen_archive/`) replaces 411 elections that these reports hold winner-only (2001–2003, 2008, 25.05.2014) by the Landeswahlleiter's own archived result pages, with counts and every Bewerber. It checks that both name the same elected person. The 472 rounds replaced are listed in `../bb_lwl_archive_supersedes.csv`; `bb_afs_parsed.csv` itself is unchanged, and 01/01b drop those rounds when the archive file is present.
