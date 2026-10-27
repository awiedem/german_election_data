# Thüringen Kommunalwahl reports 2000 and 2004 (TLS)

Sent by e-mail on 2026-10-08 by the Thüringer Landesamt für Statistik
(Stefan Seewald, auskunft@statistik.thueringen.de, Vorgang 1167/2026), in reply to
our request of 2026-10-07 for older Bürgermeister/Landrat reports. Saved verbatim
from the e-mail attachments; not on the TLS website (the webshop URLs for these
order numbers return a placeholder page).

The PDFs are **gitignored**. They name candidates and elected persons (no name
from them may be published, see the TLS's answer below), and the imprint of the
2004 reports allows reproduction for non-commercial purposes with a source note but
requires prior consent for distribution "über elektronische Systeme/Datenträger".

| File | Content | Pages | Text layer | SHA-256 |
|---|---|---|---|---|
| `29924_2000_01.pdf` | Kommunalwahlen 2000, *Vorläufige Ergebnisse*: Stichwahlen der Landräte, Oberbürgermeister und Bürgermeister (Stand 28.05.2000) | 46 | none (scan) | `5f258ade9ee3440930bff66edcf4e373a92b85ab4f4e5def4c60168c3171c9ff` |
| `20421_2004_01.pdf` | Kommunalwahlen 2004, *Endgültige Ergebnisse*: Wahlen der Bürgermeister (Bestell-Nr. 20 421, April 2008) | 172 | yes | `2ebe9d3cdef4e79b5c0e38b1ff98a81231750d97a374049c41695dad8178d8a7` |
| `29923_2004_01.pdf` | Kommunalwahlen 2004, *Vorläufige Ergebnisse*: Wahlen der Bürgermeister und Ortsbürgermeister (Bestell-Nr. 29 923, June 2004) | 56 | yes | `5ed8a0f17dfb1af05efb47ee8f2885933565b0b924bce6629444ea62e1f02ff2` |

## What is in them (first look, 2026-10-08)

- **2004 final report (20421):**
  - Table 3.4 "Übersicht der gewählten Bürgermeister" lists **every Bürgermeister in office after the election**, not only those elected on 27.06.2004. Earlier elections carry their own date (e.g. Heilbad Heiligenstadt 14.05.2000). Columns: name, Wahlvorschlag, hauptamtlich, votes, share, runoff marker.
  - Table 3.5 gives per-Gemeinde results for the 820 Gemeinden that voted on 27.06.2004 (runoffs 11.07.2004): electorate, voters, invalid and valid votes, every candidate with votes, and the elected person.
  - Under "Wahl ohne Bindung an zugelassene Wahlvorschläge" the candidate lines are voters' write-ins.
- **2004 preliminary report (29923):** preliminary results, including the Ortsbürgermeister. It is superseded by 20421 for the Bürgermeister.
- **2000 runoff report (29924):** preliminary runoff results of 28.05.2000, per Gemeinde/Kreis, with candidate names and votes. It is a scan, so it needs OCR or a visual read.

## Traps seen so far

- The 2004 text layer has font-encoding damage in some lines: each glyph sits 29 code points below its letter and the space is `\x03`, so `GDYRQ HQW¿HOHQ DXI` reads "davon entfielen auf"; brackets come out as blanks. Some Gemeinde lists in section 2 are damaged the same way. `decode()` in the parser undoes it.
- Names can differ within one entry: Berlingerode prints the elected mayor's given name one way on the candidate line and another way on the "gewählt" line. Such variants are resolved in `name_pins_restricted.csv` (gitignored, since no name from these reports may be published).

## Transcriptions (gitignored, they name losing candidates)

- `th_tls2000_runoffs_transcribed.csv`: the 31 runoff pages of 29924 (3 OB, 3 Landrat, 25 Bürgermeister), read visually from 300-dpi renders on 2026-10-08. Checks: the votes sum to the valid votes on every page; the shares match; the elected person is the top candidate; the per-area pages match the summary pages (p. 7, p. 17) and the overview tables (pp. 8-10, 18-19) exactly.
- `th_tls2000_summary_transcribed.csv`: the two summary tables (p. 7, p. 17).
- `th_tls2000_arnstadt_hw_transcribed.csv`: an unnumbered insert (PDF p. 31) with Arnstadt's first round of 14.05.2000. Two party labels are cut off at the page edge ("(FWG Pro", "(BüForumArn").
- 29924 has blank pages at PDF pp. 4 and 32. Up to p. 30 the printed page number is two lower than the PDF page, from p. 33 four lower.

## Ingested 2026-10-08

`code/mayoral_elections/00_th_tls_reports_parse.py` writes `../../thueringen_bm/th_mayoral_tls_augmented.csv` (read by 01/01b) and `data/landrat_elections/raw/thueringen_tls2000_runoffs.csv` (read by the Landrat parser `00_th_parse.R`). Summary:
- 2004: 820 Bürgermeisterwahlen. 439 are also in the database, with identical counts, and get names; 381 are added from the report.
- Mayors elected on other dates (table 3.4): 113 found in the database, 63 added as winner-only elections (3 of them runoffs, dated by the TLS on 12.10.2026).
- 2000: 23 runoffs named in the inputs; 5 Bürgermeister runoffs and 3 Landrat runoffs added.

Details are in the script's docstring and in `data/mayoral_elections/final/README.md` (Thüringen section); `99_audit.R` §25 pins the result.

## Answer of the TLS (12.10.2026, Stefan Seewald, Vorgang 1167/2026)

- **Use:** publishing the results in GERDA is allowed with a source citation, but only "unter Löschung sämtlicher personenbezogenen Daten zu Bewerbern und gewählten Kandidaten" (§ 50 Abs. 2 ThürKWO). So no name from these reports reaches the public files, the elected persons' included: `01b_mayoral_candidates.R` marks them `name_restricted` and strips them, the Landrat combine withholds the three Landrat runoffs' names, and the restricted twin keeps them. A Wahlvorschlag label that carries its candidate's surname loses that word on the rows added from the reports (2 labels, Bretleben 2004). Code, tracked docs and audits name nobody from these reports: the name pins and the audit's name fixtures sit in gitignored CSVs here (`name_pins_restricted.csv`, `audit_fixtures_restricted.csv`). `99_audit.R` §25 and the Landrat audit check all of this.
- Tautenburg 2004: the TLS confirmed which of the three printed spellings of the elected mayor's surname is right (pinned in `name_pins_restricted.csv`).
- Königsee: 07.09.2003, not 27.09.2003; Leutenberg: 15.06.2003, not 17.06.2003 (`DATES_34`). With the misprint, Leutenberg's election had been added a second time as a winner-only election.
- The three runoffs of table 3.4: its date is the first round's; the runoffs were Andenhausen 10.10.2004 (first round 26.09.2004), Berga/Elster 26.01.2003 (12.01.2003) and Teichwolframsdorf 27.06.1999 (13.06.1999) (`RUNOFF_DATES_34`). They are ingested now.
