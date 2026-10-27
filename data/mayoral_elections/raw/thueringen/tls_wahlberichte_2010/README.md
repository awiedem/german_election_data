# Thüringen Bürgermeisterwahlen 6 June 2010 (TLS report 29926)

*Kommunalwahlen in Thüringen am 6. Juni 2010: Wahlen der Bürgermeister – Endgültige Ergebnisse*, Thüringer Landesamt für Statistik (Bestell-Nr. 29 926, December 2010), 88 pages, text layer.

- Downloaded on 2026-10-12 from the public Statistische Bibliothek: `https://www.statistischebibliothek.de/mir/receive/THHeft_mods_00018635` (file `29926_2010_01.pdf`, 2,661,196 bytes, SHA-256 `7b4c7ebacf63543bdbf3635dbc25196d5020823150d5dbf6bd9e2715706448a6`).
- The PDF is **gitignored**. It names candidates and elected persons, and the TLS does not allow those names to be published (its answer of 2026-10-12, Vorgang 1167/2026, see `../tls_wahlberichte_2000_2004/README.md`).
- Its imprint, like the 2004 report's, requires the TLS's prior consent for electronic distribution. The consent of 2026-10-12 (results with a source citation, no personal data) is treated as covering this report too (user decision 2026-10-13); no separate request was sent.

## Content

- Table 3.3: per-Gemeinde results of the 689 elections of 6 June 2010. The election database (wahlen.thueringen.de) holds 389 of them, because it lists only today's Gemeinden.
- Table 4.1: every mayor in office, with the date of their election where it is not 6 June 2010. Despite the heading ("bis einschließlich 6. Juni 2010") it also lists the Neuwahlen of August and September 2010.
- **Vorbemerkungen:** "Trotz der Änderung des Thüringer Kommunalwahlgesetzes war für die Wahlen am 6. Juni 2010 die Wiedereinführung der Stichwahl nicht relevant. Um als Kandidat gewählt zu werden, genügte eine einfache Mehrheit."

## Used for

Parsed by `code/mayoral_elections/00_th_tls_reports_parse.py` (`process_2010()`, since 2026-10-13), like the 2004 report 20421.

- **Table 3.3** is read column by column (two Gemeinden per page row; pdfplumber half-page crops) and must reproduce **table 3.2** exactly for every Landkreis and the state: electorate, voters, valid votes, the votes of CDU, DIE LINKE, SPD, FDP, GRÜNE and sonstige, and their Mandate (counted as the Träger of each election's top named candidate). Every block's candidate votes add up to its valid votes.
- **The 389 elections the database also holds** agree exactly (electorate, voters, invalid and valid votes). The report's names are grafted onto the database rows (`tls_name`); the database keeps its own labels and figures.
- **The 300 it lacks** (Gemeinden merged since) are added in the database's convention: a list election carries every Wahlvorschlag; in a write-in election (a "sonstige" line) the first-listed person, every person with a Träger and the elected person stay separate, the rest are pooled as "Weitere Personen". The rule reproduces the database in 378 of the 389 overlapping elections. Eight Gemeinden that merged later in 2010 carry their 2009 code; the harm stage maps them through its year−1 fallback.
- **Winner.** No runoff was held (the no-runoff window, below). The elected person is the top named candidate in all 684 accepted elections; the parser asserts that the majority rule seats exactly the report's elected person in each.
- **Neuwahl erforderlich** (the elected person declined): 5 elections seat nobody (`flag_decisive_round_missing`); three were already pinned from the database, the two added ones come from the report. Their Neuwahlen (August/October 2010, in table 4.1) are not ingested.
- **Names** reach the restricted twin only (`name_restricted`), the elected persons' too. The panel is unchanged: its Thüringen terms use public names only.
- Traps: a degree in brackets after the name ("(Dipl.-Ing.) (DIE LINKE)") is a title, not the Träger; a bracket on the crop line can be lost in the elected-person line; a label can end in a number that runs into the votes (split by the printed share).

## Evidence for the majority rule (2026-10-12)

`decide_majority_rule()` in the same script:

- **The no-runoff window.** Thüringen required an absolute majority except while the runoff was abolished. The database has no runoff for any election between 19 April 2009 and 22 August 2010.
  - In this window, the plurality winner was elected.
  - Table 4.1 confirms 2009 plurality winners in office: Schmölln (30.8 %), Krauthausen, Altenbeuthen, Leutenberg and Kauern.
- **Window elections that seated nobody.** Five 2010 elections have "Neuwahl erforderlich" (above). Bruchstedt (21.02.2010) held a Neuwahl on 15.08.2010, whose winner table 4.1 lists as the mayor.
- **Winners of write-in elections.** Where the pooled write-ins ("Weitere Personen") hold more votes than any named person, the elected person is still the top named candidate: 7 elections of 6 June 2010 (3 in the database, 4 added) and Kauern 2009, by the same rule.

## Table 4.1 (since 2026-10-13, `process_41_2010()`)

Table 4.1 lists the mayor in office in every Gemeinde at the end of 2010: name, Träger, hauptamtlich, write-in election, votes and share of the decisive round, and a date where the election was not held on 6 June 2010.

- Read by word position (pdfplumber). A wrapped entry starts at the Gemeinde column and carries its figures on its last line.
- **Check:** all 681 undated rows are table 3.3's elected persons, with the same name and votes. The other 8 Gemeinden of 3.3 list a later election: the 5 Neuwahlen, plus Teistungen, Sitzendorf and Oberreißen, which voted again in October and November 2010.
- **264 dated rows (2005–2010):**
  - 168 are database rounds; the names go to the restricted twin only.
  - 95 are in no other source (Gemeinden merged since). They are added as winner-only elections, like table 3.4 of the 2004 report.
  - 1 runoff is left unnamed: Reichstädt 2009, where the database's three Einzelbewerber cannot be told apart.
- **Runoffs.** The table has no runoff marker. For the 25 runoffs the database holds, it prints the runoff's own date 22 times and the first round's date 3 times, always with the decisive round's votes.
  - An added election is filed as a runoff only where the date is a statewide runoff day: the database holds runoffs on that date and no first round elsewhere. Write-in elections and the no-runoff window are always first rounds.
  - Tested on the 168 database rounds, this rule is right 163 times and never calls a first round a runoff. A few of the 87 added first rounds may therefore be runoff winners whose first round is unknown.
- **Bad Berka 2006.** The runoff of 21.05.2006 was a tie (1,459 to 1,459), decided by lot. Table 4.1 names the Einzelbewerber as the winner; GERDA had seated the FWG candidate. Fixed via `is_winner_src`.
- The six Oberbürgermeister rows are not used: those names come from the Info files.
