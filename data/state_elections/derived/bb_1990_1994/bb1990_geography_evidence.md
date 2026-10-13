# Brandenburg 1990 geography evidence

The mapping preserves election-day municipalities and excludes votes cast on Mecklenburg-Vorpommern ballots. We compare municipality names and precinct counts in the official 1990 spreadsheet with the original election report and historical municipality crosswalks. The spreadsheet includes 1,812 distinct municipality-name and identifier pairs; 1,790 belong to Brandenburg in 1990 and 22 belong to Mecklenburg-Vorpommern. Restoring Brunow and Dambeck gives 1,792 Brandenburg municipalities.

## Sources and reconstruction

All paths below are relative to the repository root. AGS means the eight-digit official municipality identifier. DDR codes are the six-digit municipality identifiers printed in the original report.

- `data/state_elections/raw/Landtagswahlen_Wahlkreis/Brandenburg/BB_1990_Landtagswahl_Wahlkreis.xlsx`, sheets `Zweitstimmen` and `Erläuterungen`. SHA-256: `e164fd8bd6a28acd5b78ca1f80b28cd0a1cb321fa96374b249d33b68d839eb1d`.
- `data/state_elections/raw/Landtagswahlen/Brandenburg/Brandenburg_1990_Landtagswahl.pdf`. SHA-256: `707ad5f3dd65762b1b3a1feabbbe3bc1cb849527de5d73d00a37211e874deb8b`. Municipality register begins on printed/PDF page 464. Precinct tables occupy pages 16–447.
- `data/crosswalks/raw/gmd_auf_2021/ref-gemeinden-umrech-2021-1990-1999.xlsx`, sheets `1990` and `1993`. SHA-256: `49245cb452ff7bb258a0c0580325bf177f94bc359fcce287d4e7c61a41ae62b0`. The `1990` sheet's column headings incorrectly say 1996; the sheet itself contains the historical pre-reform identifiers. The first, second, and seventh columns supply historical AGS, historical name, and 2021 destination AGS. The `1993` sheet supplies post-reform county membership through the shared destination AGS.

The TSV maps the pair of spreadsheet identifier and municipality name. The spreadsheet identifier alone is insufficient because municipalities incorporated after the election share an identifier while retaining distinct names. Its identifiers also differ from the BBSR 1994 codes in several places; they are not treated as authoritative election-day identifiers.

Construction used normalized names: uppercase, Ä/Ö/Ü converted to AE/OE/UE, and punctuation/spaces removed. Exact matches were preferred. Prefix matching was used for spreadsheet names truncated to approximately 14 characters. Candidate historical identifiers came from the BBSR names and the original PDF register. Shared 2021 destinations connected historical municipalities to post-reform counties. Exact agreement on eligible-voter and voter totals in the original precinct tables resolved duplicate names where available. Twenty-eight exceptions were reviewed explicitly; their original names, DDR codes, count checks, or other documentary evidence appear in the TSV's `evidence` column. No approximate string-distance matching was used.

The original register was extracted with `pdftotext -layout`; six-digit codes and adjacent names were read from both register columns. The conversion from DDR codes to AGS follows the existing `DDR_KREIS_TO_AGS` table in `code/state_elections/00_bb_1990_extract.py`: historical county prefix plus the two-digit municipality number multiplied by ten. Register extraction alone is incomplete because of OCR errors. It supplements the BBSR names rather than replacing them. For example, the PDF text layer reads Meuro as “Heuro” and Mesendorf as “Hasendorf”; matching eligible-voter and voter counts confirms both identities.

The manual exceptions also cover changed names (Friedenshorst/Königshorst, Marxwalde/Neuhardenberg, Kietz/Küstrin-Kietz), abbreviated qualifiers, and duplicate names. Source `12065???` identifies Zühlsdorf; the historical identifier is `12036470`. This literal source string is retained in the mapping to ensure reproducible matching.

## Restored municipalities

Brunow (`020811`, AGS `12037110`) and Dambeck (`020816`, AGS `12037160`) appear in the original report but were transferred out of Brandenburg in 1992 and omitted from the later spreadsheet. Pages 16–19 show their complete second-vote records; page 464 identifies their names. The supplemental TSV transcribes these records. Rendered pages 16, 17, 18, and 19 were visually inspected on 2026-09-28.

Brunow has 335 eligible voters, 26 voters, three invalid votes, and 23 valid votes: SPD 5, CDU 12, PDS 2, Bündnis 90 2, FDP 1, and REP 1. Dambeck has 317 eligible voters, nine voters, one invalid vote, and eight valid votes: SPD 1, CDU 5, and PDS 2. Other party counts are zero. The very low turnout is present in the original tables and is preserved. Their combined 31 valid votes explain the remaining statewide discrepancy after excluding incoming Mecklenburg-Vorpommern ballots.

Berge's five spreadsheet precincts match the original report's eligible-voter/voter pairs exactly: 514/333, 126/84, 103/16, 87/63, and 100/89. Its data must not receive an additional Pampin/Platschow allocation.

## Why two old municipality keys disappear

The old output includes `12003060` and `12006060`, both with missing eligible-voter and voter totals. They result from inconsistent precinct identifiers in the PDF extraction. Their votes are present in the correct city rows of the spreadsheet.

- Eisenhüttenstadt: PDF page 337 uses `053206/035` on the party table for city precinct 035; the surrounding city rows use `053200`. The second-vote row has SPD 172, CDU 190, PDS 62, Bündnis 90 51, and FDP 45. These five counts sum to the old false municipality's 520 votes. The spreadsheet's Eisenhüttenstadt precinct 035 contains those same counts, all remaining parties, and 557 valid votes; it maps to `12003000`.
- Schwedt: the text layer on PDF page 106 reads `053306/029`, while visual inspection shows city precinct `053300/029`. Its second-vote counts DSU 5, Grüne 30, and REP 8 sum to the old false municipality's 43 votes. The spreadsheet's Schwedt precinct 029 contains these counts and the complete 502 valid votes; it maps to `12006000`.

Rendered pages 106 and 337 were visually inspected on 2026-09-28. Removing the false municipality keys does not remove these precincts. The municipality count changes from 1,793 to 1,792 because two false keys are removed and Zühlsdorf is restored.

## Repeatable audit

Run from the repository root:

```sh
Rscript --vanilla data/state_elections/derived/bb_1990_1994/bb1990_audit_geography.R
```

The audit checks complete source-name coverage, unique mapping keys and destinations, the 22 excluded municipality groups, all 35 incoming Mecklenburg-Vorpommern precincts, supplemental vote arithmetic, and the two city precincts behind the false old keys. It does not automatically reinterpret the manually reviewed names; their evidence remains in the mapping and this note. The audit passed on 2026-09-28.
