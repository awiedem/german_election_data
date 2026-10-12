# Thüringen 2024: Stadtratswahlen of the five kreisfreie Städte

Erfurt, Gera, Jena, Suhl and Weimar (AGS 16051000–16055000) elected their city councils on 26 May 2024, together with the Gemeinderatswahlen.

`../thueringen_2024.xlsx`, the Gemeinderatswahl workbook, covers only the 17 Landkreise (sheets "Kreis 61"–"Kreis 77"). That is why the cities were missing from GERDA's 2024 council data until October 2026; the gap was reported by Hilbig et al. on 7 October 2026.

## Source

- Each file here is one result page of the Thüringer Landesamt für Statistik database, saved verbatim. The pages are headed "Stadtratswahl 2024 in Thüringen – endgültiges Ergebnis".
- URLs and SHA-256 hashes are in `_source_manifest.csv`. Encoding is windows-1252.
- The same database serves the Landkreis overviews (`zeigeErg=SORTGEM`). Those agree exactly with `thueringen_2024.xlsx` on eligible voters, voters and valid votes in all 600 Gemeinden, so the two sources are one release.

## Layout

- Each page has a key–value block: Wahlberechtigte, Wähler, Ungültige Stimmabgaben, Gültige Stimmabgaben, Gültige Stimmen and Zu vergebene Sitze.
- It is followed by one row per list: number, short name, votes, seats and %.
- The table cells are never closed (`<td>` without `</td>`), so the parser in `01_municipal_unharm.R` splits rows on the tags rather than using an HTML table reader.

## Checks the parser enforces

- In every city, the list votes sum to "Gültige Stimmen" and the list seats sum to "Zu vergebene Sitze".
- "Gültige Stimmen" counts votes, not ballots (three per voter), as in the 2019 rows.

Joint lists (`Piraten-ÖDP`, `PIRATEN/Partei der Humanisten`, `Bürger für Thüringen/dieBasis`) go to `other`, like every other joint list in the TH 2024 block.
