# Rheinland-Pfalz Landtagswahl 22 March 2026 — Gemeinde level

Endgültiges Ergebnis (Landeswahlausschuss 02.04.2026), Landeswahlleiter Rheinland-Pfalz,
downloaded 2026-09-25 from <https://www.wahlen.rlp.de/landtagswahl/ergebnisse>. Stored verbatim.

| File | Content |
|---|---|
| `LW_2026_Endergebnis_Stimmbezirksebene.xlsx` | all levels down to Stimmbezirk; **parsed** by `01b_state_unharm_raw.R` |
| `SatzbeschreibungErgebnisseGesamtLW_2026.pdf` | record layout (13-digit key: Bezirk, WK, Kreis, VG, Gemeinde, Stadtteil) |

Use the XLSX. The CSV twin published next to it (kept under
`../../Landtagswahlen_Wahlkreis/Rheinland-Pfalz/`) writes every key of 11+ digits in Excel
scientific notation (`1,01132E+12`), which destroys the VG and Gemeinde digits.

58 Ortsgemeinden have no counts of their own (`Zusammenlegung Grund` = § 57 II LWO or
§ 10 III LWahlG): their ballots and electorate are booked in one of 40 neighbouring
Gemeinden of the same Verbandsgemeinde (`Zusammenlegung Aufnahme` = `+`). GERDA keeps
them as published; the pairs are in `data/state_elections/metadata/rp_2026_pooled_municipalities.csv`.
