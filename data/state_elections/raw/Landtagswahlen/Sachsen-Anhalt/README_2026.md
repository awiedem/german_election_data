# Sachsen-Anhalt Landtagswahl 6 September 2026 — Gemeinde level

Endgültiges Ergebnis (Landeswahlausschuss 22.09.2026), Statistisches Landesamt
Sachsen-Anhalt, downloaded 2026-09-25 from
<https://wahlergebnisse.sachsen-anhalt.de/wahlen/lt26/downloads.html>. Stored verbatim.

| File | Source name | Content |
|---|---|---|
| `Sachsen-Anhalt_2026_Landtagswahl_Gemeinden.csv` | `Ergebnisse_Gemeinden_LT_2026.csv` | 218 Gemeinden (8-digit AGS), Erst- and Zweitstimmen; **parsed** by `01b_state_unharm_raw.R` |
| `Sachsen-Anhalt_2026_Landtagswahl_alle_Ebenen.xlsx` | `Ergebnisse_LT_2026.xlsx` | same results for all levels (reference only) |
| `Sachsen-Anhalt_2026_Datensatzbeschreibung.pdf` | `DSB_LT_2026.pdf` | record layout |

Three rows per Gemeinde keyed by `Wahllokal`: `U` (Urne), `B` (Brief), empty (total).
Only the total rows are used. Postal votes are booked inside each Gemeinde, so nothing is
pooled above Gemeinde level. The 218 totals reconcile exactly with the Land row of
`Ergebnisse_Land_RKR_WKR_LT_2026.csv` (1,706,852 eligible; 1,327,991 voters; every party).
