# Thüringen — Landtagswahl results at constituency (Wahlkreis) level

**State:** Thüringen (TH)
**Constituency unit:** Wahlkreis (44 Wahlkreise)
**Primary source:** Thüringer Landesamt für Statistik / Landeswahlleiter Thüringen — Wahlen in Thüringen portal
- Overview page: https://wahlen.thueringen.de/landtagswahlen/lw_wahlergebnisse.asp
- Portal root: https://wahlen.thueringen.de/

All Thüringen Landtagswahlen since reunification (1990, 1994, 1999, 2004, 2009, 2014,
2019, 2024) are covered. Each downloaded file is the official **"Wahlkreisübersicht"**
(Excel-Download – Wahlkreisübersicht), i.e. one row per Wahlkreis with votes per party
(Wahlkreisstimmen / Landesstimmen, called Erst-/Zweitstimmen in 1990), turnout, and
valid/invalid votes. This is the cleanest constituency-level file the office publishes.

## Download mechanism

For 1994–2024 the portal serves the file via a redirect wrapper:
`https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO<YEAR>`
→ 302 → `https://wahlen.thueringen.de/downloads/LWINFO<YEAR>.xlsx`
(follow with `curl -L`; the bare `/downloads/...` guess fails for some years, so always
go through the `NeuLesen.asp?seite=LWINFO<YEAR>` wrapper).

1990 is served as a direct static file under `landtagswahlen/Daten1990/`.

## Downloaded files

| File | Year | Source URL | Format | Geo unit |
|------|------|-----------|--------|----------|
| `TH_1990_Landtagswahl_Wahlkreis.xlsx` | 1990 | https://wahlen.thueringen.de/landtagswahlen/Daten1990/L90_Ergebnisse_Wahlkreise.xlsx | xlsx | Wahlkreis |
| `TH_1994_Landtagswahl_Wahlkreis.xlsx` | 1994 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO1994 (→ /downloads/LWINFO1994.xlsx) | xlsx | Wahlkreis |
| `TH_1999_Landtagswahl_Wahlkreis.xlsx` | 1999 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO1999 (→ /downloads/LWINFO1999.xlsx) | xlsx | Wahlkreis |
| `TH_2004_Landtagswahl_Wahlkreis.xlsx` | 2004 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO2004 (→ /downloads/LWINFO2004.xlsx) | xlsx | Wahlkreis |
| `TH_2009_Landtagswahl_Wahlkreis.xlsx` | 2009 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO2009 (→ /downloads/LWINFO2009.xlsx) | xlsx | Wahlkreis |
| `TH_2014_Landtagswahl_Wahlkreis.xlsx` | 2014 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO2014 (→ /downloads/LWINFO2014.xlsx) | xlsx | Wahlkreis |
| `TH_2019_Landtagswahl_Wahlkreis.xlsx` | 2019 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO2019 (→ /downloads/LWINFO2019.xlsx) | xlsx | Wahlkreis |
| `TH_2024_Landtagswahl_Wahlkreis.xlsx` | 2024 | https://wahlen.thueringen.de/NeuLesen.asp?seite=LWINFO2024 (→ /downloads/LWINFO2024.xlsx) | xlsx | Wahlkreis |

All eight files verified: HTTP 200, content-type
`application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`, non-trivial size,
and confirmed to contain per-Wahlkreis party vote columns + turnout + valid/invalid.

## Years missing

None. The Freistaat Thüringen has held exactly these eight Landtagswahlen (1990 onward,
after reunification); all are available and downloaded at Wahlkreis level. There were no
Landtagswahlen before 1990 in the modern Freistaat (the historic 1946/1950 Landtag of
the early GDR era is out of scope and not published in machine-readable Wahlkreis form).

## Additional finer-granularity files available (not downloaded — not constituency level)

For each year the same portal also offers two deeper levels, should they ever be needed:
- "Wahlkreise und Gemeinden": `NeuLesen.asp?seite=LWINFOG<YEAR>`
  (1990: `landtagswahlen/Daten1990/L90_Ergebnisse_Gemeinden.xlsx`)
- "Wahlkreise, Gemeinden und Wahlbezirke": `NeuLesen.asp?seite=LWInfoG1<YEAR>`
  (1990: `landtagswahlen/Daten1990/L90_Ergebnisse_Wahlbezirke.xlsx`)
These contain the Wahlkreis breakdown too but at municipality / polling-district detail;
the Wahlkreisübersicht files above are the canonical constituency-level series.
