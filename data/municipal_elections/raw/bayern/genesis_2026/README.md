# Bayern Gemeinderatswahl 2026 — GENESIS-Online Bayern flat files

Source: GENESIS-Online Bayern (statistikdaten.bayern.de), statistic **14431
"Gemeinderatswahlen"**, Stichtag **08.03.2026** (abschließende Ergebnisse,
published per press release PM160 of 17.06.2026: "Kommunalwahlen 2026:
Abschließende Ergebnisse der Gremienwahlen liegen auch auf Gemeindeebene vor").

Downloaded 27.07.2026 via the GENESIS web UI (Flat-File-CSV export, Latin-1,
`;`-separated). The anonymous-user size limit (~40k values) forced the two
48-Wahlvorschlag tables to be pulled in three 16-party batches; each file also
carries the Regierungsbezirk/Kreis/Bayern aggregate rows (2,203 units total,
2,099 Gemeinden — filter to 8-digit codes). NOTE: statistikdaten.bayern.de
blocks non-German IPs on the `/genesis` paths — a German VPN (or a registered
GENESIS login for the REST API) is required to re-pull.

| File(s) | GENESIS table | Content |
| --- | --- | --- |
| `by2026_gemeinderat_gewichtete_batch{1,2,3}.csv` | 14431-003r | Gewichtete Stimmen per Wahlvorschlag (48 + Insgesamt). Gewichtete Insgesamt == gültige Stimmzettel (verified: Bayern 6,311,338). |
| `by2026_gemeinderat_turnout.csv` | 14431-001r | Wahlberechtigte, Wähler, Wahlbeteiligung |
| `by2026_gemeinderat_stimmzettel.csv` | 14431-002r | Abgegebene / gültige / ungültige Stimmzettel |
| `by2026_gemeinderat_sitze_batch{1,2,3}.csv` | 14431-005r | Sitze per Wahlvorschlag (48 + Insgesamt) |

Party batches (PARTG2 codes): batch1 = AFD ASP AUD BBP BFB BP BSP BSW CBV CSU
CWU DACG DIEBASIS DIEFRANKEN DIEFREIHEIT DIELINKE · batch2 = KBW FAMILIE IWP DS
LD DU HP PARTEIFW MUENDIGEBUER PATRIOTEN LIGA STATTPARTEI TIERSCHUTZ GEMWAHLVOR
WAEHLERGRUPPEN WAEHLGRUPPE-OW · batch3 = SPD GRUENE FDP OEDP FREIEWAEHLER
PIRATEN REP DIEPARTEI VOLT MUT V-PARTEI FBU NPD DKP EAP VSBD. INSGESAMT rides
in batch1.

**Not pulled**: table 14431-004 (absolute "gültige Stimmen" per Wahlvorschlag,
i.e. the cumulative-vote `abs_` measure of the 1996-2020 series) — it pins five
Stichtage with no time selection for anonymous users, so a full pull needs a
registered GENESIS account (REST: `data/tablefile`, `name=14431-004`). Until
then the 2026 wave carries gewichtete Stimmen only (like the county series;
shares from gewichtete are near-identical to shares from absolute votes, as the
weighting divisor is ~constant within a Gemeinde).
