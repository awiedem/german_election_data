# Brandenburg Landrat raw pages

Result pages of the Landeswahlleiter Brandenburg (LWL), parsed by `parse_bb()` in
[`code/landrat_elections/01_landrat_combine.R`](../../../../code/landrat_elections/01_landrat_combine.R).
Every file is kept verbatim; none is ever overwritten.

## Naming

`BB_ergebnis-landratswahl-<kreis>[_<date>[_r<retrieved>]].html` (Hauptwahl) and
`BB_ergebnis-stichwahl-landrat[-in]-<kreis>[_<date>[_r<retrieved>]].html` (Stichwahl).
`<date>` is the election date from the page heading. The portal serves one URL per
Kreis and overwrites it every cycle, so [`00_bb_scrape.R`](../../../../code/landrat_elections/00_bb_scrape.R)
saves a later cycle next to the cached one, and `parse_bb` keeps one file per
(Kreis, round, date): undated < dated < `_r<retrieved>`. Undated files are the first
scrape (June 2026). Files not matching `BB_ergebnis*` are not parsed.

## Later cycles saved by `00_bb_scrape.R` (September 2026)

Pages the scraper found on the live portal after the first scrape, each a new
election for a Kreis already on disk (Ostprignitz-Ruppin's undated files are its
2018 cycle) or a runoff the June index did not yet list. All are final results
("Endgültiges Ergebnis"), saved verbatim.

| File | Source | Source URL | Captured | sha256 (first 12) |
|---|---|---|---|---|
| `BB_ergebnis-stichwahl-landrat-barnim.html` | current portal (live) | [wahlen.brandenburg.de/wahlen/de/kommunalwahlen/ergebnisse/landraetewahlen/ergebnis-stichwahl-landrat-barnim/](https://wahlen.brandenburg.de/wahlen/de/kommunalwahlen/ergebnisse/landraetewahlen/ergebnis-stichwahl-landrat-barnim/) | 2026-09-25 | `e2b2ea8115da` |
| `BB_ergebnis-landratswahl-ostprignitz-ruppin_2026-06-07.html` | current portal (live) | [wahlen.brandenburg.de/wahlen/de/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-ostprignitz-ruppin/](https://wahlen.brandenburg.de/wahlen/de/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-ostprignitz-ruppin/) | 2026-09-25 | `f76148599396` |
| `BB_ergebnis-stichwahl-landrat-ostprignitz-ruppin_2026-06-28.html` | current portal (live) | [wahlen.brandenburg.de/wahlen/de/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-ostprignitz-ruppin/](https://wahlen.brandenburg.de/wahlen/de/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-ostprignitz-ruppin/) | 2026-09-25 | `435ab34ea4cc` |

## The 2010-2018 pages (added September 2026)

Direct election of Landräte began on 10 January 2010. By the time the portal was
first scraped, it had overwritten every cycle before the current one, except
Ostprignitz-Ruppin 2018. Those 34 rounds (19 cycles) come from the Internet
Archive's Wayback Machine, fetched raw (`id_` mode) and saved without changes:

- **Current portal (2018-), same per-Kreis URLs**, captured 2019: every cycle from
  2013 to 2018 that the portal still showed then. Until each Kreis voted again, the
  portal showed that Kreis's previous cycle; the earliest capture of each is kept.
  (Later captures differ only in layout: those of 2022-2025 lack the Prozent
  column, and some add or drop the "Gewählt:" footer. The counts are identical.)
- **The LWL's earlier site** (`www.wahlen.brandenburg.de/cms/detail.php/bb1.c.<id>.de`,
  "MAIS V1.0", 2010-2017): the 2010 cycles, which the current portal never carried,
  the Havelland Stichwahl of 24.04.2016, which it listed but which was never
  captured, and the Potsdam-Mittelmark Hauptwahl of 25.09.2016 (see below). Same
  Merkmal/Anzahl/Prozent table without the portal's CSS class, headed "Endgültiges
  Ergebnis der Landratswahl am ... im Landkreis ..." (Uckermark 28.02.2010:
  "Endgültige Ergebnisse der Landratswahl im Landkreis Uckermark am ...").

All are **final** results ("Endgültiges Ergebnis"). Where a cycle exists in both
places, the two were compared cell by cell. All agree except in two cases, and
for each cycle the complete one is kept:

- Oberhavel Hauptwahl 22.02.2015: the earlier-site original lists "Dr. Gerhard
  Kalinka (GRÜNE/B 90)" (a Teltow-Fläming 2013 candidate) with the AfD candidate's
  6,393 votes and leaves "Dr. Dietmar Buchberger (AfD)" without a count; the portal
  re-publication corrects this. **Portal copy kept.**
- Potsdam-Mittelmark Hauptwahl 25.09.2016: the portal copy prints the "Ungültige
  Stimmen" row (1,301) with an empty label; the 2016 original has it. **Original kept.**

The 2010 per-Kreis pages (captured 2012) also match the LWL's combined 2010 pages
"Endgültige Ergebnisse der Landrätewahlen am 10. Januar 2010" / "... Landrätestich-
wahlen am 24. Januar 2010" (captured March and September 2010) in every cell.

The four **press releases** below are the LWL's own summaries of the 2010 rounds,
still online (`wahlen.brandenburg.de/wahlen/de/pressemitteilungen/`). They carry
**preliminary** results (e.g. Spree-Neiße Stichwahl: Altekrüger 14,015 / Friese
14,805, final 14,036 / 14,784) and are kept as a cross-check only; they are not parsed.
`_master_index_wayback_2019-08-23.html` is the portal's results index as captured
on that day: it lists, per Kreis, the dates of the Hauptwahl, the Stichwahl, and
the election "durch Kreistag" for every cycle then current.

Pages that say who the Kreistag elected after a failed runoff do so in a note
below the table; see `flag_elected_by_council` in
[`../../final/README.md`](../../final/README.md).

| File | Source | Source URL | Captured | sha256 (first 12) |
|---|---|---|---|---|
| `BB_ergebnis-landratswahl-teltow-flaeming_2013-03-24.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-teltow-flaeming/](https://web.archive.org/web/20190923033317/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-teltow-flaeming/) | 2019-09-23 | `5cfdcadb61f0` |
| `BB_ergebnis-stichwahl-landrat-teltow-flaeming_2013-04-14.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-teltow-flaeming/](https://web.archive.org/web/20190919212031/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-teltow-flaeming/) | 2019-09-19 | `ed4d66346cf3` |
| `BB_ergebnis-landratswahl-maerkisch-oderland_2013-09-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-maerkisch-oderland/](https://web.archive.org/web/20190921073144/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-maerkisch-oderland/) | 2019-09-21 | `6972923ba186` |
| `BB_ergebnis-stichwahl-landrat-maerkisch-oderland_2013-10-06.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-maerkisch-oderland/](https://web.archive.org/web/20190921141614/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-maerkisch-oderland/) | 2019-09-21 | `94dd16b8cb40` |
| `BB_ergebnis-landratswahl-prignitz_2014-05-11.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-prignitz/](https://web.archive.org/web/20190919212734/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-prignitz/) | 2019-09-19 | `e496b01adf72` |
| `BB_ergebnis-landratswahl-oberhavel_2015-02-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-oberhavel/](https://web.archive.org/web/20190921134026/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-oberhavel/) | 2019-09-21 | `02f0c03be601` |
| `BB_ergebnis-stichwahl-landrat-oberhavel_2015-03-08.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-oberhavel/](https://web.archive.org/web/20190923033531/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-oberhavel/) | 2019-09-23 | `7ee264e10318` |
| `BB_ergebnis-landratswahl-dahme-spreewald_2015-10-11.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-dahme-spreewald/](https://web.archive.org/web/20190921134218/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-dahme-spreewald/) | 2019-09-21 | `b274ec6573fc` |
| `BB_ergebnis-landratswahl-havelland_2016-04-10.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-havelland/](https://web.archive.org/web/20190919214122/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-havelland/) | 2019-09-19 | `e85ba433e123` |
| `BB_ergebnis-stichwahl-landrat-potsdam-mittelmark_2016-10-09.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-potsdam-mittelmark/](https://web.archive.org/web/20190923031427/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-potsdam-mittelmark/) | 2019-09-23 | `71557fa2a777` |
| `BB_ergebnis-landratswahl-oder-spree_2016-11-27.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-oder-spree/](https://web.archive.org/web/20190921133857/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-oder-spree/) | 2019-09-21 | `d79ae324988d` |
| `BB_ergebnis-stichwahl-landrat-oder-spree_2016-12-11.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-oder-spree/](https://web.archive.org/web/20190921072853/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-oder-spree/) | 2019-09-21 | `5022e22ac3f8` |
| `BB_ergebnis-landratswahl-barnim_2018-04-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-barnim/](https://web.archive.org/web/20190921194827/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-barnim/) | 2019-09-21 | `7556bfe761d4` |
| `BB_ergebnis-stichwahl-landrat-barnim_2018-05-06.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-barnim/](https://web.archive.org/web/20190823004132/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-barnim/) | 2019-08-23 | `5bddba40ff78` |
| `BB_ergebnis-landratswahl-elbe-elster_2018-04-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-elbe-elster/](https://web.archive.org/web/20190923031608/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-elbe-elster/) | 2019-09-23 | `79a34897e72a` |
| `BB_ergebnis-landratswahl-oberspreewald-lausitz_2018-04-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-oberspreewald-lausitz/](https://web.archive.org/web/20190921141036/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-oberspreewald-lausitz/) | 2019-09-21 | `1350d1d1b2f8` |
| `BB_ergebnis-landratswahl-spree-neisse_2018-04-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-spree-neisse/](https://web.archive.org/web/20190919212508/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-spree-neisse/) | 2019-09-19 | `8950b16b6592` |
| `BB_ergebnis-stichwahl-landrat-spree-neisse_2018-05-06.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-spree-neisse/](https://web.archive.org/web/20190921073604/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-spree-neisse/) | 2019-09-21 | `40ceadca7620` |
| `BB_ergebnis-landratswahl-uckermark_2018-04-22.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-uckermark/](https://web.archive.org/web/20190921194533/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-landratswahl-uckermark/) | 2019-09-21 | `a711721a6426` |
| `BB_ergebnis-stichwahl-landrat-uckermark_2018-05-06.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-uckermark/](https://web.archive.org/web/20190921073139/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/ergebnis-stichwahl-landrat-uckermark/) | 2019-09-21 | `38793b7396b6` |
| `BB_ergebnis-landratswahl-potsdam-mittelmark_2016-09-25.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.461601.de](https://web.archive.org/web/20161012040205/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.461601.de) | 2016-10-12 | `3ab83cf6ed02` |
| `BB_ergebnis-stichwahl-landrat-havelland_2016-04-24.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.443184.de](https://web.archive.org/web/20160508055244/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.443184.de) | 2016-05-08 | `9b8154b120c5` |
| `BB_ergebnis-landratswahl-barnim_2010-01-10.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.191074.de](https://web.archive.org/web/20120214225450/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.191074.de) | 2012-02-14 | `754ae9bd34a8` |
| `BB_ergebnis-landratswahl-elbe-elster_2010-01-10.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226114.de](https://web.archive.org/web/20120214230314/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226114.de) | 2012-02-14 | `a72136445103` |
| `BB_ergebnis-landratswahl-oberspreewald-lausitz_2010-01-10.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226115.de](https://web.archive.org/web/20120214225727/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226115.de) | 2012-02-14 | `1c0bdad7aca1` |
| `BB_ergebnis-landratswahl-ostprignitz-ruppin_2010-01-10.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226116.de](https://web.archive.org/web/20120214230401/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226116.de) | 2012-02-14 | `05215d644e8b` |
| `BB_ergebnis-landratswahl-spree-neisse_2010-01-10.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226117.de](https://web.archive.org/web/20120214230319/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226117.de) | 2012-02-14 | `c5d477c1f3d0` |
| `BB_ergebnis-stichwahl-landrat-barnim_2010-01-24.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.192134.de](https://web.archive.org/web/20120214225713/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.192134.de) | 2012-02-14 | `3f2ef1e0d16d` |
| `BB_ergebnis-stichwahl-landrat-elbe-elster_2010-01-24.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226120.de](https://web.archive.org/web/20120214230324/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226120.de) | 2012-02-14 | `2485becfb1a3` |
| `BB_ergebnis-stichwahl-landrat-oberspreewald-lausitz_2010-01-24.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226122.de](https://web.archive.org/web/20120214230406/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226122.de) | 2012-02-14 | `bd8796ac14f5` |
| `BB_ergebnis-stichwahl-landrat-ostprignitz-ruppin_2010-01-24.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226124.de](https://web.archive.org/web/20120214225500/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226124.de) | 2012-02-14 | `6b8671368f51` |
| `BB_ergebnis-stichwahl-landrat-spree-neisse_2010-01-24.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.226125.de](https://web.archive.org/web/20120214225505/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.226125.de) | 2012-02-14 | `9aadfb792f91` |
| `BB_ergebnis-landratswahl-uckermark_2010-02-28.html` | earlier LWL site | [www.wahlen.brandenburg.de/sixcms/detail.php/bb1.c.202348.de](https://web.archive.org/web/20100305083102/http://www.wahlen.brandenburg.de:80/sixcms/detail.php/bb1.c.202348.de) | 2010-03-05 | `a4b770fcf892` |
| `BB_ergebnis-stichwahl-landrat-uckermark_2010-03-14.html` | earlier LWL site | [www.wahlen.brandenburg.de/cms/detail.php/bb1.c.204584.de](https://web.archive.org/web/20120213201357/http://www.wahlen.brandenburg.de:80/cms/detail.php/bb1.c.204584.de) | 2012-02-13 | `4920e6e68f3c` |
| `_master_index_wayback_2019-08-23.html` | current portal | [wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/](https://web.archive.org/web/20190823003532/https://wahlen.brandenburg.de/wahlen/de/start/kommunalwahlen/landraetewahlen/ergebnisse-landratswahlen/) | 2019-08-23 | `0f56e371cdf5` |
| `BB_pressemitteilung-auswertung-landraetewahlen_2010-01-11.html` | LWL press release (live) | [wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~11-01-2010-auswertung-landraetewahlen](https://wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~11-01-2010-auswertung-landraetewahlen) | 2026-09-26, live | `3798287a46f4` |
| `BB_pressemitteilung-einzelne-landraetewahlen_2010-01-25.html` | LWL press release (live) | [wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~25-01-2010-einzelne-landraetewahlen](https://wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~25-01-2010-einzelne-landraetewahlen) | 2026-09-26, live | `441445250f24` |
| `BB_pressemitteilung-presseinformation_2010-03-01.html` | LWL press release (live) | [wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~01-03-2010-presseinformation](https://wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~01-03-2010-presseinformation) | 2026-09-26, live | `a3830cc8db27` |
| `BB_pressemitteilung-einzelne-kommunalwahlen_2010-03-15.html` | LWL press release (live) | [wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~15-03-2010-einzelne-kommunalwahlen](https://wahlen.brandenburg.de/wahlen/de/pressemitteilungen/detail/~15-03-2010-einzelne-kommunalwahlen) | 2026-09-26, live | `4b7326bde268` |
