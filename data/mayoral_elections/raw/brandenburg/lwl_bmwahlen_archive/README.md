# Brandenburg: the Landeswahlleiter's old mayoral result pages, 2001–2017 (Internet Archive)

## Source

From 2008 until its move to the current portal (2018), the Landeswahlleiter Brandenburg published each Gemeinde's **latest** Bürgermeisterwahl. The pages were at `wahlen.brandenburg.de/bmwahlen/<AGS10>/h` (Hauptwahl) and `…/s` (Stichwahl). Both hauptamtliche and ehrenamtliche Bürgermeister were included. The live site has since overwritten or removed every page; the Internet Archive kept captures of them.

Each page is the Landeswahlleiter's MAIS form, the same form as the Gemeinden's result reports in `../lwl_ergebnismeldungen_2004_2013/`. It gives:
- Wahlberechtigte, Wähler, ungültige and gültige Stimmen;
- the Nein-Stimmen of a single-candidate ballot;
- the two thresholds of § 72 Abs. 2 BbgKWahlG;
- every Bewerber with Wahlvorschlagsträger, votes and share;
- the Wahlausschuss's finding: elected, the two runoff Bewerber, or the Gemeindevertretung to elect.

A page shows whatever election was the Gemeinde's latest when it was captured, so the captures reach back to elections of 2001.

## Files

- `cdx_bmwahlen.json` (tracked): the Internet Archive's CDX listing of every capture under `wahlen.brandenburg.de/bmwahlen/`, retrieved 2026-10-19.
- `pages/` (**gitignored**: the pages name every Bewerber): 984 captures of 2008–2017, 414 Gemeinden, fetched raw (`id_` mode) by `code/mayoral_elections/00_bb_lwl_archive_fetch.py`.
- `_capture_manifest.csv` (tracked): file, archive URL, capture time, size and sha256 of every page. The parser stops if a page and the manifest disagree.

## What GERDA takes, and why

`code/mayoral_elections/00_bb_lwl_archive_parse.py` reads every page, checks it, and decides election by election. It runs after `00_bb_afs_reports_parse.py`. The 984 captures give 737 distinct rounds: duplicate captures must agree; three pages carry no result.

| | elections |
|---|---|
| already held with counts (workbook, result reports, B VII 3-6): compared, left out | 218 |
| held **winner-only** by a B VII 3-6 report: replaced by the archived rounds | 411 |
| B VII 3-6 council record whose failed popular vote the archive holds (Pinnow 2008) | 1 |
| not in GERDA at all: added | 7 |

- **Replaced elections:** 2001–2003, 2008 and the ehrenamtliche of the Kommunalwahl day 2014. They gain their counts and every Bewerber.
  - Where the archive lacks one round of the election, that round is carried over from `bb_afs_parsed.csv` (18 elections). The missing round is either a runoff page overwritten before it was captured, or the result-less first round of a winner-only runoff cycle. The election then comes from one file and its rounds pair by `pair_key`.
  - The elected person must be the one the report names; otherwise the parser stops.
  - The B VII 3-6 rounds replaced (472) are listed in `../bb_lwl_archive_supersedes.csv`, which 01/01b drop.
- **Added elections:**
  - Zechin 27.01.2008;
  - Rosenau 25.05.2014, where the single candidate was rejected and the Gemeindevertretung elects;
  - Neu Zauche 14.09.2014;
  - Forst (Lausitz), Stichwahl 15.03.2015;
  - Mühlberg/Elbe, Stichwahl 17.04.2016;
  - Guben 26.06. and 17.07.2016;
  - Falkenberg/Elster 24.09.2017.

  The two runoffs come without their first round, which is not archived; they stay runoff-only cycles. The four hauptamtliche elections are also missing from the Landeswahlleiter's workbook; a question to the Landeswahlleiter is drafted.
- **Lychen 2002:** B VII 3-6 files one Hauptwahl on 05.05.2002. The archive shows a Hauptwahl on 21.04. and the runoff on 05.05.; GERDA now has both rounds.
- **Office** (hauptamtlich / ehrenamtlich): from the B VII 3-6 report an election replaces; for an added election, from the Gemeinde's previous election in GERDA.

## Checks (as for the result reports)

- **Per round:**
  - Sunday;
  - gültige + ungültige = Wähler;
  - Wähler ≤ Wahlberechtigte;
  - votes (+ Nein on a single-candidate ballot) = gültige;
  - printed percentages and thresholds;
  - the finding against the votes, incl. the 15 % quorum;
  - the AGS is a Gemeinde of that name in the election year.
- **Per election:**
  - the runoff 2–5 weeks after the Hauptwahl;
  - the runoff Bewerber are the two the Hauptwahl admitted;
  - similar electorates in both rounds;
  - an added election must not exist under another code.
- **Pins.** Every understood source defect is pinned in the parser and reported on every run; a pin that no longer fires stops the run.
  - Five pages carry the site's wrong date. Their counts equal a GERDA round on another date, so they are left out: Bad Freienwalde (Oder), Seelow and Wriezen 2009 shown as 2008, and two 2017 runoffs shown under their Hauptwahl's date.
  - One early capture (Falkensee 2007) lacks a Bewerber. The later capture reconciles and is used.
  - Belzig 2008 repeats the Gemeinde's misprinted electorate (GERDA keeps the corrected 9 779).
  - Five Bewerber names are misprinted. They are corrected to the spelling of the B VII 3-6 report or of the other round, and the elected persons are public.
  - The 2014 pages prefix given names with "Herr"/"Frau". These salutations are removed.
  - Hand-computed thresholds are off by one, and percentages are misprinted. No outcome depends on them.
  - Four elections print electorates 3–10 % apart between the rounds. The winner clears the quorum on either figure.

## Privacy

Every Brandenburg loser is restricted (user decision 2026-10-19). Elected persons are named in the public files; every other Bewerber (387 more in the restricted twin) keeps votes, shares, rank and Wahlvorschlagsträger only, and an own-name label of theirs is the plain list type (`code/shared/bb_own_name_label.R`). The pages and the parsed output therefore stay out of git.

## Re-run

`00_bb_lwl_parse.py` → `00_bb_lwl_scans_parse.py` → `00_bb_afs_reports_parse.py` → `00_bb_lwl_archive_parse.py` → 01 / 01b / 02 / `04a` / 04 / 03 / Landrat combine. `99_audit.R` §35 pins the result.
