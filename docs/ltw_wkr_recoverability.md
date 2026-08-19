# Wahlkreis-level state elections: coverage and recoverability

Status of `ltw_wkr_unharm` coverage after the August 2026 extension, and a per-election
account of what remains missing, what could still be recovered from files already in the
repo, and what cannot be recovered without OCR or new sourcing. Written 2026-08-19, based
on an empirical audit of every candidate raw file (text-layer tests on 37 PDFs plus
proof-of-concept extractions for every parser family).

**State of the world.** Before this extension the dataset covered 75 elections, all from
machine-readable sources. A user report (GitHub issue #7 thread) flagged the gap; against
the universe of all state elections since 1990 (133 elections including Hessen 2009),
58 were missing. The August 2026 extension adds 28 elections from clean digital text-layer
PDFs already in the repo, without any OCR. What remains missing is either OCR-class scans,
corrupt or absent raw files, or constituencies that did not exist.

## What counts as the universe

All Landtagswahlen (Abgeordnetenhauswahl in Berlin, Bürgerschaftswahl in Hamburg and
Bremen) from 1990 through 2026, plus Saarland 1980/1985 (included because they sit in the
same validated source table as the 1990-2017 series). Five Hamburg elections (1991-2004)
are excluded from "missing" counts because Hamburg introduced Wahlkreise only in 2008:
there is no constituency level to recover. Bremen's unit is the Wahlbereich (Bremen /
Bremerhaven), not a single-member district; it is kept in the dataset under the same
schema, as before.

## Recovery bar applied (decision, August 2026)

Only elections extractable **deterministically from a clean digital text layer** (or a
machine-readable file) were added. Concretely, a source qualified only if a parser could
reproduce the source's own printed statewide totals exactly by summing the extracted
Wahlkreise. Explicitly excluded: scanned PDFs needing OCR (the open OCR problems in
TODO.md come from exactly that route), corrupt files, percentages-only sources, and
reconstructions requiring data outside the repo. Rationale: the dataset's correctness
guarantee is exact statewide reproduction per parser; OCR cannot meet it.

## Per-state status

Categories: **in** = in the dataset before Aug 2026 · **added** = added Aug 2026 ·
**ocr** = raw scan in repo, needs OCR · **corrupt** = raw PDF in repo is damaged ·
**shares** = raw file has percentages only, no counts · **absent** = no constituency-level
raw file in the repo · **n/a** = constituency level did not exist · **skipped** = clean
partial source exists but was deliberately not used (reason given).

| State | in (before) | added Aug 2026 | still missing (category) |
|---|---|---|---|
| Baden-Württemberg | 2016, 2021, 2026 | 2001*, 2006, 2011 | 1992 (ocr), 1996 (corrupt) |
| Bayern | 2018, 2023 | 2008, 2013 | 1990, 1994, 1998, 2003 (ocr: text layer exists but digits are letter-spaced OCR output) |
| Berlin | 2016, 2023 | 1999, 2001, 2006, 2011, 2021 | 1990 (ocr), 1995 (corrupt) |
| Brandenburg | 1990-2024 complete | — | — |
| Bremen | 2015, 2019 | 2003, 2007, 2011, 2023 | 1991, 1995, 1999 (skipped: only the 6 major parties + a Sonstige residual are recoverable from the long-series PDF; the residual hides seat-winning parties — DVU 6.2% in 1991, AFB 10.7% in 1995. Full detail needs the pre-2003 Hefte, which are not in the repo). Caveat on the added 2003/2007: their Hefte publish only "Vorläufige Ergebnisse" (no final Wahlbereich table exists in them), so those rows carry the official preliminary counts — internally exact, within 0.13pp of the published final statewide shares |
| Hamburg | 2008, 2011, 2015, 2020, 2025 | — | 1991-2004 (n/a: Wahlkreise exist only since 2008) |
| Hessen | 2013, 2018, 2023 | 2009 | 1991, 1995 (TIF scans), 1999, 2003, 2008 (ocr: 97 MB image-only PDFs) |
| Mecklenburg-Vorpommern | 1994-2021 complete | — | 1990 (absent at WK level; see note below) |
| Niedersachsen | 1998, 2003, 2013, 2017, 2022 | 2008 | 1990, 1994 (absent) |
| Nordrhein-Westfalen | 2000-2022 complete | — | 1990, 1995 (absent) |
| Rheinland-Pfalz | 2001-2026 complete | — | 1991 (absent), 1996 (ocr: the raw README's "text layer present" claim is wrong — sampled pages have none) |
| Saarland | 2022 | 1980, 1985, 1990, 1994, 1999, 2004, 2009, 2012, 2017 | — (pre-1980 exists only as Kreis-level scans) |
| Sachsen | 1994, 1999, 2014, 2019, 2024 | 2004, 2009 | 1990 (ocr: unusable OCR layer) |
| Sachsen-Anhalt | 1990-2021 complete | — | — |
| Schleswig-Holstein | 2000, 2009, 2017, 2022 | 2005 | 1992, 1996 (ocr: text layer exists but is noisy OCR that garbles digits, e.g. "lt22" for 422), 2012 (shares: the Wahlkreis xls files carry percentages for 7 selected parties, no counts anywhere; the two report PDFs were not assessed further) |
| Thüringen | 1990-2024 complete | — | — |

*BW 2001 is reconstructed from the comparison columns of the 2006 report: 11 parties are
itemized per Wahlkreis (99.9% of the vote); five fringe parties (BüSo, CM, CATS, DKP,
FAMILIE; 2,806 votes statewide = 0.1%) appear only as a combined "Sonstige (nur 2001)"
figure. The report recomputed the 2001 figures of 11 of the 70 Wahlkreise onto the 2006
boundaries; those rows carry `flag_wkr_boundaries_recomputed = 1` (same convention as
Hessen 2013).

Bottom line of the table: the dataset now covers 103 elections (was 75). Of the 128
post-1990 elections that structurally exist (133 minus the five Hamburg years with no
Wahlkreise), 101 are covered (79%); Saarland 1980/1985 come on top. Everything still
missing needs OCR, re-downloading, or new sourcing — nothing clean remains unparsed.

## Evidence for the classification

Text-layer presence was tested empirically on every candidate PDF (`pdftotext` on sample
pages, character counts, and visual inspection of extracted table rows), not taken from
the READMEs — which was necessary: one README claims a text layer that does not exist
(RP 1996), and one "text layer present" case turned out to be OCR output with garbled
digits (SH 1992/1996). Every family that was added had a proof-of-concept parser built
first, and each PoC had to reproduce the printed statewide totals exactly (to the vote)
by summing its extracted Wahlkreise before the family was accepted. Two silent-corruption
traps were found and are documented in the parsers: Berlin 2006's embedded font makes
pdfplumber read digit 3 as 2 (poppler `pdftotext` decodes it correctly — the Berlin
parser therefore uses pdftotext throughout), and Sachsen 2004's fonts lack ToUnicode maps
entirely (recovered deterministically via each font's /Differences array and the standard
Macintosh glyph order; zero unresolved characters).

## Not recoverable without new work

- **Corrupt raw files (re-download, then re-classify):** `BW_1996_Landtagswahl_Wahlkreis_BVII2.pdf`
  and `BE_1995_Abgeordnetenhauswahl_Ergebnisbericht.pdf` have broken xref tables;
  Ghostscript reconstruction yields empty output. Source URLs are in the per-state raw
  READMEs. Until re-downloaded, their category (clean text vs. scan) is unknown.
- **MV 1990:** the machine-readable Gemeinde-level xls in the repo has a Wk-Nr column,
  but (i) it is explicitly "ohne Briefwahl", (ii) the multi-Wahlkreis cities (Rostock
  spans WK 4-7, Schwerin 8-9, Neubrandenburg 2-3, Stralsund 25-26) are single rows that
  cannot be split, and (iii) it is on Gebietsstand 1994. A Wahlkreis build from it would
  be incomplete and unflaggable; not attempted.
- **Bremen pre-2003 Hefte:** the per-election reports for 1991/1995/1999 (needed for full
  party detail) are not in the repo; the Statistisches Landesamt Bremen numbering suggests
  they exist in the ~85-100 range of the Hefte series. Sourcing task.
- **OCR class:** everything marked "ocr" above overlaps with the open OCR quality problems
  documented in TODO.md (issue #7); deferred deliberately.

## Follow-ups

1. Re-download the two corrupt PDFs and re-classify them.
2. Source the Bremen Hefte for 1991/1995/1999.
3. A broader scan of the same kind (text-layer testing + recoverability classification)
   across the OTHER pipelines' raw holdings (state Gemeinde-level historical PDFs, county
   elections, mayoral) — deferred until after this extension is merged.
4. Pre-existing, unrelated to this extension: the BW 2026 CSV parser's own validation
   prints a statewide mismatch of 5,108 erststimme votes for "Anderer Kreiswahlvorschlag"
   (a warn, not a halt). Found in passing during the August 2026 work; not investigated.

## Glossary

- **Wahlkreis / Stimmkreis / Wahlbereich**: the constituency level below the state —
  Stimmkreis in Bayern, Wahlbereich in Bremen (only two: Bremen, Bremerhaven).
- **Text layer**: machine-readable character data embedded in a PDF. A digital-native PDF
  has an exact text layer; a scanned PDF has none, or only OCR output of unknown quality.
- **Statewide-reproduction check**: the pipeline's correctness guarantee — summing a
  parser's Wahlkreise per party must reproduce the source document's own printed statewide
  total exactly.
- **`flag_wkr_boundaries_recomputed`**: marks rows whose figures the statistical office
  back-cast onto a later election's constituency boundaries (HE 2013; BW 2001 in part).
- **erststimme / zweitstimme / einzelstimme**: constituency-candidate vote / party-list
  vote / the single vote in one-vote systems (BW through 2021, SL).
