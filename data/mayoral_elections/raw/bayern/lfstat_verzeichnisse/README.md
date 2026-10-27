# Bayern Bürgermeisterverzeichnisse 1978–2014 and LfStat candidate reports

Received 2026-10-07 (the 2014 register on 2026-10-12) from the library of the Bayerisches
Landesamt für Statistik (LfStat). The library scanned the registers on request and uploaded
them to the Statistische Bibliothek, free of charge. It also pointed to two report series,
which are stored here as well.

**Status (16.10.2026): used internally only; nothing from these sources is published.** The
LfStat declined permission to redistribute the names, birth years and professions of the
elected officials from its registers and from the Statistische Berichte B VII 3-2 and 3-7
(e-mail of its data protection officer, 16.10.2026). It cites § 9 Abs. 3 of its AGB, which
bars redistributing *Verzeichnisse* in electronic form, and argues that the
Gemeinde- und Landkreiswahlordnung reserves publication of the personal data to the
Landesamt, so other databases lack a legal basis. The reply and the AGB are in
`correspondence/` (gitignored).

Consequences in GERDA:

- the public `mayoral_candidates` and `landrat_candidates` carry no Bayern name, birth year,
  profession, name origin or migration background before 2026 (`anonymise_restricted_names()`
  in `01b_mayoral_candidates.R`, and the Landrat combine); the restricted twins keep them;
- the winners' gender, derived from the withheld given name, is published (user decision
  2026-10-16; `04_candidate_characteristics.R` copies it from the restricted twin);
- the Bayern winners of 2026 stay named from the LfStat *Mandatsträger* file, which the
  request did not cover;
- `03_mayor_panel.R` reads the register names from the restricted twin to correct the Bayern
  person keys and publishes only the pseudonymous `person_id`;
- the parsed register files, the parser's name pins and the audit fixtures that name register
  entries are gitignored (`*_restricted.csv`, `by_verzeichnis_*.csv`).

The PDFs, `_text_layer/` and `_ocr_cache/` are gitignored (about 390 MB; they also contain full
birth dates and losing candidates' names). This README and `_source_manifest.csv` (URLs, bytes,
sha256) are tracked; `_page_map.csv` is not (one OCR heading snippet carries a name). To re-download, use the `download_url` column in the
manifest. `README_restricted.md` (gitignored) holds the full internal notes with examples.

Run order (OCR is cached, so re-runs are fast):

```
python3 code/mayoral_elections/00_by_verzeichnis_ocr.py      # Tesseract, 300 DPI, word TSV per page
python3 code/mayoral_elections/00_by_verzeichnis_parse.py    # entries → match → persons → fill rows
Rscript code/mayoral_elections/01b_mayoral_candidates.R      # Bayern winner names (restricted twin only)
python3 code/mayoral_elections/04a_build_gender_lookup.py
Rscript code/mayoral_elections/04_candidate_characteristics.R
Rscript code/mayoral_elections/03_mayor_panel.R              # also writes the restricted person table
python3 code/mayoral_elections/00c_by_register_review.py    # local checkpoint 417 evidence, if its restricted bundle is present
Rscript code/landrat_elections/01_landrat_combine.R
```

The parser reads its name pins from `name_pins_restricted.csv` in this folder (gitignored); it
stops if the file is missing.

Outputs of the parser (in `data/mayoral_elections/raw/bayern/`), all gitignored:

| File | Content |
|---|---|
| `by_verzeichnis_elections.csv` | one row per workbook round whose winner the registers name: name, birth year, profession, `fill_method` (`register_entry` = the round an entry reports; `register_person_key` = another round under the same (ags, first Amtsantritt) key) |
| `by_verzeichnis_persons.csv` | one row per (ags, first Amtsantritt): canonical name, birth year, profession, volumes, conflict flags |
| `by_verzeichnis_persons_restricted.csv` | as above + exact birth date (1978/1984) + birthplace (1978) |
| `by_verzeichnis_entries_restricted.csv` | every parsed entry with match status and score |

`03_mayor_panel.R` re-emits the restricted person table with GERDA `person_id` attached as
`data/mayoral_elections/final_restricted/by_register_persons_restricted.{csv,rds}`, which is
the file de_housing should read.

Parse results (entries matched to a workbook round; 2026-10-07, after the audit fixes): 1978
92.6 %, 1984 94.6 %, 1990 91.3 %, 2002 98.9 %, 2008 99.0 %, OB/Landrat reports 2008 and 2014
100 %, 2014 register 98.4 %. With the 2014 register: 6,368 persons; 17,882 pre-2026 Bayern
mayoral winner rows and 644 Landrat winner rows named in the restricted twins.

**Verification (2026-10-07):**

- *Visual sample, 67 entries:* no wrong person, number or date. 55 entries are exact. 7 have
  OCR typos, mostly in Beruf and corrected since; 2 have a missing share; 2 have a truncated
  1978 birthplace (fixed); 1 surname is lost (now withheld).
- *Candidate lists, 30 sampled:* all 30 Wahlvorschlag assignments are correct.
- *2026 Mandatsträger:* surname agrees for 161/162 persons, birth year for 161/162 (2014
  register: 404/407 and 405/406).
- *de_housing's independent collection:* surname agrees for 916/923; birth year falls inside
  its interval for 658/662; exact dates agree 95/98. The misses are register-vs-source
  disagreements and one OCR error in a 1978 birth year, which is now withheld because the
  two editions disagree.
- *Shares:* the printed share equals the workbook's in more than 99 % of matches.

**Guards, all added after an independent code review:**

- one entry per (edition, round);
- a cross-Kreis match needs a near-exact name;
- names fill only rounds that seated someone, i.e. no superseded round and no Hauptwahl later
  lost in a runoff;
- the winner row's list must equal the workbook's winning list (Sulzdorf a.d.L. 2002, a runoff
  tie, is skipped);
- no propagated name before the printed "im Amt seit" (38 withheld) or onto a minor;
- a name needs a given name, and office or profession words never count as a name;
- the 1978 name-order swap touches 1978 entries only.

**Source defect found: the workbook's first-Amtsantritt is not always the winner's.** 151
rows (100 Gemeinden) carry a date long after their own election. Example: Hohenthann's 2002
and 2008 rounds read 2014-05-01, the next mayor's start. Since `03_mayor_panel.R` keys Bayern
persons on (ags, Amtsantritt), those rows merge two mayors into one `person_id`. The parser
never passes a name THROUGH such a key; a register entry that reports the round itself still
names it. The registers also show keys under which two different people sit (split in the
panel since 2026-10-07): Augsburg OB 1990-05-01; Ansbach and Main-Spessart Landrat
1972-07-01; Nördlingen 1982; Starnberg, Schongau and Rosenheim 1996. Those keys fill only the
rounds an entry reports directly.

Purpose: the de_housing age-limit eligibility audit
(`de_housing/feedback/neweps_revision/issue_N03_selection/eligibility_audit/`) lacks birth
data for many Bavarian mayors. This folder is the shared location that de_housing reads from.

## Files

| File | What it is | Text layer |
|---|---|---|
| `BY_Verzeichnis_1978_Heft375.pdf` | Register after the 5.3.1978 Kommunalwahl (359 pp.) | typewritten; OCR usable with errors |
| `BY_Verzeichnis_1984_Heft406.pdf` | Register after 18.3.1984 (204 pp.) | **badly garbled**: re-OCR or read visually |
| `BY_Verzeichnis_1990_Heft463.pdf` | Register after 18.3.1990 (207 pp.) | usable |
| `BY_Verzeichnis_1996_Heft502.pdf` | Should be the register after 10.3.1996; **see the Heft 502 warning below** | usable |
| `BY_Verzeichnis_2002_B73516.pdf` | Register after 3.3.2002 (204 pp.) | usable; drops some birth years the image shows |
| `BY_Verzeichnis_2008.pdf` | Register after 2.3.2008 (202 pp.) | usable |
| `BY_Verzeichnis_2014_B73516.pdf` | Register after 16.3.2014, **Stand 1.5.2014** (203 pp.; loose-leaf publication with replacement sheets, per preface p. 4). Scanned by the library on request, uploaded 12.10.2026 (`BYMonografie_mods_00002156`, file `II_1000_14_2014.pdf`) | scanner OCR layer; re-OCR'd like the others |
| `B7320C_{2006,2008,2014,2020,2026}_Bewerber.pdf` | Stat. Berichte B VII 3-2 "Wahlvorschläge, Bewerber" | 2008 is `(cid:NN)` mojibake, 2020 and most of 2006 are image-only, 2014 is usable, 2026 is born-digital |
| `B7370C_{2008,2014,2026}_OB_Landraete.pdf` | Stat. Berichte B VII 3-7: Oberbürgermeister of the kreisfreie Städte and Landräte | 2008/2014 image-only, 2026 born-digital |
| `_text_layer/*.json` | the six registers' embedded text, one string per PDF page (pdfplumber) | gitignored |
| `_page_map.csv` | per page: section, Landkreis hint, `Einwohner am` date → `register_edition`, Ergänzung no./date | derived heuristically from the text layer; unreliable for 1984; gitignored |
| `name_pins_restricted.csv`, `audit_fixtures_restricted.csv` | the parser's name pins and the `99_audit.R` fixtures that name register entries | gitignored |
| `correspondence/` | the LfStat refusal of 16.10.2026 and its AGB (Stand 01.04.2021) | gitignored |

## The 2014 register (added 2026-10-12)

The preface (PDF p. 4) gives **1 May 2014** as the reference date and explicitly describes a
loose-leaf publication with roughly semiannual replacement sheets. No later replacement date
was found on the 25 entries reviewed at checkpoint 417; the edition label alone does not
establish the date of every page. The register also lists mayors elected off-cycle in
2010–2013. It is volume `2014r` in `00_by_verzeichnis_parse.py` (`2014` is the OB/Landrat
report B7370C).

- **Parse:** 2,126 entries, 2,092 matched to their workbook round (98.4 %; 2008: 99.0 %;
  checkpoint 417 corrections included). The scan's OCR sometimes drops the word "Landkreis"
  from a heading ("177 Erding"), which glued the Kreis name onto its first Gemeinde; in the
  Gemeinde section a short "code + name" line is read as a heading when the name is that
  Kreis's own (2014 only; a looser first version also swallowed the first line of 30
  OB/Landrat entries and one Gemeinde). Its footnote line ("# Stichwahl.- * Berufsm.Bgrm.- …")
  is skipped.
- **A final "l" read as "!" or "|"** was cut off by the token clean-up in every volume; it is
  now read as "l". That repaired 21 surnames across the volumes.
- **Ranked below every earlier register** for spellings, so it only names people no earlier
  volume names: its OCR is a little noisier and it drops the "sen."/"jun." earlier volumes
  print. Ranked above 2008 it had changed 147 existing names. Now no existing surname changes
  except one Vilshofen spelling, where the 2014 register and the 2026 Mandatsträger agree
  against the 2020 candidate list. Five 2014 names are pinned after the 2026 Mandatsträger (an
  OCR "rn" read as "m", swapped name fields, an initial for a given name, two double or
  particle surnames split wrongly) and one entry is withheld: Pforzen 2014 prints the 2008
  mayor with the 2014 result, while the 2026 file, the workbook (Amtsantritt 1.5.2014) and the
  municipality name another person from 2014. Academic titles printed after the given name
  ("Dr.jur", "Dr.rer.soc") now go to the title in every volume.
- **Visual check:** 30 random 2014 entries against the scan (independent reviewer): no wrong
  unit, date, birth year, given name or profession; one surname OCR slip, which the 2008
  spelling overrides. Shares and Wahlvorschlag text are sometimes garbled (4 shares, 5 lists)
  but are used only to match, and all 30 matched the right round.
- **Result:** pre-2026 Bayern mayoral winners with a name 16,714 → 17,882; with a birth year
  16,440 → 17,711 (restricted twin). Persons 5,672 → 6,368. Nothing outside Bayern changed.
- **Panel:** the new names show 8 more source keys holding two people, all checked (Freystadt,
  Buttenheim, Obernzenn, Garching a.d.Alz, Pforzen; Duggendorf's 2002 mayor now joins his own
  return in 2014). Three first-pass splits were wrong and are prevented in
  `03_mayor_panel.R`'s `same_person()`: swapped name fields, a short form of a given name
  (same surname, birth year and initial), and an OCR "rn" read as "m". Since the afternoon of
  2026-10-12 ids come from `metadata/person_id_registry.csv` (seeded from the published panel),
  so a later split no longer shifts other ids; the restricted register table follows the
  source key through splits (surname decides) and key merges to the stable id.

The checkpoint 417 source review is integrated locally by
`code/mayoral_elections/00c_by_register_review.py`. It records 25 visually checked entries,
five unresolved birth conflicts and a disputed earlier service link in a separate restricted
evidence table, and checks its name anchors against the restricted candidates twin. The
parser repairs Marktschorgast's missing code and share and Petting's missing share; names,
birth years and person IDs do not change.

## What a register entry contains

One entry per Gemeinde (Landkreise and kreisfreie Städte have their own sections). The
Gemeinde AGS is `09` + Landkreis code (in the "Landkreis 774 Günzburg" headings) + three-digit
Gde. Schl.-Nr. (printed from 1984 on; in 1978 match by name within the Landkreis).

| Field | 1978 | 1984 | 1990 | 2002 / 2008 / 2014 |
|---|---|---|---|---|
| Name | "First Last" | "Last First" | "Last First" | "Last First" |
| Birth | **full date + birthplace** | **full date** | year | year |
| Beruf | before first election | before first election | before first election | current (often "Erster Bürgermeister") |
| Wahlvorschlag, Stimmenanteil %, Wahltag | yes | yes | yes | yes (`#` = Stichwahl) |
| First Amtsantritt ("im Amt seit") | no | yes | yes | yes |
| `*` berufsmäßig (salaried), `**` Oberbürgermeister | yes | yes | yes | yes |
| VG membership | "VG" flag | VGem. Schl. | VGem. Schl. | VGem. Nr. |

## Caveats

1. **Earlier volumes contain replacement sheets from later in the term.** These scans are the
   library's loose-leaf working copies, with Ergänzungslieferungen filed in. Pages print
   `Nr. <Ergänzung> <MM/YY>` in the footer. The last supplement in each binder: 1978 has
   paste-over slips to about 1983; 1984 Nr. 12 (09/1989); 1990 Nr. 11 (12/1995); 2002 Nr. 11
   (07/2007); 2008 Nr. 9 (Stand 1.1.2013). A mayor who was elected and replaced within one
   term can be absent from both surviving snapshots. For the 2014 register, use the preface
   and page-specific evidence described above.
2. **Heft 502 (1996) is mostly not the 1996 register.** Of the register pages, only 19 carry
   `Einwohner am 31.12.1994` (the 1996 edition). About 102 are 2002-register pages
   (31.12.2000) and about 52 are 2008-register pages (31.12.2006), filed into the same binder.
   Use `_page_map.csv` `register_edition` to tell them apart. **Do not read Heft 502 as a 1996
   snapshot.** The library has been asked (reply sent 2026-10-07) whether an unaltered copy
   exists.
3. **The full 2014 Gemeinde register was supplied on 12 October 2026.** No full 2020 Gemeinde
   register is held here; B7320C covers candidates in Gemeinden over 10 000 inhabitants and
   Landrat races.
4. B7320C lists **every** candidate, including losers, with name, Beruf and Wahlvorschlag, and
   the outgoing incumbent's previous vote share. It covers only Gemeinden over 10 000
   inhabitants (and Landrat races). It has no birth year.
5. OCR errors are frequent in names and digits: typewritten umlauts ("oe" for "ö"), "rn" read
   as "m", `19%` for `1996`, dropped birth years. **Never take a number from the text layer
   without the anchor check below**, and read the image when the anchor fails.
6. **Licence.** The imprint (2008 volume) allows reuse with a source citation but requires
   prior permission for *Weitergabe an Dritte* and *Weiterverbreitung über elektronische
   Systeme*; § 9 Abs. 3 of the LfStat AGB bars electronic redistribution of *Verzeichnisse*
   altogether. Permission for the names, birth years and professions of elected officials was
   requested on 2026-10-07 and **declined on 2026-10-16**. Use for internal scientific
   analyses only.

## Linking to GERDA (validated 2026-10-07)

Every entry repeats the Wahltag and Stimmenanteil of the winner's decisive round. These
matched `mayoral_unharm` exactly (to 0.1 pp) in every spot check, so the join key is
`(ags, election_date, round(100 * winner_voteshare, 1))`. That puts the name and birth data on
the row whose `person_id` (`person_id_method = first_amtsantritt`) already exists. The "im Amt
seit" date is a second, independent check on the person link. Fixtures (Landkreis Günzburg,
read from the page images: 1978 p. 276–277, 1984 p. 175–176, 1990 p. 175–176, 2002
p. 178–179, 2008 p. 176–177) are in `audit_fixtures_restricted.csv` and
`README_restricted.md`.

## Coverage (computed 2026-10-07 from `mayor_panel.rds`)

A person counts as covered if they were in office on a volume's last-supplement date
(1984-12-31, 1989-09-30, 1995-12-31, 2007-07-01, 2012-12-31).

- GERDA, Bayern persons by first election: 1978–89: 94 %; 1990–2001: 91 %; 2002–13: 97 %;
  before 1978: 17 %; 2014–20: 0 % from these volumes. The 2014–20 cohort is reachable only via
  the 2026 Mandatsträger file (57 %) and B7320C for the large Gemeinden.
- **de_housing age audit (checkpoint 409):** 293 of the 308 Bavarian people without a usable
  birth interval were in office at one of these dates. 109 of them fall in the 1978/1984
  volumes, which print the **exact birth date**, the precision needed for people born in a
  cutoff year. Another 38 people who now have only a year interval could be upgraded to an
  exact date from the same volumes. The 15 people not covered are mostly mayors first elected
  2014–2020.

## Open items

0. **de_housing consumers — NOT yet updated; left for a separate de_housing session (user
   decision 2026-10-07).** Do not edit de_housing from a GERDA session. To do there:
   - Scripts `22_identity_aware_mayor_did.R` (l. 90/102) and
     `47_electoral_position_validation.R` (l. 248–252), both in
     `components/germany_housing/code/analyze/identification/`, assert that names restored in
     the restricted twin are Sachsen-Anhalt's only. They fail now: Hessen, Thüringen and
     Bayern losers are restricted too, and since 16.10.2026 so are the Bayern winners before
     2026. Their opponent-identity results will change once Bayern names are allowed.
   - `02e_merger_first_stage_person.R` links persons by name across units, so it now sees
     Bayern (from the restricted twin only).
   - The main build (`theory_tests/00_build_data.R`) joins on `person_id`. Ids are unchanged
     except for the 4 people in item 1, whose split-off rounds get a new id.
   - No de_housing script reads the gender or name-origin columns. The winners' gender stays
     public (derived from the withheld given name; user decision 2026-10-16); name origin is
     filled for the 2026 terms only.
   - The age-limit audit should take its birth data from item 5.
1. **Panel person keys — done 2026-10-07.** `03_mayor_panel.R` splits merged Bayern keys, using
   names and, failing those, list continuity: 30 new `person_id`s (`first_amtsantritt_split`),
   existing ids kept. In de_housing's audit population, 4 people lose rounds to a new id; all
   4 splits are backed by names.
2. **Reports — parsed since 2026-10-07.**
   - **B7370C 2008/2014** (Oberbürgermeister and Landräte, with birth year) are read by
     `00_by_verzeichnis_parse.py`, as volumes `2008b` and `2014`.
   - **B7320C 2008/2014/2020/2026** (all candidates in Gemeinden over 10 000 inhabitants, plus
     Landrat races) are read by `00_by_bewerber_parse.py`. All names are restricted (winners
     too, since 16.10.2026).
   - Stored but not parsed: B7370C 2026 and B7320C 2006.
3. The ~6 % of entries left unmatched (mostly 1978/1990 OCR damage) are in
   `by_verzeichnis_entries_restricted.csv` with their status, for visual reading.
4. **Release rules.** User decision 2026-10-07: public names, birth year and profession of
   elected officials, subject to the LfStat's permission. The LfStat declined on 2026-10-16,
   so nothing from these sources is public; all of it is restricted.
5. For de_housing, join `final_restricted/by_register_persons_restricted.rds` on `person_id`.
   All 1,013 Bavarian people in the de_housing audit matched `mayor_panel` by that key, with
   the same AGS.
