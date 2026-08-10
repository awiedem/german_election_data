# Mayor identity recovery audit (August 2026)

## Outcome

This pass turns the Hessen historical election series into a complete person-election panel even though the historical workbook normally redacts names. It also repairs conservative cross-year name variants in the other named states and verifies the exact gain for the `de_housing` mayor difference-in-differences workflow.

The comparison baseline is a frozen copy of every mayoral output taken immediately before this recovery pass.

| Measure | Baseline | Recovered | Change |
|---|---:|---:|---:|
| `mayor_panel` person-election rows | 43,375 | 45,362 | +1,987 |
| `mayor_panel` unique people | 21,501 | 22,342 | +841 net |
| Hessen person-election rows | 435 | 2,422 | +1,987 |
| Hessen unique people | 429 | 1,315 | +886 |
| Named Hessen candidate rows | 471 | 744 | +273 |
| Named Hessen winners | 435 | 552 | +117 |
| Named Hessen non-winners | 36 | 192 | +156 |
| `mayor_panel_annual_harm` rows | 253,168 | 281,170 | +28,002 |

The net people gain is 45 below the Hessen-only gain because the cross-state audit also removed 45 false identity splits: 11 from mechanical normalization of titles, accents, punctuation, or field order, and 34 from the pinned high-confidence variant map.

`mayoral_candidates` falls from 113,569 to 113,562 rows. This is not lost source coverage. Seven Hessen runoff cycles previously split one real candidate into an unnamed Hauptwahl row and a named Stichwahl row; bidirectional round matching now correctly collapses those seven pseudo-duplicates.

## What was recoverable from the raw Hessen sources

The base file, `Direktwahlen_in_Hessen_seit_1993.xlsx`, contains the full 1993--2026 history, every Wahlvorschlag and its votes, and two person-level winner fields:

- `Zahl der Amtszeiten seit 1993`
- `Zahl der Wiederwahl`

Those fields are sufficient to distinguish and reconnect winners even when names are redacted. They cover 2,415 of 2,430 pre-dedup municipal winner records; the other 15 are recent supplemental records, of which 12 remain after municipality-year dedup and are linked by public name.

Public name recovery is exhaustive relative to the files on disk:

- all 671 candidate names in the May-2024 B VII m PDF are parsed and grafted;
- all 473 names in the May-2026 `he_parsed.csv` snapshot are considered without overwriting a name already present;
- 2026 hessenschau candidate names are matched by result/rank where the official historical transmission does not yet cover them;
- 62 names are propagated bidirectionally across Hauptwahl and Stichwahl when the nomination match is unique.

The resulting `he_hist_parsed.csv` has 879 named rows. After the municipal/county split and candidate-cycle reshape, 744 named Hessen candidate rows remain in `mayoral_candidates`.

The historical workbook's term counters identify 2,410 of the final 2,422 Hessen panel rows. The remaining 12 use normalized public names. No Hessen decisive winner is represented by an anonymous election-only placeholder.

## Identity rules

### Hessen

Within a municipality:

1. `Amtszeiten == 1` starts a new person.
2. A positive `Wiederwahl` count links to the immediately preceding winner.
3. Two documented return spells reconnect a former mayor after an intervening officeholder:
   - Erhard Rohrbach, Maintal: 1995, then 2003 and 2009 after Dorothee Diehl's 2001 term.
   - Rüdiger Heß, Frankenberg (Eder): 1998, then 2012 and 2017 after Christian Engelhardt's 2003/2009 terms.
4. Waldems' 1999 election and necessary 2000 Neuwahl remain one person.
5. Recent supplemental elections without counters fall back to an order-insensitive normalized public name.

`term_number` counts all observed terms of a person. `consecutive_terms` counts only the current uninterrupted spell and resets after another person holds office or after a gap over ten years. This fixes the previous implementation, which computed an interruption group but did not actually group on it.

The Maintal and Frankenberg return links are consistent with the municipalities' published histories: [Maintal's 50-year city history](https://www.maintal.de/seite/664428/50-jahre-stadt-maintal.html) and the [official Frankenberg mayor page](https://www.frankenberg.de/stadt-rathaus/politik/buergermeisterin/). The Bad Karlshafen repeated-election interpretation is also consistent with the [municipality's mayor chronology](https://www.bad-karlshafen.de/gremien-politik/buergermeister/buergermeister).

### Other named states

The basic identity key remains deliberately conservative: normalized surname plus normalized given-name initial, within municipality and state. Normalization now removes academic titles, punctuation and parenthetical party text and transliterates accents before matching. This resolves source-only differences such as a title moving from the surname field to the given-name field.

A pinned alias table handles 34 additional high-confidence people (71 election rows). Every link is within one municipality and corresponds to a one-character transcription difference, an added/dropped given name, or a documented surname change. Examples independently supported outside the raw files include:

- Arpad Bogye/Bogya: the official Niedersachsen 2014 result and candidate register use Bogya, while the older source record uses Bogye.
- Dietmar Thönnes-Richard/Thönnes: Nottuln's official site identifies the re-elected mayor as Dr. Dietmar Thönnes.
- Filippo Smaldino-Stattaus/Smaldino: contemporary reporting states that he dropped the double surname after marriage and ran for a second term as Filippo Smaldino.
- Bernadett Hosenfeld/Hosenfeld-Wald: 2026 coverage identifies her as the incumbent first elected in 2020.

Ambiguous pairs with only a shared surname or substantially different given names are not merged. A row's method is exposed as `candidate_name_variant_link` when its person relies on the alias map.

## `de_housing` compatibility and analysis gain

The downstream script `components/germany_housing/code/analyze/theory_tests/00_build_data.R` reads `mayor_panel_annual_harm.csv`, pads `ags_21`, keeps the most recent election for duplicate `ags_21 × year` rows, and joins to `master_df` on `ags = ags_21` and `year`. The existing columns and types used by that script are unchanged. Three new provenance columns are additive:

- `person_id_method`
- `source_person_term_number`
- `source_person_reelections`

Running that exact read/dedup/join contract against `de_housing/data_processed/shared/master_df.rds` gives:

| `de_housing` measure | Baseline | Recovered | Change |
|---|---:|---:|---:|
| Annual mayor rows after its `ags_21 × year` dedup | 207,892 | 219,262 | +11,370 |
| Matched master municipality-years | 104,712 | 115,974 | +11,262 |
| Matched municipalities | 5,284 | 5,409 | +125 |
| Matched people | 12,230 | 13,137 | +907 |
| Matched people observed at term 2+ | 5,905 | 6,633 | +728 |
| Matched Hessen municipality-years | 894 | 12,156 | +11,262 |
| Matched Hessen municipalities | 296 | 421 | +125 |
| Matched Hessen people | 296 | 1,233 | +937 |

There are 11,262 new matched `ags_21 × year` keys and zero lost keys. The analysis-sample expansion is therefore entirely Hessen and does not trade away any previously matched municipality-year.

## Validation

`code/mayoral_elections/99_audit.R`, section 23, now pins:

- all 671 May-2024 public names and 879 named historical intermediate rows;
- 552 named Hessen winners and 192 named non-winners in the final candidate file;
- the seven corrected Hauptwahl/Stichwahl candidate-count fixtures;
- panel provenance columns in election and annual outputs;
- 2,422 Hessen rows, 1,315 people, and the 2,410/12 method split;
- complete coverage of every Hessen winner municipality-year;
- the Maintal and Frankenberg return identities and tenure resets;
- 71 variant-linked rows / 34 people and the allowed provenance vocabulary.

The broad `98_full_audit.R` and the Landrat audit remain the whole-pipeline checks. The Hessen name parser also supports `GERDA_HE_HIST_OUT`, allowing a temporary-output parse test without overwriting the maintained intermediate.

## Remaining source limits

- The intermediate-to-final leak audit joins on both `election_date` and `election_date_sw`. After the Hessen recovery, it found no additional unnamed winner hidden in an intermediate. In particular, all 716 named Thüringen intermediate rows already match the final data; the sole unmatched name is losing candidate Carsten Erbe, whose party label differs across sources and does not affect the mayor panel.
- Bayern's historical workbook contains no personal names, but `Tag des ersten Amtsantritt` still makes its mayors longitudinally traceable.
- Thüringen redacts most historical candidate names under § 50 ThürKWO. Those unnamed winners cannot be assigned defensible person IDs from the available files.
- Hessen's historical names remain redacted. Older B VII m snapshots could add human-readable names, but no longer add person-panel coverage because the official counters already identify every historical winner. The current official series page is [Direktwahlen in Hessen](https://statistik.hessen.de/unsere-zahlen/wahlen/direktwahlen-in-hessen), with the B VII m archive in the [Statistische Bibliothek](https://www.statistischebibliothek.de/mir/receive/HESerie_mods_00001034).
- The alias map favors precision over recall. Uncertain same-surname pairs are left split rather than risking a false multi-term mayor, which would be more damaging for person fixed effects and event-time designs.
