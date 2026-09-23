# NRW 1966 and 1970 source repair

23 September 2026. Scope: the 95 district units in 1966 and 90 in 1970 in
`state_unharm`. Raw PDFs and the legacy raw CSV are unchanged. This repair is
prepared for review and does not publish or merge a release.

## Sources and transcription

The sources are the Statistisches Landesamt Nordrhein-Westfalen's reports
already held under `data/state_elections/raw/Landtagswahlen/Nordrhein-Westfalen/`:

| Election | File | Table | Physical PDF pages | Printed pages |
|---|---|---|---|---|
| 1966 | `Nordrhein-Westfalen_1966_Landtagswahl.pdf` (B III 2/3 – 66) | 2, results in county-free cities and counties | 26–33 | 24–31 |
| 1970 | `Nordrhein-Westfalen_1970_Landtagswahl.pdf`, *Ergebnisse nach Wahlkreisen und Gemeinden* | 2, results in county-free cities and counties, including postal voting | 26–35 | 24–33 |

[Source hashes](../data/state_elections/derived/nrw_1966_1970/sources.json) pin
both PDFs and the legacy identifier source. Every **a** row was checked against
rendered page images: 103 source rows for 1966 and 111 for 1970, including all
printed regional and statewide subtotals. The checked integer values are in
[table2_transcription.tsv](../data/state_elections/derived/nrw_1966_1970/table2_transcription.tsv).
Each row records the source file, physical and printed page pair, printed row
number, administrative label, and row type. Numeric candidates were initially
read with Tesseract 5 at 300 dpi; the rendered pages, not OCR output, decided
the final values. The checked transcription is the reproducible input, so
production rebuilds do not depend on an OCR version or heuristic correction.

The **a** row is the complete result, including postal votes. The **b** row is
its postal subset and must not be added again. Electorate is the column
`Wahlberechtigte insgesamt`, including voters with a Wahlschein and the
additional statutory category. Voters are `Wähler insgesamt`, not the adjacent
Wahlschein subset. Invalid votes and valid votes are separately transcribed.
Party counts follow the actual headers: **CDU, SPD, FDP, Zentrum, UAP, FSU** in
1966 and **SPD, CDU, FDP, Zentrum, UAP, DKP, NPD** in 1970. The different ordering
matters. Printed dashes mean zero in these count columns. FSU in 1970 and
DKP/NPD in 1966 are structural zeros, not OCR guesses.

All requested counts were readable. There are no unresolved cells, inferred
counts, totals-based rescaling, or counts reconstructed from rounded shares.

## What failed

The previous OCR path could pair postal and total lines incorrectly, assign
the wrong numeric columns, and shift a subtotal onto the next district. Its
fallbacks could replace printed counts with estimates from rounded percentages
or recomputed party sums. Plausible normalized shares did not establish that
the party counts or district assignment were correct. The hard-coded statewide
controls in that extractor were also incorrect for both elections.

Examples checked directly in Table 2:

| Unit | Year | Eligible | Voters | Invalid | Valid |
|---|---:|---:|---:|---:|---:|
| Düsseldorf | 1966 | 485,697 | 347,196 | 3,705 | 343,491 |
| Bonn | 1966 | 91,537 | 60,271 | 682 | 59,589 |
| Kreis Schleiden | 1966 | 42,123 | 34,474 | 774 | 33,700 |
| Düsseldorf | 1970 | 477,614 | 335,687 | 1,575 | 334,112 |
| Bottrop | 1970 | 78,079 | 58,078 | 307 | 57,771 |

Bonn's old 1966 valid count, 2,809,059, was the Düsseldorf regional subtotal.
The old R-side patches for Schleiden (40,212) and Bottrop (71,776) used the
electorate **without** a Wahlschein, not the total electorate. Both patches
have been removed. Failed extraction is not evidence of a postal-only unit;
all repaired rows have positive electorate/voters and zero postal-only,
zero-valid-vote, and excessive-turnout flags.

## Reconciliation

| Election | Units | Eligible | Voters | Invalid | Valid | State turnout |
|---|---:|---:|---:|---:|---:|---:|
| 1966 | 95 | 11,292,041 | 8,641,646 | 99,153 | 8,542,493 | 76.5% |
| 1970 | 90 | 11,890,609 | 8,739,772 | 61,945 | 8,677,827 | 73.5% |

| Party | 1966 | 1970 |
|---|---:|---:|
| CDU | 3,653,184 | 4,020,186 |
| SPD | 4,226,604 | 3,996,808 |
| FDP | 633,765 | 478,420 |
| Zentrum | 16,181 | 9,902 |
| UAP | 3,175 | 1,504 |
| FSU | 9,584 | 0 |
| DKP | 0 | 76,964 |
| NPD | 0 | 94,043 |

Statewide counts match the independently transcribed statewide **a** rows.
The electorate and rounded turnout also agree with the Landtag's independent
retrospectives for [1966](https://www.landtag.nrw.de/home/der-landtag/landtagswahlen/wahlergebnisse-im-ruckblick/1966.html)
and [1970](https://www.landtag.nrw.de/home/der-landtag/landtagswahlen/wahlergebnisse-im-ruckblick/1970.html),
checked 23 September 2026. State turnout is the ratio of statewide voters to
electorate, not an average of district turnout.

The builder requires **800 exact equalities**: ballot and party identities for
all 214 source rows, every field of all 29 printed geographic subtotals, and
separate statewide controls. The eight 1966 controls include the six regions,
NRW, and its county subtotal. The 21 controls for 1970 include six regions,
their city/county subtotals, and all three statewide rows. See
[source_reconciliation.tsv](../data/data_checks/nrw_1966_1970/source_reconciliation.tsv).
Source subtotals are retained as independent observations, never calculated
and then presented as source inputs.

## Geography and identifiers

These remain **county / county-free-city reporting units**, not ordinary
municipality observations. The 185-row
[identifier map](../data/state_elections/derived/nrw_1966_1970/unit_identifiers.tsv)
exposes the name, type, source row, geographic level, and code semantics.
The published `050xx000` identifiers are synthetic and year-specific. They
followed the legacy CSV's alphabetical name order, not the regional order of
the printed table. The repair preserves each existing year/code/name mapping.
They are not official AGS or a stable municipal panel and should not be joined
to official municipal crosswalks as if they were.

The 1966 report lists Siegen city separately from Kreis Siegen. Its printed
page 30 footnote notes incorporation into the county from 1 July 1966. This
repair retains the report's separate units and legacy labels; it does not
silently regroup them. The county subtotal reconciles with the printed
county rows under that presentation.

## Pipeline and validation

`00_nrw_1966_1970_verified.py` checks hashes, row coverage, integer values,
identities, subtotal reconciliation and identifier mapping, then writes only
under `derived/` and `data_checks/`. The old extractor routes these two years
through the checked transcription. Its remaining legacy OCR output was moved
outside `raw/`; older years were not re-extracted or claimed newly validated.

`01b_state_unharm_raw.R` excludes these years from the legacy CSV and uses
`nrw_verified.R`. Shares remain party votes divided by valid votes. The focused
rebuild uses the same helper to replace the 185 affected rows in the baseline
release, preserving all other rows, column types, row order and identifiers.

Validation completed:

- All 800 source equalities pass, including every party subtotal.
- All repaired party shares recover the source counts within floating-point
  precision; valid plus invalid votes equal voters in every row.
- Running the actual old and new NRW parser sections against the same sources
  produces identical results for every unrelated NRW year. The repaired parser
  rows match the focused rebuild (allowing the final global empty-column drop).
- CSV and RDS agree across all 365 columns, including missingness. The 149,167
  unrelated observations and all RDS column types are identical to the baseline.
  The CSV also preserves every unrelated line byte for byte.
- `state_unharm.xlsx` was regenerated using the repository exporter and its
  existing Artifact Tool template. Every repaired Excel cell is checked against
  CSV, every unrelated Excel row and non-sheet ZIP member against the previous
  export. Source excerpts before and after were rendered for visual checking.
- `state_harm`, `state_harm_21`, `state_harm_23` and `state_harm_25` contain no
  NRW 1966/1970 observations. They remain unchanged; see
  [harmonized_scope.tsv](../data/data_checks/nrw_1966_1970/harmonized_scope.tsv).

The comparison totals and output hashes are retained in
[the repair diagnostics](../data/data_checks/nrw_1966_1970/).
No website, package, other election, or general missingness changes are included.

## Reproduce

Run from the repair checkout, with an untouched baseline checkout for the
comparison. LFS sources must be hydrated; the generator rejects pointer files.
The optional source directory supports reading PDFs directly from that baseline.

```bash
python3 code/state_elections/00_nrw_1966_1970_verified.py \
  --source-root "$BASELINE/data/state_elections/raw/Landtagswahlen/Nordrhein-Westfalen"
Rscript --vanilla code/checks/rebuild_nrw_1966_1970.R "$BASELINE"
Rscript --vanilla code/checks/check_nrw_parser.R "$BASELINE"
python3 code/export_excel.py --only state_unharm --force
python3 code/checks/check_nrw_exports.py "$BASELINE"
```

For a full data build, run the ordinary `01b_state_unharm_raw.R` after generating
the verified intermediate. The focused rebuild is intended for this repair;
when integrating concurrent changes, use the full parser or a baseline that
already includes those changes, rather than overwriting them with an older
release snapshot. Only this checkout's `state_unharm.{csv,rds,xlsx}` and its
Excel manifest entry are regenerated here.
