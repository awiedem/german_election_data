# Schleswig-Holstein, 13 March 1983

`table6_transcription.tsv` is the reviewed input for `state_unharm`. It replaces
the unreliable `raw/.../sh_1983_extracted.csv`, which is retained unchanged.
All election counts come from **Table 6, printed/PDF pages 18–117** of
`raw/Landtagswahlen/Schleswig-Holstein/Schleswig-Holstein_1983_Landtagswahl.pdf`.
Source and reviewed-file SHA-256 hashes are recorded in `sources.json`.

## Scope and interpretation

- **1,128 municipal result records**, including the four independent cities.
  AGS is an eight-digit string: `01` + three-digit county + three-digit municipality.
- The election used a single vote. Counts and party shares cover **in-person
  voting only**. `eligible_voters` is the full electorate; `number_voters`,
  `valid_votes`, `invalid_votes` and all party counts exclude postal ballots.
  `turnout` is therefore `NA` in the published data. The in-person voter/electorate
  ratio must not be interpreted as overall turnout.
- The source explicitly states on p. 4 that postal votes cannot be assigned to
  municipalities. No proportional allocation or rescaling is performed.
- Wiedenborstel has no separate result: its electorate voted in Hennstedt
  (p. 4). No extra or zero-filled Wiedenborstel row is created.
- The final printed column combines independents and Linke Liste
  Schleswig-Holstein (LLSH). Per p. 4, in Table 6 it means independents in
  counties **01 and 54**, and LLSH elsewhere. The published `einzelbewerber`
  and new `llsh` columns separate them: 50 and 94 in-person votes respectively.
  LLSH is distinct from `linke_pds`.

## Recovery and evidence

The 50 left/right page pairs were rendered at 400 dpi and read with Tesseract
(`eng`, `--psm 6`), independently of the PDF's existing text layer. Each count
cell retained both readings as candidates. Rows were accepted only when a
unique candidate combination satisfied electorate components, voters = valid
+ invalid, and valid = sum of all printed party columns. Ambiguous cells were
read directly from the scan, including the whole right-hand Rade row on p. 109.
Counts were not estimated from percentages or adjusted to hit totals.

`cell_evidence.tsv` records all 18,048 accepted count cells, their page/row,
both OCR readings and whether a visual correction was required (180 cells).
The auxiliary count of in-person voters with a voting certificate is not used
or included; it was not part of these count checks. The two electorate
certificate columns are included and reconcile exactly.

`identifier_checks.tsv` preserves the printed-code readings. The complete
code set was cross-checked against Schleswig-Holstein's municipal register in
the federal election data from 6 March 1983; 33 ambiguous codes were visually
checked in the state-election scan. `name_reference` is from the federal
`BTW83_Leitband.txt`, for identification only. **No federal election counts
were used to reconstruct state-election counts.**

`percentage_checks.tsv` contains 7,959 legible printed percentages checked
against the recovered counts (one-decimal rounding tolerance). Of 148 apparent
non-anchor percentage disagreements, visual inspection confirmed 145 printed
percentages and three zero-count dashes that OCR had mistaken for percentages.
The latter are recorded as zero counts in the cell evidence. Unreadable
percentages are omitted, not fabricated. The 100.0% electorate/valid-vote
anchor cells are not part of this independent percentage check.

## Printed controls

Table 3 (pp. 12–13) distinguishes municipal/in-person results from postal votes.
Their sum equals the overall result on p. 5; the electorate is counted once.

| Field | In person | Postal | Overall |
|---|---:|---:|---:|
| Eligible voters | 1,965,881 | — | 1,965,881 |
| Voters | 1,506,849 | 160,445 | 1,667,294 |
| Invalid votes | 4,372 | 450 | 4,822 |
| Valid votes | 1,502,477 | 159,995 | 1,662,472 |
| CDU | 729,026 | 85,531 | 814,557 |
| SPD | 669,816 | 56,816 | 726,632 |
| FDP | 31,993 | 3,839 | 35,832 |
| SSW | 19,869 | 1,938 | 21,807 |
| DKP | 1,888 | 311 | 2,199 |
| DGL | 1,300 | 206 | 1,506 |
| GRÜNE | 48,100 | 11,258 | 59,358 |
| FP | 333 | 76 | 409 |
| FSU | 8 | 0 | 8 |
| Independents + LLSH | 144 | 20 | 164 |

Every in-person total matches exactly. County electorate totals match p. 14.
The all-ballot result has 56 independent and 108 LLSH votes (p. 5); the postal
split is not needed to construct any municipal observation.

## Validation and rebuild

From the repository root:

```sh
python3 code/checks/check_sh_1983_source.py
Rscript --vanilla code/checks/rebuild_sh_1983.R /path/to/baseline
Rscript --vanilla code/checks/check_sh_1983_parser.R /path/to/baseline
python3 code/export_excel.py --only state_unharm
python3 code/checks/check_sh_1983_exports.py
```

The baseline supplies the pre-change `state_unharm.rds` and parser script for
comparison. The focused rebuild replaces only SH 1983 and checks all unrelated
rows. Without a baseline argument it rebuilds from the current output. The full
`01b_state_unharm_raw.R` pipeline uses the same `sh_1983_verified.R` helper.
`00_sh_1983_extract.py` now invokes the read-only source validator; the optional
vision YAML writes unreviewed candidates only under `derived/`.

The validator checks source hashes, identifiers, row identities, all printed
totals, county electorate, percentages and six deliberate corruption cases,
including a party transfer preserving both row and statewide totals.
SH 1983 is absent from every harmonized output, which starts in 1990 or later.

This repair is independent of the NRW, BW 1952 and missing-value branches.
When integrating those branches, rebuild exports and coverage/schema metadata
from the combined data, retaining the postal-vote limitation documented here.
