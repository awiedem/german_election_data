# Historical state-election corrections, 24 September 2026

This release integrates the separately reviewed missingness, NRW 1966/1970,
BW 1952 and SH 1983 branches. The combined `state_unharm` has **150,512 rows and
368 columns**: 1,111 BW records added, 49 SH records restored, and new party
columns `dg_bhe`, `uwg` and `llsh`.

- NRW counts and turnout reconcile with the source; its pre-1975 identifiers
  remain synthetic county identifiers specific to each election.
- BW 1952 is a constituent-assembly election on 1979 municipal boundaries.
  Full electorate and turnout remain missing; the documented source KPD +1
  and SRP -1 differences are retained.
- SH 1983 has 1,128 verified municipal result records, covering in-person
  voting. Postal votes cannot be assigned to municipalities, so overall
  turnout is missing. Wiedenborstel is included in Hennstedt.
- Unknown invalid counts remain missing. The three current harmonized
  datasets change only in `invalid_votes`; historical additions and repairs
  fall outside their coverage.

`rebuild_historical_state_release.R` combines the reviewed source helpers
with the missingness-corrected baseline. Against the previous main release,
all 148,088 unrelated rows are identical outside the reviewed zero-to-missing
invalid-count corrections. Election keys include the date: Hamburg held two
state elections in 1982. See `data/data_checks/historical_state_release/`.

Validation includes SH's complete source controls and printed percentages,
eight NRW source-integrity tests, 398 BW checks, actual SH/NRW parser-section
regressions, and 1,496 CSV/RDS column comparisons. Coverage, schema, source
limitations, Excel exports and the codebook are regenerated for the combined
release. The website publishes the existing concise historical-update entry
once these data are on main.

The September 23 branch audit documents pre-integration snapshots. Its NRW
and SH extraction warnings and BW start date are superseded by this release;
remaining source limitations are in `data/state_elections/metadata/`.
