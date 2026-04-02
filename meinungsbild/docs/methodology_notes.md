# Methodology Notes — Meinungsbild

## Current Model (production)

We use **multilevel regression and poststratification (MRP)** estimated with `lme4::glmer()`.

### Model specification

For each binary issue:

```r
y ~ male +
    fed_afd_share_z + fed_cdu_share_z + fed_turnout_z + log_pop_density_z +
    (1 | age_cat) + (1 | educ_label) +
    (1 | educ_label:age_cat) + (1 | male:age_cat) + (1 | male:educ_label) +
    (1 | survey_source) + (1 | legperiod) +
    (1 | state_code) + (1 | county_code) + (1 | wkr_nr)
```

**Fixed effects:**
- `male` — gender (binary)
- `fed_afd_share_z` — standardized AfD vote share at most recent federal election
- `fed_cdu_share_z` — standardized CDU/CSU vote share
- `fed_turnout_z` — standardized voter turnout
- `log_pop_density_z` — standardized log population density

**Random effects (varying intercepts):**
- `age_cat` — 5 age groups (18–29, 30–44, 45–59, 60–74, 75+)
- `educ_label` — 5 education levels (no degree, Hauptschule, Realschule, Abitur, university)
- `educ_label:age_cat` — education × age interaction (25 cells)
- `male:age_cat` — gender × age interaction (10 cells)
- `male:educ_label` — gender × education interaction (10 cells)
- `survey_source` — absorbs mode/house effects across 5 survey programs
- `legperiod` — legislative period (absorbs temporal shifts)
- `state_code` — 16 federal states
- `county_code` — ~253 counties with respondents
- `wkr_nr` — ~299 electoral districts (cross-classified, not nested in counties)

### Design choices

- **Dropped `east` indicator**: Highly collinear with AfD vote share (r > 0.8). Including both caused reification — geographic patterns driven by fixed effects rather than data.
- **Deep interactions**: Following Ghitza & Gelman (2013), we include all three two-way demographic interactions (gender×age, gender×education, education×age). Goplerud (2024) shows these match ML ensembles.
- **Cross-classified geography**: Electoral districts (Wahlkreise) don't nest within counties — we handle this via cross-classification rather than hierarchical nesting.
- **Adaptive formula**: Random effects are only added when > 1 level is observed in the data for that issue.

## Data

### Survey data (5 sources, ~118,000 respondents)

| Source | N respondents | Years | Geographic IDs |
|--------|--------------|-------|----------------|
| GLES Tracking (ZA6832) | 52,336 | 2009–2023 | Bundesland, Wahlkreis |
| GLES Cross-Section 2025 (ZA10100) | 7,337 | 2025 | Bundesland, Wahlkreis |
| GLES RCS 2025 (ZA10101) | 8,561 | 2025 | Bundesland, Wahlkreis |
| GLES Cumulation (ZA6835) | 21,040 | 2009–2021 | Bundesland |
| ALLBUS (ZA8974) | 29,112 | 2023–2024 | Bundesland |

Total: 1.7 million issue-response observations across 43 binary issues.

### Poststratification frame

Zensus 2022 cross-tabulations of age × sex × education at the county level.
- 400 counties × 50 demographic cells = 20,000 poststratification cells
- Population: ~70 million adults (18+)

### Geographic covariates

From GERDA (German Election Database): federal election results (AfD, CDU/CSU vote shares, turnout) and INKAR (population density). All standardized (z-scored).

## Poststratification

For each geographic unit g:

```
θ̂_g = Σ_j (N_jg / N_g) × Pr(y=1 | demographics_j, geography_g)
```

where j indexes demographic cells (age × sex × education), N_jg is the census population count, and Pr(y=1) is the model-predicted probability.

### Geographic levels

- **Bundesländer** (16 federal states) — highest reliability
- **Kreise** (400 counties) — finest resolution
- **Wahlkreise** (299 electoral districts) — politically relevant; constructed from county estimates via population-weighted crosswalk

## Validation

### Bundesland-level correlation with direct survey estimates

- **Median r = 0.899** across 43 issues
- **Median RMSE = 5.5 percentage points**
- Top issues: ukraine_arms (0.993), rent_control (0.992), taxes_vs_spending (0.988), nuclear_energy (0.981)
- Weak issues (r < 0.5): internet_control, income_reduce, econ_current_personal, mandatory_service, fear_war, higher_earners_tax — flagged on website

## Geographic boundaries

| Level | Source | File |
|-------|--------|------|
| Bundeslaender | VG250 (BKG) | `bundeslaender.geojson` |
| Kreise | VG250 (BKG) | `kreise.geojson` |
| Wahlkreise | Bundeswahlleiterin BTW 2025 boundaries (generalized, WGS84) | `wahlkreise.geojson` |

Wahlkreis boundaries obtained from [bundeswahlleiterin.de](https://www.bundeswahlleiterin.de/bundestagswahlen/2025/wahlkreiseinteilung/downloads.html), simplified with `sf::st_simplify(dTolerance = 0.002)` for web performance.

## Key references

1. Ghitza & Gelman (2013). "Deep Interactions MRP" — two-way demographic interactions
2. Goplerud (2024). "Re-evaluating ML for MRP" — deep hierarchical models match ML ensembles
3. Gao et al. (2021). "Improving MRP with Structured Priors" — RW1 + BYM2 (future extension)
4. Selb & Munzert (2011). MRP for German Wahlkreise
5. Warshaw & Rodden (2012). District-level MRP validation

## Pipeline scripts

| Script | Purpose |
|--------|---------|
| `01_harmonize_all.R` | Pool 5 survey datasets, harmonize demographics + issues |
| `02_build_poststrat_frame.R` | Build county-level poststratification frame |
| `02b_download_zensus.R` | Download Zensus 2022 cross-tabs from API |
| `03_load_covariates.R` | Load election results + INKAR covariates for counties |
| `03b_build_adjacency.R` | Build county adjacency matrix from VG250 shapefiles |
| `04b_fit_all_lme4.R` | **Primary**: fit lme4 models for all issues + poststratify |
| `04_fit_mrp.R` | *Experimental*: brms/Stan with RW1 priors + BYM2 spatial |
| `07_export_estimates.R` | Export estimates to JSON (website) and CSV (download) |
| `08_check_pipeline.R` | 135-check validation pipeline |
