"""
GERDA Pipeline Python Cross-Checks 2026
========================================
Re-implements key computational steps in Python (pandas) to catch R-idiomatic bugs.

Each check:
  - Loads a CSV output produced by the R pipeline
  - Re-computes a specific property
  - Returns a DataFrame of flagged rows

All results are written to review/python_review_results.json.

Usage (from repo root):
    review/.venv/bin/python review/python_checks.py

R-idiomatic bugs this can detect:
  - na.rm=TRUE silently treating NA as 0 in weighted sums
  - sum() without na.rm propagating NA across whole groups
  - conflict_prefer_all("dplyr") masking unexpected behavior
  - ifelse(col==0|is.na(col), fill, col) overwriting valid zeros
"""

import json
from pathlib import Path

import numpy as np
import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[1]

# Tolerance for share row sums
TOL_SHARES_STATE = 0.02
TOL_SHARES_MUNI  = 0.05   # higher: Kumulieren/Panaschieren → shares >1 are normal
TOL_CW           = 1e-4   # tolerance for crosswalk weight sums

# Meta columns (not party shares) for state elections
STATE_META = [
    "ags", "election_year", "state", "state_name", "election_date",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes", "turnout",
    "county", "area_ags", "population_ags", "employees_ags", "pop_density_ags",
    "flag_unsuccessful_naive_merge", "flag_total_votes_incongruent",
    "total_vote_share",
]

# Reference set of expected party column names (standardized snake_case)
KNOWN_PARTIES = {
    "cdu", "csu", "cdu_csu", "spd", "gruene", "fdp", "linke_pds", "afd", "bsw",
    "npd", "rep", "dvu", "iii_weg", "die_rechte", "dkp", "mlpd", "sgp",
    "piraten", "freie_wahler", "odp", "die_partei", "tierschutzallianz",
    "tierschutz_hier", "tierschutz", "v_partei3", "ssw", "volt", "diebasis",
    "bundnis_c", "bundnis_deutschland", "familie", "bp", "schill", "statt_partei",
    "graue", "zentrum", "freie_sachsen", "werteunion", "team_todenhofer",
    "die_humanisten", "violetten", "volksabstimmung", "lkr", "pro_deutschland",
    "rentner", "big", "unabhangige", "aufbruch", "bfb", "bge", "bueso",
    "gesundheitsforschung", "mera25", "patrioten", "pbc", "partei_der_vernunft",
    "dsu", "ab_jetzt", "ad_demokraten", "dib", "dm", "menschliche_welt",
    "gartenpartei", "naturgesetz", "da", "other",
    # BB 1990-specific
    "buendnis_90", "chrl", "dbu", "dfp", "domowina",
    # MV-specific (not explicitly mapped)
    "bfb", "buerger", "einzelbewerber",
    # Municipal-specific
    "replaced_cdu_csu", "replaced_spd", "replaced_gruene", "replaced_fdp",
    "replaced_linke_pds", "replaced_afd", "replaced_freie_wahler",
    "replaced_piraten", "replaced_other", "replaced_die_partei",
}


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 1: State election share row sums
# ─────────────────────────────────────────────────────────────────────────────

def check_state_share_rowsum() -> pd.DataFrame:
    """
    Load state_unharm.csv; compute row-wise sum of party share columns.
    Flag rows where abs(sum - 1.0) > TOL_SHARES_STATE.

    R-idiomatic risk caught:
      - If shares were divided by number_voters instead of valid_votes,
        sums will deviate systematically (turnout factor).
      - If NA was silently converted to 0 (na.rm=TRUE behaviour),
        rows with non-participating parties will appear to sum to < 1.
    """
    path = REPO_ROOT / "data/state_elections/final/state_unharm.csv"
    print(f"  Loading {path.name} ({path.stat().st_size / 1e6:.0f} MB)...")
    df = pd.read_csv(path, low_memory=False)

    party_cols = [c for c in df.columns if c not in STATE_META and not c.startswith("replaced_")]
    # Only include genuine share columns (non-negative, ≤ 2 for most states)
    # cdu_csu is a derived column — exclude to avoid double-counting
    party_cols = [c for c in party_cols if c != "cdu_csu"]

    row_sums = df[party_cols].fillna(0).sum(axis=1)
    df["_py_share_sum"] = row_sums
    flags = df[
        (df["valid_votes"].notna()) &
        (df["valid_votes"] > 0) &
        (abs(row_sums - 1.0) > TOL_SHARES_STATE)
    ].copy()

    flags["_check"] = "state_share_rowsum"
    flags["_detail"] = flags["_py_share_sum"].apply(lambda x: f"sum={x:.4f}, dev={abs(x-1):.4f}")

    result = flags[["ags", "election_year", "state", "_py_share_sum", "_detail"]].copy()
    result.columns = ["ags", "election_year", "state", "share_sum", "detail"]
    print(f"    → {len(result)} flagged rows (tol={TOL_SHARES_STATE})")
    return result


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 2: Municipal share row sums
# ─────────────────────────────────────────────────────────────────────────────

def check_municipal_share_rowsum() -> pd.DataFrame:
    """
    Load municipal_unharm.csv.
    For Kumulieren/Panaschieren elections, shares >1 per party are normal;
    the ROW SUM can exceed 1.0.
    Flag: sum > 10 (implausibly high) or sum == 0 (no parties recorded).

    Note: column names in municipal data differ from state data:
      - uses CamelCase party names (CDU, SPD, ...)
      - 'other' is called 'other' or 'Wählergruppen'
    """
    path = REPO_ROOT / "data/municipal_elections/final/municipal_unharm.csv"
    print(f"  Loading {path.name} ({path.stat().st_size / 1e6:.0f} MB)...")
    df = pd.read_csv(path, low_memory=False)

    muni_meta = [
        "ags", "ags_name", "state", "election_year", "election_date",
        "election_type", "eligible_voters", "number_voters", "valid_votes", "turnout",
    ]
    party_cols = [c for c in df.columns
                  if c not in muni_meta and not c.startswith("replaced_")]

    row_sums = df[party_cols].fillna(0).sum(axis=1)
    df["_py_share_sum"] = row_sums

    flags = df[
        (df["valid_votes"].notna()) &
        (df["valid_votes"] > 0) &
        ((row_sums > 10) | (row_sums == 0))
    ].copy()

    flags["_detail"] = flags["_py_share_sum"].apply(
        lambda x: f"sum={x:.2f} {'(implausibly high)' if x > 10 else '(zero)'}"
    )
    result = flags[["ags", "election_year", "state", "_py_share_sum", "_detail"]].copy()
    result.columns = ["ags", "election_year", "state", "share_sum", "detail"]
    print(f"    → {len(result)} flagged rows (sum>10 or sum==0)")
    return result


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 3: Base crosswalk weight sums
# ─────────────────────────────────────────────────────────────────────────────

def check_crosswalk_base_weightsums() -> pd.DataFrame:
    """
    Load ags_crosswalks.csv (base 1990-2021 crosswalk from 01_ags_crosswalk.R).
    Group by (ags, year); assert sum(pop_cw) == 1.0 within TOL_CW.

    R-idiomatic risk: 01_ags_crosswalk.R does NOT explicitly normalize weights.
    It relies entirely on StatBA source data summing to 1.0. This check verifies
    that assumption empirically.
    """
    path = REPO_ROOT / "data/crosswalks/final/ags_crosswalks.csv"
    print(f"  Loading {path.name} ({path.stat().st_size / 1e6:.0f} MB)...")
    df = pd.read_csv(path, low_memory=False)

    sums = df.groupby(["ags", "year"])["pop_cw"].sum().reset_index()
    sums.columns = ["ags", "year", "pop_cw_sum"]
    flags = sums[abs(sums["pop_cw_sum"] - 1.0) > TOL_CW].copy()
    flags["detail"] = flags["pop_cw_sum"].apply(lambda x: f"sum={x:.6f}, dev={abs(x-1):.6f}")
    print(f"    → {len(flags)} (ags, year) pairs with weight sum ≠ 1.0 (tol={TOL_CW})")
    return flags[["ags", "year", "pop_cw_sum", "detail"]]


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 4: Extended crosswalk weight sums (1990–2025)
# ─────────────────────────────────────────────────────────────────────────────

def check_crosswalk_extended_weightsums() -> pd.DataFrame:
    """
    Load ags_1990_to_2025_crosswalk.csv.
    Group by (ags, year); assert sum(pop_cw) == 1.0.

    R-idiomatic risk: chain multiplication (pop_cw * pop_w_21_23 * pop_w_23_25)
    without na.rm in intermediate sums can produce NA weights that survive the
    explicit rescaling step (sum(pop_cw) without na.rm → NA for whole group).
    Also checks for NA weight rows as a separate category.
    """
    path = REPO_ROOT / "data/crosswalks/final/ags_1990_to_2025_crosswalk.csv"
    print(f"  Loading {path.name} ({path.stat().st_size / 1e6:.0f} MB)...")
    df = pd.read_csv(path, low_memory=False)

    # NA weights
    na_weights = df[df["pop_cw"].isna()][["ags", "year", "pop_cw"]].copy()
    na_weights["detail"] = "pop_cw is NA"

    # Sum deviation
    sums = df.groupby(["ags", "year"])["pop_cw"].sum().reset_index()
    sums.columns = ["ags", "year", "pop_cw_sum"]
    dev = sums[abs(sums["pop_cw_sum"] - 1.0) > TOL_CW].copy()
    dev["pop_cw"] = dev["pop_cw_sum"]
    dev["detail"] = dev["pop_cw_sum"].apply(lambda x: f"sum={x:.6f}, dev={abs(x-1):.6f}")

    combined = pd.concat([
        na_weights[["ags", "year", "detail"]].assign(pop_cw_sum=float("nan")),
        dev[["ags", "year", "pop_cw_sum", "detail"]]
    ], ignore_index=True)
    print(f"    → {len(na_weights)} rows with NA weights; {len(dev)} groups with sum ≠ 1.0")
    return combined


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 5: Briefwahl allocation proxy
# ─────────────────────────────────────────────────────────────────────────────

def check_briefwahl_allocation() -> pd.DataFrame:
    """
    Load state_unharm.csv.
    For states with K-row Briefwahl allocation (Thüringen=16, MV=13, ST=15):
    Flag municipalities where valid_votes / number_voters > 1.05.

    This can signal a Briefwahl allocation anomaly: if more valid_votes were
    allocated than there were voters in a municipality, the proportional weights
    were incorrectly computed.

    Note: valid_votes > number_voters is theoretically possible if invalid_votes
    is negative (data error) or turnout was exactly 100% + postal. So we use
    a 5% margin.
    """
    path = REPO_ROOT / "data/state_elections/final/state_unharm.csv"
    df = pd.read_csv(path, low_memory=False)

    briefwahl_states = ["16", "13", "15"]
    df_brief = df[df["state"].isin(briefwahl_states)].copy()
    df_brief["_ratio"] = df_brief["valid_votes"] / df_brief["number_voters"].replace(0, float("nan"))

    flags = df_brief[
        df_brief["_ratio"].notna() &
        (df_brief["_ratio"] > 1.05)
    ][["ags", "election_year", "state", "valid_votes", "number_voters", "_ratio"]].copy()
    flags.columns = ["ags", "election_year", "state", "valid_votes", "number_voters", "ratio_vv_nv"]
    flags["detail"] = flags["ratio_vv_nv"].apply(lambda x: f"valid_votes/number_voters={x:.3f}")
    print(f"    → {len(flags)} rows with valid_votes/number_voters > 1.05 in Briefwahl states")
    return flags


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 6: Party column inventory
# ─────────────────────────────────────────────────────────────────────────────

def check_party_column_inventory() -> pd.DataFrame:
    """
    Load state_unharm.csv and municipal_unharm.csv.
    Extract all non-meta column names; split into:
      - 'known': in KNOWN_PARTIES reference set
      - 'fallback': not in reference set (likely normalise_party() fallback output)

    R-idiomatic risk: normalise_party() silently maps unrecognized raw names to
    snake_case. Fallback-produced column names indicate parties that were not
    explicitly handled — they could be real small parties, OCR artifacts, or
    encoding errors.
    """
    results = []
    for fname, meta in [
        ("state_unharm.csv",     STATE_META),
        ("municipal_unharm.csv", []),  # all columns are relevant here
    ]:
        path = REPO_ROOT / "data" / (
            "state_elections/final" if "state" in fname else "municipal_elections/final"
        ) / fname
        print(f"  Scanning columns in {fname}...")
        df = pd.read_csv(path, nrows=0)  # headers only

        muni_meta = [
            "ags", "ags_name", "state", "election_year", "election_date",
            "election_type", "eligible_voters", "number_voters", "valid_votes", "turnout",
        ]
        effective_meta = meta if meta else muni_meta
        candidate_cols = [c for c in df.columns
                          if c not in effective_meta and not c.startswith("replaced_")]

        for col in candidate_cols:
            status = "known" if col in KNOWN_PARTIES else "fallback"
            if status == "fallback":
                results.append({"file": fname, "column": col, "status": status})

    result = pd.DataFrame(results)
    print(f"    → {len(result)} fallback/unexpected column names found")
    return result


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 7: NA vs. 0 distinction (state harm)
# ─────────────────────────────────────────────────────────────────────────────

def check_na_zero_distinction() -> pd.DataFrame:
    """
    Load state_harm_21.csv.
    For each party column, identify (state, election_year) pairs where ALL
    municipality values are exactly 0 (not NA) AND there are > 5 municipalities.

    Heuristic: if a party got 0 votes in every single municipality of a state
    in a given year, it almost certainly did not run (should be NA, not 0).
    Exception: very small parties in a single state for one election (documented cases).

    R-idiomatic risk: na.rm=TRUE in weighted sums converts NA → 0 contribution;
    ifelse(col==0|is.na(col), fill, col) overwrites valid zeros.
    """
    path = REPO_ROOT / "data/state_elections/final/state_harm_21.csv"
    print(f"  Loading {path.name} ({path.stat().st_size / 1e6:.0f} MB)...")
    df = pd.read_csv(path, low_memory=False)

    harm_meta = STATE_META + ["state_name"]
    party_cols = [c for c in df.columns if c not in harm_meta and c != "cdu_csu"]

    results = []
    for col in party_cols:
        grouped = df.groupby(["state", "election_year"])[col].agg(
            n_total="count",
            n_zero=lambda x: (x == 0).sum(),
            n_na=lambda x: x.isna().sum(),
        ).reset_index()
        # Flag: all non-NA values are 0, and there are at least 5 observations
        suspects = grouped[
            (grouped["n_total"] >= 5) &
            (grouped["n_zero"] == grouped["n_total"]) &
            (grouped["n_na"] == 0)
        ].copy()
        if len(suspects) > 0:
            suspects["party"] = col
            suspects["detail"] = suspects.apply(
                lambda r: f"all {r['n_total']} municipalities have {col}=0 (not NA)", axis=1
            )
            results.append(suspects[["state", "election_year", "party", "n_total", "detail"]])

    if results:
        result = pd.concat(results, ignore_index=True)
    else:
        result = pd.DataFrame(columns=["state", "election_year", "party", "n_total", "detail"])

    print(f"    → {len(result)} suspicious (state, year, party) combinations (all zeros, no NAs)")
    return result


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 8: Vote count conservation through harmonization
# ─────────────────────────────────────────────────────────────────────────────

def check_harm_vote_conservation() -> pd.DataFrame:
    """
    Compare sum(valid_votes) in state_unharm.csv vs. state_harm_21.csv
    for each (state, election_year) pair.

    Flag pairs where the ratio deviates by > 5%.

    R-idiomatic risk: pop_cw weights that don't sum to 1.0, or rows lost
    during the crosswalk merge, would cause vote counts to change unexpectedly.
    Factor vs. string mismatches in join keys can silently drop rows.
    """
    print("  Loading state_unharm.csv and state_harm_21.csv...")
    unharm = pd.read_csv(REPO_ROOT / "data/state_elections/final/state_unharm.csv",
                         usecols=["state", "election_year", "valid_votes"], low_memory=False)
    harm = pd.read_csv(REPO_ROOT / "data/state_elections/final/state_harm_21.csv",
                       usecols=["state", "election_year", "valid_votes"], low_memory=False)

    unharm_sum = unharm.groupby(["state", "election_year"])["valid_votes"].sum().reset_index()
    unharm_sum.columns = ["state", "election_year", "vv_unharm"]

    harm_sum = harm.groupby(["state", "election_year"])["valid_votes"].sum().reset_index()
    harm_sum.columns = ["state", "election_year", "vv_harm"]

    merged = unharm_sum.merge(harm_sum, on=["state", "election_year"], how="outer")
    merged["ratio"] = merged["vv_harm"] / merged["vv_unharm"].replace(0, float("nan"))
    merged["deviation"] = abs(merged["ratio"] - 1.0)

    flags = merged[
        merged["deviation"].notna() & (merged["deviation"] > 0.05)
    ].copy()
    flags["detail"] = flags.apply(
        lambda r: f"ratio={r['ratio']:.4f} (unharm={r['vv_unharm']:.0f}, harm={r['vv_harm']:.0f})",
        axis=1
    )
    print(f"    → {len(flags)} (state, year) pairs with >5% vote count change after harmonization")
    return flags[["state", "election_year", "vv_unharm", "vv_harm", "ratio", "detail"]]


# ─────────────────────────────────────────────────────────────────────────────
# CHECK 9: Briefwahl re-implementation (Thüringen, state 16)
# ─────────────────────────────────────────────────────────────────────────────

def reimpl_briefwahl_allocation() -> pd.DataFrame:
    """
    Full re-implementation of the K-row Briefwahl allocation for Thüringen (state=16).

    Reads the same XLSX source files that 01b_state_unharm_raw.R processes.
    Applies the allocation logic independently in Python and compares the resulting
    valid_votes per municipality against state_unharm.csv.

    What we check:
      1. Were there any Wahlkreise with g_vv == 0 (skipped silently in R)?
         Python logs these explicitly rather than skipping.
      2. Do the resulting municipality-level valid_votes match R's output?
         Discrepancies indicate a bug in the proportional allocation logic.

    Scope: years 1994-2024 (1990 is skipped in R too — DDR-era codes).

    R-idiomatic risks caught:
      - na.rm=TRUE in g_vv sum may include NA municipalities with weight 0
      - Proportional weight w = valid[g] / sum(valid[g_in_wkr]) relies on
        correct G-row filtering; any leakage of K-rows into result would corrupt weights
    """
    raw_base = REPO_ROOT / "data/state_elections/raw/Landtagswahlen/Thüringen"
    years = [1994, 1999, 2004, 2009, 2014, 2019, 2024]

    # Load R output for comparison
    state_unharm = pd.read_csv(
        REPO_ROOT / "data/state_elections/final/state_unharm.csv",
        usecols=["ags", "election_year", "state", "valid_votes"],
        low_memory=False
    )
    th_r = state_unharm[state_unharm["state"] == "16"].copy()
    th_r["election_year"] = th_r["election_year"].astype(int)

    results = []
    skipped_wahlkreise = []

    for yr in years:
        fpath = raw_base / f"Thüringen_{yr}_Landtag.xlsx"
        if not fpath.exists():
            results.append({
                "election_year": yr, "ags": "—", "vv_python": None, "vv_r": None,
                "delta": None, "detail": f"File not found: {fpath.name}"
            })
            continue

        try:
            raw = pd.read_excel(fpath, sheet_name=0, header=None, dtype=str)
        except Exception as e:
            results.append({
                "election_year": yr, "ags": "—", "vv_python": None, "vv_r": None,
                "delta": None, "detail": f"Read error: {e}"
            })
            continue

        # Column indices are 0-based in pandas (R uses 1-based)
        # Satzart is column index 1 (col2 in R)
        satzart_col = 1

        # Identify G and K rows
        g_rows = raw[raw.iloc[:, satzart_col] == "G"].copy()
        k_rows = raw[raw.iloc[:, satzart_col] == "K"].copy()

        if len(g_rows) == 0:
            results.append({
                "election_year": yr, "ags": "—", "vv_python": None, "vv_r": None,
                "delta": None, "detail": "No G rows found — format may have changed"
            })
            continue

        # Construct AGS: "16" + sprintf("%03d", col4) + sprintf("%03d", col5)
        # In R: cnames[4] = col4 (kreisnr), cnames[5] = col5 (gemeindenr)
        kreis_col   = 3  # col4 in R → index 3
        gemeinde_col = 4  # col5 in R → index 4

        def make_ags(row):
            try:
                k = int(float(str(row.iloc[kreis_col]).strip()))
                g = int(float(str(row.iloc[gemeinde_col]).strip()))
                return f"16{k:03d}{g:03d}"
            except Exception:
                return None

        g_rows = g_rows.copy()
        g_rows["ags"] = g_rows.apply(make_ags, axis=1)
        k_rows = k_rows.copy()

        # Wahlkreis column: col3 in R → index 2
        wkr_col = 2
        g_rows["wkr"] = g_rows.iloc[:, wkr_col].astype(str).str.strip()
        k_rows_wkr = k_rows.iloc[:, wkr_col].astype(str).str.strip()

        # Find Landesstimmen gültig column
        # R searches row 5 (index 4) for "gültige" / "Landesstimmen"
        # We use the same heuristic: find "Landesstimmen" in row 4 (index 3)
        r4 = raw.iloc[3].astype(str)

        ls_header_cols = [i for i, v in enumerate(r4) if "Landesstimmen" in str(v)]
        if not ls_header_cols:
            results.append({
                "election_year": yr, "ags": "—", "vv_python": None, "vv_r": None,
                "delta": None, "detail": "Could not locate Landesstimmen header in row 4"
            })
            continue

        ls_header_col = ls_header_cols[0]
        ls_gueltig_col = ls_header_col + 1  # R: ls_header_col + 1

        def safe_num(val):
            try:
                v = str(val).strip()
                if v in ("-", "—", "", "nan", "None"):
                    return np.nan
                return float(v)
            except Exception:
                return np.nan

        g_rows["valid_votes_g"] = g_rows.iloc[:, ls_gueltig_col].apply(safe_num)

        # --- Allocate Briefwahl from K rows ---
        k_rows = k_rows.copy()
        k_rows["wkr"] = k_rows_wkr
        k_rows["valid_votes_k"] = k_rows.iloc[:, ls_gueltig_col].apply(safe_num)

        for _, krow in k_rows.iterrows():
            wkr = str(krow["wkr"]).strip()
            gidx = g_rows[g_rows["wkr"] == wkr].index
            if len(gidx) == 0:
                continue
            g_vv = g_rows.loc[gidx, "valid_votes_g"].fillna(0).sum()
            if g_vv == 0:
                skipped_wahlkreise.append({
                    "election_year": yr, "wkr": wkr,
                    "k_valid_votes": krow["valid_votes_k"],
                    "detail": "g_vv==0: Briefwahl silently skipped (mirrors R behaviour)"
                })
                continue
            # Proportional weights
            w = g_rows.loc[gidx, "valid_votes_g"].fillna(0) / g_vv
            vv_k = safe_num(krow["valid_votes_k"])
            if not np.isnan(vv_k):
                residual = vv_k - g_vv
                if residual > 0:
                    g_rows.loc[gidx, "valid_votes_g"] += residual * w.values

        # Aggregate duplicate AGS
        py_result = g_rows.groupby("ags")["valid_votes_g"].sum().reset_index()
        py_result.columns = ["ags", "vv_python"]
        py_result["election_year"] = yr

        # Merge with R output
        r_yr = th_r[th_r["election_year"] == yr][["ags", "valid_votes"]].rename(
            columns={"valid_votes": "vv_r"}
        )
        comparison = py_result.merge(r_yr, on="ags", how="outer")
        comparison["delta"] = abs(comparison["vv_python"] - comparison["vv_r"])
        comparison["rel_delta"] = comparison["delta"] / comparison["vv_r"].replace(0, np.nan)

        flagged = comparison[
            comparison["delta"].notna() & (comparison["delta"] > 1)
        ].copy()
        flagged["detail"] = flagged.apply(
            lambda r: f"python={r['vv_python']:.1f}, R={r['vv_r']:.1f}, delta={r['delta']:.1f}", axis=1
        )
        flagged["election_year"] = yr
        results.append(flagged[["ags", "election_year", "vv_python", "vv_r", "delta", "detail"]])

    skipped_df = pd.DataFrame(skipped_wahlkreise) if skipped_wahlkreise else pd.DataFrame(
        columns=["election_year", "wkr", "k_valid_votes", "detail"]
    )

    flagged_results = [r for r in results if isinstance(r, pd.DataFrame)]
    if flagged_results:
        discrepancies = pd.concat(flagged_results, ignore_index=True)
    else:
        discrepancies = pd.DataFrame(columns=["ags", "election_year", "vv_python", "vv_r", "delta", "detail"])

    print(f"    → {len(discrepancies)} municipality-year pairs with |vv_python - vv_R| > 1")
    print(f"    → {len(skipped_df)} Wahlkreise silently skipped (g_vv==0) — would be lost in R")

    # Combine
    discrepancies["finding_type"] = "vv_mismatch"
    skipped_df["finding_type"] = "silently_skipped_wahlkreis"
    return pd.concat([
        discrepancies[["ags", "election_year", "detail", "finding_type"]],
        skipped_df[["wkr", "election_year", "detail", "finding_type"]].rename(columns={"wkr": "ags"})
    ], ignore_index=True)


# ─────────────────────────────────────────────────────────────────────────────
# MAIN
# ─────────────────────────────────────────────────────────────────────────────

def main():
    output_path = REPO_ROOT / "review/python_review_results.json"
    output_path.parent.mkdir(exist_ok=True)

    checks = [
        ("state_share_rowsum",       check_state_share_rowsum),
        ("municipal_share_rowsum",   check_municipal_share_rowsum),
        ("cw_base_weightsums",       check_crosswalk_base_weightsums),
        ("cw_extended_weightsums",   check_crosswalk_extended_weightsums),
        ("briefwahl_proxy",          check_briefwahl_allocation),
        ("party_inventory",          check_party_column_inventory),
        ("na_zero_distinction",      check_na_zero_distinction),
        ("harm_vote_conservation",   check_harm_vote_conservation),
        ("briefwahl_reimpl",         reimpl_briefwahl_allocation),
    ]

    results = {}
    for name, fn in checks:
        print(f"\n[{name}]")
        try:
            df = fn()
            records = df.to_dict(orient="records")
            records = [
                {k: (None if isinstance(v, float) and np.isnan(v) else v)
                 for k, v in row.items()}
                for row in records
            ]
            results[name] = {"n_issues": len(df), "issues": records}
        except Exception as e:
            print(f"  ERROR: {e}")
            results[name] = {"n_issues": -1, "error": str(e), "issues": []}

    with open(output_path, "w", encoding="utf-8") as f:
        json.dump(results, f, indent=2, default=str)

    print(f"\n\nResults written to {output_path}")
    print("\nSummary:")
    for name, res in results.items():
        n = res["n_issues"]
        status = f"{n} issues" if n >= 0 else f"ERROR: {res.get('error','?')}"
        print(f"  {name:40s} {status}")


if __name__ == "__main__":
    main()
