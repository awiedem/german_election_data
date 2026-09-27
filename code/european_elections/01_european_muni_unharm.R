### European Elections 2009-2024: Municipality-Level Results
# Aggregates ballot-district data to municipality level
# Handles Amt/VG-level mail-in (Briefwahl) vote allocation
# Processes all 4 European Parliament elections: 2009, 2014, 2019, 2024
# Vincent Heddesheimer
# April 2026

rm(list = ls())
options(scipen = 999)

pacman::p_load("tidyverse", "data.table", "haschaR")
conflict_prefer("filter", "dplyr")


# --- 0. Party name normalisation ---------------------------------------------

normalise_party_eu <- function(pname) {
  mapping <- c(
    # Major parties
    "CDU"                     = "cdu",
    "SPD"                     = "spd",
    "CSU"                     = "csu",
    "FDP"                     = "fdp",
    "AfD"                     = "afd",
    "BSW"                     = "bsw",

    # Greens
    "GR\u00dcNE"              = "gruene",
    "GRÜNE"                   = "gruene",

    # Left
    "DIE LINKE"               = "die_linke",

    # Tierschutz variants
    "Tierschutzpartei"        = "tierschutz",
    "Die Tierschutzpartei"    = "tierschutz",
    "TIERSCHUTZ hier!"        = "tierschutz_hier",
    "Tierschutzallianz"       = "tierschutzallianz",
    "PARTEI F\u00dcR DIE TIERE" = "partei_fuer_die_tiere",
    "PARTEI FÜR DIE TIERE"   = "partei_fuer_die_tiere",

    # Free voters
    "FREIE W\u00c4HLER"      = "freie_waehler",
    "FREIE WÄHLER"            = "freie_waehler",
    "FW FREIE W\u00c4HLER"   = "freie_waehler",
    "FW FREIE WÄHLER"         = "freie_waehler",

    # ÖDP
    "\u00d6DP"                = "oedp",
    "ÖDP"                     = "oedp",
    "\u00f6dp"                = "oedp",
    "ödp"                     = "oedp",

    # Die PARTEI
    "Die PARTEI"              = "die_partei",
    "DIE PARTEI"              = "die_partei",

    # PIRATEN
    "PIRATEN"                 = "piraten",

    # FAMILIE
    "FAMILIE"                 = "familie",

    # Volt
    "Volt"                    = "volt",

    # NPD / HEIMAT
    "NPD"                     = "npd",
    "HEIMAT"                  = "heimat",

    # REP
    "REP"                     = "rep",

    # DVU
    "DVU"                     = "dvu",

    # BIG
    "BIG"                     = "big",

    # Bündnis C
    "B\u00fcndnis C"          = "buendnis_c",
    "Bündnis C"               = "buendnis_c",

    # BÜNDNIS DEUTSCHLAND
    "B\u00dcNDNIS DEUTSCHLAND" = "buendnis_deutschland",
    "BÜNDNIS DEUTSCHLAND"     = "buendnis_deutschland",

    # BP
    "BP"                      = "bp",

    # DKP
    "DKP"                     = "dkp",

    # MLPD
    "MLPD"                    = "mlpd",

    # PSG / SGP
    "PSG"                     = "psg",
    "SGP"                     = "sgp",

    # BüSo
    "B\u00fcSo"               = "bueso",
    "BüSo"                    = "bueso",

    # Volksabstimmung
    "Volksabstimmung"         = "volksabstimmung",

    # PBC
    "PBC"                     = "pbc",

    # CM
    "CM"                      = "cm",

    # DIE FRAUEN
    "DIE FRAUEN"              = "die_frauen",

    # MENSCHLICHE WELT
    "MENSCHLICHE WELT"        = "menschliche_welt",

    # V-Partei³
    "V-Partei\u00b3"          = "v_partei3",
    "V-Partei³"               = "v_partei3",

    # dieBasis
    "dieBasis"                = "die_basis",

    # DAVA
    "DAVA"                    = "dava",

    # KLIMALISTE
    "KLIMALISTE"              = "klimaliste",

    # LETZTE GENERATION
    "LETZTE GENERATION"       = "letzte_generation",

    # PDV
    "PDV"                     = "pdv",

    # PdF
    "PdF"                     = "pdf_partei",

    # PdH
    "PdH"                     = "pdh",

    # ABG
    "ABG"                     = "abg",

    # MERA25
    "MERA25"                  = "mera25",

    # Verjüngungsforschung
    "Verj\u00fcngungsforschung" = "verjuengungsforschung",
    "Verjüngungsforschung"    = "verjuengungsforschung",

    # 2009-specific parties
    "AUFBRUCH"                = "aufbruch",
    "50Plus"                  = "x50plus",
    "AUF"                     = "auf",
    "DIE GRAUEN"              = "die_grauen",
    "Die Grauen"              = "die_grauen",
    "DIE VIOLETTEN"           = "die_violetten",
    "EDE"                     = "ede",
    "FBI"                     = "fbi",
    "F\u00dcR VOLKSENTSCHEIDE" = "fuer_volksentscheide",
    "FÜR VOLKSENTSCHEIDE"     = "fuer_volksentscheide",
    "Newropeans"              = "newropeans",
    "RRP"                     = "rrp",
    "RENTNER"                 = "rentner",

    # 2014-specific
    "PRO NRW"                 = "pro_nrw",

    # 2019-specific
    "BGE"                     = "bge",
    "DIE DIREKTE!"            = "die_direkte",
    "DiEM25"                  = "diem25",
    "III. Weg"                = "iii_weg",
    "DIE RECHTE"              = "die_rechte",
    "LIEBE"                   = "liebe",
    "Graue Panther"           = "graue_panther",
    "LKR"                     = "lkr",
    "NL"                      = "nl",
    "\u00d6koLinX"            = "oekolinx",
    "ÖkoLinX"                 = "oekolinx",
    "Die Humanisten"          = "die_humanisten",
    "Gesundheitsforschung"    = "gesundheitsforschung"
  )

  result <- mapping[pname]
  if (!is.na(result)) return(unname(result))

  # Fallback: clean to snake_case
  cleaned <- tolower(pname)
  cleaned <- gsub("\u00e4", "ae", cleaned)
  cleaned <- gsub("\u00f6", "oe", cleaned)
  cleaned <- gsub("\u00fc", "ue", cleaned)
  cleaned <- gsub("\u00df", "ss", cleaned)
  cleaned <- gsub("[^a-z0-9]+", "_", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  return(cleaned)
}


# --- 1. Year configurations --------------------------------------------------

ew_configs <- list(
  list(
    year       = 2009L,
    date       = "2009-06-07",
    file       = "data/european_elections/raw/ew09_wbz/EW09_Wbz_Ergebnisse_utf8.csv",
    sep        = "\t",
    skip       = 4,
    encoding   = "UTF-8",
    elig_col   = "Wahlberechtigte (A)",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte nach § 25 Abs. 2 BWO (A3)",
    voters_col = "Wähler (B)",
    wahlschein_col = "Wähler mit Wahlschein (B1)",
    invalid_col = "Ungültig",
    valid_col   = "Gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = c()
  ),
  list(
    year       = 2014L,
    date       = "2014-05-25",
    file       = "data/european_elections/raw/ew14_wbz/EW14_Wbz_Ergebnisse.csv",
    sep        = ";",
    skip       = 4,
    encoding   = "UTF-8",
    elig_col   = "Wahlberechtigte (A)",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte nach § 25 Abs. 2 BWO (A3)",
    voters_col = "Wähler (B)",
    wahlschein_col = "Wähler mit Wahlschein (B1)",
    invalid_col = "Ungültig",
    valid_col   = "Gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = c()
  ),
  list(
    year       = 2019L,
    date       = "2019-05-26",
    file       = "data/european_elections/raw/ew19_wbz/ew19_wbz_ergebnisse.csv",
    sep        = ";",
    skip       = 4,
    encoding   = "Latin-1",
    elig_col   = "Wahlberechtigte (A)",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte nach § 24 Abs. 2 EuWO (A3)",
    voters_col = "Wähler (B)",
    wahlschein_col = "Wähler mit Wahlschein (B1)",
    invalid_col = "Ungültig",
    valid_col   = "Gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = "Ungekürzte Wahlbezirksbezeichnung"
  ),
  list(
    year       = 2024L,
    date       = "2024-06-09",
    file       = "data/european_elections/raw/ew24_wbz/ew24_wbz_ergebnisse.csv",
    sep        = ";",
    skip       = 0,
    encoding   = "UTF-8",
    elig_col   = "Wahlberechtigte",
    a1_col     = "Wahlberechtigte ohne Sperrvermerk (A1)",
    a2_col     = "Wahlberechtigte mit Sperrvermerk (A2)",
    a3_col     = "Wahlberechtigte § 24 (2) EuWO (A3)",
    voters_col = "Wählende",
    wahlschein_col = "dar. Wählende mit Wahlschein",
    invalid_col = "ungültig",
    valid_col   = "gültig",
    ba_col      = "Bezirksart",
    bwbez_col   = "Kennziffer Briefwahlzugehörigkeit",
    drop_cols   = c("Kennziffer zusammengelegte Urnenwahlbezirke § 61 EuWO",
                    "Optional: Ungekürzte Wahlbezirksbezeichnung")
  )
)


# --- 1b. Mail-in allocation ---------------------------------------------------

# Largest-remainder rounding: whole numbers, each within 1 of its target, that
# add up to exactly `total`.
round_to_total <- function(target, total) {
  out <- floor(target + 1e-9)
  left <- round(total - sum(out))
  stopifnot(left >= 0, left <= length(out))
  top <- order(target - out, target, decreasing = TRUE)[seq_len(left)]
  out[top] <- out[top] + 1
  out
}

# Splits one pooled mail-in district (`pool`: named vector of its counts) over
# the municipalities that vote in it, by their eligible-voter weights `w`.
# Counts stay whole numbers and add up to exactly the pool's. Invalid votes and
# Wahlschein voters follow the allocated voters, so voters = valid + invalid in
# every municipality. Party votes are the allocated valid votes split in the
# pool's party proportions and are not rounded: rounding each party on its own
# made party votes miss valid_votes, so vote shares did not sum to 1.
allocate_pool <- function(w, pool, party_cols) {
  out <- matrix(0, length(w), length(pool), dimnames = list(NULL, names(pool)))
  for (v in c("eligible_voters", "voters_wo_sperrvermerk",
              "voters_w_sperrvermerk", "voters_par24_2", "number_voters")) {
    out[, v] <- round_to_total(w * pool[[v]], pool[[v]])
  }
  voters <- out[, "number_voters"]
  for (v in c("invalid_votes", "voters_w_wahlschein")) {
    target <- if (pool[["number_voters"]] > 0) {
      voters * pool[[v]] / pool[["number_voters"]]
    } else {
      w * pool[[v]]
    }
    out[, v] <- round_to_total(target, pool[[v]])
  }
  out[, "valid_votes"] <- voters - out[, "invalid_votes"]
  if (pool[["valid_votes"]] > 0) {
    out[, party_cols] <- outer(out[, "valid_votes"], pool[party_cols]) /
      pool[["valid_votes"]]
  }
  out
}


# --- 2. Processing function ---------------------------------------------------

process_ew_year <- function(cfg) {

  cat("\n========== Processing European Election", cfg$year, "==========\n")

  # 2a: Read raw data — read as character to prevent fread from misinterpreting
  # German thousands separators (e.g., "1.510" = 1510) as decimal values.
  # Without colClasses = "character", fread parses "1.510" as numeric 1.51,
  # then as.character(1.51) drops the trailing zero, and gsub produces "151" not "1510".
  df <- fread(
    cfg$file,
    sep = cfg$sep,
    skip = cfg$skip,
    header = TRUE,
    encoding = cfg$encoding,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  cat("Raw rows:", nrow(df), "| Cols:", ncol(df), "\n")

  # 2b: Drop non-data columns
  for (dc in cfg$drop_cols) {
    if (dc %in% names(df)) df[[dc]] <- NULL
  }

  # 2c: Standardize meta column names
  # valid_votes is the vote-share denominator. rename(any_of()) would skip a
  # header that changed, and the valid-vote column would then be read as a party.
  needed <- c(cfg$elig_col, cfg$voters_col, cfg$invalid_col, cfg$valid_col)
  if (!all(needed %in% names(df))) {
    stop(cfg$year, ": missing columns ",
         paste(setdiff(needed, names(df)), collapse = ", "), call. = FALSE)
  }
  # Rename year-specific voter column names to common names
  rename_map <- c(
    "eligible_voters"        = cfg$elig_col,
    "voters_wo_sperrvermerk" = cfg$a1_col,
    "voters_w_sperrvermerk"  = cfg$a2_col,
    "voters_par24_2"         = cfg$a3_col,
    "number_voters"          = cfg$voters_col,
    "voters_w_wahlschein"    = cfg$wahlschein_col,
    "invalid_votes"          = cfg$invalid_col,
    "valid_votes"            = cfg$valid_col,
    "BA"                     = cfg$ba_col,
    "BWBez"                  = cfg$bwbez_col
  )
  df <- df |> rename(any_of(rename_map))

  # 2d: Construct 8-digit AGS
  df <- df |>
    mutate(
      Land = pad_zero_conditional(Land, 1),
      Kreis = pad_zero_conditional(Kreis, 1),
      Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
      Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
      ags = paste0(Land, Regierungsbezirk, Kreis, Gemeinde),
      county = substr(ags, 1, 5)
    )

  # 2e: Identify party columns (everything after valid_votes meta cols)
  vote_meta_cols <- c(
    "eligible_voters", "voters_wo_sperrvermerk", "voters_w_sperrvermerk",
    "voters_par24_2", "number_voters", "voters_w_wahlschein",
    "invalid_votes", "valid_votes"
  )
  geo_cols <- c("Land", "Regierungsbezirk", "Kreis", "Verbandsgemeinde",
                "Gemeinde", "Wahlbezirk", "ags", "county", "BA", "BWBez")
  party_cols <- setdiff(names(df), c(geo_cols, vote_meta_cols))
  numeric_cols <- c(vote_meta_cols, party_cols)

  cat("Party columns found:", length(party_cols), "\n")
  cat("  ", paste(head(party_cols, 10), collapse = ", "), "...\n")

  # 2f: Ensure numeric (strip German thousands separators)
  df <- df |>
    mutate(across(all_of(numeric_cols), ~ as.numeric(gsub("\\.", "", as.character(.x)))))

  # The aggregation below sums with na.rm = TRUE, which would turn a missing
  # valid-vote count into 0; stop instead.
  if (anyNA(df$valid_votes)) {
    stop(cfg$year, ": ", sum(is.na(df$valid_votes)),
         " ballot districts lack valid_votes", call. = FALSE)
  }
  # Mail-in allocation (section 6) needs a count in every cell and, for vote
  # shares to sum to 1, party votes adding up to valid votes and valid +
  # invalid to voters. A blank party cell is a party not on that ballot
  # (CSU in 11 Rheinland-Pfalz districts in 2019): 0 votes.
  if (anyNA(select(df, all_of(vote_meta_cols)))) {
    stop(cfg$year, ": missing turnout counts in ballot districts", call. = FALSE)
  }
  df <- df |> mutate(across(all_of(party_cols), ~ replace_na(.x, 0)))
  n_bad <- sum(rowSums(select(df, all_of(party_cols))) != df$valid_votes |
                 df$valid_votes + df$invalid_votes != df$number_voters)
  if (n_bad > 0) {
    stop(cfg$year, ": ", n_bad, " ballot districts where party votes do not ",
         "sum to valid votes or valid + invalid != voters", call. = FALSE)
  }

  # 2g: Remap BA=6→0 and BA=8→0 (Sonderwahlbezirke → polling station equivalent)
  n_ba6 <- sum(df$BA == 6, na.rm = TRUE)
  n_ba8 <- sum(df$BA == 8, na.rm = TRUE)
  if (n_ba6 > 0 || n_ba8 > 0) {
    cat("Remapping BA: 6→0 (", n_ba6, "rows), 8→0 (", n_ba8, "rows)\n")
  }
  df <- df |> mutate(BA = ifelse(BA %in% c(6, 8), 0L, as.integer(BA)))

  # 2h: Keep every Niedersachsen row. Every row of the file is one ballot
  # district, so none is an aggregate. NI codes with Gemeinde suffix >= 400
  # are Briefwahl districts of a Samtgemeinde (or of a Kreis-wide pool:
  # 9xx, and 999 in 2014, BA=5, no electorate) that section 6 allocates to
  # the municipalities with the same (county, BWBez), and the gemeindefreie
  # Bezirke Lohheide (03351501) and Osterheide (03358501), which are real
  # municipalities. Dropping these rows until September 2026 deleted every
  # NI postal vote counted at Samtgemeinde level (163,216 valid votes in 2024).

  # Raw totals per state, for the reconciliation check after section 7
  raw_totals <- df |>
    mutate(party_votes = rowSums(across(all_of(party_cols)))) |>
    group_by(state = Land) |>
    summarise(across(all_of(c(vote_meta_cols, "party_votes")), sum), .groups = "drop")

  # 2i: Distribute Gem=999 dummy rows (Sonderwahlbezirke without real municipality)
  # Remap their BA to 5 so they get distributed proportionally across real
  # municipalities in the same (county, BWBez) group via the Briefwahl logic
  n_gem999 <- sum(df$Gemeinde == "999" & df$BA == 0, na.rm = TRUE)
  if (n_gem999 > 0) {
    cat("Remapping", n_gem999, "Gem=999 BA=0 rows to BA=5 for proportional distribution\n")
    df <- df |> mutate(BA = ifelse(Gemeinde == "999" & BA == 0, 5L, BA))
  }

  # --- 3. Aggregate ballot districts by (ags, BWBez, BA) ---
  df_agg <- df |>
    group_by(ags, county, BWBez, BA) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # --- 4. Separate joint mail-in pools from each municipality's own rows ---
  # All municipalities of a Kreis that form a joint Briefwahlvorstand carry the
  # same 2-digit Briefwahlzugehörigkeit (BWBez); 0/00 means none (Hinweise zur
  # Wahlbezirksstatistik). So a Briefwahl row (BA=5) with a nonzero BWBez
  # belongs to that joint board whichever code it is booked on: usually the
  # board's own 9xx/999 code, but in some Thüringen Verwaltungsgemeinschaften
  # (2014, 2024) and in Sachsen 14628 (2009) also the lead municipality's,
  # while the board's votes come from lead and members alike (postal voters
  # match the Wahlschein holders, A2, of the whole group, not of one side).
  # Such rows are pooled and allocated over every municipality with polling
  # stations under the same (county, BWBez); a key with a single municipality
  # hands it all of its votes. Briefwahl rows under BWBez 0/00 stay with
  # their municipality.
  df_agg <- df_agg |> mutate(pooled = BA == 5 & !BWBez %in% c("0", "00"))
  ags_with_ba0 <- df_agg |> filter(BA == 0) |> pull(ags) |> unique()

  # Outside a joint board, Briefwahl must be booked on a real municipality
  stray <- df_agg |>
    filter(BA == 5, !pooled, !ags %in% ags_with_ba0,
           number_voters > 0 | eligible_voters > 0)
  if (nrow(stray) > 0) {
    stop(cfg$year, ": Briefwahl without a joint board on codes with no ",
         "polling stations: ", paste(unique(stray$ags), collapse = ", "),
         call. = FALSE)
  }

  # --- 5. Own rows: polling stations and Briefwahl under BWBez 0/00 ---
  df_own <- df_agg |>
    filter(!pooled, ags %in% ags_with_ba0) |>
    group_by(ags, county) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # --- 6. Allocate the joint mail-in pools ---
  # 6a: Pool totals per (county, BWBez)
  df_mailin <- df_agg |>
    filter(pooled) |>
    group_by(county, BWBez) |>
    summarise(across(all_of(numeric_cols), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

  # 6b: Municipalities voting in each pool, with their eligible voters there
  df_recv <- df_agg |>
    filter(BA == 0) |>
    semi_join(df_mailin, by = c("county", "BWBez")) |>
    group_by(ags, county, BWBez) |>
    summarise(eligible_voters = sum(eligible_voters, na.rm = TRUE), .groups = "drop")

  cat("Real municipalities:", length(ags_with_ba0), "\n")
  cat("Mail-in pools:", nrow(df_mailin), "| municipalities receiving from one:",
      n_distinct(df_recv$ags), "\n")

  # Every pool needs a municipality to go to; otherwise its votes would be lost
  orphans <- anti_join(df_mailin, df_recv, by = c("county", "BWBez"))
  if (nrow(orphans) > 0) {
    stop(cfg$year, ": ", nrow(orphans), " mail-in pools (",
         sum(orphans$number_voters), " voters) have no municipality with ",
         "polling stations in their (county, BWBez): ",
         paste(orphans$county, orphans$BWBez, collapse = ", "), call. = FALSE)
  }

  # 6c: Calculate eligible-voter weights within each (county, BWBez) group
  df_recv <- df_recv |>
    group_by(county, BWBez) |>
    mutate(
      group_elig = sum(eligible_voters, na.rm = TRUE),
      elig_weight = ifelse(
        group_elig > 0,
        eligible_voters / group_elig,
        1 / n()
      )
    ) |>
    ungroup()

  # 6d: Distribute mail-in votes (allocate_pool(), section 1b)
  recv_key <- paste(df_recv$county, df_recv$BWBez)
  pool_key <- paste(df_mailin$county, df_mailin$BWBez)
  counts <- matrix(0, nrow(df_recv), length(numeric_cols),
                   dimnames = list(NULL, numeric_cols))
  for (k in seq_along(pool_key)) {
    i <- which(recv_key == pool_key[k])
    counts[i, ] <- allocate_pool(df_recv$elig_weight[i],
                                 unlist(df_mailin[k, numeric_cols]), party_cols)
  }
  df_allocated <- bind_cols(df_recv |> select(ags, county), as_tibble(counts))

  # --- 7. Combine: own rows plus allocated mail-in, one row per municipality ---
  df_muni <- bind_rows(df_own, df_allocated) |>
    group_by(ags, county) |>
    summarise(across(all_of(numeric_cols), sum), .groups = "drop") |>
    arrange(ags)

  # Remove uninhabited areas (eligible_voters=0, number_voters=0)
  n_zero <- sum(df_muni$eligible_voters == 0 & df_muni$number_voters == 0, na.rm = TRUE)
  if (n_zero > 0) {
    cat("Removing", n_zero, "zero-voter municipalities (gemeindefreie Gebiete)\n")
    df_muni <- df_muni |> filter(!(eligible_voters == 0 & number_voters == 0))
  }

  # Reconcile every state with the raw file. Aggregation and mail-in
  # allocation move votes between municipalities of a state but must neither
  # create nor lose any; the removed zero-voter rows hold no votes. Counts must
  # match exactly, the unrounded party votes to floating-point precision.
  muni_totals <- df_muni |>
    mutate(state = substr(ags, 1, 2),
           party_votes = rowSums(across(all_of(party_cols)))) |>
    group_by(state) |>
    summarise(across(all_of(c(vote_meta_cols, "party_votes")), sum), .groups = "drop")
  recon <- full_join(raw_totals, muni_totals, by = "state", suffix = c("_raw", "_muni"))
  for (v in c(vote_meta_cols, "party_votes")) {
    gap <- recon[[paste0(v, "_muni")]] - recon[[paste0(v, "_raw")]]
    off <- is.na(gap) | abs(gap) > if (v == "party_votes") 1e-6 else 0
    if (any(off)) {
      stop(cfg$year, ": ", v, " does not reconcile with the raw file in state(s) ",
           paste0(recon$state[off], " (", gap[off], ")", collapse = ", "),
           call. = FALSE)
    }
  }
  cat("Per-state totals reconcile with the raw file\n")

  cat("Final municipality count:", nrow(df_muni), "\n")

  # Check duplicates
  dupl <- df_muni |> count(ags) |> filter(n > 1)
  if (nrow(dupl) > 0) {
    warning("Duplicate AGS found in ", cfg$year, ": ", paste(dupl$ags, collapse = ", "))
  }

  # --- 8. Normalise party column names ---
  # Apply normalise_party_eu to party columns
  old_party_names <- intersect(party_cols, names(df_muni))
  new_party_names <- sapply(old_party_names, normalise_party_eu)

  # Check for duplicates in new names
  if (any(duplicated(new_party_names))) {
    dups <- new_party_names[duplicated(new_party_names)]
    warning("Duplicate normalised party names in ", cfg$year, ": ",
            paste(unique(dups), collapse = ", "), " — will aggregate")
    # Rename then aggregate duplicates
    names(df_muni)[match(old_party_names, names(df_muni))] <- new_party_names
    df_muni <- df_muni |>
      group_by(ags, county) |>
      summarise(across(everything(), ~ if (is.numeric(.x)) sum(.x, na.rm = TRUE) else first(.x)),
                .groups = "drop")
  } else {
    names(df_muni)[match(old_party_names, names(df_muni))] <- new_party_names
  }

  # --- 9. Add metadata ---
  df_muni <- df_muni |>
    mutate(
      state = substr(ags, 1, 2),
      state_name = state_id_to_names(state),
      election_year = cfg$year,
      election_date = lubridate::ymd(cfg$date)
    )

  cat("Columns:", ncol(df_muni), "\n")
  return(df_muni)
}


# --- 3. Process all years -----------------------------------------------------

all_years <- map(ew_configs, process_ew_year)

# Combine: bind_rows fills missing party columns with NA
df_all <- bind_rows(all_years)

cat("\n========== Combined ==========\n")
cat("Total rows:", nrow(df_all), "\n")
cat("Total cols:", ncol(df_all), "\n")

# Identify all party columns (exclude meta/geo/flag columns)
meta_cols <- c("ags", "county", "state", "state_name", "election_year",
               "election_date", "eligible_voters", "number_voters",
               "valid_votes", "invalid_votes", "voters_wo_sperrvermerk",
               "voters_w_sperrvermerk", "voters_par24_2", "voters_w_wahlschein")
party_cols_all <- sort(setdiff(names(df_all), c(meta_cols, "turnout",
                                                 "flag_turnout_above_1")))

cat("Total unique parties:", length(party_cols_all), "\n")

# Replace NA with 0 for party columns (party not running = 0 votes)
df_all <- df_all |>
  mutate(across(all_of(party_cols_all), ~ replace_na(.x, 0)))


# --- 4. Compute turnout and vote shares ---------------------------------------

# Vote shares are shares of valid votes, as in the state and municipal data.
stopifnot(!anyNA(df_all$valid_votes))

df_all <- df_all |>
  mutate(
    turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_),
    across(all_of(party_cols_all), ~ ifelse(valid_votes > 0, .x / valid_votes, NA_real_))
  )

# Party votes add up to valid votes in every municipality, including those
# that received pooled mail-in votes (allocate_pool()), so shares sum to 1
share_sum <- rowSums(select(df_all, all_of(party_cols_all)))
stopifnot(all(abs(share_sum[df_all$valid_votes > 0] - 1) < 1e-9))

# Cap turnout at 1 and flag
df_all <- df_all |>
  mutate(
    flag_turnout_above_1 = as.integer(!is.na(turnout) & turnout > 1),
    turnout = ifelse(!is.na(turnout) & turnout > 1, 1, turnout)
  )


# --- 5. Reorder columns and write ---------------------------------------------

df_all <- df_all |>
  select(
    ags, county, state, state_name,
    election_year, election_date,
    eligible_voters, number_voters, valid_votes, invalid_votes,
    voters_wo_sperrvermerk, voters_w_sperrvermerk, voters_par24_2, voters_w_wahlschein,
    turnout,
    all_of(party_cols_all),
    flag_turnout_above_1
  ) |>
  arrange(election_year, ags)

glimpse(df_all)

write_rds(df_all, "data/european_elections/final/european_muni_unharm.rds", compress = "gz")
fwrite(df_all, "data/european_elections/final/european_muni_unharm.csv")

cat("\nWritten:", nrow(df_all), "rows x", ncol(df_all), "columns\n")


# --- 6. Sanity checks ---------------------------------------------------------

# Rows per year
cat("\nRows per year:\n")
df_all |> count(election_year) |> print()

# State distribution per year
cat("\nState distribution:\n")
df_all |>
  group_by(election_year, state, state_name) |>
  summarise(n = n(), mean_turnout = mean(turnout, na.rm = TRUE), .groups = "drop") |>
  arrange(election_year, state) |>
  print(n = 70)

# National totals per year
cat("\nNational totals per year:\n")
df_all |>
  group_by(election_year) |>
  summarise(
    n_muni = n(),
    eligible = sum(eligible_voters, na.rm = TRUE),
    voters = sum(number_voters, na.rm = TRUE),
    valid = sum(valid_votes, na.rm = TRUE),
    turnout = sum(number_voters, na.rm = TRUE) / sum(eligible_voters, na.rm = TRUE)
  ) |>
  print()

# Major party shares per year
cat("\nNational party shares per year:\n")
major_parties <- c("cdu", "csu", "spd", "gruene", "afd", "die_linke", "fdp", "bsw")
for (yr in c(2009, 2014, 2019, 2024)) {
  cat(sprintf("\n  %d:\n", yr))
  sub <- df_all |> filter(election_year == yr)
  total_v <- sum(sub$valid_votes, na.rm = TRUE)
  for (p in major_parties) {
    if (p %in% names(sub)) {
      s <- sum(sub[[p]] * sub$valid_votes, na.rm = TRUE) / total_v
      if (s > 0.001) cat(sprintf("    %s: %.4f\n", p, s))
    }
  }
}

# Check duplicates
cat("\nDuplicate (ags, year) pairs:\n")
dupl <- df_all |> count(ags, election_year) |> filter(n > 1)
cat("  Count:", nrow(dupl), "\n")

# Turnout flags
cat("\nTurnout > 1 flags per year:\n")
df_all |>
  group_by(election_year) |>
  summarise(n_flag = sum(flag_turnout_above_1, na.rm = TRUE)) |>
  print()


### END
