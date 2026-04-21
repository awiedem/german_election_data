#### State Elections: Clean raw data (1990-2024) ####
## Builds state_unharm from raw files, replacing the DESTATIS API source.
##
## This script reads municipality-level raw election data for all 16 German
## states (where machine-readable data is available) and produces a unified
## unharmonized dataset. It replaces 01_state_unharm.R (API-based, 2006-2019)
## and 03_state_2022-24.R (manual, 2022-2024) with a single source of truth.
##
## Output: data/state_elections/final/state_unharm.rds / .csv
##
## Authors: Vincent Heddesheimer (with Claude Code assistance)
## Date: February 2026

#### Setup ####
rm(list = ls())

packages <- c("readxl", "xml2", "here", "janitor", "tidyverse", "data.table")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

setwd(here())

raw_path <- "data/state_elections/raw/Landtagswahlen"

## Meta columns (non-party) present in every row ----------------------------
meta_cols <- c(
  "ags", "election_year", "state", "election_date",
  "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
  "turnout"
)

## normalise_party: map raw party name → snake_case column name ------------
## Aligned with federal elections pipeline naming where possible.
normalise_party <- function(pname) {
  p <- trimws(pname)
  p_up <- toupper(p)
  # Major parties
  if (p_up == "CDU")                                    return("cdu")
  if (grepl("CHRISTLICH.DEMOKRATISCHE UNION", p_up))    return("cdu")
  if (p_up == "CSU")                                    return("csu")
  if (grepl("CHRISTLICH.SOZIALE UNION", p_up))          return("csu")
  if (p_up == "SPD")                                    return("spd")
  if (grepl("SOZIALDEMOKRATISCHE PARTEI", p_up))        return("spd")
  if (grepl("^GR[UÜ]NE|^B[UÜ]NDNIS\\s*90.*GR|^DIE GR", p_up)) return("gruene")
  if (grepl("^FDP|^F[.]D[.]P", p_up))                  return("fdp")
  if (grepl("^FDP/DVP", p_up))                          return("fdp")
  if (grepl("FREIE DEMOKRATISCHE PARTEI", p_up))        return("fdp")
  if (grepl("LINKE|^PDS|^LL.PDS|^DIE LINKE", p_up))    return("linke_pds")
  if (grepl("^AFD$|^AFD[[:space:]]", p_up))             return("afd")
  if (grepl("^BSW$|^BSW[[:space:]]|WAGENKNECHT", p_up)) return("bsw")
  # Far right
  if (grepl("^NPD", p_up))                              return("npd")
  if (grepl("^REP$|^REPUBLIKANER", p_up))                return("rep")
  if (grepl("^DVU$", p_up))                              return("dvu")
  if (grepl("^III[.]|^DER III", p_up))                   return("iii_weg")
  if (grepl("^DIE.?RECHTE", p_up))                        return("die_rechte")
  if (grepl("HEIMAT$", p_up))                            return("npd")
  # Far left
  if (grepl("^DKP$", p_up))                              return("dkp")
  if (grepl("DEUTSCHE KOMMUNISTISCHE PARTEI", p_up))     return("dkp")
  if (grepl("^MLPD$", p_up))                             return("mlpd")
  if (grepl("^SGP$", p_up))                              return("sgp")
  # Smaller parties (aligned with federal pipeline)
  if (grepl("^PIRATEN", p_up))                           return("piraten")
  if (grepl("^FREIE W[AÄ]HLER|^FW$|^FWG", p_up))       return("freie_wahler")
  if (grepl("^BVB.*FREIE", p_up))                        return("freie_wahler")
  if (grepl("[OÖ]DP$|^[OÖ]DP[[:space:]]", p_up))        return("odp")
  if (grepl("[OÖ]KOLOGISCH.DEMOKRATISCHE PARTEI", p_up)) return("odp")
  if (grepl("^DIE.?PARTEI$|^PARTEI$", p_up))              return("die_partei")
  if (grepl("TIER.?SCHUTZ.*ALLIANZ", p_up))              return("tierschutzallianz")
  if (grepl("TIERSCHUTZ.*HIER", p_up))                   return("tierschutz_hier")
  if (grepl("TIERSCHUTZ", p_up))                          return("tierschutz")
  if (grepl("^V.PARTEI", p_up))                          return("v_partei3")
  if (grepl("^SSW$", p_up))                              return("ssw")
  if (grepl("^VOLT$", p_up))                             return("volt")
  if (grepl("^DIEBASIS|^DIE BASIS", p_up))               return("diebasis")
  if (grepl("B[UÜ]NDNIS\\s*C$|^B[UÜ]NDNIS C[[:space:]]", p_up)) return("bundnis_c")
  if (grepl("B[UÜ]NDNIS.*DEUTSCHLAND", p_up))            return("bundnis_deutschland")
  if (grepl("^FAMILIE$|^FAMILIEN", p_up))                 return("familie")
  if (grepl("^BAYERNPARTEI$|^BP$", p_up))                 return("bp")
  if (grepl("^SCHILL|^PRO$|^RECHTSSTAATLICHE", p_up))     return("schill")
  if (grepl("^STATT", p_up))                              return("statt_partei")
  if (grepl("^GRAUE|^DIE GRAUEN", p_up))                  return("graue")
  if (grepl("^ZENTRUM$", p_up))                            return("zentrum")
  if (grepl("^FREIE SACHSEN", p_up))                       return("freie_sachsen")
  if (grepl("WERTEUNION|^WU$", p_up))                     return("werteunion")
  if (grepl("^TEAM TODENHÖFER|^TEAM TODENH", p_up))       return("team_todenhofer")
  if (grepl("^HUMANISTEN|^DIE HUMANISTEN", p_up))          return("die_humanisten")
  if (grepl("^VIOLETTEN|^DIE VIOLETTEN", p_up))            return("violetten")
  if (grepl("^VOLKSAB.?STIMMUNG", p_up))                   return("volksabstimmung")
  if (grepl("^LKR$|^LIBERAL.KONSERVATIV", p_up))           return("lkr")
  if (grepl("^PRO DEUTSCHLAND|^PRO NRW|^PRO CHEMNITZ", p_up)) return("pro_deutschland")
  if (grepl("^RENTNER|^RRP", p_up))                        return("rentner")
  if (grepl("^BIG$", p_up))                                return("big")
  if (grepl("^UNABH[AÄ]NGIGE", p_up))                     return("unabhangige")
  if (grepl("^AUFBRUCH", p_up))                            return("aufbruch")
  if (grepl("^BFB$|^B[UÜ]RGER F[UÜ]R", p_up))            return("bfb")
  if (grepl("^BGE$", p_up))                                return("bge")
  if (grepl("^B[UÜ]SO$|B[UÜ]RGERRECHTSBEWEGUNG.SOLIDARIT", p_up)) return("bueso")
  if (grepl("GESUNDHEITS.*FORSCHUNG", p_up))                return("gesundheitsforschung")
  if (grepl("^MERA25", p_up))                              return("mera25")
  if (grepl("^PATRIOTEN", p_up))                            return("patrioten")
  if (grepl("^PBC$", p_up))                                return("pbc")
  if (grepl("PARTEI BIBELTREUER CHRISTEN", p_up))          return("pbc")
  if (grepl("^PDV$|^PARTEI DER VERNUNFT", p_up))           return("partei_der_vernunft")
  if (grepl("^DSU$", p_up))                                return("dsu")
  if (grepl("^AB JETZT", p_up))                             return("ab_jetzt")
  if (grepl("^AD.?DEMOKRATEN", p_up))                        return("ad_demokraten")
  if (grepl("^DIB$|^DEMOKRATIE IN BEWEGUNG", p_up))         return("dib")
  if (grepl("^DM$|^DEUTSCHE MITTE", p_up))                   return("dm")
  if (grepl("^MENSCHLICHE WELT", p_up))                      return("menschliche_welt")
  if (grepl("GARTENPARTEI", p_up))                            return("gartenpartei")
  if (grepl("^NATURGESETZ", p_up))                            return("naturgesetz")
  if (grepl("^DA$|^DEMOKRATIEALTERNATIVE", p_up))              return("da")
  if (grepl("^50.?PLUS", p_up))                                return("50plus")
  if (grepl("^GB.?BHE", p_up))                                 return("gb_bhe")
  if (grepl("^B[UÜ]NDNIS.?DKP.?KPD", p_up))                   return("buendnis_dkp_kpd")
  if (grepl("^OFFEN.*SIVE.*D", p_up))                          return("offensive_d")
  if (grepl("^CHR.?L$", p_up))                                 return("chr_l")
  # Fallback: clean to snake_case (warn about potential collisions)
  cleaned <- tolower(p)
  cleaned <- gsub("[^a-z0-9äöüß]+", "_", cleaned)
  cleaned <- gsub("ä", "ae", cleaned)
  cleaned <- gsub("ö", "oe", cleaned)
  cleaned <- gsub("ü", "ue", cleaned)
  cleaned <- gsub("ß", "ss", cleaned)
  cleaned <- gsub("^_|_$", "", cleaned)
  cleaned <- gsub("_+", "_", cleaned)
  message(sprintf("  normalise_party fallback: '%s' -> '%s' (add explicit mapping to avoid collisions)", p, cleaned))
  return(cleaned)
}

## Helper: standardise a cleaned state data frame --------------------------
## Ensures meta columns exist and coerces types. Party columns are kept as-is.
standardise <- function(df) {
  # Ensure meta columns exist
  for (col in meta_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
  }
  # Identify party columns (everything not in meta_cols, other, cdu_csu)
  all_names <- names(df)
  pcols <- setdiff(all_names, c(meta_cols, "other", "cdu_csu"))

  df <- df |>
    mutate(
      ags = as.character(ags),
      election_year = as.integer(election_year),
      state = as.character(state),
      election_date = as.Date(election_date),
      across(c(eligible_voters, number_voters, valid_votes, invalid_votes),
             ~ as.numeric(.x)),
      across(any_of(c(pcols, "other", "cdu_csu")), ~ as.numeric(.x))
    )

  # Reorder: meta_cols, sorted party cols, other, cdu_csu
  pcols_sorted <- sort(pcols)
  final_order <- c(meta_cols, pcols_sorted)
  if ("other" %in% all_names) final_order <- c(final_order, "other")
  if ("cdu_csu" %in% all_names) final_order <- c(final_order, "cdu_csu")
  df <- df |> select(all_of(final_order))
  return(df)
}

## Collector list: each state section appends its result here
all_states <- list()

###############################################################################
####                         THÜRINGEN (16)                                ####
###############################################################################
## 8 elections: 1990, 1994, 1999, 2004, 2009, 2014, 2019, 2024
## Format: XLSX, one file per year, Satzart "G" = Gemeinde rows
## AGS = "16" + kreisnr(3) + gemeindenr(3)
## Kreisfreie Städte may appear in multiple Wahlkreise → aggregate
## 1990 uses DDR-era Kreis codes (3-digit) → skip for now (needs crosswalk)
##
## Column layout groups (anchored on Landesstimmen header position):
## 1994-2009: eligible=col10, wahler=col11, LS_header=col15
## 2014-2019: eligible=col10, wahler=col14, LS_header=col19
## 2024:      eligible=col13, wahler=col17, LS_header=col22

th_dates <- c(
  "1990" = "1990-10-14", "1994" = "1994-10-16", "1999" = "1999-09-12",
  "2004" = "2004-06-13", "2009" = "2009-08-30", "2014" = "2014-09-14",
  "2019" = "2019-10-27", "2024" = "2024-09-01"
)

th_results <- list()

for (yr in names(th_dates)) {
  cat("Processing Thüringen", yr, "...\n")

  if (yr == "1990") {
    cat("  Skipping 1990 (DDR-era codes, needs crosswalk).\n")
    next
  }

  fpath <- here(raw_path, "Thüringen",
                paste0("Thüringen_", yr, "_Landtag.xlsx"))
  raw <- read_excel(fpath, sheet = 1, col_names = FALSE, col_types = "text")
  cnames <- names(raw)

  ## Helper: extract numeric column by index from a filtered data frame
  get_num <- function(df, col_idx) {
    as.numeric(na_if(df[[cnames[col_idx]]], "-"))
  }

  ## --- Locate Landesstimmen header (anchor point) ---
  ## Row 4 always contains the main section headers.
  ## Find the column where "Landesstimmen" appears.
  r4 <- as.character(raw[4, ])
  ls_header_col <- which(grepl("Landesstimmen", r4))[1]
  # Row 5 has "ungültige" / "gültige" directly under Landesstimmen
  r5 <- as.character(raw[5, ])
  ls_ungueltig_col <- ls_header_col      # "ungültige" under Landesstimmen
  ls_gueltig_col   <- ls_header_col + 1  # "gültige" next to it

  ## --- Locate Wahlberechtigte and Wähler ---
  ## These are always a few columns before the Landesstimmen block.
  ## Search row 5 for "berechtigte" (part of "Wahl-berechtigte insgesamt")
  wb_col <- which(grepl("berechtigte", r5))[1]
  # If not found in row 5, check row 4
  if (is.na(wb_col)) wb_col <- which(grepl("berechtigte", r4))[1]

  ## Wähler: search row 4 for "Wähler"
  wahler_col <- which(r4 == "Wähler")[1]
  # 2014+ have "Wähler" in row 4 at a different position; 1994-2009 have it at col 11
  if (is.na(wahler_col)) {
    # Fall back: Wähler is 1 column after Wahlberechtigte in 1994-2009
    wahler_col <- wb_col + 1
  }

  ## --- Locate party name row (row 6 = party_row_idx) ---
  r6 <- as.character(raw[6, ])

  ## --- Filter to Gemeinde-level rows ---
  df <- raw |> filter(.data[[cnames[2]]] == "G")

  ## --- Construct AGS: "16" + kreisnr(3-digit) + gemeindenr(3-digit) ---
  kreisnr  <- as.numeric(df[[cnames[4]]])
  gemeindenr <- as.numeric(df[[cnames[5]]])
  ags_vec <- paste0("16", sprintf("%03d", kreisnr), sprintf("%03d", gemeindenr))

  ## --- Extract admin columns ---
  eligible <- get_num(df, wb_col)
  voters   <- get_num(df, wahler_col)
  valid    <- get_num(df, ls_gueltig_col)
  invalid  <- get_num(df, ls_ungueltig_col)

  ## --- Extract ALL party vote counts (Landesstimmen absolute) ---
  ## Party names in row 6 appear in pairs (WKS, LS) or quads (WKS abs, WKS%, LS abs, LS%).
  ## Second occurrence of each name = Landesstimmen column.
  admin_labels <- c("nr.", "Nr.", "Name", "insgesamt", "Wahlschein",
                    "nach § 23(2) LWO", "Briefwahl-", "Gemeinde/",
                    "bezirk", "Wahlbezirk")
  all_party_names <- unique(r6[!is.na(r6) & !r6 %in% admin_labels])

  party_votes <- list()
  party_col_map <- list()   # std_name → vector of column indices (for K-row extraction)
  for (pname in all_party_names) {
    positions <- which(r6 == pname)
    col_idx <- NA
    if (length(positions) >= 2) {
      ## Default: 2nd occurrence is Landesstimmen.
      ## But verify it's actually LS, not WKS (e.g. Einzelbewerber with 3 WKS cols)
      col_idx <- positions[2]
      sec2 <- r5[col_idx]
      if (!is.na(sec2) && grepl("Wahlkreisstimmen", sec2)) {
        ## 2nd occurrence is WKS — search for a Landesstimmen occurrence
        ls_pos <- positions[!is.na(r5[positions]) & grepl("Landesstimmen", r5[positions])]
        col_idx <- if (length(ls_pos) > 0) ls_pos[1] else NA
      }
    } else if (length(positions) == 1 && positions[1] > ls_gueltig_col) {
      ## Verify column is in Landesstimmen section (not WKS-only, e.g. Einzelbewerber)
      sec <- r5[positions[1]]
      if (!is.na(sec) && grepl("Wahlkreisstimmen", sec)) {
        ## WKS-only column (e.g. Einzelbewerber) — skip
      } else {
        col_idx <- positions[1]
      }
    }
    if (!is.na(col_idx)) {
      std_name <- normalise_party(pname)
      party_col_map[[std_name]] <- c(party_col_map[[std_name]], col_idx)
      # If duplicate normalised name, aggregate
      if (std_name %in% names(party_votes)) {
        existing <- party_votes[[std_name]]
        new_v <- get_num(df, col_idx)
        new_v[is.na(new_v)] <- 0
        existing[is.na(existing)] <- 0
        party_votes[[std_name]] <- existing + new_v
      } else {
        party_votes[[std_name]] <- get_num(df, col_idx)
      }
    }
  }

  ## --- Build result (pre-aggregation, one row per G entry) ---
  wkr_vec <- as.character(df[[cnames[3]]])   # Wahlkreisnr for Briefwahl allocation
  result <- tibble(
    ags = ags_vec,
    wkr = wkr_vec,
    election_year = as.integer(yr),
    state = "16",
    election_date = as.Date(th_dates[yr]),
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid,
    invalid_votes = invalid
  )

  # Add ALL party vote counts with _n suffix
  for (std_name in names(party_votes)) {
    result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
  }

  # Other = valid - sum(all named party counts)
  mapped_sum <- rep(0, nrow(result))
  for (std_name in names(party_votes)) {
    v <- party_votes[[std_name]]
    v[is.na(v)] <- 0
    mapped_sum <- mapped_sum + v
  }
  result$other_n <- pmax(valid - mapped_sum, 0, na.rm = TRUE)

  ## --- Allocate Briefwahl from Wahlkreis (K) to Gemeinde (G) level ---
  ## Some municipalities lack their own Briefwahlbezirk, so G-level data
  ## excludes centrally-counted postal votes.  K (Wahlkreis) rows contain
  ## full totals.  Proportionally distribute the residual per Wahlkreis.
  count_cols <- c(paste0(names(party_votes), "_n"), "other_n")
  kdf <- raw |> filter(.data[[cnames[2]]] == "K")
  if (nrow(kdf) > 0) {
    k_wkr <- as.character(kdf[[cnames[3]]])

    ## Extract K-level party counts using same column mapping
    k_party <- list()
    for (std_name in names(party_col_map)) {
      k_vals <- rep(0, nrow(kdf))
      for (cidx in party_col_map[[std_name]]) {
        v <- as.numeric(na_if(kdf[[cnames[cidx]]], "-"))
        v[is.na(v)] <- 0
        k_vals <- k_vals + v
      }
      k_party[[std_name]] <- k_vals
    }
    k_valid   <- as.numeric(na_if(kdf[[cnames[ls_gueltig_col]]], "-"))
    k_invalid <- as.numeric(na_if(kdf[[cnames[ls_ungueltig_col]]], "-"))
    k_voters  <- as.numeric(na_if(kdf[[cnames[wahler_col]]], "-"))

    for (ki in seq_along(k_wkr)) {
      g_idx <- which(result$wkr == k_wkr[ki])
      if (length(g_idx) == 0) next
      g_vv <- sum(result$valid_votes[g_idx], na.rm = TRUE)
      if (g_vv == 0) next
      w <- result$valid_votes[g_idx] / g_vv   # proportional weights

      ## valid_votes
      vv_r <- k_valid[ki] - g_vv
      if (!is.na(vv_r) && vv_r > 0)
        result$valid_votes[g_idx] <- result$valid_votes[g_idx] + vv_r * w
      ## invalid_votes
      iv_r <- k_invalid[ki] - sum(result$invalid_votes[g_idx], na.rm = TRUE)
      if (!is.na(iv_r) && iv_r > 0)
        result$invalid_votes[g_idx] <- result$invalid_votes[g_idx] + iv_r * w
      ## number_voters
      nv_r <- k_voters[ki] - sum(result$number_voters[g_idx], na.rm = TRUE)
      if (!is.na(nv_r) && nv_r > 0)
        result$number_voters[g_idx] <- result$number_voters[g_idx] + nv_r * w
      ## party counts
      for (std_name in names(k_party)) {
        col_n <- paste0(std_name, "_n")
        p_r <- k_party[[std_name]][ki] - sum(result[[col_n]][g_idx], na.rm = TRUE)
        if (!is.na(p_r) && p_r > 0)
          result[[col_n]][g_idx] <- result[[col_n]][g_idx] + p_r * w
      }
      ## other_n
      k_mapped <- 0
      for (std_name in names(k_party)) k_mapped <- k_mapped + k_party[[std_name]][ki]
      k_other <- max(k_valid[ki] - k_mapped, 0)
      o_r <- k_other - sum(result$other_n[g_idx], na.rm = TRUE)
      if (!is.na(o_r) && o_r > 0)
        result$other_n[g_idx] <- result$other_n[g_idx] + o_r * w
    }
  }

  ## --- Aggregate duplicate AGS (kreisfreie Städte split across Wahlkreise) ---
  result <- result |>
    select(-wkr) |>
    group_by(ags) |>
    summarise(
      election_year = first(election_year),
      state = first(state),
      election_date = first(election_date),
      eligible_voters = sum(eligible_voters, na.rm = TRUE),
      number_voters = sum(number_voters, na.rm = TRUE),
      valid_votes = sum(valid_votes, na.rm = TRUE),
      invalid_votes = sum(invalid_votes, na.rm = TRUE),
      across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  ## --- Convert counts to shares ---
  result <- result |>
    mutate(turnout = number_voters / eligible_voters)
  for (std_name in c(names(party_votes), "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  ## --- Standardise and validate ---
  result <- standardise(result)
  n_muni <- nrow(result)
  total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
  cat("  TH", yr, ":", n_muni, "municipalities,",
      format(total_eligible, big.mark = ","), "eligible voters\n")

  th_results[[yr]] <- result
}

all_states[["th"]] <- bind_rows(th_results)
cat("Thüringen total:", nrow(all_states[["th"]]), "rows\n\n")


###############################################################################
####                      SACHSEN-ANHALT (15)                              ####
###############################################################################
## 8 elections: 1990, 1994, 1998, 2002, 2006, 2011, 2016, 2021
## Three format variants:
##   1990-2006: Wahlbezirk-level, AGS in col3, eligible=col8, voters=col9.
##              Two vote sections: Personenstimmen + Parteienstimmen.
##              Use Parteienstimmen (proportional vote). Col7 = Wahllokalart.
##              "Gültige Parteienstimmen" column anchors the party block.
##   2011-2016: Wahlbezirk-level with Verbandsgemeinde hierarchy.
##              AGS in col3, eligible=col12, voters=col13.
##              Two vote sections: Erststimmen + Zweitstimmen.
##              Use Zweitstimmen. Col11 = Wahllokalart.
##   2021:      Gemeinde-level (sheet "Gemeinden"), skip 7 header rows.
##              AGS in col1, eligible=col4, voters=col8.
##              Filter "Insgesamt" rows with 8-digit AGS. Zweitstimmen at cols 30+.
## All years: aggregate Wahlbezirk → Gemeinde by AGS (sum counts).

st_dates <- c(
  "1990" = "1990-10-14", "1994" = "1994-06-26", "1998" = "1998-04-26",
  "2002" = "2002-04-21", "2006" = "2006-03-26", "2011" = "2011-03-20",
  "2016" = "2016-03-13", "2021" = "2021-06-06"
)

st_results <- list()

for (yr in names(st_dates)) {
  cat("Processing Sachsen-Anhalt", yr, "...\n")

  fpath <- here(raw_path, "Sachsen-Anhalt",
                paste0("Sachsen-Anhalt_", yr, "_Landtagswahl.xlsx"))

  if (yr == "2021") {
    ## ---- 2021: Gemeinde-level, sheet "Gemeinden" ----
    ## Read with headers to get party names from row 5
    raw_full <- read_excel(fpath, sheet = "Gemeinden", col_names = FALSE,
                           col_types = "text", skip = 0)
    r5 <- as.character(raw_full[5, ])

    raw <- raw_full[8:nrow(raw_full), ]
    cnames <- names(raw)

    ## Filter to "Insgesamt" rows with 8-digit AGS
    df <- raw |> filter(.data[[cnames[3]]] == "Insgesamt",
                        nchar(.data[[cnames[1]]]) == 8)

    ## Helper: safe numeric conversion
    safe_num <- function(x) as.numeric(na_if(na_if(x, "-"), "x"))

    ## Zweitstimmen block: cols 31 (Gültige) through end
    ## Party columns start at 32
    zs_valid_col <- 31
    party_start <- 32
    party_end <- length(r5)

    ## Extract ALL party vote counts from Zweitstimmen block
    ## Map raw header names to standardised names
    ## ST 2021 headers: CDU, AfD, DIE LINKE, SPD, GRÜNE, FDP, FREIE (WÄHLER),
    ## NPD, Tier- (schutzpartei), Tier- (schutzallianz), LKR, Die PARTEI,
    ## Garten- (partei), FBM, TIERSCHUTZ (hier!), dieBasis, Klimaliste, ÖDP,
    ## Die Humanisten, Gesundheits- (forschung), PIRATEN, WiR2020
    st_2021_name_map <- c(
      "32" = "CDU", "33" = "AfD", "34" = "DIE LINKE", "35" = "SPD",
      "36" = "GRÜNE", "37" = "FDP", "38" = "FREIE WÄHLER", "39" = "NPD",
      "40" = "Tierschutzpartei", "41" = "Tierschutzallianz",
      "42" = "LKR", "43" = "Die PARTEI", "44" = "Gartenpartei",
      "45" = "FBM", "46" = "TIERSCHUTZ hier!", "47" = "dieBasis",
      "48" = "Klimaliste", "49" = "ÖDP", "50" = "Die Humanisten",
      "51" = "Gesundheitsforschung", "52" = "PIRATEN", "53" = "WiR2020"
    )

    party_votes <- list()
    for (ci_str in names(st_2021_name_map)) {
      ci <- as.integer(ci_str)
      raw_name <- st_2021_name_map[ci_str]
      std_name <- normalise_party(raw_name)
      v <- safe_num(df[[cnames[ci]]])
      if (std_name %in% names(party_votes)) {
        existing <- party_votes[[std_name]]
        existing[is.na(existing)] <- 0
        v[is.na(v)] <- 0
        party_votes[[std_name]] <- existing + v
      } else {
        party_votes[[std_name]] <- v
      }
    }

    result <- tibble(
      ags            = as.character(df[[cnames[1]]]),
      election_year  = as.integer(2021),
      state          = "15",
      election_date  = as.Date(st_dates["2021"]),
      eligible_voters = safe_num(df[[cnames[4]]]),
      number_voters  = safe_num(df[[cnames[8]]]),
      invalid_votes  = safe_num(df[[cnames[30]]]),
      valid_votes    = safe_num(df[[cnames[31]]])
    )

    for (std_name in names(party_votes)) {
      result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
    }

    ## Other = valid - sum(all named party counts)
    mapped_sum <- rep(0, nrow(result))
    for (std_name in names(party_votes)) {
      v <- party_votes[[std_name]]; v[is.na(v)] <- 0
      mapped_sum <- mapped_sum + v
    }
    result$other_n <- pmax(result$valid_votes - mapped_sum, 0, na.rm = TRUE)

    ## Convert counts to shares
    result <- result |> mutate(turnout = number_voters / eligible_voters)
    for (std_name in c(names(party_votes), "other")) {
      result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-ends_with("_n"))

  } else {
    ## ---- 1990-2016: Wahlbezirk-level, single sheet ----
    raw <- read_excel(fpath, sheet = 1, col_names = FALSE, col_types = "text")
    cnames <- names(raw)

    ## Helper: safe numeric
    get_num <- function(df, col_idx) {
      as.numeric(na_if(na_if(df[[cnames[col_idx]]], "-"), "x"))
    }

    ## Read header row (row 4 contains all column headers)
    r4 <- as.character(raw[4, ])

    ## --- Determine format variant and column positions ---
    if (yr %in% c("2011", "2016")) {
      ## 2011-2016: VG columns shift everything right
      ags_col     <- 3
      eligible_col <- 12
      voters_col  <- 13
      lokal_col   <- 11

      ## Find Zweitstimmen block: "Ungültige Zweit-stimmen" column
      zs_invalid_col <- which(grepl("Zweit.*stimmen", r4) & grepl("ng.ltig", r4))[1]
      zs_valid_col   <- zs_invalid_col + 1

      ## Party columns start right after gültige Zweitstimmen
      party_start <- zs_valid_col + 1
      ## Find last party col: it's either last col or cols before "Gemeinde" at end
      gemeinde_end <- which(r4 == "Gemeinde")
      gemeinde_end <- gemeinde_end[gemeinde_end > zs_valid_col]
      if (length(gemeinde_end) > 0) {
        party_end <- gemeinde_end[1] - 1
      } else {
        party_end <- length(r4)
      }

    } else {
      ## 1990-2006: simpler layout
      ags_col     <- 3
      eligible_col <- 8
      voters_col  <- 9
      lokal_col   <- 7

      ## Find Parteienstimmen block via "Partei(en)" keyword in header
      ## "Ungültige Parteien-stimmen" = invalid party votes column
      ## "Gültige Parteien-stimmen" = valid party votes column
      zs_invalid_col <- which(grepl("Parteien|Partei", r4) &
                                grepl("ng.ltig", r4))[1]
      zs_valid_col   <- which(grepl("Parteien|Partei", r4) &
                                grepl("[Gg].ltig", r4) &
                                !grepl("ng.ltig", r4))[1]

      party_start <- zs_valid_col + 1
      ## Find "Gemeinde" column at end
      gemeinde_end <- which(r4 == "Gemeinde")
      gemeinde_end <- gemeinde_end[gemeinde_end > zs_valid_col]
      if (length(gemeinde_end) > 0) {
        party_end <- gemeinde_end[1] - 1
      } else {
        party_end <- length(r4)
      }
    }

    ## --- Filter to data rows (skip header rows 1-6) ---
    df <- raw[7:nrow(raw), ]
    ## Remove empty/total rows (keep only rows with valid AGS starting with "15")
    df <- df |> filter(grepl("^15\\d{6}$", .data[[cnames[ags_col]]]))

    ## --- Extract columns ---
    ags_vec  <- as.character(df[[cnames[ags_col]]])
    eligible <- get_num(df, eligible_col)
    voters   <- get_num(df, voters_col)
    valid    <- get_num(df, zs_valid_col)
    invalid  <- get_num(df, zs_invalid_col)

    ## --- Extract ALL party vote counts from party block ---
    party_votes <- list()
    for (ci in party_start:party_end) {
      raw_name <- r4[ci]
      if (is.na(raw_name) || raw_name == "") next
      ## Clean word-wrap hyphens from Excel column width
      raw_name <- gsub("-\n", "", raw_name)
      raw_name <- gsub("\r\n", "", raw_name)
      raw_name <- trimws(raw_name)
      if (raw_name == "" || grepl("^(Gemeinde|zusammen|insgesamt)$", raw_name,
                                  ignore.case = TRUE)) next
      std_name <- normalise_party(raw_name)
      v <- get_num(df, ci)
      if (std_name %in% names(party_votes)) {
        existing <- party_votes[[std_name]]
        existing[is.na(existing)] <- 0
        v[is.na(v)] <- 0
        party_votes[[std_name]] <- existing + v
      } else {
        party_votes[[std_name]] <- v
      }
    }

    ## --- Build result ---
    result <- tibble(
      ags = ags_vec,
      election_year = as.integer(yr),
      state = "15",
      election_date = as.Date(st_dates[yr]),
      eligible_voters = eligible,
      number_voters = voters,
      valid_votes = valid,
      invalid_votes = invalid
    )

    ## Add ALL party vote counts
    for (std_name in names(party_votes)) {
      result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
    }

    ## Other = valid - sum(all named party counts)
    mapped_sum <- rep(0, nrow(result))
    for (std_name in names(party_votes)) {
      v <- party_votes[[std_name]]; v[is.na(v)] <- 0
      mapped_sum <- mapped_sum + v
    }
    result$other_n <- pmax(valid - mapped_sum, 0, na.rm = TRUE)

    ## --- Aggregate Wahlbezirk → Gemeinde (sum all counts by AGS) ---
    count_cols <- c(paste0(names(party_votes), "_n"), "other_n")
    result <- result |>
      group_by(ags) |>
      summarise(
        election_year   = first(election_year),
        state           = first(state),
        election_date   = first(election_date),
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## --- Convert counts to shares ---
    result <- result |> mutate(turnout = number_voters / eligible_voters)
    for (std_name in c(names(party_votes), "other")) {
      result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-ends_with("_n"))
  }

  ## --- Standardise and validate ---
  result <- standardise(result)
  n_muni <- nrow(result)
  total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
  cat("  ST", yr, ":", n_muni, "municipalities,",
      format(total_eligible, big.mark = ","), "eligible voters\n")

  st_results[[yr]] <- result
}

all_states[["st"]] <- bind_rows(st_results)
cat("Sachsen-Anhalt total:", nrow(all_states[["st"]]), "rows\n\n")


###############################################################################
####                          SACHSEN (14)                                  ####
###############################################################################
## 8 elections: 1990, 1994, 1999, 2004, 2009, 2014, 2019, 2024
## Four format variants:
##   1990-1994: Separate Listenstimmen files. Col1 has LW-codes.
##              Filter to LW##-14XXXXXXX rows (8-digit AGS in the code).
##              Col3=eligible, col4=voters, col5=invalid, col6=valid, col7+=parties.
##              Party names in row 5. Headers in rows 4-7, data starts row 8+.
##   1999-2009: Combined Direkt+Listenstimmen files. Same column layout.
##              Find "Listenstimmen" marker row, extract municipality data below it.
##   2014:      Sheet "LW14_Ergebnisse_GE_TG". Named headers in row 1.
##              Col4=Ebene (GE/TG), col5=AGS. _2 suffix = Listenstimmen.
##              Filter to Ebene=="GE", or aggregate TG→GE by 8-digit AGS.
##   2019-2024: Sheet "LW##_endgErgebnisse_GE&TG". Named headers in row 1.
##              Col5=Ebene (GE/TG), col6=AGS. _2 suffix = Listenstimmen.
## All years: aggregate split municipalities by 8-digit AGS.

sn_dates <- c(
  "1990" = "1990-10-14", "1994" = "1994-09-11", "1999" = "1999-09-19",
  "2004" = "2004-09-19", "2009" = "2009-08-30", "2014" = "2014-08-31",
  "2019" = "2019-09-01", "2024" = "2024-09-01"
)

sn_results <- list()

for (yr in names(sn_dates)) {
  cat("Processing Sachsen", yr, "...\n")

  if (yr %in% c("2014", "2019", "2024")) {
    ## ---- 2014-2024: New structured format with named columns ----
    if (yr == "2014") {
      sheet_name <- "LW14_Ergebnisse_GE_TG"
    } else if (yr == "2019") {
      sheet_name <- "LW19_endgErgebnisse_GE&TG"
    } else {
      sheet_name <- "LW24_endgErgebnisse_GE&TG"
    }

    fpath <- here(raw_path, "Sachsen",
                  paste0("Sachsen_", yr, "_Landtagswahl.xlsx"))
    raw <- read_excel(fpath, sheet = sheet_name, col_names = FALSE,
                      col_types = "text")
    cnames_raw <- as.character(raw[1, ])  # Header names in row 1

    ## Determine column positions by name
    if (yr == "2014") {
      ebene_col <- which(cnames_raw == "Ebene")[1]  # col 4
      ags_col   <- which(cnames_raw == "AGS")[1]    # col 5
    } else {
      ## 2019/2024: col2 is "Ebene" (WK-level), col5 is "Ebene" (GE/TG-level)
      ebene_col <- which(cnames_raw == "Ebene")[2]  # second "Ebene" = col 5
      ags_col   <- which(cnames_raw == "AGS")[1]    # col 6
    }
    eligible_col <- which(cnames_raw == "Wahlberechtigte")[1]
    voters_col   <- which(cnames_raw == "W\u00e4hler")[1]
    zs_inv_col   <- which(cnames_raw == "ung\u00fcltige_2")[1]
    zs_val_col   <- which(cnames_raw == "g\u00fcltige_2")[1]

    cnames <- names(raw)

    ## Include both GE (Gemeinde) and TG (Teilgemeinde) rows.
    ## TG rows have 9-digit AGS (e.g., 145243301); truncate to 8 digits
    ## for aggregation. The 4 largest cities (Chemnitz, Dresden, Leipzig,
    ## Zwickau) only appear as TG rows, not GE rows.
    df <- raw[2:nrow(raw), ]
    df <- df |> filter(.data[[cnames[ebene_col]]] %in% c("GE", "TG"))
    ## Truncate AGS to 8 digits for TG rows
    df[[cnames[ags_col]]] <- substr(df[[cnames[ags_col]]], 1, 8)

    safe_num <- function(x) as.numeric(na_if(na_if(x, "-"), "x"))

    result <- tibble(
      ags           = as.character(df[[cnames[ags_col]]]),
      election_year = as.integer(yr),
      state         = "14",
      election_date = as.Date(sn_dates[yr]),
      eligible_voters = safe_num(df[[cnames[eligible_col]]]),
      number_voters = safe_num(df[[cnames[voters_col]]]),
      invalid_votes = safe_num(df[[cnames[zs_inv_col]]]),
      valid_votes   = safe_num(df[[cnames[zs_val_col]]])
    )

    ## Extract ALL party counts from _2 columns (Listenstimmen)
    ## Find all column names ending in _2 that are party names
    party_votes <- list()
    for (ci in seq_along(cnames_raw)) {
      nm <- cnames_raw[ci]
      if (is.na(nm)) next
      if (!grepl("_2$", nm)) next
      ## Skip admin columns
      if (grepl("^(ung.ltig|g.ltig)", nm, ignore.case = TRUE)) next
      raw_name <- sub("_2$", "", nm)
      std_name <- normalise_party(raw_name)
      v <- safe_num(df[[cnames[ci]]])
      if (std_name %in% names(party_votes)) {
        existing <- party_votes[[std_name]]; existing[is.na(existing)] <- 0
        v[is.na(v)] <- 0
        party_votes[[std_name]] <- existing + v
      } else {
        party_votes[[std_name]] <- v
      }
    }

    for (std_name in names(party_votes)) {
      result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
    }

    ## Compute other
    mapped_sum <- rep(0, nrow(result))
    for (std_name in names(party_votes)) {
      v <- party_votes[[std_name]]; v[is.na(v)] <- 0
      mapped_sum <- mapped_sum + v
    }
    result$other_n <- pmax(result$valid_votes - mapped_sum, 0, na.rm = TRUE)

    ## Aggregate duplicate AGS (cities split across Wahlkreise)
    count_cols <- c(paste0(names(party_votes), "_n"), "other_n")
    result <- result |>
      group_by(ags) |>
      summarise(
        election_year = first(election_year), state = first(state),
        election_date = first(election_date),
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Convert to shares
    result <- result |> mutate(turnout = number_voters / eligible_voters)
    for (std_name in c(names(party_votes), "other")) {
      result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-ends_with("_n"))

  } else {
    ## ---- 1990-2009: Statistical office format ----
    ## Determine file path
    if (yr == "1990") {
      fpath <- here(raw_path, "Sachsen",
                    "Sachsen_1990_Landtagswahl_Listenstimmen.xlsx")
    } else if (yr == "1994") {
      fpath <- here(raw_path, "Sachsen",
                    "Sachsen_1994_Landtagswahl_Listenstimme.xlsx")
    } else {
      fpath <- here(raw_path, "Sachsen",
                    paste0("Sachsen_", yr, "_Landtagswahl.xlsx"))
    }

    raw <- read_excel(fpath, sheet = 1, col_names = FALSE, col_types = "text")
    cnames <- names(raw)

    ## Header row 5 contains party names
    r5 <- as.character(raw[5, ])

    ## For combined files (1999-2009), find "Listenstimmen" marker and offset data
    if (yr %in% c("1999", "2004", "2009")) {
      ls_marker <- which(raw[[1]] == "Listenstimmen")
      if (length(ls_marker) == 0) stop("Could not find 'Listenstimmen' marker for ", yr)
      data_start <- ls_marker[1] + 1  # State total row is right after marker
    } else {
      ## 1990/1994: separate files, data starts after header (row 8+)
      data_start <- 8
    }

    ## Find valid column positions from row 5
    ## Col3=eligible, col4=voters, col5=invalid, col6=valid, col7+=parties
    eligible_col <- which(grepl("Wahlberechtigte", as.character(raw[4, ])))[1]
    if (is.na(eligible_col)) eligible_col <- 3
    voters_col   <- eligible_col + 1  # voters = next col after eligible
    invalid_col  <- which(grepl("Ung.ltig", r5))[1]
    if (is.na(invalid_col)) invalid_col <- 5
    valid_col    <- which(grepl("G.ltig", r5) & !grepl("Ung.ltig", r5))[1]
    if (is.na(valid_col)) valid_col <- 6
    party_start  <- valid_col + 1

    get_num <- function(df, col_idx) {
      as.numeric(na_if(na_if(df[[cnames[col_idx]]], "-"), "x"))
    }

    ## Extract municipality-level rows
    ## Municipality codes: LW##-14XXXXXX (8 digits after the state prefix)
    df <- raw[data_start:nrow(raw), ]
    yr2 <- substr(yr, 3, 4)
    ## Pattern: LW90-14XXXXXXX or LW99-14XXXXXXX-X (split cities)
    ags_pattern <- paste0("^LW", yr2, "-14[0-9]{6}")
    df <- df |> filter(grepl(ags_pattern, .data[[cnames[1]]]))

    ## Extract 8-digit AGS from col1 (strip LW##- prefix and any -X suffix)
    ags_vec <- sub(paste0("^LW", yr2, "-"), "", df[[cnames[1]]])
    ags_vec <- substr(ags_vec, 1, 8)  # Take first 8 digits (drops -1, -2 suffixes)

    eligible <- get_num(df, eligible_col)
    voters   <- get_num(df, voters_col)
    valid    <- get_num(df, valid_col)
    invalid  <- get_num(df, invalid_col)

    ## Extract ALL party votes from party block
    party_votes <- list()

    ## Find last party column (before any non-party cols)
    party_end <- ncol(raw)
    for (ci in party_start:ncol(raw)) {
      if (is.na(r5[ci]) || r5[ci] == "") {
        party_end <- ci - 1
        break
      }
    }

    for (ci in party_start:party_end) {
      raw_name <- r5[ci]
      if (is.na(raw_name) || raw_name == "") next
      raw_name <- gsub("-\n", "", raw_name)
      raw_name <- gsub("\r\n", "", raw_name)
      raw_name <- trimws(raw_name)
      if (raw_name == "") next
      std_name <- normalise_party(raw_name)
      v <- get_num(df, ci)
      if (std_name %in% names(party_votes)) {
        existing <- party_votes[[std_name]]; existing[is.na(existing)] <- 0
        v[is.na(v)] <- 0
        party_votes[[std_name]] <- existing + v
      } else {
        party_votes[[std_name]] <- v
      }
    }

    ## Build result
    result <- tibble(
      ags = ags_vec,
      election_year = as.integer(yr),
      state = "14",
      election_date = as.Date(sn_dates[yr]),
      eligible_voters = eligible,
      number_voters = voters,
      valid_votes = valid,
      invalid_votes = invalid
    )

    for (std_name in names(party_votes)) {
      result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
    }

    ## Other
    mapped_sum <- rep(0, nrow(result))
    for (std_name in names(party_votes)) {
      v <- party_votes[[std_name]]; v[is.na(v)] <- 0
      mapped_sum <- mapped_sum + v
    }
    result$other_n <- pmax(valid - mapped_sum, 0, na.rm = TRUE)

    ## Aggregate split municipalities (same AGS from multiple Wahlkreise)
    count_cols <- c(paste0(names(party_votes), "_n"), "other_n")
    result <- result |>
      group_by(ags) |>
      summarise(
        election_year = first(election_year), state = first(state),
        election_date = first(election_date),
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Convert to shares
    result <- result |> mutate(turnout = number_voters / eligible_voters)
    for (std_name in c(names(party_votes), "other")) {
      result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-ends_with("_n"))
  }

  ## Standardise and validate
  result <- standardise(result)
  n_muni <- nrow(result)
  total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
  cat("  SN", yr, ":", n_muni, "municipalities,",
      format(total_eligible, big.mark = ","), "eligible voters\n")
  sn_results[[yr]] <- result
}

all_states[["sn"]] <- bind_rows(sn_results)
cat("Sachsen total:", nrow(all_states[["sn"]]), "rows\n\n")


###############################################################################
####                       BRANDENBURG (12)                                ####
###############################################################################
## 7 elections: 1994, 1999, 2004, 2009, 2014, 2019, 2024
## (1990 file is PDF → skip; 1994/1999 use OCR CSVs)
##
## Five format variants:
##   2004: XLS, 44 Tab5-Wk## sheets (one per Wahlkreis). Each municipality has
##         4 rows: E Anz., E %, Z Anz., Z %. Krs(col3)+Gem(col4) → AGS.
##         Party names in row 5 (cols 13-28). Briefwahl rows in each sheet.
##         kfS split across multiple Wahlkreise → aggregate by AGS.
##   2009: XLS, per-Landkreis sheets (5.1-5.14) for Zweitstimmen + kreisfreie
##         Städte in sheets 4.1-4.4. Names only (no AGS) → match to 2014 ref.
##         Party names in row 4 (cols 5-17). Briefwahl/county total rows mixed in.
##   2014: XLSX, two Zweitstimmen Gemeinden sheets (amtsfrei 5-digit codes,
##         amtsangehörig 8-digit ARS codes). kfS NOT in file → supplemented
##         from existing API data. Briefwahl for amtsangehörig in Amt totals →
##         allocate proportionally.
##   2019: XLSX, sheet "Brandenburg_Landtagswahl_A_2" (Zweistimme).
##         Rows tagged by Gebiet type. ARS in col2 (8 or 10+digits with space).
##         AGS = first 8 digits. Amt rows contain Briefwahl → allocate.
##   2024: XLSX, sheet 4 = Zweitstimme polling-district level.
##         12-digit ARS. Amt-level Briefwahl rows. Reuse 03_state_2022-24.R logic.

bb_dates <- c(
  "1990" = "1990-10-14",
  "1994" = "1994-09-11",
  "1999" = "1999-09-05",
  "2004" = "2004-09-19", "2009" = "2009-09-27", "2014" = "2014-09-14",
  "2019" = "2019-09-01", "2024" = "2024-09-22"
)

bb_results <- list()

for (yr in names(bb_dates)) {
  cat("Processing Brandenburg", yr, "...\n")

  fpath <- here(raw_path, "Brandenburg",
                paste0("Brandenburg_", yr, "_Landtagswahl.",
                       ifelse(yr %in% c("2004", "2009"), "xls", "xlsx")))

  safe_num <- function(x) as.numeric(na_if(na_if(x, "-"), "x"))

  if (yr == "1990") {
    ## ==================================================================
    ## 1990: OCR-extracted CSV (from scanned PDF, see 00_bb_1990_extract.py)
    ## ==================================================================
    ocr_path <- here(raw_path, "Brandenburg", "bb_1990_ocr.csv")
    raw90 <- read.csv(ocr_path, colClasses = "character")

    ## Map OCR column names to standard party names
    ## In 1990, Bündnis 90 and Grüne ran as separate parties (merger was 1993)
    party_map <- c(
      spd = "spd", cdu = "cdu", pds_ll = "linke_pds",
      buendnis_90 = "buendnis_90", fdp = "fdp",
      chrl = "chr_l", dbu = "dbu", dfp = "dfp", dsu = "dsu",
      gruene = "gruene", rep = "rep", domowina = "domowina", npd = "npd"
    )

    result <- tibble(
      ags             = raw90$ags,
      eligible_voters = as.numeric(raw90$eligible_voters),
      number_voters   = as.numeric(raw90$number_voters),
      invalid_votes   = as.numeric(raw90$invalid_votes),
      valid_votes     = as.numeric(raw90$valid_votes)
    )

    ## Add party vote counts and shares
    for (ocr_col in names(party_map)) {
      std_name <- party_map[[ocr_col]]
      votes <- as.numeric(raw90[[ocr_col]])
      votes[is.na(votes)] <- 0
      result[[paste0(std_name, "_n")]] <- votes
      result[[std_name]] <- votes / result$valid_votes
    }

    ## Compute other
    mapped_n_cols <- paste0(unique(party_map), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE),
        other   = other_n / valid_votes
      )

    ## Add metadata
    result <- result |>
      mutate(
        election_year = 1990L,
        state = "12",
        election_date = as.Date(bb_dates["1990"]),
        turnout = number_voters / eligible_voters,
        cdu_csu = cdu
      ) |>
      select(-ends_with("_n"))

  } else if (yr == "1994") {
    ## ==================================================================
    ## 1994: OCR-extracted CSV (from scanned PDF, see 00_bb_1994_ocr.py)
    ## ==================================================================
    ocr_path <- here(raw_path, "Brandenburg", "bb_1994_ocr.csv")
    raw94 <- read.csv(ocr_path, colClasses = "character")

    ## Fix OCR-misread AGS codes (single-digit errors in scanned PDF)
    ## Verified against PDF pages and BBSR crosswalk/federal data
    ocr_ags_fixes <- c(
      "12063146" = "12063148",  # Ketzin, Stadt (PDF p138 confirms)
      "12064226" = "12064228",  # Hoppegarten/Mü.
      "12064246" = "12064248",  # Klein Neuendorf
      "12065162" = "12065152",  # Kappe (EV=151 matches federal)
      "12067025" = "12067028",  # Bahro
      "12067045" = "12067048",  # Birkholz
      "12068049" = "12068044",  # Bork/Lellichow (PDF p40 confirms)
      "12068309" = "12068300",  # Luhme (federal EV=128 ≈ OCR 127)
      "12073126" = "12073128"   # Eickstedt
    )
    fix_mask <- raw94$ags %in% names(ocr_ags_fixes)
    if (any(fix_mask)) {
      raw94$ags[fix_mask] <- ocr_ags_fixes[raw94$ags[fix_mask]]
      cat("  Fixed", sum(fix_mask), "OCR-misread AGS codes in BB 1994\n")
    }

    ## Map OCR column names to standard party names
    party_map <- c(
      spd = "spd", cdu = "cdu", pds = "linke_pds",
      fdp = "fdp", gruene_b90 = "gruene", bfwg = "freie_wahler",
      rep = "rep", eb = "einzelbewerber"
    )

    result <- tibble(
      ags             = raw94$ags,
      eligible_voters = as.numeric(raw94$eligible_voters),
      number_voters   = as.numeric(raw94$number_voters),
      invalid_votes   = as.numeric(raw94$invalid_votes),
      valid_votes     = as.numeric(raw94$valid_votes)
    )

    ## Add party vote counts and shares
    for (ocr_col in names(party_map)) {
      std_name <- party_map[[ocr_col]]
      votes <- as.numeric(raw94[[ocr_col]])
      votes[is.na(votes)] <- 0
      result[[paste0(std_name, "_n")]] <- votes
      result[[std_name]] <- votes / result$valid_votes
    }

    ## Compute other (buerger, dsu, graue, kpd, odp, uwvb)
    mapped_n_cols <- paste0(unique(party_map), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE),
        other   = other_n / valid_votes
      )

    ## Add metadata
    result <- result |>
      mutate(
        election_year = 1994L,
        state = "12",
        election_date = as.Date(bb_dates["1994"]),
        turnout = number_voters / eligible_voters,
        cdu_csu = cdu
      ) |>
      select(-ends_with("_n"))

  } else if (yr == "1999") {
    ## ==================================================================
    ## 1999: OCR-extracted CSV (from scanned PDF, see 00_bb_1999_ocr.py)
    ## ==================================================================
    ocr_path <- here(raw_path, "Brandenburg", "bb_1999_ocr.csv")
    raw99 <- read.csv(ocr_path, colClasses = "character")

    ## Map OCR column names to standard party names
    party_map <- c(
      spd = "spd", cdu = "cdu", pds = "linke_pds",
      bfwg = "freie_wahler", bfb = "bfb", gruene_b90 = "gruene",
      buerger = "buerger", dvu = "dvu", fdp = "fdp",
      npd = "npd", eb = "einzelbewerber"
    )

    result <- tibble(
      ags             = raw99$ags,
      eligible_voters = as.numeric(raw99$eligible_voters),
      number_voters   = as.numeric(raw99$number_voters),
      invalid_votes   = as.numeric(raw99$invalid_votes),
      valid_votes     = as.numeric(raw99$valid_votes)
    )

    ## Add party vote counts and shares
    for (ocr_col in names(party_map)) {
      std_name <- party_map[[ocr_col]]
      votes <- as.numeric(raw99[[ocr_col]])
      votes[is.na(votes)] <- 0
      result[[paste0(std_name, "_n")]] <- votes
      result[[std_name]] <- votes / result$valid_votes
    }

    ## Compute other
    mapped_n_cols <- paste0(unique(party_map), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE),
        other   = other_n / valid_votes
      )

    ## Add metadata
    result <- result |>
      mutate(
        election_year = 1999L,
        state = "12",
        election_date = as.Date(bb_dates["1999"]),
        turnout = number_voters / eligible_voters,
        cdu_csu = cdu
      ) |>
      select(-ends_with("_n"))

  } else if (yr == "2004") {
    ## ==================================================================
    ## 2004: 44 Tab5-Wk## sheets, 4 rows per municipality (E Anz, E %, Z Anz, Z %)
    ## ==================================================================
    ## Structure per sheet:
    ##   Row 5: party names in cols 13-28
    ##   Row 9+: WK header row (col3 = WK name) followed by WK-level E/Z
    ##   Identifier rows: col3=Kreis(2-digit), col4=Gem(3-digit), col6=name
    ##   Immediately after: E Anz, E %, Z Anz, Z % rows (col7=E/Z, col8=Anz./%)
    ##   Briefwahl: identifier row with col6="Briefwahl", treated as separate

    ## Read party names from first sheet
    d_ref <- read_excel(fpath, sheet = "Tab5-Wk01", col_names = FALSE,
                        col_types = "text")
    cnames <- names(d_ref)
    r5 <- as.character(d_ref[5, ])

    ## Collect all Z+Anz municipality data + Briefwahl across all 44 sheets
    all_bb04 <- list()
    all_brief <- list()

    for (wk in sprintf("Tab5-Wk%02d", 1:44)) {
      d <- read_excel(fpath, sheet = wk, col_names = FALSE, col_types = "text")

      ## Find identifier rows: non-NA col3 with numeric Kreis code
      ## and col4 with numeric Gem code
      id_rows <- which(!is.na(d[[cnames[3]]]) & grepl("^\\d+$", d[[cnames[3]]]) &
                         !is.na(d[[cnames[4]]]) & grepl("^\\d+$", d[[cnames[4]]]))

      ## Exclude Amt-level aggregate rows (col4=000 AND col5!=00)
      ## Keep kreisfreie Städte (col4=000 AND col5=00): Brandenburg, Cottbus, Frankfurt, Potsdam
      amt_mask <- d[[cnames[4]]][id_rows] == "000" &
                  !is.na(d[[cnames[5]]][id_rows]) & d[[cnames[5]]][id_rows] != "00"
      if (any(amt_mask)) {
        cat("  Excluding", sum(amt_mask), "Amt-level rows from", wk, "\n")
        id_rows <- id_rows[!amt_mask]
      }

      for (id_r in id_rows) {
        kreis <- as.integer(d[[cnames[3]]][id_r])
        gem   <- as.integer(d[[cnames[4]]][id_r])
        ags   <- paste0("120", sprintf("%02d", kreis), sprintf("%03d", gem))

        ## Find the Z+Anz row following this identifier (within next 5 rows)
        for (offset in 1:5) {
          r <- id_r + offset
          if (r > nrow(d)) break
          if (!is.na(d[[cnames[7]]][r]) && d[[cnames[7]]][r] == "Z" &&
              !is.na(d[[cnames[8]]][r]) && d[[cnames[8]]][r] == "Anz.") {
            ## This is the Zweitstimmen absolute row
            row_data <- tibble(
              ags = ags,
              wk_sheet = wk,
              eligible_voters = safe_num(d[[cnames[9]]][r]),
              number_voters   = NA_real_,  # "x" for Z rows
              invalid_votes   = safe_num(d[[cnames[11]]][r]),
              valid_votes     = safe_num(d[[cnames[12]]][r])
            )

            ## Extract party votes
            for (ci in 13:min(28, ncol(d))) {
              pname <- r5[ci]
              if (!is.na(pname)) {
                row_data[[paste0("p_", ci)]] <- safe_num(d[[cnames[ci]]][r])
              }
            }
            all_bb04[[length(all_bb04) + 1]] <- row_data
            break
          }
        }
      }

      ## Capture Briefwahl Z+Anz row (identifier: col6 = "Briefwahl")
      brief_id <- which(!is.na(d[[cnames[6]]]) & d[[cnames[6]]] == "Briefwahl")
      if (length(brief_id) == 1) {
        for (offset in 1:5) {
          r <- brief_id + offset
          if (r > nrow(d)) break
          if (!is.na(d[[cnames[7]]][r]) && d[[cnames[7]]][r] == "Z" &&
              !is.na(d[[cnames[8]]][r]) && d[[cnames[8]]][r] == "Anz.") {
            brief_data <- tibble(
              wk_sheet = wk,
              brief_invalid = safe_num(d[[cnames[11]]][r]),
              brief_valid   = safe_num(d[[cnames[12]]][r])
            )
            for (ci in 13:min(28, ncol(d))) {
              pname <- r5[ci]
              if (!is.na(pname)) {
                brief_data[[paste0("brief_p_", ci)]] <- safe_num(d[[cnames[ci]]][r])
              }
            }
            all_brief[[length(all_brief) + 1]] <- brief_data
            break
          }
        }
      }
    }

    result <- bind_rows(all_bb04)
    brief_df <- bind_rows(all_brief)
    cat("  Raw Z+Anz rows:", nrow(result), "\n")
    cat("  Briefwahl WK rows:", nrow(brief_df),
        "total VV:", sum(brief_df$brief_valid, na.rm = TRUE), "\n")

    ## Allocate Briefwahl to municipalities proportionally (by Urnenwahl VV)
    wk_weights <- result |>
      group_by(wk_sheet) |>
      mutate(weight = valid_votes / sum(valid_votes, na.rm = TRUE)) |>
      ungroup() |>
      select(ags, wk_sheet, weight)

    alloc <- wk_weights |>
      inner_join(brief_df, by = "wk_sheet") |>
      mutate(
        alloc_invalid = round(brief_invalid * weight),
        alloc_valid   = round(brief_valid * weight)
      )
    pcols_brief <- grep("^brief_p_", names(brief_df), value = TRUE)
    for (bp in pcols_brief) {
      acol <- sub("^brief_", "alloc_", bp)
      alloc[[acol]] <- round(alloc[[bp]] * alloc$weight)
    }

    ## Add allocated Briefwahl to municipality Urnenwahl data
    result <- result |>
      left_join(alloc |> select(ags, wk_sheet, starts_with("alloc_")),
                by = c("ags", "wk_sheet")) |>
      mutate(
        invalid_votes = invalid_votes + coalesce(alloc_invalid, 0),
        valid_votes   = valid_votes + coalesce(alloc_valid, 0)
      )
    for (bp in pcols_brief) {
      pcol <- sub("^brief_", "", bp)
      acol <- sub("^brief_", "alloc_", bp)
      if (pcol %in% names(result) && acol %in% names(result)) {
        result[[pcol]] <- result[[pcol]] + coalesce(result[[acol]], 0)
      }
    }
    result <- result |> select(-starts_with("alloc_"), -wk_sheet)
    cat("  After Briefwahl allocation: VV =", sum(result$valid_votes, na.rm = TRUE), "\n")

    ## Get eligible voters from E+Anz rows (where eligible is not "x")
    ## Re-read to get E+Anz eligible voters by AGS
    all_e_anz <- list()
    for (wk in sprintf("Tab5-Wk%02d", 1:44)) {
      d <- read_excel(fpath, sheet = wk, col_names = FALSE, col_types = "text")
      id_rows <- which(!is.na(d[[cnames[3]]]) & grepl("^\\d+$", d[[cnames[3]]]) &
                         !is.na(d[[cnames[4]]]) & grepl("^\\d+$", d[[cnames[4]]]))
      ## Exclude Amt rows but keep kfS (col4=000, col5=00)
      id_rows <- id_rows[!(d[[cnames[4]]][id_rows] == "000" &
                            !is.na(d[[cnames[5]]][id_rows]) & d[[cnames[5]]][id_rows] != "00")]
      for (id_r in id_rows) {
        kreis <- as.integer(d[[cnames[3]]][id_r])
        gem   <- as.integer(d[[cnames[4]]][id_r])
        ags   <- paste0("120", sprintf("%02d", kreis), sprintf("%03d", gem))
        for (offset in 1:5) {
          r <- id_r + offset
          if (r > nrow(d)) break
          if (!is.na(d[[cnames[7]]][r]) && d[[cnames[7]]][r] == "E" &&
              !is.na(d[[cnames[8]]][r]) && d[[cnames[8]]][r] == "Anz.") {
            e_val <- safe_num(d[[cnames[9]]][r])
            if (!is.na(e_val)) {
              all_e_anz[[length(all_e_anz) + 1]] <- tibble(
                ags = ags, eligible_voters = e_val
              )
            }
            break
          }
        }
      }
    }
    eligible_df <- bind_rows(all_e_anz) |>
      group_by(ags) |>
      summarise(eligible_voters = sum(eligible_voters, na.rm = TRUE),
                .groups = "drop")

    ## Map ALL party columns using normalise_party
    pcol_map <- list()
    for (ci in 13:min(28, ncol(d_ref))) {
      pname <- r5[ci]
      if (is.na(pname)) next
      pname_clean <- gsub("\r?\n", " ", trimws(pname))
      pname_clean <- gsub("\\s+", " ", pname_clean)
      if (pname_clean == "") next
      pcol_map[[paste0("p_", ci)]] <- normalise_party(pname_clean)
    }

    ## Aggregate to AGS level (sum across Wahlkreise + Briefwahl)
    pcols <- paste0("p_", 13:min(28, ncol(d_ref)))
    pcols <- pcols[pcols %in% names(result)]

    result_agg <- result |>
      group_by(ags) |>
      summarise(
        invalid_votes = sum(invalid_votes, na.rm = TRUE),
        valid_votes   = sum(valid_votes, na.rm = TRUE),
        across(all_of(pcols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Add eligible voters
    result_agg <- result_agg |>
      left_join(eligible_df, by = "ags", suffix = c("_z", ""))

    ## Compute mapped party vote counts
    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      if (pcol %in% names(result_agg)) {
        if (paste0(std_name, "_n") %in% names(result_agg)) {
          result_agg[[paste0(std_name, "_n")]] <- result_agg[[paste0(std_name, "_n")]] +
            result_agg[[pcol]]
        } else {
          result_agg[[paste0(std_name, "_n")]] <- result_agg[[pcol]]
        }
      }
    }

    ## Compute other
    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result_agg)]
    result_agg <- result_agg |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE)
      )

    ## Build final result
    result <- result_agg |>
      mutate(
        election_year = as.integer(2004),
        state = "12",
        election_date = as.Date(bb_dates["2004"]),
        number_voters = invalid_votes + valid_votes,
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"),
                                -any_of("eligible_voters_z"))

  } else if (yr == "2009") {
    ## ==================================================================
    ## 2009: Sheets 5.1-5.14 (Zweitstimmen by Landkreis) + 4.1-4.4 (kfS)
    ## Names only → match to AGS using 2014 reference
    ## ==================================================================

    ## Build name → AGS reference from 2014 raw file
    f14_path <- here(raw_path, "Brandenburg",
                     "Brandenburg_2014_Landtagswahl.xlsx")
    d14_af <- read_excel(f14_path,
                         sheet = "Zweitstimmen_Gemeinden amtsfrei",
                         col_names = FALSE, col_types = "text")
    d14_ag <- read_excel(f14_path,
                         sheet = "Zweitstimmen_Gemeinden amtsang.",
                         col_names = FALSE, col_types = "text")
    af_data <- d14_af[10:nrow(d14_af), ] |> filter(!is.na(.data[["...1"]]))
    ag_data <- d14_ag[10:nrow(d14_ag), ] |> filter(!is.na(.data[["...1"]]))

    ref_ags <- bind_rows(
      tibble(name = af_data[["...2"]],
             ags = paste0("120", af_data[["...1"]])),
      tibble(name = ag_data[["...2"]],
             ags = paste0("120", substr(ag_data[["...1"]], 1, 2),
                          substr(ag_data[["...1"]], 6, 8)))
    )
    ## Clean names: remove ", Stadt" suffix, trim whitespace
    ref_ags$name_clean <- gsub(",\\s*Stadt$", "", ref_ags$name)
    ref_ags$name_clean <- trimws(ref_ags$name_clean)

    ## Add kfS
    kfs_ref <- tibble(
      name = c("Brandenburg an der Havel", "Cottbus",
               "Frankfurt (Oder)", "Potsdam"),
      ags = c("12051000", "12052000", "12053000", "12054000"),
      name_clean = c("Brandenburg an der Havel", "Cottbus",
                     "Frankfurt (Oder)", "Potsdam")
    )
    ref_ags <- bind_rows(ref_ags, kfs_ref)

    ## Manual mapping for known name mismatches (2009→2014 name changes)
    manual_map <- c(
      "Krausnick- Groß Wasserburg" = "Krausnick-Groß Wasserburg",
      "Krausnick-\nGroß Wasserburg" = "Krausnick-Groß Wasserburg",
      "Buckow  (Märkische Schweiz)" = "Buckow (Märkische Schweiz)",
      "Buckow \n(Märkische Schweiz)" = "Buckow (Märkische Schweiz)",
      "Groß Schacksdorf- Simmersdorf" = "Groß Schacksdorf-Simmersdorf",
      "Groß Schacksdorf-\nSimmersdorf" = "Groß Schacksdorf-Simmersdorf",
      "Hohenselchow- Groß Pinnow" = "Hohenselchow-Groß Pinnow",
      "Hohenselchow-\nGroß Pinnow" = "Hohenselchow-Groß Pinnow",
      "Madlitz-Wilmersdorf" = "Falkenhagen (Mark)",
      "Ketzin" = "Ketzin/Havel",
      "Belzig" = "Bad Belzig"
    )

    ## Read Landkreis Zweitstimmen (sheets 5.1-5.14)
    all_bb09 <- list()
    county_names <- c("Barnim", "Dahme-Spreewald", "Elbe-Elster",
                      "Havelland", "M\u00e4rkisch-Oderland", "Oberhavel",
                      "Oberspreewald-Lausitz", "Oder-Spree",
                      "Ostprignitz-Ruppin", "Potsdam-Mittelmark",
                      "Prignitz", "Spree-Nei\u00dfe", "Teltow-Fl\u00e4ming",
                      "Uckermark")

    for (si in 1:14) {
      s <- paste0("5.", si)
      d <- read_excel(fpath, sheet = s, col_names = FALSE, col_types = "text")
      cnames <- names(d)
      r4 <- as.character(d[4, ])

      ## Party names in row 4, cols 5+ (cols 1-4 = Region, Eligible, Voters, Valid)
      ## Stop before the "%" section (percentages, not counts)
      pct_row <- which(trimws(as.character(d[[cnames[2]]])) == "%")
      last_row <- if (length(pct_row) > 0) min(pct_row) - 1 else nrow(d)
      data_rows <- d[10:last_row, ]
      names_col <- trimws(data_rows[[cnames[1]]])

      ## Filter out county totals, Briefwahl, empty, and summary rows
      valid_idx <- !is.na(names_col) & names_col != "" &
        !grepl("^insgesamt|^darunter|^Briefwahl|^Land ", names_col, ignore.case = TRUE) &
        !names_col %in% county_names

      data_rows <- data_rows[valid_idx, ]
      names_col <- names_col[valid_idx]

      for (i in seq_along(names_col)) {
        row_name <- names_col[i]
        ## Clean name (remove newlines, extra spaces)
        row_name_clean <- gsub("\r?\n", " ", row_name)
        row_name_clean <- gsub("\\s+", " ", row_name_clean)
        row_name_clean <- trimws(row_name_clean)

        ## Apply manual mapping
        if (row_name_clean %in% names(manual_map)) {
          row_name_clean <- manual_map[row_name_clean]
        }

        ## Match to reference
        ags_match <- ref_ags$ags[ref_ags$name_clean == row_name_clean]
        if (length(ags_match) == 0) {
          ## Try without ", Stadt"
          ags_match <- ref_ags$ags[ref_ags$name_clean == row_name_clean]
        }
        if (length(ags_match) == 0) {
          cat("  WARNING: No AGS match for '", row_name_clean, "' in sheet ", s, "\n")
          next
        }
        ags_val <- ags_match[1]

        entry <- tibble(
          ags = ags_val,
          eligible_voters = safe_num(data_rows[[cnames[2]]][i]),
          voters          = safe_num(data_rows[[cnames[3]]][i]),
          valid_votes     = safe_num(data_rows[[cnames[4]]][i])
        )

        ## Party votes (cols 5 to 17, or last non-duplicate col)
        last_party <- ncol(d) - 1  # Col 18 is duplicate of col 1
        if (identical(r4[ncol(d)], r4[1])) last_party <- ncol(d) - 1
        for (ci in 5:last_party) {
          if (!is.na(r4[ci])) {
            entry[[paste0("p_", ci)]] <- safe_num(data_rows[[cnames[ci]]][i])
          }
        }

        all_bb09[[length(all_bb09) + 1]] <- entry
      }
    }

    ## Read kreisfreie Städte (sheets 4.1-4.4)
    kfs_ags <- c("12051000", "12052000", "12053000", "12054000")
    kfs_names_09 <- c("Brandenburg an der Havel", "Cottbus",
                      "Frankfurt (Oder)", "Potsdam")
    for (ki in 1:4) {
      s <- paste0("4.", ki)
      d <- read_excel(fpath, sheet = s, col_names = FALSE, col_types = "text")
      cnames <- names(d)
      r4 <- as.character(d[4, ])

      ## Find city total row: the row with the city name AND numeric data
      ## in col2 (eligible voters). The first such row is Anzahl (counts),
      ## the second is % (percentages) → we want the first.
      city_name <- kfs_names_09[ki]
      found <- FALSE
      for (r in 10:nrow(d)) {
        v <- trimws(as.character(d[[cnames[1]]][r]))
        if (!is.na(v) && v == city_name) {
          ## Check if col2 has numeric data (not "x")
          col2 <- as.character(d[[cnames[2]]][r])
          if (!is.na(col2) && col2 != "x" && grepl("^\\d", col2)) {
            entry <- tibble(
              ags = kfs_ags[ki],
              eligible_voters = safe_num(d[[cnames[2]]][r]),
              voters          = safe_num(d[[cnames[3]]][r]),
              valid_votes     = safe_num(d[[cnames[4]]][r])
            )
            last_party <- ncol(d) - 1
            if (identical(r4[ncol(d)], r4[1])) last_party <- ncol(d) - 1
            for (ci in 5:last_party) {
              if (!is.na(r4[ci])) {
                entry[[paste0("p_", ci)]] <- safe_num(d[[cnames[ci]]][r])
              }
            }
            all_bb09[[length(all_bb09) + 1]] <- entry
            found <- TRUE
            break
          }
        }
      }
      if (!found) cat("  WARNING: kfS total not found for ", city_name, "\n")
    }

    result <- bind_rows(all_bb09)
    cat("  Raw 2009 rows:", nrow(result), "\n")

    ## Map ALL party columns using normalise_party
    d_ref09 <- read_excel(fpath, sheet = "5.1", col_names = FALSE,
                          col_types = "text")
    r4_ref <- as.character(d_ref09[4, ])

    pcol_map <- list()
    for (ci in 5:17) {
      pname <- r4_ref[ci]
      if (is.na(pname)) next
      pname_clean <- gsub("\r?\n", " ", trimws(pname))
      pname_clean <- gsub("\\s+", " ", pname_clean)
      if (pname_clean == "") next
      pcol_map[[paste0("p_", ci)]] <- normalise_party(pname_clean)
    }

    ## Aggregate by AGS (some municipalities may appear multiple times)
    pcols <- grep("^p_", names(result), value = TRUE)
    result_agg <- result |>
      group_by(ags) |>
      summarise(
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        across(all_of(pcols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Map party columns
    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      if (pcol %in% names(result_agg)) {
        if (paste0(std_name, "_n") %in% names(result_agg)) {
          result_agg[[paste0(std_name, "_n")]] <- result_agg[[paste0(std_name, "_n")]] +
            result_agg[[pcol]]
        } else {
          result_agg[[paste0(std_name, "_n")]] <- result_agg[[pcol]]
        }
      }
    }

    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result_agg)]
    result_agg <- result_agg |>
      mutate(
        invalid_votes = number_voters - valid_votes,
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE)
      )

    result <- result_agg |>
      mutate(
        election_year = as.integer(2009),
        state = "12",
        election_date = as.Date(bb_dates["2009"]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

  } else if (yr == "2014") {
    ## ==================================================================
    ## 2014: Zweitstimmen_Gemeinden amtsfrei + amtsangehörig
    ## NOTE: amtsangehörig votes are "ohne Briefwahlergebnis"
    ##       Briefwahl is in Amt totals → needs proportional allocation
    ## kfS (12051-12054) not in file → use existing API data
    ## ==================================================================

    ## --- Read amtsfrei (independent municipalities) ---
    d_af <- read_excel(fpath, sheet = "Zweitstimmen_Gemeinden amtsfrei",
                       col_names = FALSE, col_types = "text")
    cnames <- names(d_af)
    ## Headers in row 6, party names in row 7-8 merged cells
    ## Col layout: 1=code, 2=name, 3-5=admin, 6=Wahl, 7=eligible,
    ##   8=voters, 9=turnout%, 10=invalid, 11=valid,
    ##   12-33=party pairs (count,%), 34=Wahl2, 35=code2
    r7 <- as.character(d_af[7, ])
    r8 <- as.character(d_af[8, ])

    af_data <- d_af[10:nrow(d_af), ] |> filter(!is.na(.data[[cnames[1]]]))
    af_ags <- paste0("120", af_data[[cnames[1]]])

    af_result <- tibble(
      ags = af_ags,
      eligible_voters = safe_num(af_data[[cnames[7]]]),
      number_voters   = safe_num(af_data[[cnames[8]]]),
      invalid_votes   = safe_num(af_data[[cnames[10]]]),
      valid_votes     = safe_num(af_data[[cnames[11]]])
    )

    ## Extract party count columns (even-indexed cols 12,14,16,...,32)
    party_cols_idx <- seq(12, 32, by = 2)
    for (ci in party_cols_idx) {
      af_result[[paste0("p_", ci)]] <- safe_num(af_data[[cnames[ci]]])
    }

    ## --- Read amtsangehörig (municipalities in Ämter) ---
    ## NOTE: "ohne Briefwahlergebnis" - mail-in votes are NOT included
    d_ag <- read_excel(fpath, sheet = "Zweitstimmen_Gemeinden amtsang.",
                       col_names = FALSE, col_types = "text")
    ag_data <- d_ag[10:nrow(d_ag), ] |> filter(!is.na(.data[[cnames[1]]]))
    ## AGS: "120" + Krs(first 2 digits) + Gem(last 3 digits of 8-digit code)
    ag_ags <- paste0("120", substr(ag_data[[cnames[1]]], 1, 2),
                     substr(ag_data[[cnames[1]]], 6, 8))

    ag_result <- tibble(
      ags = ag_ags,
      amt_code = substr(ag_data[[cnames[1]]], 1, 5),
      eligible_voters = safe_num(ag_data[[cnames[7]]]),
      number_voters   = safe_num(ag_data[[cnames[8]]]),
      invalid_votes   = safe_num(ag_data[[cnames[10]]]),
      valid_votes     = safe_num(ag_data[[cnames[11]]])
    )
    for (ci in party_cols_idx) {
      ag_result[[paste0("p_", ci)]] <- safe_num(ag_data[[cnames[ci]]])
    }

    ## --- Read Amt totals (includes Briefwahl) ---
    d_amt <- read_excel(fpath, sheet = "Zweitstimmen_\u00c4mter",
                        col_names = FALSE, col_types = "text")
    amt_data <- d_amt[10:nrow(d_amt), ] |> filter(!is.na(.data[[cnames[1]]]))
    amt_codes <- amt_data[[cnames[1]]]

    amt_totals <- tibble(
      amt_code = amt_codes,
      amt_eligible = safe_num(amt_data[[cnames[7]]]),
      amt_voters   = safe_num(amt_data[[cnames[8]]]),
      amt_invalid  = safe_num(amt_data[[cnames[10]]]),
      amt_valid    = safe_num(amt_data[[cnames[11]]])
    )
    for (ci in party_cols_idx) {
      amt_totals[[paste0("amt_p_", ci)]] <- safe_num(amt_data[[cnames[ci]]])
    }

    ## --- Allocate Briefwahl from Amt to municipalities proportionally ---
    ## Briefwahl = Amt total - sum(municipality data within Amt)
    ag_sums <- ag_result |>
      group_by(amt_code) |>
      summarise(
        sum_eligible = sum(eligible_voters, na.rm = TRUE),
        sum_voters   = sum(number_voters, na.rm = TRUE),
        sum_invalid  = sum(invalid_votes, na.rm = TRUE),
        sum_valid    = sum(valid_votes, na.rm = TRUE),
        across(starts_with("p_"), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Compute Briefwahl residuals per Amt
    brief_alloc <- amt_totals |>
      inner_join(ag_sums, by = "amt_code") |>
      mutate(
        brief_voters  = amt_voters - sum_voters,
        brief_invalid = amt_invalid - sum_invalid,
        brief_valid   = amt_valid - sum_valid
      )
    for (ci in party_cols_idx) {
      brief_alloc[[paste0("brief_p_", ci)]] <-
        brief_alloc[[paste0("amt_p_", ci)]] - brief_alloc[[paste0("p_", ci)]]
    }

    ## Compute weights (by eligible voters within each Amt)
    ag_weights <- ag_result |>
      group_by(amt_code) |>
      mutate(weight = eligible_voters / sum(eligible_voters, na.rm = TRUE)) |>
      ungroup() |>
      select(ags, amt_code, weight)

    ## Allocate Briefwahl to municipalities
    ag_alloc <- ag_weights |>
      inner_join(brief_alloc |>
                   select(amt_code, starts_with("brief_")),
                 by = "amt_code") |>
      mutate(
        alloc_voters  = round(brief_voters * weight),
        alloc_invalid = round(brief_invalid * weight),
        alloc_valid   = round(brief_valid * weight)
      )
    for (ci in party_cols_idx) {
      ag_alloc[[paste0("alloc_p_", ci)]] <-
        round(ag_alloc[[paste0("brief_p_", ci)]] * ag_alloc$weight)
    }

    ## Add allocated Briefwahl to municipality data
    ag_final <- ag_result |>
      left_join(ag_alloc |> select(ags, starts_with("alloc_")),
                by = "ags") |>
      mutate(
        number_voters = number_voters + coalesce(alloc_voters, 0L),
        invalid_votes = invalid_votes + coalesce(alloc_invalid, 0L),
        valid_votes   = valid_votes + coalesce(alloc_valid, 0L)
      )
    for (ci in party_cols_idx) {
      pcol <- paste0("p_", ci)
      acol <- paste0("alloc_p_", ci)
      if (acol %in% names(ag_final)) {
        ag_final[[pcol]] <- ag_final[[pcol]] + coalesce(ag_final[[acol]], 0)
      }
    }
    ag_final <- ag_final |> select(-starts_with("alloc_"), -amt_code)

    ## --- Combine amtsfrei + amtsangehörig ---
    result <- bind_rows(af_result, ag_final)

    ## --- Add kfS from existing API data ---
    existing <- readRDS("data/state_elections/final/_old_regionalstatistik/state_unharm.rds")
    kfs_2014 <- existing |>
      filter(state == "12", election_year == 2014,
             ags %in% c("12051000", "12052000", "12053000", "12054000"))
    if (nrow(kfs_2014) == 4) {
      cat("  Adding 4 kfS from existing API data\n")
      ## kfs_2014 already has shares (no number_voters/invalid_votes in API data)
      ## Reconstruct: number_voters ≈ valid_votes / (1 - invalid_share)
      ## Since we don't know exact invalid votes, approximate:
      ## number_voters ≈ eligible_voters * turnout
      kfs_rows <- kfs_2014 |>
        mutate(
          number_voters = round(eligible_voters * turnout),
          invalid_votes = number_voters - valid_votes,
          invalid_votes = pmax(invalid_votes, 0)
        ) |>
        select(ags, eligible_voters, number_voters, invalid_votes, valid_votes,
               cdu, spd, linke_pds, gruene, fdp, afd, other, turnout)
    }

    ## Map ALL party columns using normalise_party
    pcol_map <- list()
    for (ci in party_cols_idx) {
      pname <- r7[ci]
      if (is.na(pname)) pname <- r8[ci]
      if (is.na(pname)) next
      pname_clean <- gsub("\r?\n", " ", trimws(pname))
      pname_clean <- gsub("\\s+", " ", pname_clean)
      if (pname_clean == "") next
      pcol_map[[paste0("p_", ci)]] <- normalise_party(pname_clean)
    }

    ## Compute mapped party counts
    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      if (pcol %in% names(result)) {
        result[[paste0(std_name, "_n")]] <- result[[pcol]]
      }
    }

    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE)
      )

    ## Convert counts to shares
    result <- result |>
      mutate(
        election_year = as.integer(2014),
        state = "12",
        election_date = as.Date(bb_dates["2014"]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

    ## Append kfS
    if (nrow(kfs_2014) == 4) {
      result <- bind_rows(result, kfs_rows |>
        mutate(election_year = 2014L, state = "12",
               election_date = as.Date(bb_dates["2014"]),
               cdu_csu = cdu))
    }

  } else if (yr == "2019") {
    ## ==================================================================
    ## 2019: Sheet "Brandenburg_Landtagswahl_A_2" (Zweistimme)
    ## Row types identified by col3 (Gebiet): amtsfreie Gemeinde,
    ##   amtsangehörige Gemeinde, kreisefreie Stadt, Amt
    ## ARS in col2 (8-digit or "XXXXXXXX XX" with space for amtsangehörig)
    ## Party columns: alternating count/% starting at col 13
    ## ==================================================================
    raw <- read_excel(fpath, sheet = "Brandenburg_Landtagswahl_A_2",
                      col_names = FALSE, col_types = "text")
    cnames <- names(raw)
    r1 <- as.character(raw[1, ])  # Main header
    r2 <- as.character(raw[2, ])  # Sub-header (Anzahl/%)

    ## Identify municipality + kfS rows
    gem_types <- c("amtsfreie Gemeinde", "amtsangehörige Gemeinde",
                   "kreisefreie Stadt")
    gem <- raw[3:nrow(raw), ] |>
      filter(.data[[cnames[3]]] %in% gem_types)

    ## Construct AGS: first 8 digits of col2 (remove spaces)
    ars_clean <- gsub(" ", "", gem[[cnames[2]]])
    ags_vec <- substr(ars_clean, 1, 8)

    ## Party count columns: odd-positioned from 13 (13=SPD count, 14=SPD%,
    ##   15=CDU count, 16=CDU%, ...)
    result <- tibble(
      ags = ags_vec,
      eligible_voters = safe_num(gem[[cnames[5]]]),
      number_voters   = safe_num(gem[[cnames[9]]]),
      invalid_votes   = safe_num(gem[[cnames[11]]]),
      valid_votes     = safe_num(gem[[cnames[12]]])
    )

    ## Extract party count columns (odd cols: 13, 15, 17, ...)
    party_count_cols <- seq(13, ncol(raw), by = 2)
    party_count_cols <- party_count_cols[party_count_cols <= ncol(raw)]
    for (ci in party_count_cols) {
      if (!is.na(r1[ci])) {
        result[[paste0("p_", ci)]] <- safe_num(gem[[cnames[ci]]])
      }
    }

    ## Map ALL party columns using normalise_party
    pcol_map <- list()
    for (ci in party_count_cols) {
      pname <- r1[ci]
      if (is.na(pname)) next
      pname_clean <- gsub("\r?\n", " ", trimws(pname))
      pname_clean <- gsub("\\s+", " ", pname_clean)
      if (pname_clean == "") next
      pcol_map[[paste0("p_", ci)]] <- normalise_party(pname_clean)
    }

    ## Handle Amt-level Briefwahl: Amt rows contain Briefwahl data
    ## that needs to be allocated to member municipalities
    amt_rows <- raw[3:nrow(raw), ] |> filter(.data[[cnames[3]]] == "Amt")
    ag_rows <- raw[3:nrow(raw), ] |>
      filter(.data[[cnames[3]]] == "amtsangehörige Gemeinde")

    if (nrow(amt_rows) > 0 && nrow(ag_rows) > 0) {
      ## Extract Amt data
      amt_ars <- gsub(" ", "", amt_rows[[cnames[2]]])
      amt_id <- substr(amt_ars, 1, 8)  # 8-digit Amt code
      ## Suffix after space = Amt sub-code
      amt_suffix <- sub("^\\d{8}", "", gsub(" ", "", amt_rows[[cnames[2]]]))

      amt_df <- tibble(
        amt_id = paste0(amt_id, amt_suffix),
        amt_voters   = safe_num(amt_rows[[cnames[9]]]),
        amt_invalid  = safe_num(amt_rows[[cnames[11]]]),
        amt_valid    = safe_num(amt_rows[[cnames[12]]])
      )
      for (ci in party_count_cols) {
        if (paste0("p_", ci) %in% names(result)) {
          amt_df[[paste0("amt_p_", ci)]] <- safe_num(amt_rows[[cnames[ci]]])
        }
      }

      ## Link amtsangehörige to their Amt
      ag_ars <- gsub(" ", "", ag_rows[[cnames[2]]])
      ## Amt identifier = chars 1-8 of the full code + suffix (chars 9-10)
      ag_amt_id <- substr(ag_ars, 1, 10)

      ag_link <- tibble(
        ags = substr(ag_ars, 1, 8),
        amt_id = ag_amt_id,
        eligible = safe_num(ag_rows[[cnames[5]]])
      )

      ## Sum municipality-level data per Amt
      ag_sums <- ag_link |>
        group_by(amt_id) |>
        summarise(sum_eligible = sum(eligible, na.rm = TRUE), .groups = "drop")

      ## Compute Briefwahl residual per Amt
      ## Amt voters - sum(Gemeinde voters within Amt) = Briefwahl
      ag_gem_voters <- tibble(
        ags = substr(ag_ars, 1, 8),
        amt_id = ag_amt_id,
        eligible = safe_num(ag_rows[[cnames[5]]]),
        voters   = safe_num(ag_rows[[cnames[9]]]),
        invalid  = safe_num(ag_rows[[cnames[11]]]),
        valid    = safe_num(ag_rows[[cnames[12]]])
      )
      for (ci in party_count_cols) {
        if (paste0("p_", ci) %in% names(result)) {
          ag_gem_voters[[paste0("p_", ci)]] <- safe_num(ag_rows[[cnames[ci]]])
        }
      }

      ag_gem_sums <- ag_gem_voters |>
        group_by(amt_id) |>
        summarise(
          sum_voters  = sum(voters, na.rm = TRUE),
          sum_invalid = sum(invalid, na.rm = TRUE),
          sum_valid   = sum(valid, na.rm = TRUE),
          across(starts_with("p_"), ~ sum(.x, na.rm = TRUE)),
          .groups = "drop"
        )

      brief_resid <- amt_df |>
        inner_join(ag_gem_sums, by = "amt_id", suffix = c("_amt", "_gem")) |>
        mutate(
          brief_voters  = amt_voters - sum_voters,
          brief_invalid = amt_invalid - sum_invalid,
          brief_valid   = amt_valid - sum_valid
        )
      for (ci in party_count_cols) {
        pcol <- paste0("p_", ci)
        apcol <- paste0("amt_p_", ci)
        if (apcol %in% names(brief_resid) && paste0(pcol, "_gem") %in% names(brief_resid)) {
          brief_resid[[paste0("brief_p_", ci)]] <-
            brief_resid[[apcol]] - brief_resid[[paste0(pcol, "_gem")]]
        }
      }

      ## Allocate by eligible voter weight
      ag_weights <- ag_gem_voters |>
        group_by(amt_id) |>
        mutate(weight = eligible / sum(eligible, na.rm = TRUE)) |>
        ungroup() |>
        select(ags, amt_id, weight)

      ag_alloc <- ag_weights |>
        inner_join(brief_resid |>
                     select(amt_id, starts_with("brief_")),
                   by = "amt_id") |>
        mutate(
          alloc_voters  = round(brief_voters * weight),
          alloc_invalid = round(brief_invalid * weight),
          alloc_valid   = round(brief_valid * weight)
        )
      for (ci in party_count_cols) {
        bpcol <- paste0("brief_p_", ci)
        if (bpcol %in% names(ag_alloc)) {
          ag_alloc[[paste0("alloc_p_", ci)]] <-
            round(ag_alloc[[bpcol]] * ag_alloc$weight)
        }
      }

      ## Add allocations to result
      alloc_cols <- grep("^alloc_", names(ag_alloc), value = TRUE)
      alloc_summary <- ag_alloc |>
        select(ags, all_of(alloc_cols)) |>
        group_by(ags) |>
        summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

      result <- result |>
        left_join(alloc_summary, by = "ags")

      if ("alloc_voters" %in% names(result)) {
        result <- result |>
          mutate(
            number_voters = number_voters + coalesce(alloc_voters, 0),
            invalid_votes = invalid_votes + coalesce(alloc_invalid, 0),
            valid_votes   = valid_votes + coalesce(alloc_valid, 0)
          )
        for (ci in party_count_cols) {
          pcol <- paste0("p_", ci)
          acol <- paste0("alloc_p_", ci)
          if (pcol %in% names(result) && acol %in% names(result)) {
            result[[pcol]] <- result[[pcol]] + coalesce(result[[acol]], 0)
          }
        }
        result <- result |> select(-starts_with("alloc_"))
      }
    }

    ## Map party columns and compute "other"
    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      if (pcol %in% names(result)) {
        result[[paste0(std_name, "_n")]] <- result[[pcol]]
      }
    }

    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE)
      )

    ## Aggregate any duplicates (shouldn't be any, but safety)
    count_cols <- c(mapped_n_cols, "other_n")
    result <- result |>
      group_by(ags) |>
      summarise(
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Convert to shares
    result <- result |>
      mutate(
        election_year = as.integer(2019),
        state = "12",
        election_date = as.Date(bb_dates["2019"]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

  } else if (yr == "2024") {
    ## ==================================================================
    ## 2024: Adapted from 03_state_2022-24.R (lines 1527-1695)
    ## Sheet 4 = Zweitstimme, polling-district level with W/B flag
    ## 12-digit ARS → AGS, Amt-level Briefwahl allocation
    ## ==================================================================
    raw24 <- read_excel(fpath, sheet = 4) |> janitor::clean_names()

    bb24_party_cols <- c("spd", "af_d", "cdu", "grune_b_90", "die_linke",
                         "bvb_freie_wahler", "fdp", "tierschutzpartei",
                         "plus", "bsw", "iii_weg", "dkp", "dlw", "wu")

    raw24 <- raw24 |>
      mutate(across(c(wahlberechtigte_insgesamt, wahlende, gultige_stimmen,
                      ungultige_stimmen, all_of(bb24_party_cols)), as.numeric))

    ## Separate rows with valid Gemeinde vs Amt-level Briefwahl
    bb24_with_gem <- raw24 |>
      filter(!is.na(gemeindenummer) & gemeindenummer != "keine Gemeinde") |>
      mutate(ags = paste0(substr(ars, 1, 5), substr(ars, 10, 12)))

    ## Aggregate to municipality (combine W + B rows)
    bb24_agg <- bb24_with_gem |>
      group_by(ags) |>
      summarize(
        eligible_voters = sum(wahlberechtigte_insgesamt, na.rm = TRUE),
        number_voters   = sum(wahlende, na.rm = TRUE),
        valid_votes     = sum(gultige_stimmen, na.rm = TRUE),
        invalid_votes   = sum(ungultige_stimmen, na.rm = TRUE),
        across(all_of(bb24_party_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Handle Amt-level Briefwahl allocation
    bb24_no_gem <- raw24 |> filter(gemeindenummer == "keine Gemeinde")

    if (nrow(bb24_no_gem) > 0) {
      cat("  Amt-level Briefwahl rows:", nrow(bb24_no_gem), "\n")

      bb24_amt_brief <- bb24_no_gem |>
        mutate(amt_id = paste0(substr(ars, 1, 5), substr(ars, 6, 9))) |>
        group_by(amt_id) |>
        summarize(
          across(c(wahlende, gultige_stimmen, ungultige_stimmen,
                   all_of(bb24_party_cols)),
                 ~ sum(.x, na.rm = TRUE)),
          .groups = "drop"
        )

      bb24_gem_weights <- bb24_with_gem |>
        mutate(amt_id = paste0(substr(ars, 1, 5), substr(ars, 6, 9))) |>
        group_by(amt_id, ags) |>
        summarize(ev = sum(wahlberechtigte_insgesamt, na.rm = TRUE),
                  .groups = "drop") |>
        group_by(amt_id) |>
        mutate(weight = ev / sum(ev, na.rm = TRUE)) |>
        ungroup()

      bb24_alloc <- bb24_gem_weights |>
        inner_join(bb24_amt_brief, by = "amt_id",
                   suffix = c("_gem", "_brief")) |>
        mutate(
          across(c(wahlende, gultige_stimmen, ungultige_stimmen,
                   all_of(bb24_party_cols)),
                 ~ round(.x * weight))
        ) |>
        select(ags, number_voters = wahlende,
               valid_votes = gultige_stimmen,
               invalid_votes = ungultige_stimmen,
               all_of(bb24_party_cols)) |>
        mutate(eligible_voters = 0L)

      bb24_agg <- bind_rows(bb24_agg, bb24_alloc) |>
        group_by(ags) |>
        summarize(
          eligible_voters = sum(eligible_voters, na.rm = TRUE),
          number_voters   = sum(number_voters, na.rm = TRUE),
          valid_votes     = sum(valid_votes, na.rm = TRUE),
          invalid_votes   = sum(invalid_votes, na.rm = TRUE),
          across(all_of(bb24_party_cols), ~ sum(.x, na.rm = TRUE)),
          .groups = "drop"
        )
    }

    ## Rename janitor-cleaned names to normalised names
    ## bb24_party_cols from janitor: spd, af_d, cdu, grune_b_90, die_linke,
    ##   bvb_freie_wahler, fdp, tierschutzpartei, plus, bsw, iii_weg, dkp, dlw, wu
    bb24_rename <- c(
      "af_d" = "afd", "grune_b_90" = "gruene", "die_linke" = "linke_pds",
      "bvb_freie_wahler" = "freie_wahler", "tierschutzpartei" = "tierschutz",
      "iii_weg" = "iii_weg", "wu" = "werteunion", "dlw" = "dlw"
    )
    for (old_name in names(bb24_rename)) {
      if (old_name %in% names(bb24_agg)) {
        names(bb24_agg)[names(bb24_agg) == old_name] <- bb24_rename[old_name]
      }
    }

    result <- bb24_agg |>
      mutate(
        election_year = as.integer(2024),
        state = "12",
        election_date = as.Date(bb_dates["2024"])
      )

    ## Identify all party columns (everything except meta)
    all_party_names_24 <- setdiff(
      names(result),
      c("ags", "election_year", "state", "election_date",
        "eligible_voters", "number_voters", "valid_votes", "invalid_votes")
    )

    ## Compute "other" as residual
    party_sum <- rowSums(result[, all_party_names_24], na.rm = TRUE)
    result$other_n <- pmax(result$valid_votes - party_sum, 0, na.rm = TRUE)

    ## Convert all party columns to shares
    result <- result |> mutate(turnout = number_voters / eligible_voters)
    for (p in all_party_names_24) {
      result[[p]] <- result[[p]] / result$valid_votes
    }
    result$other <- result$other_n / result$valid_votes
    result$cdu_csu <- result$cdu
    result <- result |> select(-other_n)
  }

  ## --- Standardise and validate ---
  result <- standardise(result)
  n_muni <- nrow(result)
  total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
  cat("  BB", yr, ":", n_muni, "municipalities,",
      format(total_eligible, big.mark = ","), "eligible voters\n")

  ## Check for duplicate AGS
  dups <- result |> group_by(ags) |> filter(n() > 1)
  if (nrow(dups) > 0) {
    cat("  WARNING:", nrow(dups), "duplicate AGS rows!\n")
  }

  bb_results[[yr]] <- result
}

all_states[["bb"]] <- bind_rows(bb_results)
cat("Brandenburg total:", nrow(all_states[["bb"]]), "rows\n\n")


###############################################################################
####        Mecklenburg-Vorpommern (state code 13, 1990-2021)              ####
###############################################################################

cat("=== Mecklenburg-Vorpommern ===\n")
mv_raw_path <- file.path(raw_path, "Mecklenburg-Vorpommern")

mv_dates <- c(
  "1990" = "1990-10-14",
  "1994" = "1994-10-16",
  "1998" = "1998-09-27",
  "2002" = "2002-09-22",
  "2006" = "2006-09-17",
  "2011" = "2011-09-04",
  "2016" = "2016-09-04",
  "2021" = "2021-09-26"
)

mv_files <- c(
  "1990" = "Mecklenburg-Vorpommern_1990_Landtagswahl.xls",
  "1994" = "Mecklenburg-Vorpommern_1994_Landtagswahl.xls",
  "1998" = "Mecklenburg-Vorpommern_1998_Landtagswahl.xls",
  "2002" = "Mecklenburg-Vorpommern_2002_Landtagswahl.xls",
  "2006" = "Mecklenburg-Vorpommern_2006_Landtagswahl.csv",
  "2011" = "Mecklenburg-Vorpommern_2011_Landtagswahl.xls",
  "2016" = "Mecklenburg-Vorpommern_2021_2016_Landtagswahl.xlsx"
)

## Party mappings per year (Zweitstimmen column names → standard names)
mv_party_map <- list(
  "1990" = list(
    cols = 8:22,  # party columns in data
    names = c("CDU", "SPD", "LL_PDS", "FDP", "B90", "CSU", "DBU", "DSU",
              "GRÜNE", "FABU", "LVP", "NPD", "Forum", "GRAUE", "REP"),
    map = c("CDU" = "cdu", "SPD" = "spd", "LL_PDS" = "linke_pds",
            "FDP" = "fdp", "B90" = "gruene", "GRÜNE" = "gruene")
  ),
  "1994" = list(
    # ZS party cols 28-40 (row 7 labels)
    cols = 28:40,
    names = c("CDU", "SPD", "PDS", "F.D.P.", "GRÜNE", "BUMV", "GRAUE",
              "NATUR", "REP", "NPD", "Norddt.", "PBC", "PASS"),
    map = c("CDU" = "cdu", "SPD" = "spd", "PDS" = "linke_pds",
            "F.D.P." = "fdp", "GRÜNE" = "gruene")
  ),
  "1998" = list(
    # ZS party cols 28-40 (row 7 labels)
    cols = 28:40,
    names = c("CDU", "SPD", "PDS", "BFB", "GRÜNE", "DVU", "GRAUE",
              "REP", "F.D.P.", "Pro DM", "NPD", "PBC", "AB 2000"),
    map = c("CDU" = "cdu", "SPD" = "spd", "PDS" = "linke_pds",
            "F.D.P." = "fdp", "GRÜNE" = "gruene")
  ),
  "2002" = list(
    # ZS party cols 25-38 (row 6 labels)
    cols = 25:38,
    names = c("SPD", "CDU", "PDS", "GRÜNE", "FDP", "NPD", "REP",
              "GRAUE", "PBC", "BMV", "SPASSPARTEI", "Schill", "SLP", "V.P.M.V."),
    map = c("CDU" = "cdu", "SPD" = "spd", "PDS" = "linke_pds",
            "FDP" = "fdp", "GRÜNE" = "gruene")
  ),
  "2011" = list(
    # ZS party cols 35-50 (row 7 labels)
    cols = 35:50,
    names = c("SPD", "CDU", "DIE LINKE", "FDP", "NPD", "GRÜNE",
              "FAMILIE", "PBC", "AB", "APD", "AUF", "REP",
              "FREIE WÄHLER", "ödp", "Die PARTEI", "PIRATEN"),
    map = c("CDU" = "cdu", "SPD" = "spd", "DIE LINKE" = "linke_pds",
            "FDP" = "fdp", "GRÜNE" = "gruene")
  )
)

mv_results <- list()

for (yr in names(mv_dates)) {
  ## 2021 is handled together with 2016 from the same combined file
  if (yr == "2021") next
  cat("Processing MV", yr, "...\n")

  if (yr == "1990") {
    ## ==================================================================
    ## 1990: Sheet LW90-GEM — municipality-level Zweitstimmen
    ## "ohne Briefwahl" (Gebietsstand Oktober 1994)
    ## Col 2 = AGS (8-digit), Col 4 = eligible, Col 5 = voters,
    ## Col 6 = invalid ZS, Col 7 = valid ZS, Cols 8-22 = parties
    ## Data starts row 6
    ## ==================================================================
    fpath <- file.path(mv_raw_path, mv_files[["1990"]])
    raw <- read_excel(fpath, sheet = "LW90-GEM", col_names = FALSE,
                      col_types = "text")
    cnames <- names(raw)

    ## Extract data rows (6 to end), filtering for valid AGS
    data_rows <- raw[6:nrow(raw), ]
    ags_vec <- data_rows[[cnames[2]]]
    valid_rows <- !is.na(ags_vec) & grepl("^13", ags_vec) & nchar(ags_vec) == 8
    data_rows <- data_rows[valid_rows, ]

    pmap <- mv_party_map[["1990"]]
    result <- tibble(
      ags = data_rows[[cnames[2]]],
      eligible_voters = safe_num(data_rows[[cnames[4]]]),
      number_voters   = safe_num(data_rows[[cnames[5]]]),
      invalid_votes   = safe_num(data_rows[[cnames[6]]]),
      valid_votes     = safe_num(data_rows[[cnames[7]]])
    )

    ## Extract party counts
    for (i in seq_along(pmap$cols)) {
      ci <- pmap$cols[i]
      pname <- pmap$names[i]
      result[[paste0("p_", pname)]] <- safe_num(data_rows[[cnames[ci]]])
    }

    ## Map ALL parties to standard names using normalise_party
    mapped_cols <- list()
    for (pname in pmap$names) {
      std <- normalise_party(pname)
      ncol_name <- paste0(std, "_n")
      pcol_name <- paste0("p_", pname)
      if (ncol_name %in% names(result)) {
        result[[ncol_name]] <- result[[ncol_name]] + coalesce(result[[pcol_name]], 0)
      } else {
        result[[ncol_name]] <- result[[pcol_name]]
      }
      mapped_cols[[ncol_name]] <- TRUE
    }

    mapped_n_cols <- names(mapped_cols)
    result$other_n <- result$valid_votes -
      rowSums(result[, mapped_n_cols], na.rm = TRUE)
    result$other_n <- pmax(result$other_n, 0, na.rm = TRUE)

    ## Convert to shares
    result <- result |>
      mutate(
        election_year = 1990L,
        state = "13",
        election_date = as.Date(mv_dates["1990"]),
        turnout = number_voters / eligible_voters
      )
    all_std_names <- unique(sapply(pmap$names, normalise_party))
    for (std_name in c(all_std_names, "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

  } else if (yr %in% c("1994", "1998")) {
    ## ==================================================================
    ## 1994/1998: Wahlbezirk-level data with Erst+Zweitstimmen
    ## AGS in col 2, WBZ in col 3
    ## 1994: eligible=col7, voters=col8, ZS invalid=col26, valid=col27,
    ##        parties=cols28-40, data starts row 10
    ## 1998: eligible=col7, voters=col8, ZS invalid=col26, valid=col27,
    ##        parties=cols28-40, data starts row 10
    ## ==================================================================
    fpath <- file.path(mv_raw_path, mv_files[[yr]])
    if (yr == "1994") {
      raw <- read_excel(fpath, sheet = 1, col_names = FALSE,
                        col_types = "text")
    } else {
      raw <- read_excel(fpath, sheet = "B724W 199801", col_names = FALSE,
                        col_types = "text")
    }
    cnames <- names(raw)
    data_start <- 10

    ## Filter to rows with valid AGS in col 2
    data_rows <- raw[data_start:nrow(raw), ]
    ags_vec <- data_rows[[cnames[2]]]
    valid_rows <- !is.na(ags_vec) & grepl("^13", ags_vec) & nchar(ags_vec) == 8
    data_rows <- data_rows[valid_rows, ]

    pmap <- mv_party_map[[yr]]

    ## Build WBZ-level tibble
    wbz <- tibble(
      ags = data_rows[[cnames[2]]],
      eligible_voters = safe_num(data_rows[[cnames[7]]]),
      number_voters   = safe_num(data_rows[[cnames[8]]]),
      invalid_votes   = safe_num(data_rows[[cnames[26]]]),
      valid_votes     = safe_num(data_rows[[cnames[27]]])
    )

    for (i in seq_along(pmap$cols)) {
      ci <- pmap$cols[i]
      wbz[[paste0("p_", i)]] <- safe_num(data_rows[[cnames[ci]]])
    }

    ## Map ALL parties using normalise_party
    pcol_map <- list()
    for (i in seq_along(pmap$names)) {
      pcol_map[[paste0("p_", i)]] <- normalise_party(pmap$names[i])
    }

    ## Aggregate WBZ → Gemeinde
    count_cols <- grep("^p_", names(wbz), value = TRUE)
    result <- wbz |>
      group_by(ags) |>
      summarise(
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Briefwahl allocation: distribute Kreis-level Briefwahl to municipalities
    ## Briefwahl WBZ aggregate to separate AGS codes (suffix "249") with EV=0
    is_brief <- result$eligible_voters == 0 & substr(result$ags, 6, 8) == "249"
    if (any(is_brief)) {
      brief_rows <- result[is_brief, ]
      real_rows <- result[!is_brief, ]
      vote_cols <- c("number_voters", "invalid_votes", "valid_votes", count_cols)
      vote_cols <- intersect(vote_cols, names(result))
      for (i in seq_len(nrow(brief_rows))) {
        kreis <- substr(brief_rows$ags[i], 1, 5)
        rl_idx <- which(substr(real_rows$ags, 1, 5) == kreis)
        if (length(rl_idx) == 0) next
        ev <- real_rows$eligible_voters[rl_idx]
        ev_sum <- sum(ev, na.rm = TRUE)
        if (ev_sum == 0) next
        weights <- ev / ev_sum
        weights[is.na(weights)] <- 0
        for (vc in vote_cols) {
          brief_val <- brief_rows[[vc]][i]
          if (is.na(brief_val) || brief_val == 0) next
          real_rows[[vc]][rl_idx] <- real_rows[[vc]][rl_idx] + brief_val * weights
        }
      }
      ## Cap voters at eligible after allocation
      overcap <- !is.na(real_rows$number_voters) & !is.na(real_rows$eligible_voters) &
        real_rows$eligible_voters > 0 & real_rows$number_voters > real_rows$eligible_voters
      if (any(overcap)) {
        real_rows$number_voters[overcap] <- real_rows$eligible_voters[overcap]
      }
      result <- real_rows
      cat(sprintf(" (allocated %d Kreis-level Brief rows)", nrow(brief_rows)))
    }

    ## Rename party columns to standard names
    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[ncol_name]] <- result[[ncol_name]] + coalesce(result[[pcol]], 0)
      } else {
        result[[ncol_name]] <- result[[pcol]]
      }
    }

    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result$other_n <- result$valid_votes -
      rowSums(result[, mapped_n_cols], na.rm = TRUE)
    result$other_n <- pmax(result$other_n, 0, na.rm = TRUE)

    ## Convert to shares
    result <- result |>
      mutate(
        election_year = as.integer(yr),
        state = "13",
        election_date = as.Date(mv_dates[yr]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

  } else if (yr == "2002") {
    ## ==================================================================
    ## 2002: Wahlbezirk-level, sheet "B724W 200201"
    ## AGS in col 2, data starts row 8
    ## eligible=col4, voters=col5, ZS invalid=col23, valid=col24,
    ## parties=cols 25-38
    ## ==================================================================
    fpath <- file.path(mv_raw_path, mv_files[["2002"]])
    raw <- read_excel(fpath, sheet = "B724W 200201", col_names = FALSE,
                      col_types = "text")
    cnames <- names(raw)

    data_rows <- raw[8:nrow(raw), ]
    ags_vec <- data_rows[[cnames[2]]]
    valid_rows <- !is.na(ags_vec) & grepl("^13", ags_vec) & nchar(ags_vec) == 8
    data_rows <- data_rows[valid_rows, ]

    pmap <- mv_party_map[["2002"]]

    wbz <- tibble(
      ags = data_rows[[cnames[2]]],
      wbz_nr = data_rows[[cnames[3]]],
      eligible_voters = safe_num(data_rows[[cnames[4]]]),
      number_voters   = safe_num(data_rows[[cnames[5]]]),
      invalid_votes   = safe_num(data_rows[[cnames[23]]]),
      valid_votes     = safe_num(data_rows[[cnames[24]]])
    )

    for (i in seq_along(pmap$cols)) {
      ci <- pmap$cols[i]
      wbz[[paste0("p_", i)]] <- safe_num(data_rows[[cnames[ci]]])
    }

    ## Map ALL parties using normalise_party
    pcol_map <- list()
    for (i in seq_along(pmap$names)) {
      pcol_map[[paste0("p_", i)]] <- normalise_party(pmap$names[i])
    }

    ## Separate Briefwahl WBZ (AGS suffix "249", EV=0) BEFORE aggregation
    ## In 2002, each Briefwahl WBZ corresponds to one Amt (117 WBZ = 117 Ämter).
    ## WBZ numbers (911,912,...) map positionally to GV Amt VB codes within each Kreis.
    count_cols <- grep("^p_", names(wbz), value = TRUE)
    is_brief_wbz <- substr(wbz$ags, 6, 8) == "249" & (is.na(wbz$eligible_voters) | wbz$eligible_voters == 0)
    brief_wbz <- wbz[is_brief_wbz, ]
    real_wbz <- wbz[!is_brief_wbz, ]

    ## Aggregate regular WBZ → Gemeinde
    result <- real_wbz |>
      group_by(ags) |>
      summarise(
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

    ## Amt-level Briefwahl allocation using GV 2002
    if (nrow(brief_wbz) > 0) {
      mv_gv02 <- read_xlsx(
        "data/covars_municipality/raw/municipality_sizes/31122002_Auszug_GV.xlsx",
        sheet = 2, col_types = "text", skip = 4
      )
      ## Ämter (Satzart=50, Land=13, VB != 0000)
      mv02_amts <- mv_gv02[mv_gv02[[1]] == "50" & !is.na(mv_gv02[[1]]) &
                            mv_gv02[[3]] == "13" & !is.na(mv_gv02[[3]]) &
                            mv_gv02[[6]] != "0000", ]
      ## Member Gemeinden (Satzart=60, Land=13, VB != 0000)
      mv02_gems <- mv_gv02[mv_gv02[[1]] == "60" & !is.na(mv_gv02[[1]]) &
                            mv_gv02[[3]] == "13" & !is.na(mv_gv02[[3]]) &
                            mv_gv02[[6]] != "0000", ]
      gem_to_amt <- data.frame(
        gem_ags = paste0("130", mv02_gems[[5]], mv02_gems[[7]]),
        amt_vb = mv02_gems[[6]],
        amt_kreis = mv02_gems[[5]],
        stringsAsFactors = FALSE
      )

      ## Build WBZ→Amt mapping: within each Kreis, sort Brief WBZ by number and
      ## Ämter by VB code — they align positionally (verified: WBZ 911↔VB xx11, etc.)
      ## Aggregate Brief WBZ per (kreis, wbz_nr) first (some WBZ appear once already)
      brief_agg <- brief_wbz |>
        mutate(kreis = substr(ags, 4, 5)) |>
        group_by(kreis, wbz_nr) |>
        summarise(across(c(number_voters, invalid_votes, valid_votes, all_of(count_cols)),
                         ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
        arrange(kreis, wbz_nr)

      vote_cols <- c("number_voters", "invalid_votes", "valid_votes", count_cols)
      n_allocated <- 0
      for (kr in unique(brief_agg$kreis)) {
        br_kr <- brief_agg[brief_agg$kreis == kr, ]
        kr_amts <- mv02_amts[mv02_amts[[5]] == kr, ]
        kr_amts <- kr_amts[order(kr_amts[[6]]), ]  # sort by VB
        if (nrow(br_kr) != nrow(kr_amts)) {
          cat(sprintf("  WARNING: Kreis %s Brief WBZ (%d) != Ämter (%d), falling back to Kreis-level\n",
                      kr, nrow(br_kr), nrow(kr_amts)))
          ## Fallback: allocate all Brief WBZ in this Kreis to entire Kreis
          rl_idx <- which(substr(result$ags, 4, 5) == kr)
          if (length(rl_idx) == 0) next
          ev <- result$eligible_voters[rl_idx]
          ev_sum <- sum(ev, na.rm = TRUE)
          if (ev_sum == 0) next
          weights <- ev / ev_sum; weights[is.na(weights)] <- 0
          for (vc in vote_cols) {
            brief_total <- sum(br_kr[[vc]], na.rm = TRUE)
            if (brief_total == 0) next
            result[[vc]][rl_idx] <- result[[vc]][rl_idx] + brief_total * weights
          }
          n_allocated <- n_allocated + nrow(br_kr)
          next
        }
        ## Positional matching: i-th Brief WBZ → i-th Amt (sorted by VB)
        for (j in seq_len(nrow(br_kr))) {
          amt_vb <- kr_amts[[6]][j]
          members <- gem_to_amt[gem_to_amt$amt_vb == amt_vb & gem_to_amt$amt_kreis == kr, ]
          rl_idx <- which(result$ags %in% members$gem_ags)
          if (length(rl_idx) == 0) next
          ev <- result$eligible_voters[rl_idx]
          ev_sum <- sum(ev, na.rm = TRUE)
          if (ev_sum == 0) next
          weights <- ev / ev_sum; weights[is.na(weights)] <- 0
          for (vc in vote_cols) {
            brief_val <- br_kr[[vc]][j]
            if (is.na(brief_val) || brief_val == 0) next
            result[[vc]][rl_idx] <- result[[vc]][rl_idx] + brief_val * weights
          }
          n_allocated <- n_allocated + 1
        }
      }
      ## Cap voters at eligible
      overcap <- !is.na(result$number_voters) & !is.na(result$eligible_voters) &
        result$eligible_voters > 0 & result$number_voters > result$eligible_voters
      if (any(overcap)) {
        result$number_voters[overcap] <- result$eligible_voters[overcap]
      }
      cat(sprintf(" (allocated %d Amt-level Brief WBZ via GV 2002)", n_allocated))
    }

    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[ncol_name]] <- result[[ncol_name]] + coalesce(result[[pcol]], 0)
      } else {
        result[[ncol_name]] <- result[[pcol]]
      }
    }

    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result$other_n <- result$valid_votes -
      rowSums(result[, mapped_n_cols], na.rm = TRUE)
    result$other_n <- pmax(result$other_n, 0, na.rm = TRUE)

    result <- result |>
      mutate(
        election_year = 2002L,
        state = "13",
        election_date = as.Date(mv_dates["2002"]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

  } else if (yr == "2006") {
    ## ==================================================================
    ## 2006: CSV semicolon-delimited, Latin-1 encoding
    ## Skip 4 header lines. Filter: Ausgabe=="A" & Erst-/Zweitstimme==2
    ## AGS in "Gemeinde" column, eligible in "Wahlberechtigte",
    ## voters in "Waehler", invalid in "ungueltige Stimmen",
    ## valid in "gueltige Stimmen"
    ## Party columns: SPD, CDU, Die Linke., FDP, GRÜNE, + others
    ## ==================================================================
    fpath <- file.path(mv_raw_path, mv_files[["2006"]])
    raw06 <- fread(fpath, encoding = "Latin-1", sep = ";", skip = 4,
                   header = TRUE, fill = TRUE)

    ## Filter to Zweitstimme + Absolute values
    zs06 <- raw06[Ausgabe == "A" & `Erst-/Zweitstimme` == 2]

    ## Extract meta columns
    result <- tibble(
      ags             = as.character(zs06$Gemeinde),
      eligible_voters = as.numeric(zs06$Wahlberechtigte),
      number_voters   = as.numeric(zs06$Waehler),
      invalid_votes   = as.numeric(zs06$`ungueltige Stimmen`),
      valid_votes     = as.numeric(zs06$`gueltige Stimmen`)
    )

    ## Extract ALL party columns dynamically
    skip_cols <- c("Ausgabe", "Kreis", "Kreisname", "Amt", "Amtsname",
                   "Gemeinde", "Gemeindename", "Wahlbezirke insg.",
                   "erf. Wahlbezirke", "Wahlberechtigte", "Waehler",
                   "Wahlbeteiligung", "Erst-/Zweitstimme",
                   "ungueltige Stimmen", "gueltige Stimmen")
    party_col_names <- setdiff(names(zs06), skip_cols)
    ## Remove any empty or V## columns at end
    party_col_names <- party_col_names[!grepl("^V\\d+$", party_col_names)]

    party_votes <- list()
    for (pcn in party_col_names) {
      std_name <- normalise_party(pcn)
      v <- as.numeric(zs06[[pcn]])
      if (std_name %in% names(party_votes)) {
        existing <- party_votes[[std_name]]; existing[is.na(existing)] <- 0
        v[is.na(v)] <- 0
        party_votes[[std_name]] <- existing + v
      } else {
        party_votes[[std_name]] <- v
      }
    }

    for (std_name in names(party_votes)) {
      result[[paste0(std_name, "_n")]] <- party_votes[[std_name]]
    }

    ## Filter to valid AGS
    result <- result |> filter(grepl("^13", ags) & nchar(ags) == 8)

    ## Briefwahl allocation: distribute Amt-level Briefwahl votes to municipalities
    ## The "Amt" column maps each AGS to its Amt, allowing proper allocation.
    amt_vec <- as.integer(zs06$Amt[match(result$ags, as.character(zs06$Gemeinde))])
    result$amt <- amt_vec
    is_brief <- result$eligible_voters == 0 & !is.na(result$valid_votes) & result$valid_votes > 0
    brief_rows <- result[is_brief, ]
    real_rows  <- result[!is_brief, ]

    if (nrow(brief_rows) > 0) {
      mapped_n_cols <- paste0(names(party_votes), "_n")
      vote_cols <- c("number_voters", "invalid_votes", "valid_votes", mapped_n_cols)
      vote_cols <- intersect(vote_cols, names(result))
      for (amt_id in unique(brief_rows$amt)) {
        if (is.na(amt_id)) next
        br <- brief_rows[brief_rows$amt == amt_id & !is.na(brief_rows$amt), ]
        rl <- real_rows[real_rows$amt == amt_id & !is.na(real_rows$amt), ]
        if (nrow(rl) == 0) next
        weights <- rl$eligible_voters / sum(rl$eligible_voters, na.rm = TRUE)
        weights[is.na(weights)] <- 0
        for (vc in vote_cols) {
          brief_total <- sum(br[[vc]], na.rm = TRUE)
          idx <- which(real_rows$amt == amt_id & !is.na(real_rows$amt))
          real_rows[[vc]][idx] <- real_rows[[vc]][idx] + brief_total * weights
        }
      }
      # Cap voters at eligible after allocation
      overcap <- !is.na(real_rows$number_voters) & !is.na(real_rows$eligible_voters) &
        real_rows$eligible_voters > 0 & real_rows$number_voters > real_rows$eligible_voters
      if (any(overcap)) {
        real_rows$number_voters[overcap] <- real_rows$eligible_voters[overcap]
      }
      result <- real_rows
      cat(sprintf(" (allocated %d Brief rows across %d Ämter)",
                  nrow(brief_rows), length(unique(brief_rows$amt))))
    }
    result <- result |> select(-amt)

    mapped_n_cols <- paste0(names(party_votes), "_n")
    result$other_n <- result$valid_votes -
      rowSums(result[, mapped_n_cols], na.rm = TRUE)
    result$other_n <- pmax(result$other_n, 0, na.rm = TRUE)

    result <- result |>
      mutate(
        election_year = 2006L,
        state = "13",
        election_date = as.Date(mv_dates["2006"]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(names(party_votes), "other")) {
      result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-ends_with("_n"))

  } else if (yr == "2011") {
    ## ==================================================================
    ## 2011: Sheet "gem" — municipality-level (ohne Briefwahl for amtsangehörig)
    ## AGS in col 3, name in col 4, eligible in col 8
    ## voters in col 9, ZS invalid=col33, valid=col34, parties=cols 35-50
    ## Data starts row 10. Note: 884 unique AGS
    ## ==================================================================
    fpath <- file.path(mv_raw_path, mv_files[["2011"]])
    raw <- read_excel(fpath, sheet = "gem", col_names = FALSE,
                      col_types = "text")
    cnames <- names(raw)

    data_rows <- raw[10:nrow(raw), ]
    ags_vec <- data_rows[[cnames[3]]]
    valid_rows <- !is.na(ags_vec) & grepl("^13", ags_vec) & nchar(ags_vec) == 8
    data_rows <- data_rows[valid_rows, ]

    pmap <- mv_party_map[["2011"]]

    result <- tibble(
      ags = data_rows[[cnames[3]]],
      eligible_voters = safe_num(data_rows[[cnames[8]]]),
      number_voters   = safe_num(data_rows[[cnames[9]]]),
      invalid_votes   = safe_num(data_rows[[cnames[33]]]),
      valid_votes     = safe_num(data_rows[[cnames[34]]])
    )

    for (i in seq_along(pmap$cols)) {
      ci <- pmap$cols[i]
      result[[paste0("p_", i)]] <- safe_num(data_rows[[cnames[ci]]])
    }

    ## Briefwahl allocation: distribute Amt-level Briefwahl to member municipalities
    ## Briefwahl rows have EV=0 and AGS suffix 751-767; names start with "Briefwahl"
    result$mv_name <- data_rows[[cnames[4]]]
    p_cols <- grep("^p_", names(result), value = TRUE)
    is_brief <- result$eligible_voters == 0 &
      grepl("^Briefwahl", result$mv_name, ignore.case = TRUE)
    if (any(is_brief)) {
      ## Read GV 2013 for post-Kreisreform Amt→Gemeinde mapping
      mv_gv13 <- read_xlsx(
        "data/covars_municipality/raw/municipality_sizes/31122013_Auszug_GV.xlsx",
        sheet = 2, col_types = "text", skip = 4
      )
      gv_cn <- names(mv_gv13)
      ## Ämter (Satzart=50, Land=13, VB starts with "5")
      mv_amts <- mv_gv13[mv_gv13[[1]] == "50" & !is.na(mv_gv13[[1]]) &
                          mv_gv13[[3]] == "13" & !is.na(mv_gv13[[3]]) &
                          grepl("^5", mv_gv13[[6]]), ]
      amt_lookup <- data.frame(
        amt_vb = mv_amts[[6]],
        amt_kreis = mv_amts[[5]],
        amt_name = trimws(mv_amts[[8]]),
        stringsAsFactors = FALSE
      )
      ## Member municipalities (Satzart=60, Land=13, VB starts with "5")
      mv_gems <- mv_gv13[mv_gv13[[1]] == "60" & !is.na(mv_gv13[[1]]) &
                          mv_gv13[[3]] == "13" & !is.na(mv_gv13[[3]]) &
                          grepl("^5", mv_gv13[[6]]), ]
      gem_to_amt <- data.frame(
        gem_ags = paste0("130", mv_gems[[5]], mv_gems[[7]]),
        amt_vb = mv_gems[[6]],
        amt_kreis = mv_gems[[5]],
        stringsAsFactors = FALSE
      )

      brief_rows <- result[is_brief, ]
      real_rows <- result[!is_brief, ]
      ## Parse Amt name from "Briefwahl {Amt name}"
      brief_rows$parsed_amt <- trimws(gsub("^Briefwahl\\s+", "", brief_rows$mv_name))

      vote_cols <- c("number_voters", "invalid_votes", "valid_votes", p_cols)
      vote_cols <- intersect(vote_cols, names(result))

      n_allocated <- 0
      for (i in seq_len(nrow(brief_rows))) {
        bname <- brief_rows$parsed_amt[i]
        bkreis <- substr(brief_rows$ags[i], 4, 5)  # 2-digit Kreis within AGS
        ## Match to Amt: exact name + same Kreis
        amt_match <- amt_lookup[amt_lookup$amt_name == bname &
                                amt_lookup$amt_kreis == bkreis, ]
        if (nrow(amt_match) == 0) {
          ## Fuzzy match within same Kreis
          kreis_amts <- amt_lookup[amt_lookup$amt_kreis == bkreis, ]
          if (nrow(kreis_amts) > 0) {
            dists <- adist(bname, kreis_amts$amt_name, ignore.case = TRUE)
            best <- which.min(dists)
            if (dists[best] <= 5) amt_match <- kreis_amts[best, ]
          }
        }
        if (nrow(amt_match) == 0) {
          cat(sprintf("  WARNING: MV 2011 Brief '%s' unmatched\n", bname))
          next
        }
        ## Find member municipalities of this Amt
        members <- gem_to_amt[gem_to_amt$amt_vb == amt_match$amt_vb[1] &
                              gem_to_amt$amt_kreis == amt_match$amt_kreis[1], ]
        rl_idx <- which(real_rows$ags %in% members$gem_ags)
        if (length(rl_idx) == 0) next
        ev <- real_rows$eligible_voters[rl_idx]
        ev_sum <- sum(ev, na.rm = TRUE)
        if (ev_sum == 0) next
        weights <- ev / ev_sum
        weights[is.na(weights)] <- 0
        for (vc in vote_cols) {
          brief_val <- brief_rows[[vc]][i]
          if (is.na(brief_val) || brief_val == 0) next
          real_rows[[vc]][rl_idx] <- real_rows[[vc]][rl_idx] + brief_val * weights
        }
        n_allocated <- n_allocated + 1
      }
      ## Cap voters at eligible after allocation
      overcap <- !is.na(real_rows$number_voters) & !is.na(real_rows$eligible_voters) &
        real_rows$eligible_voters > 0 & real_rows$number_voters > real_rows$eligible_voters
      if (any(overcap)) {
        real_rows$number_voters[overcap] <- real_rows$eligible_voters[overcap]
      }
      result <- real_rows
      cat(sprintf(" (allocated %d/%d Amt-level Brief rows via GV 2013)",
                  n_allocated, nrow(brief_rows)))
    }
    result <- result |> select(-mv_name)

    ## Map ALL parties using normalise_party
    pcol_map <- list()
    for (i in seq_along(pmap$names)) {
      pcol_map[[paste0("p_", i)]] <- normalise_party(pmap$names[i])
    }

    for (pcol in names(pcol_map)) {
      std_name <- pcol_map[[pcol]]
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[ncol_name]] <- result[[ncol_name]] + coalesce(result[[pcol]], 0)
      } else {
        result[[ncol_name]] <- result[[pcol]]
      }
    }

    mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result$other_n <- result$valid_votes -
      rowSums(result[, mapped_n_cols], na.rm = TRUE)
    result$other_n <- pmax(result$other_n, 0, na.rm = TRUE)

    result <- result |>
      mutate(
        election_year = 2011L,
        state = "13",
        election_date = as.Date(mv_dates["2011"]),
        turnout = number_voters / eligible_voters
      )
    for (std_name in c(unique(unlist(pcol_map)), "other")) {
      ncol_name <- paste0(std_name, "_n")
      if (ncol_name %in% names(result)) {
        result[[std_name]] <- result[[ncol_name]] / result$valid_votes
      }
    }
    result$cdu_csu <- result$cdu
    result <- result |> select(-starts_with("p_"), -ends_with("_n"))

  } else if (yr %in% c("2016", "2021")) {
    ## ==================================================================
    ## 2016+2021: Combined XLSX, sheet "3.12 Zweitst. n. Gemeinden"
    ## Skip 10 header rows. 4 rows per municipality:
    ##   2021 Anzahl, 2016 Anzahl, 2021 %, 2016 %
    ## NO AGS column — name-based matching needed
    ## Col 1=name, col 2=Wahljahr, col 3=Maßeinheit
    ## Col 4=Wahlberechtigte, col 5=Wähler, col 6=ungültig, col 7=gültig
    ## Party columns 8-32
    ## Landkreis headers for disambiguation
    ## Briefwahl rows need allocation to Amt member municipalities
    ## Adapted from 03_state_2022-24.R lines 2341-2648
    ## ==================================================================
    fpath <- file.path(mv_raw_path, mv_files[["2016"]])
    raw <- read_xlsx(fpath, sheet = "3.12 Zweitst. n. Gemeinden",
                     col_names = FALSE, col_types = "text", skip = 10)
    cnames <- names(raw)

    ## County (Landkreis) mapping for disambiguation
    mv_lk_map <- c(
      "Mecklenburgische Seenplatte" = "13071",
      "Landkreis Rostock" = "13072",
      "Landkreis Vorpommern R\u00fcgen" = "13073",
      "Landkreis Nordwestmecklenburg" = "13074",
      "Landkreis Vorpommern-Greifswald" = "13075",
      "Landkreis Ludwigslust-Parchim" = "13076"
    )

    ## Clean name helper
    clean_name <- function(x) {
      x |>
        gsub("\r\n|\n", " ", x = _) |>
        gsub("\\s+", " ", x = _) |>
        trimws()
    }

    ## Detect Landkreis headers and fill municipality names
    mv_data <- raw |>
      mutate(
        name = .data[[cnames[1]]],
        is_lk_header = !is.na(name) & is.na(.data[[cnames[2]]]) &
          is.na(.data[[cnames[3]]])
      ) |>
      mutate(
        lk_name = if_else(is_lk_header, clean_name(name), NA_character_)
      ) |>
      tidyr::fill(lk_name, .direction = "down") |>
      mutate(county_prefix = mv_lk_map[lk_name]) |>
      tidyr::fill(name, .direction = "down")

    ## Read header row 6 for party names (shared by both years)
    mv_hdr <- read_xlsx(fpath, sheet = "3.12 Zweitst. n. Gemeinden",
                        col_names = FALSE, col_types = "text", n_max = 10)
    mv_r6 <- as.character(mv_hdr[6, ])

    ## Process BOTH years from the combined file
    for (sub_yr in c("2021", "2016")) {
      mv_yr_data <- mv_data |>
        filter(.data[[cnames[2]]] == sub_yr, .data[[cnames[3]]] == "Anzahl")

      ## Separate Briefwahl from municipality rows
      mv_brief <- mv_yr_data |>
        filter(grepl("^Briefwahl", name))
      mv_munis <- mv_yr_data |>
        filter(!grepl("^Briefwahl", name),
               !grepl("^Mecklenburg-Vorpommern$", clean_name(name)))

      ## Convert numeric columns
      num_cols <- cnames[4:min(32, length(cnames))]
      mv_munis <- mv_munis |>
        mutate(across(all_of(num_cols), ~ as.numeric(na_if(na_if(.x, "-"), "x"))))
      mv_brief <- mv_brief |>
        mutate(across(all_of(num_cols), ~ as.numeric(na_if(na_if(.x, "-"), "x"))))

      ## Build name-to-AGS crosswalk from BBSR reference
      ref_year <- if (sub_yr == "2021") "2021" else "2020"
      mv_ref <- read_xlsx(
        "data/crosswalks/raw/ref-gemeinden-ab-2020.xlsx",
        sheet = ref_year, col_types = "text"
      )
      ref_col1 <- names(mv_ref)[1]
      ref_col2 <- names(mv_ref)[2]
      mv_ref_mv <- mv_ref |>
        filter(grepl("^13", .data[[ref_col1]])) |>
        transmute(
          ref_ags = .data[[ref_col1]],
          ref_name = .data[[ref_col2]],
          ref_county = substr(ref_ags, 1, 5)
        ) |>
        mutate(ref_name_clean = clean_name(ref_name))

      mv_munis <- mv_munis |> mutate(name_clean = clean_name(name))

      ## Normalize name: replace decorative suffixes with "Stadt" or strip them
      ## Election data uses "Güstrow, Barlachstadt" while BBSR has "Güstrow, Stadt"
      mv_normalize_name <- function(x) {
        ## Decorative city titles that replace "Stadt" in election data
        city_titles <- c("Barlachstadt", "Bergringstadt", "Bernsteinstadt",
                         "Windm\u00fchlenstadt", "Residenzstadt", "Warbelstadt",
                         "Peenestadt", "Inselstadt", "Vier-Tore-Stadt",
                         "Hanse- und Universit\u00e4tsstadt",
                         "Universit\u00e4ts- und Hansestadt")
        for (suf in city_titles) {
          x <- gsub(paste0(",\\s*", suf, "\\s*$"), ", Stadt", x)
          x <- gsub(paste0(",\\s*", suf, ","), ", Stadt,", x)
        }
        ## Resort/spa suffixes (strip completely)
        resort_titles <- c("Ostseebad", "Seebad", "Ostseeheilbad")
        for (suf in resort_titles) {
          x <- gsub(paste0(",\\s*", suf, "\\s*$"), "", x)
          x <- gsub(paste0(",\\s*", suf, ","), ",", x)
        }
        ## Strip "Schliemann-gemeinde" / "Schliemanngemeinde"
        x <- gsub(",\\s*Schliemann-?\\s*gemeinde\\s*$", "", x)
        trimws(x)
      }

      ## Manual name overrides for known discrepancies
      mv_name_overrides <- c(
        "Neubukow, Stadt"     = "13072074",
        "Tessin, Stadt"       = "13072105",
        "Hansestadt Stralsund" = "13073088",
        "Hansestadt Wismar"    = "13074087",
        "Rostock, Hanse- und Universit\u00e4tsstadt" = "13003000",
        "Rostock, Hansestadt"  = "13003000",
        "Greifswald, Universit\u00e4ts- und Hansestadt" = "13075039",
        "Greifswald, Hansestadt" = "13075039",
        "Greifswald, Stadt"    = "13075039",
        "Anklam, Hansestadt"   = "13075005",
        "Ribnitz-Damgarten, Bernsteinstadt" = "13073075",
        "Ribnitz-Damgarten, Stadt" = "13073075",
        "K\u00fchlungsborn, Ostseebad, Stadt" = "13072060",
        "K\u00fchlungsborn, Stadt" = "13072060",
        "Rerik, Ostseebad, Stadt" = "13072085",
        "Rerik, Stadt"         = "13072085",
        "Ueckerm\u00fcnde, Seebad, Stadt" = "13075136",
        "Ueckerm\u00fcnde, Stadt" = "13075136",
        "M\u00f6nchgut, Ostseebad" = "13073107",
        "M\u00f6nchgut"           = "13073107"
      )

      ## Match municipalities to AGS
      mv_munis <- mv_munis |>
        mutate(
          name_norm = mv_normalize_name(name_clean),
          ref_ags = mv_name_overrides[name_clean]
        )
      ## Try normalized name overrides
      unmatched_idx <- which(is.na(mv_munis$ref_ags))
      for (idx in unmatched_idx) {
        nm_norm <- mv_munis$name_norm[idx]
        if (nm_norm %in% names(mv_name_overrides)) {
          mv_munis$ref_ags[idx] <- mv_name_overrides[nm_norm]
        }
      }

      ## Also normalize BBSR reference names
      mv_ref_mv <- mv_ref_mv |>
        mutate(ref_name_norm = mv_normalize_name(ref_name_clean))

      for (i in seq_len(nrow(mv_munis))) {
        if (!is.na(mv_munis$ref_ags[i])) next
        nm <- mv_munis$name_clean[i]
        nm_norm <- mv_munis$name_norm[i]
        cp <- mv_munis$county_prefix[i]

        ref_sub <- if (!is.na(cp)) {
          mv_ref_mv |> filter(ref_county == cp)
        } else {
          mv_ref_mv
        }

        ## Try exact match on original name
        exact <- ref_sub |> filter(ref_name_clean == nm)
        if (nrow(exact) == 1) {
          mv_munis$ref_ags[i] <- exact$ref_ags
          next
        }

        ## Try exact match on normalized name
        exact_norm <- ref_sub |> filter(ref_name_clean == nm_norm |
                                        ref_name_norm == nm_norm)
        if (nrow(exact_norm) == 1) {
          mv_munis$ref_ags[i] <- exact_norm$ref_ags
          next
        }

        ## Fuzzy match on normalized name (tight threshold)
        dists <- adist(nm_norm, ref_sub$ref_name_norm, ignore.case = TRUE,
                       partial = FALSE)
        best <- which.min(dists)
        if (length(best) > 0 && dists[best] <= 3) {
          mv_munis$ref_ags[i] <- ref_sub$ref_ags[best]
          if (dists[best] > 0) {
            cat("  Fuzzy matched:", nm, "->", ref_sub$ref_name_clean[best],
                "(AGS:", ref_sub$ref_ags[best], ")\n")
          }
        } else {
          cat("  FAILED to match:", nm, "/ normalized:", nm_norm,
              "(county:", cp, ")\n")
        }
      }

      mv_munis <- mv_munis |> filter(!is.na(ref_ags)) |> mutate(ags = ref_ags)
      cat("  MV", sub_yr, "matched:", nrow(mv_munis), "municipalities\n")

      ## Resolve duplicate AGS (e.g., two "Neuenkirchen" in same county)
      mv_dup_ags <- mv_munis |> group_by(ags) |> filter(n() > 1) |> ungroup()
      if (nrow(mv_dup_ags) > 0) {
        cat("  Resolving", nrow(mv_dup_ags), "duplicate AGS rows\n")
        for (dup_name in unique(mv_dup_ags$name_clean)) {
          dup_cp <- mv_dup_ags$county_prefix[mv_dup_ags$name_clean == dup_name][1]
          ref_candidates <- mv_ref_mv |>
            filter(ref_name_clean == dup_name, ref_county == dup_cp) |>
            arrange(ref_ags)
          dup_rows <- which(mv_munis$name_clean == dup_name &
                            mv_munis$county_prefix == dup_cp)
          if (length(dup_rows) == nrow(ref_candidates)) {
            ev_order <- order(as.numeric(mv_munis[[cnames[4]]][dup_rows]))
            for (j in seq_along(dup_rows)) {
              mv_munis$ags[dup_rows[ev_order[j]]] <- ref_candidates$ref_ags[j]
            }
          }
        }
      }

      ## Briefwahl allocation using GV file for Amt membership
      gv_file <- "data/crosswalks/raw/31122021_Auszug_GV.xlsx"
      mv_gv <- read_xlsx(gv_file, sheet = 2, col_types = "text", skip = 3)

      ## Municipality → Amt mapping (Satzart=60, Land=13, VB != 9999)
      mv_gem_amt <- mv_gv |>
        filter(.data[[names(mv_gv)[1]]] == "60", Land == "13",
               VB != "9999", Gem != "999") |>
        mutate(
          ags = paste0(Land, "0", Kreis, Gem),
          amt_code = paste0(Land, "0", Kreis, VB)
        ) |>
        select(ags, amt_code, VB)

      ## Amt names (Satzart=50, VB starting with 5 = actual Ämter)
      mv_amt_names <- mv_gv |>
        filter(.data[[names(mv_gv)[1]]] == "50", Land == "13",
               grepl("^5", VB)) |>
        mutate(
          amt_code = paste0(Land, "0", Kreis, VB),
          amt_name = clean_name(.data[[names(mv_gv)[8]]])
        ) |>
        select(amt_code, amt_name)

      ## Match Briefwahl rows to Amt codes
      mv_brief <- mv_brief |>
        mutate(
          brief_name = clean_name(gsub("^Briefwahl\\s+", "", name)),
          brief_name_alt = gsub("^Amt\\s+", "", brief_name)
        )

      mv_brief <- mv_brief |>
        left_join(mv_amt_names, by = c("brief_name" = "amt_name"))

      unmatched <- mv_brief |> filter(is.na(amt_code))
      if (nrow(unmatched) > 0) {
        for (i in seq_len(nrow(unmatched))) {
          alt <- unmatched$brief_name_alt[i]
          match_idx <- which(mv_amt_names$amt_name == alt)
          if (length(match_idx) == 1) {
            unmatched$amt_code[i] <- mv_amt_names$amt_code[match_idx]
          } else {
            dists <- adist(alt, mv_amt_names$amt_name, ignore.case = TRUE)
            best <- which.min(dists)
            if (dists[best] <= 5) {
              unmatched$amt_code[i] <- mv_amt_names$amt_code[best]
            }
          }
        }
        mv_brief <- mv_brief |> filter(!is.na(amt_code)) |>
          bind_rows(unmatched)
      }

      ## Join municipalities to their Amt
      mv_munis <- mv_munis |>
        left_join(mv_gem_amt |> filter(grepl("^5", VB)) |> select(ags, amt_code),
                  by = "ags")

      ## Compute weights for allocation
      mv_weights <- mv_munis |>
        filter(!is.na(amt_code)) |>
        mutate(ev = as.numeric(.data[[cnames[4]]])) |>
        group_by(amt_code) |>
        mutate(weight = ev / sum(ev, na.rm = TRUE)) |>
        ungroup() |>
        select(ags, amt_code, weight)

      ## Allocate Briefwahl votes
      brief_num_cols <- cnames[5:min(32, length(cnames))]
      mv_brief_alloc <- mv_brief |>
        filter(!is.na(amt_code)) |>
        select(amt_code, all_of(brief_num_cols)) |>
        inner_join(mv_weights, by = "amt_code",
                   relationship = "many-to-many") |>
        mutate(across(all_of(brief_num_cols), ~ round(.x * weight))) |>
        select(-amt_code, -weight) |>
        group_by(ags) |>
        summarise(across(all_of(brief_num_cols), ~ sum(.x, na.rm = TRUE)),
                  .groups = "drop")

      ## Add Briefwahl to municipality data
      mv_munis <- mv_munis |>
        left_join(mv_brief_alloc, by = "ags", suffix = c("", "_brief"))

      for (col in brief_num_cols) {
        brief_col <- paste0(col, "_brief")
        if (brief_col %in% names(mv_munis)) {
          mv_munis[[col]] <- as.numeric(mv_munis[[col]]) +
            coalesce(mv_munis[[brief_col]], 0)
        }
      }

      ## Party mapping for 2016/2021 — extract ALL party cols dynamically
      r6 <- mv_r6
      party_start <- 8
      party_end <- length(cnames)
      ## Find last non-empty party col
      for (ci in party_start:length(cnames)) {
        if (is.na(r6[ci]) || r6[ci] == "") { party_end <- ci - 1; break }
      }
      ## Exclude "Sonstige" aggregate column (last col if present)
      ## Clean word-wrap hyphens first (e.g. "Sons-\r\ntige" → "Sonstige")
      last_pname <- gsub("-\\s*\r?\n\\s*", "", r6[party_end])
      last_pname <- gsub("\r?\n", " ", last_pname)
      last_pname <- gsub("[[:space:]]+", " ", last_pname)
      last_pname <- trimws(last_pname)
      if (grepl("^Sonstige", last_pname, ignore.case = TRUE)) {
        party_end <- party_end - 1
      }

      ## Build party column map
      ## Clean word-wrap hyphens from Excel headers before normalising
      pcol_map <- list()
      for (ci in party_start:party_end) {
        pname <- r6[ci]
        if (is.na(pname) || trimws(pname) == "") next
        ## Strip ALL word-wrap artifacts from Excel headers:
        ## hyphens + optional whitespace + newline (e.g. "Sons-\r\ntige")
        pname <- gsub("-\\s*\r?\n\\s*", "", pname)
        ## Remaining newlines (e.g. "FREiER HORI\r\nZONT" after first gsub)
        pname <- gsub("\r?\n", " ", pname)
        ## Mid-word hyphens from word-wrap without newlines (e.g. "Tier-schutz-partei")
        pname <- gsub("(?<=[[:alpha:]])-(?=[[:alpha:]])", "", pname, perl = TRUE)
        pname <- gsub("[[:space:]]+", " ", pname)
        pname <- trimws(pname)
        std <- normalise_party(pname)
        pcol_map[[as.character(ci)]] <- std
      }

      result <- mv_munis |>
        transmute(
          ags,
          eligible_voters = as.numeric(.data[[cnames[4]]]),
          number_voters   = as.numeric(.data[[cnames[5]]]),
          invalid_votes   = as.numeric(.data[[cnames[6]]]),
          valid_votes     = as.numeric(.data[[cnames[7]]])
        )

      ## Extract all party vote counts
      for (ci_str in names(pcol_map)) {
        ci <- as.integer(ci_str)
        std <- pcol_map[[ci_str]]
        col_n <- paste0(std, "_n")
        v <- as.numeric(mv_munis[[cnames[ci]]])
        if (col_n %in% names(result)) {
          result[[col_n]] <- result[[col_n]] + ifelse(is.na(v), 0, v)
        } else {
          result[[col_n]] <- v
        }
      }

      mapped_n_cols <- paste0(unique(unlist(pcol_map)), "_n")
      result$other_n <- pmax(result$valid_votes -
        rowSums(result[, mapped_n_cols], na.rm = TRUE), 0, na.rm = TRUE)

      result <- result |>
        mutate(
          election_year = as.integer(sub_yr),
          state = "13",
          election_date = as.Date(mv_dates[sub_yr]),
          turnout = number_voters / eligible_voters
        )
      for (std_name in c(unique(unlist(pcol_map)), "other")) {
        result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
      }
      result$cdu_csu <- result$cdu
      result <- result |> select(-ends_with("_n"))

      ## Standardise and validate
      result <- standardise(result)
      n_muni <- nrow(result)
      total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
      cat("  MV", sub_yr, ":", n_muni, "municipalities,",
          format(total_eligible, big.mark = ","), "eligible voters\n")

      dups <- result |> group_by(ags) |> filter(n() > 1)
      if (nrow(dups) > 0) {
        cat("  WARNING:", nrow(dups), "duplicate AGS rows!\n")
      }

      mv_results[[sub_yr]] <- result
    }

    ## Skip the normal standardise block below
    next
  }

  ## --- Standardise and validate ---
  result <- standardise(result)
  n_muni <- nrow(result)
  total_eligible <- sum(result$eligible_voters, na.rm = TRUE)
  cat("  MV", yr, ":", n_muni, "municipalities,",
      format(total_eligible, big.mark = ","), "eligible voters\n")

  dups <- result |> group_by(ags) |> filter(n() > 1)
  if (nrow(dups) > 0) {
    cat("  WARNING:", nrow(dups), "duplicate AGS rows!\n")
  }

  mv_results[[yr]] <- result
}

all_states[["mv"]] <- bind_rows(mv_results)
cat("Mecklenburg-Vorpommern total:", nrow(all_states[["mv"]]), "rows\n\n")


###############################################################################
####                     BADEN-WÜRTTEMBERG (08)                             ####
###############################################################################
## 16 elections: 1956-2021
## Source: Single XLSX compilation file with one sheet per election year
## Three format variants:
##   A (1956-1972, 1980): One row per muni, party cols alternate Anzahl/%.
##      AGS=col1, eligible=col3, voters=col4, invalid=col6, valid=col8.
##      Party names in row 3 at even indices starting col10. Skip=4.
##   B (1976 only): Two rows per muni (Anzahl/%), single col per party.
##      AGS=col1, Einheit=col3, eligible=col4, voters=col5, invalid=col6,
##      valid=col7. Party cols start col8. Skip=3.
##   C (1984-2021): Six rows per muni (U/B/Z × Anzahl/%).
##      AGS=col2, type=col3 (G/K/L), vote_type=col5 (U/B/Z), unit=col6.
##      eligible=col7, voters=col8, invalid=col9, valid=col10.
##      Party cols start col11. Filter: G + Z + Anzahl. Skip=3.
## AGS: 6-digit Kennziffer → prepend "08" for 8-digit AGS
## Drop gemeindefreie Gebiete (AGS ending in 99x)

bw_file <- list.files(file.path(raw_path, "Baden-Württemberg"),
                      pattern = "LW1956_bis_2021", recursive = TRUE,
                      full.names = TRUE)

bw_dates <- c(
  "1956" = "1956-03-04", "1960" = "1960-05-15", "1964" = "1964-04-26",
  "1968" = "1968-04-28", "1972" = "1972-04-23", "1976" = "1976-04-04",
  "1980" = "1980-03-16", "1984" = "1984-03-25", "1988" = "1988-03-20",
  "1992" = "1992-04-05", "1996" = "1996-03-24", "2001" = "2001-03-25",
  "2006" = "2006-03-26", "2011" = "2011-03-27", "2016" = "2016-03-13",
  "2021" = "2021-03-14"
)

bw_sheets <- c(
  "1956" = "LW1956", "1960" = "LW1960", "1964" = "LW1964",
  "1968" = "LW1968", "1972" = "LW1972", "1976" = "LW1976",
  "1980" = "LW1980", "1984" = "LW1984", "1988" = "LW1988",
  "1992" = "LW1992", "1996" = "LW1996", "2001" = "LW2001",
  "2006" = "LW2006", "2011" = "LW2011", "2016" = "LW2016",
  "2021" = "LW2021"
)

## Format classification
bw_format_a <- c("1956", "1960", "1964", "1968", "1972", "1980")
bw_format_b <- c("1976")
bw_format_c <- c("1984", "1988", "1992", "1996", "2001", "2006",
                 "2011", "2016", "2021")

## Helper: clean party names from Excel headers (remove word-wrap hyphens)
bw_clean_party <- function(x) {
  x <- gsub("-\n", "", x)      # Word-wrap hyphenation
  x <- gsub("\n", " ", x)      # Remaining newlines → spaces
  x <- gsub("[[:space:]]+", " ", x)  # Collapse whitespace
  x <- gsub("- ", "", x)       # Remaining broken hyphens
  trimws(x)
}

## Map raw party names to standard names (all parties kept)
bw_map_party <- function(pname) normalise_party(pname)

bw_results <- list()

for (yr in names(bw_dates)) {
  cat("BW", yr, "...")
  sheet <- bw_sheets[yr]
  raw <- read_excel(bw_file, sheet = sheet, col_names = FALSE,
                    .name_repair = "minimal")
  cnames <- paste0("....", seq_len(ncol(raw)))  # consistent column refs
  colnames(raw) <- cnames

  if (yr %in% bw_format_a) {
    ## ── Format A: 1956-1972, 1980 ──────────────────────────────────────────
    ## Skip 4 header rows. One row per muni.
    ## Party cols: alternating Anzahl/% starting col 10 (every 2nd col = votes)
    r3 <- as.character(raw[3, ])  # party names row
    data_start <- 5
    df <- raw[data_start:nrow(raw), ]

    # Filter to valid municipality rows (col 1 = 6-digit AGS)
    ags_raw <- as.character(df[[cnames[1]]])
    valid_row <- grepl("^[0-9]{6}$", ags_raw)
    df <- df[valid_row, ]
    ags_vec <- paste0("08", ags_raw[valid_row])

    eligible <- as.numeric(df[[cnames[3]]])
    voters   <- as.numeric(df[[cnames[4]]])
    invalid  <- as.numeric(df[[cnames[6]]])
    valid_v  <- as.numeric(df[[cnames[8]]])

    # Party columns at even indices starting from 10
    # Row 3 has party name at even cols (10, 12, 14, ...) and "AusdrN" at odd
    party_vote_cols <- seq(10, ncol(raw), by = 2)
    party_vote_cols <- party_vote_cols[party_vote_cols <= ncol(raw)]
    # Filter to columns that actually have party names
    party_vote_cols <- party_vote_cols[
      !is.na(r3[party_vote_cols]) & !grepl("^Ausdr", r3[party_vote_cols])
    ]

    party_names_raw <- vapply(r3[party_vote_cols], bw_clean_party,
                              character(1), USE.NAMES = FALSE)
    party_std <- vapply(party_names_raw, bw_map_party, character(1),
                        USE.NAMES = FALSE)

    # Extract vote counts for mapped parties
    mapped_votes <- list()
    for (i in seq_along(party_vote_cols)) {
      std <- party_std[i]
      if (!is.na(std)) {
        v <- as.numeric(df[[cnames[party_vote_cols[i]]]])
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }

  } else if (yr %in% bw_format_b) {
    ## ── Format B: 1976 only ────────────────────────────────────────────────
    ## Skip 3 header rows. Two rows per muni (Anzahl/%).
    ## Single col per party starting col 8.
    r3 <- as.character(raw[3, ])
    data_start <- 4
    df <- raw[data_start:nrow(raw), ]

    # Keep Anzahl rows only (col 3 = "Anzahl")
    einheit <- as.character(df[[cnames[3]]])
    df <- df[!is.na(einheit) & einheit == "Anzahl", ]

    ags_raw <- as.character(df[[cnames[1]]])
    valid_row <- grepl("^[0-9]{6}$", ags_raw)
    df <- df[valid_row, ]
    ags_vec <- paste0("08", ags_raw[valid_row])

    eligible <- as.numeric(df[[cnames[4]]])
    voters   <- as.numeric(df[[cnames[5]]])
    invalid  <- as.numeric(df[[cnames[6]]])
    valid_v  <- as.numeric(df[[cnames[7]]])

    # Party columns: single cols from 8 onward
    party_start <- 8
    party_end <- ncol(raw)
    # Find last party column (before trailing NA/duplicate AGS)
    for (ci in party_start:ncol(raw)) {
      if (is.na(r3[ci]) || r3[ci] == "") {
        party_end <- ci - 1
        break
      }
    }
    party_cols_idx <- party_start:party_end

    party_names_raw <- vapply(r3[party_cols_idx], bw_clean_party,
                              character(1), USE.NAMES = FALSE)
    party_std <- vapply(party_names_raw, bw_map_party, character(1),
                        USE.NAMES = FALSE)

    mapped_votes <- list()
    for (i in seq_along(party_cols_idx)) {
      std <- party_std[i]
      if (!is.na(std)) {
        v <- as.numeric(df[[cnames[party_cols_idx[i]]]])
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }

  } else {
    ## ── Format C: 1984-2021 ────────────────────────────────────────────────
    ## Skip 3 header rows + 1 blank row 4. Six rows per muni.
    ## Filter: col3=="G", col5=="Z", col6=="Anzahl"
    r3 <- as.character(raw[3, ])
    data_start <- 4
    df <- raw[data_start:nrow(raw), ]

    geo_type  <- as.character(df[[cnames[3]]])
    vote_type <- as.character(df[[cnames[5]]])
    unit      <- as.character(df[[cnames[6]]])

    keep <- !is.na(geo_type) & geo_type == "G" &
            !is.na(vote_type) & vote_type == "Z" &
            !is.na(unit) & unit == "Anzahl"
    df <- df[keep, ]

    ags_raw <- as.character(df[[cnames[2]]])
    ags_vec <- paste0("08", ags_raw)

    eligible <- as.numeric(df[[cnames[7]]])
    voters   <- as.numeric(df[[cnames[8]]])
    invalid  <- as.numeric(df[[cnames[9]]])
    valid_v  <- as.numeric(df[[cnames[10]]])

    # Party columns: single cols from 11 onward
    party_start_c <- 11
    party_end_c <- ncol(raw)
    for (ci in party_start_c:ncol(raw)) {
      if (is.na(r3[ci]) || r3[ci] == "") {
        party_end_c <- ci - 1
        break
      }
    }
    party_cols_idx <- party_start_c:party_end_c

    party_names_raw <- vapply(r3[party_cols_idx], bw_clean_party,
                              character(1), USE.NAMES = FALSE)
    party_std <- vapply(party_names_raw, bw_map_party, character(1),
                        USE.NAMES = FALSE)

    mapped_votes <- list()
    for (i in seq_along(party_cols_idx)) {
      std <- party_std[i]
      if (!is.na(std)) {
        v <- as.numeric(df[[cnames[party_cols_idx[i]]]])
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }
  }

  ## ── Build result (common to all formats) ───────────────────────────────
  result <- tibble(
    ags = ags_vec,
    election_year = as.integer(yr),
    state = "08",
    election_date = as.Date(bw_dates[yr]),
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid_v,
    invalid_votes = invalid
  )

  for (std_name in names(mapped_votes)) {
    result[[paste0(std_name, "_n")]] <- mapped_votes[[std_name]]
  }

  # Other = valid_votes - sum(mapped party votes)
  count_cols <- paste0(names(mapped_votes), "_n")
  mapped_sum <- rowSums(result[count_cols], na.rm = TRUE)
  result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)
  count_cols <- c(count_cols, "other_n")

  # Drop gemeindefreie Gebiete (AGS ending in 99x)
  result <- result |> filter(!grepl("99[0-9]$", ags))

  # Aggregate duplicate AGS (split municipalities / non-allocatable Briefwahl)
  count_cols_all <- c(count_cols, "other_n")
  if (any(duplicated(result$ags))) {
    result <- result |>
      group_by(ags) |>
      summarise(
        election_year = first(election_year), state = first(state),
        election_date = first(election_date),
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        across(any_of(count_cols_all), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # Convert to shares
  result <- result |>
    mutate(turnout = number_voters / eligible_voters)
  for (std_name in c(names(mapped_votes), "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  n_muni <- nrow(result)
  cat(n_muni, "munis,", sum(result$eligible_voters, na.rm = TRUE), "EV\n")

  bw_results[[yr]] <- result
}

all_states[["bw"]] <- bind_rows(bw_results)
cat("Baden-Württemberg total:", nrow(all_states[["bw"]]), "rows\n\n")


###############################################################################
####                            HESSEN (06)                                ####
###############################################################################
## 21 elections: 1946-2023
## Source: XLSX compilation (1946-2018, 20 sheets) + 2023 CSV
## Three sub-formats in the XLSX:
##   Pre-reform (1946-1978): Col 1 = modern GKZ for crosswalk, many pre-reform
##     municipalities per modern GKZ → aggregate. Col 9 = eligible (Insgesamt),
##     Col 10 = voters, Col 12 = invalid, Col 13 = valid. Row 5 = state total.
##     Exception: 1978 has eligible at col 7. Party names in Row 3.
##   Post-reform single vote (1982-1987): Col 2 = 6-digit GKZ, Col 3 = name.
##     "Wahl nach Landeslisten" (single vote type). Row 6 = state total.
##   Dual vote (1991-2018): Col 2 = GKZ. Separate Wahlkreisstimmen + Landesstimmen.
##     Find Landesstimmen via Row 3 search. Row 6 = state total.
## AGS: 6-digit GKZ → prepend "06" for 8-digit AGS
## 2023 CSV: Filter Gebietstyp ∈ {VF, KS}, AGS = "06" + substr(Gebietsschlüssel, 4, 9)

he_file <- list.files(file.path(raw_path, "Hessen"), pattern = "xlsx",
                      full.names = TRUE)

he_dates <- c(
  "1946" = "1946-12-01", "1950" = "1950-11-19", "1954" = "1954-11-28",
  "1958" = "1958-11-23", "1962" = "1962-11-11", "1966" = "1966-11-06",
  "1970" = "1970-11-08", "1974" = "1974-10-27", "1978" = "1978-10-08",
  "1982" = "1982-09-26", "1983" = "1983-09-25", "1987" = "1987-04-05",
  "1991" = "1991-01-20", "1995" = "1995-02-19", "1999" = "1999-02-07",
  "2003" = "2003-02-02", "2008" = "2008-01-27", "2009" = "2009-01-18",
  "2013" = "2013-09-22", "2018" = "2018-10-28"
)

he_prereform   <- c("1946", "1950", "1954", "1958", "1962", "1966",
                    "1970", "1974", "1978")
he_single_vote <- c("1982", "1983", "1987")
he_dual_vote   <- c("1991", "1995", "1999", "2003", "2008", "2009",
                    "2013", "2018")

## Helper: clean party/header names from Excel (handles word-wrap hyphens)
he_clean_header <- function(x) {
  x[is.na(x)] <- ""
  x <- gsub("-\n", "", x)
  x <- gsub("\n", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("- ", "", x)
  trimws(x)
}

## Map raw party names to standard names (all parties kept)
he_map_party <- function(pname) normalise_party(pname)

he_results <- list()

for (yr in names(he_dates)) {
  cat("HE", yr, "...")
  raw <- read_excel(he_file, sheet = yr, col_names = FALSE,
                    .name_repair = "minimal")
  nc <- ncol(raw)
  cnames <- paste0("c", seq_len(nc))
  colnames(raw) <- cnames

  r3 <- vapply(as.character(raw[3, ]), he_clean_header, character(1),
               USE.NAMES = FALSE)
  r4 <- vapply(as.character(raw[4, ]), he_clean_header, character(1),
               USE.NAMES = FALSE)

  if (yr %in% he_prereform) {
    ## ── Pre-reform: 1946-1978 ──────────────────────────────────────────
    ung_col <- which(grepl("ung.ltig", r3, ignore.case = TRUE))[1]
    gul_col <- which(grepl("^g.ltig", r3, ignore.case = TRUE))[1]
    insgesamt_col <- which(grepl("^Insgesamt", r3))[1]
    voters_col <- insgesamt_col + 1

    df <- raw[6:nrow(raw), ]  # row 5 = state total
    gkz <- as.character(df[[cnames[1]]])
    valid_row <- grepl("^[0-9]{3,6}$", gkz)
    df <- df[valid_row, ]
    gkz <- sprintf("%06d", as.integer(gkz[valid_row]))
    ags_vec <- paste0("06", gkz)

    eligible <- as.numeric(df[[cnames[insgesamt_col]]])
    voters   <- as.numeric(df[[cnames[voters_col]]])
    invalid  <- as.numeric(df[[cnames[ung_col]]])
    valid_v  <- as.numeric(df[[cnames[gul_col]]])

    party_start <- gul_col + 1
    party_end <- nc
    for (ci in party_start:nc) {
      if (r3[ci] == "") { party_end <- ci - 1; break }
    }
    party_cols_idx <- party_start:party_end
    party_names_raw <- r3[party_cols_idx]
    party_std <- vapply(party_names_raw, he_map_party, character(1),
                        USE.NAMES = FALSE)

    mapped_votes <- list()
    for (i in seq_along(party_cols_idx)) {
      std <- party_std[i]
      if (!is.na(std)) {
        v <- as.numeric(gsub("x", NA, df[[cnames[party_cols_idx[i]]]]))
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }

    result <- tibble(
      ags = ags_vec, election_year = as.integer(yr), state = "06",
      election_date = as.Date(he_dates[yr]),
      eligible_voters = eligible, number_voters = voters,
      valid_votes = valid_v, invalid_votes = invalid
    )
    for (std_name in names(mapped_votes)) {
      result[[paste0(std_name, "_n")]] <- mapped_votes[[std_name]]
    }
    count_cols <- paste0(names(mapped_votes), "_n")
    mapped_sum <- rowSums(result[count_cols], na.rm = TRUE)
    result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)
    count_cols <- c(count_cols, "other_n")

    # Aggregate pre-reform municipalities to modern GKZ
    result <- result |>
      group_by(ags) |>
      summarise(
        election_year = first(election_year), state = first(state),
        election_date = first(election_date),
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        across(any_of(count_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )

  } else if (yr %in% he_single_vote) {
    ## ── Post-reform, single vote: 1982-1987 ────────────────────────────
    ung_col <- which(grepl("ung.ltig", r3, ignore.case = TRUE))[1]
    gul_col <- which(grepl("^g.ltig", r3, ignore.case = TRUE))[1]
    party_header_row <- r3
    if (is.na(ung_col)) {
      ung_col <- which(grepl("ung.ltig", r4, ignore.case = TRUE))[1]
      gul_col <- which(grepl("^g.ltig", r4, ignore.case = TRUE))[1]
      party_header_row <- r4
    }
    insgesamt_col <- which(grepl("^Insgesamt", r3))[1]
    voters_col <- insgesamt_col + 1

    df <- raw[7:nrow(raw), ]  # row 6 = state total
    gkz <- as.character(df[[cnames[2]]])
    valid_row <- grepl("^[0-9]{6}$", gkz)
    df <- df[valid_row, ]
    ags_vec <- paste0("06", gkz[valid_row])

    eligible <- as.numeric(df[[cnames[insgesamt_col]]])
    voters   <- as.numeric(df[[cnames[voters_col]]])
    invalid  <- as.numeric(df[[cnames[ung_col]]])
    valid_v  <- as.numeric(df[[cnames[gul_col]]])

    party_start <- gul_col + 1
    party_end <- nc
    for (ci in party_start:nc) {
      if (party_header_row[ci] == "") { party_end <- ci - 1; break }
    }
    party_cols_idx <- party_start:party_end
    party_names_raw <- party_header_row[party_cols_idx]
    party_std <- vapply(party_names_raw, he_map_party, character(1),
                        USE.NAMES = FALSE)

    mapped_votes <- list()
    for (i in seq_along(party_cols_idx)) {
      std <- party_std[i]
      if (!is.na(std)) {
        v <- as.numeric(gsub("x", NA, df[[cnames[party_cols_idx[i]]]]))
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }

    result <- tibble(
      ags = ags_vec, election_year = as.integer(yr), state = "06",
      election_date = as.Date(he_dates[yr]),
      eligible_voters = eligible, number_voters = voters,
      valid_votes = valid_v, invalid_votes = invalid
    )
    for (std_name in names(mapped_votes)) {
      result[[paste0(std_name, "_n")]] <- mapped_votes[[std_name]]
    }
    count_cols <- paste0(names(mapped_votes), "_n")
    mapped_sum <- rowSums(result[count_cols], na.rm = TRUE)
    result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)
    count_cols <- c(count_cols, "other_n")

  } else {
    ## ── Dual vote: 1991-2018 ───────────────────────────────────────────
    ls_col <- which(grepl("Landesstimmen", r3))[1]
    insgesamt_col <- which(grepl("^Insgesamt", r3))[1]
    voters_col <- insgesamt_col + 1

    df <- raw[7:nrow(raw), ]
    gkz <- as.character(df[[cnames[2]]])
    valid_row <- grepl("^[0-9]{6}$", gkz)
    df <- df[valid_row, ]
    ags_vec <- paste0("06", gkz[valid_row])

    eligible <- as.numeric(df[[cnames[insgesamt_col]]])
    voters   <- as.numeric(df[[cnames[voters_col]]])
    invalid  <- as.numeric(df[[cnames[ls_col]]])
    valid_v  <- as.numeric(df[[cnames[ls_col + 1]]])

    party_start <- ls_col + 2
    party_end <- nc
    for (ci in party_start:nc) {
      if (r4[ci] == "") { party_end <- ci - 1; break }
    }
    party_cols_idx <- party_start:party_end
    party_names_raw <- r4[party_cols_idx]
    party_std <- vapply(party_names_raw, he_map_party, character(1),
                        USE.NAMES = FALSE)

    mapped_votes <- list()
    for (i in seq_along(party_cols_idx)) {
      std <- party_std[i]
      if (!is.na(std)) {
        v <- as.numeric(gsub("x", NA, df[[cnames[party_cols_idx[i]]]]))
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }

    result <- tibble(
      ags = ags_vec, election_year = as.integer(yr), state = "06",
      election_date = as.Date(he_dates[yr]),
      eligible_voters = eligible, number_voters = voters,
      valid_votes = valid_v, invalid_votes = invalid
    )
    for (std_name in names(mapped_votes)) {
      result[[paste0(std_name, "_n")]] <- mapped_votes[[std_name]]
    }
    count_cols <- paste0(names(mapped_votes), "_n")
    mapped_sum <- rowSums(result[count_cols], na.rm = TRUE)
    result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)
    count_cols <- c(count_cols, "other_n")
  }

  ## ── Common: convert to shares ──────────────────────────────────────
  result <- result |>
    mutate(turnout = number_voters / eligible_voters)
  for (std_name in c(names(mapped_votes), "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  cat(nrow(result), "munis,", sum(result$eligible_voters, na.rm = TRUE), "EV\n")
  he_results[[yr]] <- result
}

## ── HE 2023: CSV ────────────────────────────────────────────────────────
cat("HE 2023 ...")
he23_csv <- list.files(file.path(raw_path, "Hessen"), pattern = "2023.*csv",
                       full.names = TRUE)
he23_raw <- read_csv2(he23_csv, skip = 1, show_col_types = FALSE,
                      col_types = cols(.default = "c"))

he23 <- he23_raw |> filter(Gebietstyp %in% c("VF", "KS"))
he23$ags <- paste0("06", substr(he23[["Gebietsschlüssel"]], 4, 9))

# Landesstimmen party columns (absolute counts, not %)
ls_cols <- names(he23)[grepl("Landesstimmen$", names(he23))]
ls_invalid_col <- names(he23)[grepl("ungültige Landesstimmen$", names(he23))][1]
ls_valid_col <- names(he23)[grepl("gültige Landesstimmen$", names(he23)) &
                            !grepl("ungültige", names(he23))][1]
ls_party_cols <- setdiff(ls_cols, c(ls_invalid_col, ls_valid_col))
ls_party_cols <- ls_party_cols[!grepl("%", ls_party_cols)]

he23_party_map <- list()
for (col in ls_party_cols) {
  pname <- gsub(" Landesstimmen$", "", col)
  std <- he_map_party(pname)
  if (!is.na(std)) he23_party_map[[col]] <- std
}

he23_result <- tibble(
  ags = he23$ags, election_year = 2023L, state = "06",
  election_date = as.Date("2023-10-08"),
  eligible_voters = as.numeric(he23[["Wahlberechtigte"]]),
  number_voters = as.numeric(he23[["Wählerinnen und Wähler"]]),
  valid_votes = as.numeric(he23[[ls_valid_col]]),
  invalid_votes = as.numeric(he23[[ls_invalid_col]])
)

for (col in names(he23_party_map)) {
  std <- he23_party_map[[col]]
  v <- as.numeric(he23[[col]])
  if (paste0(std, "_n") %in% names(he23_result)) {
    he23_result[[paste0(std, "_n")]] <- he23_result[[paste0(std, "_n")]] +
      ifelse(is.na(v), 0, v)
  } else {
    he23_result[[paste0(std, "_n")]] <- v
  }
}

he23_count_cols <- paste0(unique(unlist(he23_party_map)), "_n")
he23_mapped_sum <- rowSums(he23_result[he23_count_cols], na.rm = TRUE)
he23_result$other_n <- pmax(he23_result$valid_votes - he23_mapped_sum, 0,
                            na.rm = TRUE)

he23_result <- he23_result |>
  mutate(turnout = number_voters / eligible_voters)
for (std_name in c(unique(unlist(he23_party_map)), "other")) {
  he23_result[[std_name]] <- he23_result[[paste0(std_name, "_n")]] /
    he23_result$valid_votes
}
he23_result$cdu_csu <- he23_result$cdu
he23_result <- he23_result |> select(-ends_with("_n"))

cat(nrow(he23_result), "munis,", sum(he23_result$eligible_voters, na.rm = TRUE),
    "EV\n")
he_results[["2023"]] <- he23_result

all_states[["he"]] <- bind_rows(he_results)
cat("Hessen total:", nrow(all_states[["he"]]), "rows\n\n")


###############################################################################
####                         Saarland (10)                                 ####
###############################################################################

cat("=== SAARLAND ===\n")

sl_raw_path <- file.path(raw_path, "Saarland")

## ---------- Name → AGS mapping from 2017 CSV (has 8-digit AGS) ----------
sl_csv17 <- list.files(sl_raw_path, pattern = "2017.*\\.csv$",
                       full.names = TRUE, recursive = TRUE)
sl17_raw <- read_delim(sl_csv17, delim = ";", locale = locale(encoding = "latin1"),
                       col_types = cols(.default = "c"), show_col_types = FALSE)
sl_name2ags <- setNames(sl17_raw[[1]][nchar(sl17_raw[[1]]) == 8],
                        sl17_raw[[2]][nchar(sl17_raw[[1]]) == 8])

## Normalise: strip "/Saar" suffix used in XLSX but not in CSV
sl_normalise <- function(x) gsub("/Saar$", "", trimws(x))
sl_ags_lookup <- setNames(sl_name2ags, sl_normalise(names(sl_name2ags)))

sl_map_party <- function(pname) normalise_party(pname)

sl_dates <- c(
  "1970" = "1970-06-14", "1975" = "1975-05-04",
  "1980" = "1980-04-27", "1985" = "1985-03-10", "1990" = "1990-01-28",
  "1994" = "1994-10-16", "1999" = "1999-09-05", "2004" = "2004-09-05",
  "2009" = "2009-08-30", "2012" = "2012-03-25", "2017" = "2017-03-26",
  "2022" = "2022-03-27"
)

sl_results <- list()

## ---------- OCR: 1970 & 1975 (from scanned PDF, see 00_sl_1970_1975_ocr.py) ----------
sl_ocr_path <- here(raw_path, "Saarland", "sl_1970_1975_ocr.csv")
if (file.exists(sl_ocr_path)) {
  sl_ocr <- read.csv(sl_ocr_path, colClasses = "character")

  ## OCR corrections: Mettlach (10042114) has column misalignment in source
  ## (left-page extraction shifted WB→NV, GS→SPD, SPD→CDU; CDU value lost)
  fix_m70 <- sl_ocr$ags == "10042114" & sl_ocr$election_year == "1970"
  if (any(fix_m70)) {
    sl_ocr[fix_m70, c("eligible_voters","number_voters","invalid_votes",
                       "valid_votes","spd","cdu")] <-
      list("9036", "7667", "104", "7563", "2862", "4172")
  }
  fix_m75 <- sl_ocr$ags == "10042114" & sl_ocr$election_year == "1975"
  if (any(fix_m75)) {
    sl_ocr[fix_m75, c("eligible_voters","number_voters","invalid_votes",
                       "valid_votes","spd","cdu")] <-
      list("9234", "8448", "95", "8353", "3535", "4220")
  }

  sl_ocr_party_map <- c(
    spd = "spd", cdu = "cdu", fdp = "fdp",
    dkp = "dkp", npd = "npd", sonst = "sonst"
  )

  for (ocr_yr in c("1970", "1975")) {
    yr_data <- sl_ocr[sl_ocr$election_year == ocr_yr, ]
    if (nrow(yr_data) == 0) next

    result <- tibble(
      ags             = yr_data$ags,
      eligible_voters = as.numeric(yr_data$eligible_voters),
      number_voters   = as.numeric(yr_data$number_voters),
      invalid_votes   = as.numeric(yr_data$invalid_votes),
      valid_votes     = as.numeric(yr_data$valid_votes)
    )

    for (ocr_col in names(sl_ocr_party_map)) {
      std_name <- sl_ocr_party_map[[ocr_col]]
      votes <- as.numeric(yr_data[[ocr_col]])
      votes[is.na(votes)] <- 0
      result[[paste0(std_name, "_n")]] <- votes
      result[[std_name]] <- votes / result$valid_votes
    }

    mapped_n_cols <- paste0(unique(sl_ocr_party_map), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE),
        other   = other_n / valid_votes
      )

    result <- result |>
      mutate(
        election_year = as.integer(ocr_yr),
        state = "10",
        election_date = as.Date(sl_dates[ocr_yr]),
        turnout = number_voters / eligible_voters,
        cdu_csu = cdu
      ) |>
      select(-ends_with("_n"))

    cat("SL", ocr_yr, "...", nrow(result), "munis,",
        round(sum(result$eligible_voters, na.rm = TRUE)), "EV\n")
    sl_results[[ocr_yr]] <- result
  }
}

## ---------- XLSX compilation (1980-2022) ----------
sl_xlsx <- list.files(sl_raw_path, pattern = "1980.*\\.xlsx$",
                      full.names = TRUE, recursive = TRUE)
sl_raw <- read_excel(sl_xlsx, sheet = "Tabelle1", col_names = FALSE,
                     .name_repair = "minimal")

## Fill-forward Gemeinde (col 1) and filter to data rows
sl_gemeinde_raw <- as.character(sl_raw[[1]])
sl_jahr_raw     <- as.character(sl_raw[[2]])

# Data starts at row 5
sl_data <- sl_raw[5:nrow(sl_raw), ]
sl_gem_col <- sl_gemeinde_raw[5:length(sl_gemeinde_raw)]
sl_yr_col  <- sl_jahr_raw[5:length(sl_jahr_raw)]

# Fill-forward Gemeinde names
for (i in seq_along(sl_gem_col)) {
  if (is.na(sl_gem_col[i]) || sl_gem_col[i] == "") {
    if (i > 1) sl_gem_col[i] <- sl_gem_col[i - 1]
  }
}

# Keep only rows with a valid year
valid_row <- !is.na(sl_yr_col) & sl_yr_col %in% names(sl_dates)
sl_data <- sl_data[valid_row, ]
sl_gem_col <- sl_gem_col[valid_row]
sl_yr_col  <- sl_yr_col[valid_row]

# Map Gemeinde names → AGS
sl_gem_norm <- sl_normalise(sl_gem_col)
sl_ags_vec <- sl_ags_lookup[sl_gem_norm]
unmatched <- unique(sl_gem_col[is.na(sl_ags_vec)])
if (length(unmatched) > 0) {
  cat("WARNING: unmatched SL gemeinde names:", paste(unmatched, collapse = ", "), "\n")
}

# XLSX absolute-value columns: 3=EV, 4=Wähler, 6=Ungültig, 8=Gültig
# Party abs cols at even indices starting 10; % cols at odd indices
# Extract party names from header rows (row 2 has party names; row 3 is "absolut"/"%")
sl_safe_num <- function(x) {
  x <- as.character(x)
  x[x == "-" | x == "" | x == "–"] <- NA
  suppressWarnings(as.numeric(x))
}

sl_r3 <- as.character(sl_raw[2, ])
## Build party map: even-indexed cols from 10 onward with valid party names
## Exclude "Sonstige" and "Wählergruppen" (aggregate columns)
sl_xlsx_parties <- list()
for (ci in seq(10, ncol(sl_raw), by = 2)) {
  pname <- trimws(sl_r3[ci])
  if (is.na(pname) || pname == "") next
  if (grepl("^Sonstige$|^W.hlergruppen$", pname, ignore.case = TRUE)) next
  std <- normalise_party(pname)
  sl_xlsx_parties[[as.character(ci)]] <- std
}

## XLSX only has 1980+; 1970/1975 come from OCR above
sl_xlsx_years <- names(sl_dates)[!names(sl_dates) %in% c("1970", "1975")]
for (yr in sl_xlsx_years) {
  cat("SL", yr, "...")
  yr_rows <- sl_yr_col == yr
  df_yr <- sl_data[yr_rows, ]
  ags_yr <- sl_ags_vec[yr_rows]

  eligible <- sl_safe_num(df_yr[[3]])
  voters   <- sl_safe_num(df_yr[[4]])
  invalid  <- sl_safe_num(df_yr[[6]])
  valid_v  <- sl_safe_num(df_yr[[8]])

  result <- tibble(
    ags = ags_yr,
    election_year = as.integer(yr),
    state = "10",
    election_date = as.Date(sl_dates[yr]),
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid_v,
    invalid_votes = invalid
  )

  # All party votes from mapped columns
  mapped_sum <- rep(0, nrow(result))
  party_names_seen <- c()
  for (ci_str in names(sl_xlsx_parties)) {
    ci <- as.integer(ci_str)
    std <- sl_xlsx_parties[[ci_str]]
    v <- sl_safe_num(df_yr[[ci]])
    col_n <- paste0(std, "_n")
    if (col_n %in% names(result)) {
      result[[col_n]] <- result[[col_n]] + ifelse(is.na(v), 0, v)
    } else {
      result[[col_n]] <- v
    }
    mapped_sum <- mapped_sum + ifelse(is.na(v), 0, v)
    party_names_seen <- c(party_names_seen, std)
  }
  party_names_seen <- unique(party_names_seen)
  result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)

  # Drop rows with no AGS (shouldn't happen)
  result <- result |> filter(!is.na(ags))

  # Shares
  result <- result |> mutate(turnout = number_voters / eligible_voters)
  for (std_name in c(party_names_seen, "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  cat(nrow(result), "munis,", round(sum(result$eligible_voters, na.rm = TRUE)), "EV\n")
  sl_results[[yr]] <- result
}

all_states[["sl"]] <- bind_rows(sl_results)
cat("Saarland total:", nrow(all_states[["sl"]]), "rows\n\n")


###############################################################################
####                         Hamburg (02)                                   ####
###############################################################################

cat("=== HAMBURG ===\n")

hh_raw_path <- file.path(raw_path, "Hamburg")

hh_map_party <- function(pname) {
  p <- trimws(pname)
  # Strip numeric code prefix (e.g. "021 CDU" → "CDU")
  p <- gsub("^[0-9]+ ", "", p)
  # Strip " - Gesamtstimmen" suffix
  p <- gsub(" - Gesamtstimmen$", "", p, ignore.case = TRUE)
  p <- trimws(p)
  normalise_party(p)
}

hh_safe_num <- function(x) {
  x <- as.character(x)
  x[x == "-" | x == "" | x == "\u2013"] <- NA
  suppressWarnings(as.numeric(x))
}

hh_results <- list()

## ---------- Compilation XLSX (1966-2008, 14 elections) ----------
hh_comp <- list.files(hh_raw_path, pattern = "1966.*\\.xlsx$",
                      full.names = TRUE, recursive = TRUE)
hh_raw <- read_excel(hh_comp, sheet = "Tabelle1", col_names = FALSE,
                     .name_repair = "minimal")

# Headers in row 1
hh_headers <- as.character(hh_raw[1, ])
stichtag <- as.character(hh_raw[[1]])
# Valid election dates (exclude header row)
hh_dates_raw <- unique(stichtag[!is.na(stichtag) & stichtag != "Stichtag"])

# Map stichtag → ISO date and election_year
hh_parse_date <- function(d) {
  parts <- strsplit(d, "\\.")[[1]]
  as.Date(paste(parts[3], parts[2], parts[1], sep = "-"))
}

# Identify party columns (12+)
hh_party_cols <- 12:ncol(hh_raw)
hh_party_names <- hh_headers[hh_party_cols]
hh_party_std <- vapply(hh_party_names, hh_map_party, character(1), USE.NAMES = FALSE)

for (dt in hh_dates_raw) {
  iso_date <- hh_parse_date(dt)
  yr <- as.integer(format(iso_date, "%Y"))

  # Handle two 1982 elections: use month to distinguish
  if (yr == 1982) {
    month <- as.integer(format(iso_date, "%m"))
    yr_key <- if (month <= 6) "1982a" else "1982b"
  } else {
    yr_key <- as.character(yr)
  }

  cat("HH", yr_key, "...")

  # Select ALL rows for this date (including Briefwahl with EV=0)
  rows <- which(stichtag == dt)
  ev_all <- hh_safe_num(hh_raw[[7]][rows])

  # EV: sum only non-Briefwahl rows (EV > 0); votes: sum ALL rows
  eligible  <- sum(ev_all[ev_all > 0], na.rm = TRUE)
  voters    <- sum(hh_safe_num(hh_raw[[8]][rows]), na.rm = TRUE)
  invalid   <- sum(hh_safe_num(hh_raw[[10]][rows]), na.rm = TRUE)
  valid_v   <- sum(hh_safe_num(hh_raw[[11]][rows]), na.rm = TRUE)

  # Party votes (sum ALL rows including Briefwahl)
  mapped_votes <- list()
  for (i in seq_along(hh_party_cols)) {
    std <- hh_party_std[i]
    if (!is.na(std)) {
      v <- sum(hh_safe_num(hh_raw[[hh_party_cols[i]]][rows]), na.rm = TRUE)
      if (std %in% names(mapped_votes)) {
        mapped_votes[[std]] <- mapped_votes[[std]] + v
      } else {
        mapped_votes[[std]] <- v
      }
    }
  }

  mapped_sum <- sum(unlist(mapped_votes), na.rm = TRUE)
  other_v <- max(valid_v - mapped_sum, 0)

  result <- tibble(
    ags = "02000000",
    election_year = yr,
    state = "02",
    election_date = iso_date,
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid_v,
    invalid_votes = invalid,
    turnout = voters / eligible
  )
  for (std_name in names(mapped_votes)) {
    result[[std_name]] <- mapped_votes[[std_name]] / valid_v
  }
  result$other <- other_v / valid_v
  result$cdu_csu <- result$cdu

  cat("EV=", eligible, "valid=", valid_v, "\n")
  hh_results[[yr_key]] <- result
}

## ---------- 2011 (XLS, Gesamtstimmen) ----------
cat("HH 2011 ...")
hh_f11 <- list.files(hh_raw_path, pattern = "2011.*\\.xls$",
                     full.names = TRUE, recursive = TRUE)
hh_raw11 <- read_excel(hh_f11, col_names = FALSE, .name_repair = "minimal")
# Header in row 3, data from row 4
hh_h11 <- as.character(hh_raw11[3, ])
hh_data11 <- hh_raw11[4:nrow(hh_raw11), ]

# EV from non-Briefwahl rows only; votes from ALL rows
ev11_all <- hh_safe_num(hh_data11[[5]])
eligible11 <- sum(ev11_all[ev11_all > 0], na.rm = TRUE)
voters11   <- sum(hh_safe_num(hh_data11[[6]]), na.rm = TRUE)
invalid11  <- sum(hh_safe_num(hh_data11[[9]]), na.rm = TRUE)
valid11    <- sum(hh_safe_num(hh_data11[[11]]), na.rm = TRUE)

# Find Gesamtstimmen columns
gesamt_cols11 <- which(grepl("Gesamtstimmen", hh_h11))
gesamt_names11 <- hh_h11[gesamt_cols11]
gesamt_std11 <- vapply(gesamt_names11, hh_map_party, character(1), USE.NAMES = FALSE)

mapped_votes11 <- list()
for (i in seq_along(gesamt_cols11)) {
  std <- gesamt_std11[i]
  if (!is.na(std)) {
    v <- sum(hh_safe_num(hh_data11[[gesamt_cols11[i]]]), na.rm = TRUE)
    if (std %in% names(mapped_votes11)) {
      mapped_votes11[[std]] <- mapped_votes11[[std]] + v
    } else {
      mapped_votes11[[std]] <- v
    }
  }
}
mapped_sum11 <- sum(unlist(mapped_votes11), na.rm = TRUE)
other11 <- max(valid11 - mapped_sum11, 0)

result11 <- tibble(
  ags = "02000000", election_year = 2011L, state = "02",
  election_date = as.Date("2011-02-20"),
  eligible_voters = eligible11, number_voters = voters11,
  valid_votes = valid11, invalid_votes = invalid11,
  turnout = voters11 / eligible11
)
for (std_name in names(mapped_votes11)) {
  result11[[std_name]] <- mapped_votes11[[std_name]] / valid11
}
result11$other <- other11 / valid11
result11$cdu_csu <- result11$cdu
cat("EV=", eligible11, "valid=", valid11, "\n")
hh_results[["2011"]] <- result11

## ---------- 2015 (XLSX, Gesamtstimmen) ----------
cat("HH 2015 ...")
hh_f15 <- list.files(hh_raw_path, pattern = "2015.*\\.xlsx$",
                     full.names = TRUE, recursive = TRUE)
hh_raw15 <- read_excel(hh_f15, col_names = FALSE, .name_repair = "minimal")
# Header in row 4, data from row 5
hh_h15 <- as.character(hh_raw15[4, ])
hh_data15 <- hh_raw15[5:nrow(hh_raw15), ]

# EV from non-Briefwahl rows only; votes from ALL rows
ev15_all <- hh_safe_num(hh_data15[[5]])
eligible15 <- sum(ev15_all[ev15_all > 0], na.rm = TRUE)
voters15   <- sum(hh_safe_num(hh_data15[[6]]), na.rm = TRUE)
invalid15  <- sum(hh_safe_num(hh_data15[[9]]), na.rm = TRUE)
valid15    <- sum(hh_safe_num(hh_data15[[11]]), na.rm = TRUE)

gesamt_cols15 <- which(grepl("Gesamtstimmen", hh_h15))
gesamt_names15 <- hh_h15[gesamt_cols15]
gesamt_std15 <- vapply(gesamt_names15, hh_map_party, character(1), USE.NAMES = FALSE)

mapped_votes15 <- list()
for (i in seq_along(gesamt_cols15)) {
  std <- gesamt_std15[i]
  if (!is.na(std)) {
    v <- sum(hh_safe_num(hh_data15[[gesamt_cols15[i]]]), na.rm = TRUE)
    if (std %in% names(mapped_votes15)) {
      mapped_votes15[[std]] <- mapped_votes15[[std]] + v
    } else {
      mapped_votes15[[std]] <- v
    }
  }
}
mapped_sum15 <- sum(unlist(mapped_votes15), na.rm = TRUE)
other15 <- max(valid15 - mapped_sum15, 0)

result15 <- tibble(
  ags = "02000000", election_year = 2015L, state = "02",
  election_date = as.Date("2015-02-15"),
  eligible_voters = eligible15, number_voters = voters15,
  valid_votes = valid15, invalid_votes = invalid15,
  turnout = voters15 / eligible15
)
for (std_name in names(mapped_votes15)) {
  result15[[std_name]] <- mapped_votes15[[std_name]] / valid15
}
result15$other <- other15 / valid15
result15$cdu_csu <- result15$cdu
cat("EV=", eligible15, "valid=", valid15, "\n")
hh_results[["2015"]] <- result15

## ---------- 2020 (XLSX, Landesliste Gesamtstimmen) ----------
## NOTE: The original Hamburg_2020_Bürgerschaft.xlsx contained Wahlkreislistenstimmen
## (district list votes). Seat allocation uses Landesstimmen, so we use the
## Landesliste file instead, consistent with 2011/2015/2025.
cat("HH 2020 ...")
hh_f20 <- list.files(hh_raw_path, pattern = "2020.*Landesliste.*\\.xlsx$",
                     full.names = TRUE, recursive = TRUE)
hh_raw20 <- read_excel(hh_f20, col_names = FALSE, .name_repair = "minimal")
# Header in row 3, data from row 4
hh_h20 <- as.character(hh_raw20[3, ])
hh_data20 <- hh_raw20[4:nrow(hh_raw20), ]

# EV from non-Briefwahl rows only; votes from ALL rows
# 2020 Landesliste layout: col 6=EV, col 7=voters, col 10=invalid, col 12=valid Gesamtstimmen
ev20_all <- hh_safe_num(hh_data20[[6]])
eligible20 <- sum(ev20_all[ev20_all > 0], na.rm = TRUE)
voters20   <- sum(hh_safe_num(hh_data20[[7]]), na.rm = TRUE)
invalid20  <- sum(hh_safe_num(hh_data20[[10]]), na.rm = TRUE)
valid20    <- sum(hh_safe_num(hh_data20[[12]]), na.rm = TRUE)

# Filter for Gesamtstimmen columns only (4 cols per party: Gesamt/Listen/Person/HR)
gesamt_cols20 <- which(grepl("Gesamtstimmen", hh_h20))
gesamt_names20 <- hh_h20[gesamt_cols20]
gesamt_std20 <- vapply(gesamt_names20, hh_map_party, character(1), USE.NAMES = FALSE)

mapped_votes20 <- list()
for (i in seq_along(gesamt_cols20)) {
  std <- gesamt_std20[i]
  if (!is.na(std)) {
    v <- sum(hh_safe_num(hh_data20[[gesamt_cols20[i]]]), na.rm = TRUE)
    if (std %in% names(mapped_votes20)) {
      mapped_votes20[[std]] <- mapped_votes20[[std]] + v
    } else {
      mapped_votes20[[std]] <- v
    }
  }
}
mapped_sum20 <- sum(unlist(mapped_votes20), na.rm = TRUE)
other20 <- max(valid20 - mapped_sum20, 0)

result20 <- tibble(
  ags = "02000000", election_year = 2020L, state = "02",
  election_date = as.Date("2020-02-23"),
  eligible_voters = eligible20, number_voters = voters20,
  valid_votes = valid20, invalid_votes = invalid20,
  turnout = voters20 / eligible20
)
for (std_name in names(mapped_votes20)) {
  result20[[std_name]] <- mapped_votes20[[std_name]] / valid20
}
result20$other <- other20 / valid20
result20$cdu_csu <- result20$cdu
cat("EV=", eligible20, "valid=", valid20, "\n")
hh_results[["2020"]] <- result20

## ---------- 2025 (XLSX, city-wide Landesliste Gesamtstimmen) ----------
## Source: BUE2025_e01_Landesliste.xlsx from Statistik Nord
## Simple vertical layout: Merkmal (col 1), 2025 absolute (col 2), 2025 % (col 3)
## Rows: Wahlberechtigte, Wählende, ungültige/gültige Stimmzettel, gültige Stimmen,
##        then party names (SPD, CDU, ...) with Gesamtstimmen
cat("HH 2025 ...")
hh_f25 <- list.files(hh_raw_path, pattern = "2025.*Landesliste.*\\.xlsx$",
                     full.names = TRUE, recursive = TRUE)
if (length(hh_f25) == 0) hh_f25 <- list.files(hh_raw_path, pattern = "2025.*e01.*\\.xlsx$",
                                                full.names = TRUE, recursive = TRUE)
hh_raw25 <- read_excel(hh_f25[1], col_names = FALSE, .name_repair = "minimal")
hh_labels25 <- trimws(as.character(hh_raw25[[1]]))
hh_vals25   <- as.character(hh_raw25[[2]])

# Extract metadata by matching labels
hh_ev25 <- function(pattern) {
  idx <- grep(pattern, hh_labels25, ignore.case = TRUE)
  if (length(idx) > 0) return(hh_safe_num(hh_vals25[idx[1]]))
  NA_real_
}
eligible25 <- hh_ev25("^Wahlberechtigte$")
voters25   <- hh_ev25("^W.hlende")
invalid25  <- hh_ev25("ung.ltige Stimmzettel")
valid25    <- hh_ev25("^g.ltige Stimmen\\b")

# Party rows: between "gültige Stimmen für" and the copyright line
party_start <- grep("g.ltige Stimmen f.r", hh_labels25, ignore.case = TRUE)
party_end   <- grep("^©|^Andere|^$", hh_labels25)
party_end   <- min(party_end[party_end > party_start]) - 1

mapped_votes25 <- list()
for (ri in (party_start + 1):party_end) {
  pname <- hh_labels25[ri]
  pval  <- hh_safe_num(hh_vals25[ri])
  if (is.na(pname) || pname == "" || is.na(pval)) next
  if (grepl("^darunter|^davon", pname, ignore.case = TRUE)) next
  std <- hh_map_party(pname)
  if (!is.na(std)) {
    if (std %in% names(mapped_votes25)) {
      mapped_votes25[[std]] <- mapped_votes25[[std]] + pval
    } else {
      mapped_votes25[[std]] <- pval
    }
  }
}
mapped_sum25 <- sum(unlist(mapped_votes25), na.rm = TRUE)
other25 <- max(valid25 - mapped_sum25, 0)

result25 <- tibble(
  ags = "02000000", election_year = 2025L, state = "02",
  election_date = as.Date("2025-03-02"),
  eligible_voters = eligible25, number_voters = voters25,
  valid_votes = valid25, invalid_votes = invalid25,
  turnout = voters25 / eligible25
)
for (std_name in names(mapped_votes25)) {
  result25[[std_name]] <- mapped_votes25[[std_name]] / valid25
}
result25$other <- other25 / valid25
result25$cdu_csu <- result25$cdu
cat("EV=", eligible25, "valid=", valid25, "\n")
hh_results[["2025"]] <- result25

all_states[["hh"]] <- bind_rows(hh_results)
cat("Hamburg total:", nrow(all_states[["hh"]]), "rows\n\n")


###############################################################################
####                    Nordrhein-Westfalen (05)                           ####
###############################################################################

cat("=== NORDRHEIN-WESTFALEN ===\n")

nrw_raw_path <- file.path(raw_path, "Nordrhein-Westfalen")

nrw_map_party <- function(pname) normalise_party(pname)

nrw_safe_num <- function(x) {
  x <- as.character(x)
  x[x == "x" | x == "-" | x == "" | x == "–" | x == "."] <- NA
  suppressWarnings(as.numeric(x))
}

## Election years and dates (extracted from row 7 of each file)
nrw_dates <- c(
  "1975" = "1975-05-04", "1980" = "1980-05-11", "1985" = "1985-05-12",
  "1990" = "1990-05-13", "1995" = "1995-05-14", "2000" = "2000-05-14",
  "2005" = "2005-05-22", "2010" = "2010-05-09", "2012" = "2012-05-13",
  "2017" = "2017-05-14", "2022" = "2022-05-15"
)

nrw_results <- list()

## ---------- OCR: pre-1975 (Kreis level, from scanned PDFs, see 00_nrw_pre1975_extract.py) ----------
nrw_pre75_path <- here(raw_path, "Nordrhein-Westfalen", "nrw_pre1975_kreis.csv")
nrw_pre75_dates <- c("1947" = "1947-04-20", "1950" = "1950-06-18", "1954" = "1954-06-27", "1958" = "1958-07-06", "1962" = "1962-07-08", "1966" = "1966-07-10", "1970" = "1970-06-14")

if (file.exists(nrw_pre75_path)) {
  nrw_pre75 <- read.csv(nrw_pre75_path, colClasses = "character")

  ## OCR corrections: left-page extraction failed for these rows,
  ## putting WB(ohne) into NV and WB(mit) into IV; fix from source PDFs
  fix_schl <- nrw_pre75$name == "Kreis Schleiden" & nrw_pre75$election_year == "1966"
  if (any(fix_schl)) {
    nrw_pre75[fix_schl, c("eligible_voters","number_voters","invalid_votes")] <-
      list("40212", "34474", "774")
  }
  fix_bott <- nrw_pre75$name == "Bottrop" & nrw_pre75$election_year == "1970"
  if (any(fix_bott)) {
    nrw_pre75[fix_bott, c("eligible_voters","number_voters","invalid_votes")] <-
      list("71776", "58078", "307")
  }

  nrw_pre75_party_map <- c(
    cdu = "cdu", spd = "spd", fdp = "fdp",
    dkp = "dkp", npd = "npd", zentrum = "zentrum", uap = "uap", fsu = "fsu",
    dg = "dg", dfu = "dfu", gdp = "gdp", parteilose = "parteilose",
    bdd = "bdd", dp = "dp", drp = "drp", dsu = "dsu",
    bhe = "bhe", kpd = "kpd",
    rsf = "rsf", srp = "srp", csab = "csab", unabhaengige = "unabhaengige",
    rwvp = "rwvp"
  )

  for (ocr_yr in unique(nrw_pre75$election_year)) {
    yr_data <- nrw_pre75[nrw_pre75$election_year == ocr_yr, ]
    if (nrow(yr_data) == 0) next

    ## Synthetic AGS: 050xx000 (RB "0" unused in NRW, so no collision)
    ags_syn <- paste0("050", sprintf("%02d", seq_len(nrow(yr_data))), "000")

    result <- tibble(
      ags             = ags_syn,
      eligible_voters = as.numeric(yr_data$eligible_voters),
      number_voters   = as.numeric(yr_data$number_voters),
      invalid_votes   = as.numeric(yr_data$invalid_votes),
      valid_votes     = as.numeric(yr_data$valid_votes)
    )

    ## Clamp negative invalid_votes to NA (OCR artefacts)
    result$invalid_votes[result$invalid_votes < 0] <- NA_real_

    for (ocr_col in names(nrw_pre75_party_map)) {
      std_name <- nrw_pre75_party_map[[ocr_col]]
      if (ocr_col %in% names(yr_data)) {
        votes <- as.numeric(yr_data[[ocr_col]])
        votes[is.na(votes)] <- 0
      } else {
        votes <- rep(0, nrow(yr_data))
      }
      result[[paste0(std_name, "_n")]] <- votes
      result[[std_name]] <- votes / result$valid_votes
    }

    mapped_n_cols <- paste0(unique(nrw_pre75_party_map), "_n")
    mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result)]
    result <- result |>
      mutate(
        other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
        other_n = pmax(other_n, 0, na.rm = TRUE),
        other   = other_n / valid_votes
      )

    result <- result |>
      mutate(
        election_year = as.integer(ocr_yr),
        state = "05",
        election_date = as.Date(nrw_pre75_dates[ocr_yr]),
        turnout = number_voters / eligible_voters,
        cdu_csu = cdu
      ) |>
      select(-ends_with("_n"))

    cat("NRW", ocr_yr, "(Kreis) ...", nrow(result), "Kreise,",
        round(sum(result$valid_votes, na.rm = TRUE)), "VV\n")
    nrw_results[[ocr_yr]] <- result
  }
}

## ---------- XLSX: 1975-2022 (municipality level) ----------
for (yr in names(nrw_dates)) {
  cat("NRW", yr, "...")

  ## Find the file: old format = single file, new format = Wahlbeteiligung
  if (as.integer(yr) <= 2005) {
    nrw_file <- list.files(nrw_raw_path,
      pattern = paste0(yr, "_Landtagswahl\\.xlsx$"),
      full.names = TRUE, recursive = TRUE)
  } else {
    nrw_file <- list.files(nrw_raw_path,
      pattern = paste0(yr, "_Landtagswahl_Wahlbeteiligung\\.xlsx$"),
      full.names = TRUE, recursive = TRUE)
  }

  nrw_raw <- read_excel(nrw_file, col_names = FALSE, .name_repair = "minimal")

  ## Header structure: row 5 = party names, row 7 = election date, row 8+ = data
  r5 <- as.character(nrw_raw[5, ])
  nrw_data <- nrw_raw[8:nrow(nrw_raw), ]

  ## Column layout (same for both formats):
  ## Col 1 = AGS, Col 2 = Name, Col 3 = Wahlberechtigte (EV),
  ## Col 4 = Wähler, Col 5 = Gültige Stimmen / Zweitstimmen Insgesamt
  ## Cols 6+ = Party vote counts
  ags_raw <- as.character(nrw_data[[1]])
  ags_nchar <- nchar(ags_raw)

  ## Keep only municipality-level rows:
  ## - 8-digit: municipalities within Kreise
  ## - 5-digit: kreisfreie Städte (pad to 8 digits with "000")
  ## Exclude: 2-digit (state), 3-digit (Regierungsbezirk), 5-digit Kreise
  ## Kreise have 8-digit sub-entries; kreisfreie Städte do NOT.
  ## Identify 5-digit entries that are kreisfreie Städte (no 8-digit sub-rows)
  is_5digit <- ags_nchar == 5 & !is.na(ags_raw)
  is_8digit <- ags_nchar == 8 & !is.na(ags_raw)

  # A 5-digit AGS is kreisfrei if no 8-digit entry starts with it
  ags_5 <- ags_raw[is_5digit]
  ags_8_prefixes <- unique(substr(ags_raw[is_8digit], 1, 5))
  kreisfrei_5 <- ags_5[!ags_5 %in% ags_8_prefixes]

  keep <- (is_8digit) | (is_5digit & ags_raw %in% kreisfrei_5)
  nrw_data <- nrw_data[keep, ]
  ags_kept <- ags_raw[keep]

  # Pad 5-digit kreisfreie Städte to 8 digits
  ags_vec <- ifelse(nchar(ags_kept) == 5, paste0(ags_kept, "000"), ags_kept)

  eligible <- nrw_safe_num(nrw_data[[3]])
  voters   <- nrw_safe_num(nrw_data[[4]])
  valid_v  <- nrw_safe_num(nrw_data[[5]])
  invalid  <- voters - valid_v  # No separate invalid column

  ## Drop rows with no data ("-" entries due to Städteregion Aachen reform)
  ## Pre-2010: 05334xxx entries have "-"; 2010+: 05313/05354xxx have "-"
  has_data <- !is.na(eligible)
  nrw_data <- nrw_data[has_data, ]
  ags_vec  <- ags_vec[has_data]
  eligible <- eligible[has_data]
  voters   <- voters[has_data]
  valid_v  <- valid_v[has_data]
  invalid  <- invalid[has_data]

  ## Map party columns (col 6 onwards, skip "Insgesamt" and "Sonstige"/"Sonstige Parteien")
  party_start <- 6
  party_end <- ncol(nrw_raw)
  party_cols_idx <- party_start:party_end
  party_names_raw <- r5[party_cols_idx]

  ## Map each party column to standard name (skip aggregates)
  party_std <- vapply(party_names_raw, function(pn) {
    if (is.na(pn) || pn == "") return(NA_character_)
    if (grepl("^Insgesamt$|^Sonstige|^Summe", trimws(pn), ignore.case = TRUE)) return(NA_character_)
    nrw_map_party(pn)
  }, character(1), USE.NAMES = FALSE)

  mapped_votes <- list()
  for (i in seq_along(party_cols_idx)) {
    std <- party_std[i]
    if (!is.na(std)) {
      v <- nrw_safe_num(nrw_data[[party_cols_idx[i]]])
      if (std %in% names(mapped_votes)) {
        mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
      } else {
        mapped_votes[[std]] <- v
      }
    }
  }

  result <- tibble(
    ags = ags_vec,
    election_year = as.integer(yr),
    state = "05",
    election_date = as.Date(nrw_dates[yr]),
    eligible_voters = eligible,
    number_voters = voters,
    valid_votes = valid_v,
    invalid_votes = invalid
  )

  for (std_name in names(mapped_votes)) {
    result[[paste0(std_name, "_n")]] <- mapped_votes[[std_name]]
  }
  count_cols <- paste0(names(mapped_votes), "_n")
  mapped_sum <- rowSums(result[count_cols], na.rm = TRUE)
  result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)

  # Aggregate duplicate AGS (shouldn't happen but safety check)
  count_cols_all <- c(count_cols, "other_n")
  if (any(duplicated(result$ags))) {
    result <- result |>
      group_by(ags) |>
      summarise(
        election_year = first(election_year), state = first(state),
        election_date = first(election_date),
        eligible_voters = sum(eligible_voters, na.rm = TRUE),
        number_voters   = sum(number_voters, na.rm = TRUE),
        valid_votes     = sum(valid_votes, na.rm = TRUE),
        invalid_votes   = sum(invalid_votes, na.rm = TRUE),
        across(any_of(count_cols_all), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # Shares
  result <- result |> mutate(turnout = number_voters / eligible_voters)
  for (std_name in c(names(mapped_votes), "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  cat(nrow(result), "munis,", round(sum(result$eligible_voters, na.rm = TRUE)), "EV\n")
  nrw_results[[yr]] <- result
}

all_states[["nrw"]] <- bind_rows(nrw_results)
cat("NRW total:", nrow(all_states[["nrw"]]), "rows\n\n")


###############################################################################
####                       Niedersachsen (03)                              ####
###############################################################################

cat("=== NIEDERSACHSEN ===\n")

ni_raw_path <- file.path(raw_path, "Niedersachsen")

ni_map_party <- function(pname) normalise_party(pname)

## NI ZS party column mapping (vals index → party name)
## Header: cols 5-8 = CDU, SPD, GRÜNE, FDP; col 9 = Sonstige (aggregate, skip);
## cols 10-52 = individual parties under Sonstige
ni_zs_party_map <- c(
  "4" = "CDU", "5" = "SPD", "6" = "GRÜNE", "7" = "FDP",
  # Col 9 (vals[8]) = Sonstige aggregate — skip
  "9" = "AfD", "10" = "BüSo", "11" = "Bündnis C",
  "12" = "Bündnis Grundeinkommen", "13" = "DDP", "14" = "Deutsche Mitte",
  "15" = "dieBasis", "16" = "DIE FRAUEN", "17" = "DIE FREIHEIT",
  "18" = "Die Friesen", "19" = "DIE HAIE", "20" = "Die Humanisten",
  "21" = "Die Linke", "22" = "Die PARTEI", "23" = "Die Weissen",
  "24" = "DKP", "25" = "DP", "26" = "Einzelbewerber", "27" = "FAMILIE",
  "28" = "Freie Wähler", "29" = "Gesundheitsforschung", "30" = "GRAUE",
  "31" = "DIE GRAUEN", "32" = "LKR", "33" = "MDU", "34" = "NEIN",
  "35" = "NPD", "36" = "ÖDP", "37" = "PBC", "38" = "PIRATEN",
  "39" = "REP", "40" = "RRP", "41" = "Schill", "42" = "SGV",
  "43" = "sonstige", "44" = "Team Todenhöfer", "45" = "SFP",
  "46" = "STATT Partei", "47" = "Tierschutzpartei", "48" = "Volksabstimmung",
  "49" = "V-Partei³", "50" = "VOLT", "51" = "ZENTRUM"
)

## Helper: classify NI entities and return indices to keep
## Keeps: Mitgliedsgemeinde (indented 6d), Einheitsgemeinde (non-indented 6d, suffix<400),
##        kreisfreie Städte (3d, no 6d sub-entries)
## Skips: Samtgemeinde (6d, suffix>=400), Kreise (3d with sub-entries),
##        state/region aggregates (1d)
ni_keep_entities <- function(entities, codes, code_lengths, is_indented) {
  codes_6d <- codes[code_lengths == 6]
  prefixes_3d <- unique(substr(codes_6d, 1, 3))
  codes_3d <- codes[code_lengths == 3]
  kreisfrei_3d <- codes_3d[!codes_3d %in% prefixes_3d]

  keep <- rep(FALSE, length(entities))
  for (i in seq_along(entities)) {
    cl <- code_lengths[i]
    c <- codes[i]
    if (cl == 6) {
      suffix <- as.numeric(substr(c, 4, 6))
      if (is_indented[i]) keep[i] <- TRUE
      else if (suffix < 400) keep[i] <- TRUE
    } else if (cl == 3 && c %in% kreisfrei_3d) {
      keep[i] <- TRUE
    }
  }
  keep
}

## Helper: construct 8-digit AGS from NI code
ni_make_ags <- function(code) {
  if (nchar(code) == 3) paste0("03", code, "000") else paste0("03", code)
}

## ---- Individual ZS files (1998-2022, all use 2021 Gebietsstand) ----
## All files are SpreadsheetML XML with identical column layout:
## Col 2=EV, 3=Wähler, 4=Gültige, 5=CDU, 6=SPD, 7=GRÜNE, 8=FDP,
## 9=Sonstige, 10=AfD, ..., 22=Die Linke (fixed positions across all years)

ni_parse_zs <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)

  entity_idx <- grep('MergeAcross="51"', lines)

  entities <- sapply(entity_idx, function(i) {
    m <- regmatches(lines[i], gregexpr("<Data[^>]*>([^<]*)</Data>", lines[i]))[[1]]
    if (length(m) == 0) return("")
    sub("<Data[^>]*>", "", sub("</Data>", "", m[1]))
  })

  codes <- sub("\\s+.*", "", trimws(entities))
  code_lengths <- nchar(codes)
  is_indented <- grepl("^\\s", entities)
  keep <- ni_keep_entities(entities, codes, code_lengths, is_indented)

  extract_row_values <- function(eidx) {
    search_end <- min(eidx + 10, length(lines))
    hit <- grep("Anzahl</Data>", lines[(eidx + 1):search_end])
    if (length(hit) == 0) return(rep(NA_real_, 51))
    anzahl_line <- eidx + hit[1]
    row_end <- anzahl_line - 1 +
      grep("</Row>", lines[anzahl_line:min(anzahl_line + 60, length(lines))])[1]
    row_block <- paste(lines[anzahl_line:row_end], collapse = "")
    m <- regmatches(row_block, gregexpr("<Data[^>]*>([^<]*)</Data>", row_block))[[1]]
    vals <- sub("<Data[^>]*>", "", sub("</Data>", "", m))
    vals <- vals[-1]
    suppressWarnings(as.numeric(vals))
  }

  results <- vector("list", sum(keep))
  j <- 0
  for (i in which(keep)) {
    j <- j + 1
    vals <- extract_row_values(entity_idx[i])
    ags <- ni_make_ags(codes[i])
    row_data <- tibble(
      ags = ags,
      eligible_voters = vals[1], number_voters = vals[2], valid_votes = vals[3]
    )
    ## Extract all party columns from ni_zs_party_map
    for (vi in names(ni_zs_party_map)) {
      idx <- as.integer(vi)
      pname <- ni_zs_party_map[vi]
      std <- normalise_party(pname)
      col_n <- paste0(std, "_n")
      v <- if (idx <= length(vals)) vals[idx] else NA_real_
      if (col_n %in% names(row_data)) {
        row_data[[col_n]] <- row_data[[col_n]] + ifelse(is.na(v), 0, v)
      } else {
        row_data[[col_n]] <- v
      }
    }
    results[[j]] <- row_data
  }
  bind_rows(results)
}

ni_zs_dates <- c(
  "1998" = "1998-03-01", "2003" = "2003-02-02", "2008" = "2008-01-27",
  "2013" = "2013-01-20", "2017" = "2017-10-15", "2022" = "2022-10-09"
)

ni_results <- list()

for (yr in names(ni_zs_dates)) {
  cat("NI", yr, "...")
  zs_file <- list.files(ni_raw_path, pattern = paste0(yr, ".*_ZS[.]xml$"),
                        full.names = TRUE, recursive = TRUE)
  df <- ni_parse_zs(zs_file)

  df$invalid_votes <- df$number_voters - df$valid_votes

  ## Identify all _n party columns
  n_cols <- grep("_n$", names(df), value = TRUE)
  mapped_sum <- rowSums(df[n_cols], na.rm = TRUE)
  df$other_n <- pmax(df$valid_votes - mapped_sum, 0, na.rm = TRUE)
  n_cols <- c(n_cols, "other_n")

  ## Build result with meta + shares
  result <- tibble(
    ags = df$ags, election_year = as.integer(yr), state = "03",
    election_date = as.Date(ni_zs_dates[yr]),
    eligible_voters = df$eligible_voters, number_voters = df$number_voters,
    valid_votes = df$valid_votes, invalid_votes = df$invalid_votes,
    turnout = df$number_voters / df$eligible_voters
  )
  for (col_n in n_cols) {
    share_name <- sub("_n$", "", col_n)
    result[[share_name]] <- df[[col_n]] / df$valid_votes
  }
  result$cdu_csu <- result$cdu

  cat(nrow(result), "munis,", sum(result$eligible_voters, na.rm = TRUE), "EV\n")
  ni_results[[yr]] <- result
}

## ---- Compilation file (1974-1994, historical Gebietsstand) ----
## SpreadsheetML with 10 data columns: EV, Wähler, Gültige, CDU, SPD, FDP, GRÜNE,
## Sonstige, ödp, ABG. Entity blocks: name → "Stimmen" → data rows → "Anteile".
## 1974-1986: single vote (no I/II). 1990-1994: I=Erststimme, II=Zweitstimme.
## We use II for 1990+; 1998 rows are skipped (use individual file instead).

ni_comp_dates <- c(
  "1974" = "1974-06-09", "1978" = "1978-06-04", "1982" = "1982-03-21",
  "1986" = "1986-06-15", "1990" = "1990-05-13", "1994" = "1994-03-13"
)

ni_comp_file <- list.files(ni_raw_path, pattern = "1974-1998.*_ES[.]xml$",
                           full.names = TRUE, recursive = TRUE)
ni_comp_lines <- readLines(ni_comp_file, warn = FALSE)

# Find all MergeAcross="10" rows (entity names, Stimmen, Anteile labels)
ni_comp_merge_idx <- grep('MergeAcross="10"', ni_comp_lines)
ni_comp_merge_text <- sapply(ni_comp_merge_idx, function(i) {
  m <- regmatches(ni_comp_lines[i],
                  gregexpr("<Data[^>]*>([^<]*)</Data>", ni_comp_lines[i]))[[1]]
  if (length(m) == 0) return("")
  sub("<Data[^>]*>", "", sub("</Data>", "", m[1]))
})

ni_comp_is_geo <- !grepl("Stimmen|Anteile", ni_comp_merge_text)
ni_comp_geo_idx <- ni_comp_merge_idx[ni_comp_is_geo]
ni_comp_geo_names <- ni_comp_merge_text[ni_comp_is_geo]
ni_comp_stim_idx <- ni_comp_merge_idx[grepl("Stimmen", ni_comp_merge_text)]
ni_comp_antl_idx <- ni_comp_merge_idx[grepl("Anteile", ni_comp_merge_text)]

ni_comp_codes <- sub("\\s+.*", "", trimws(ni_comp_geo_names))
ni_comp_code_lens <- nchar(ni_comp_codes)
ni_comp_indented <- grepl("^\\s", ni_comp_geo_names)
ni_comp_keep <- ni_keep_entities(ni_comp_geo_names, ni_comp_codes,
                                 ni_comp_code_lens, ni_comp_indented)

cat("NI compilation: keeping", sum(ni_comp_keep), "of",
    length(ni_comp_geo_names), "entities\n")

for (ki in which(ni_comp_keep)) {
  ags <- ni_make_ags(ni_comp_codes[ki])
  stim_line <- ni_comp_stim_idx[ki]
  antl_line <- ni_comp_antl_idx[ki]

  block <- ni_comp_lines[(stim_line + 1):(antl_line - 1)]
  row_starts <- grep("<Row>", block)

  prev_i_data <- NULL
  for (rs in row_starts) {
    data_line <- stim_line + rs
    row_end <- data_line - 1 +
      grep("</Row>", ni_comp_lines[data_line:min(data_line + 20,
           length(ni_comp_lines))])[1]
    if (is.na(row_end)) next
    row_block <- paste(ni_comp_lines[data_line:row_end], collapse = "")
    m <- regmatches(row_block,
                    gregexpr("<Data[^>]*>([^<]*)</Data>", row_block))[[1]]
    vals_raw <- sub("<Data[^>]*>", "", sub("</Data>", "", m))
    label <- trimws(vals_raw[1])
    vals_raw <- vals_raw[-1]
    suppressWarnings(vals <- as.numeric(vals_raw))

    if (grepl("^LW", label)) {
      date_part <- sub("^LW\\s+", "", sub("\\s+I$", "", label))
      date_parts <- strsplit(trimws(date_part), "\\.")[[1]]
      if (length(date_parts) != 3) next
      year_2d <- as.integer(date_parts[3])
      year <- if (year_2d >= 70) 1900 + year_2d else 2000 + year_2d
      if (year == 1998) next

      if (year < 1990) {
        # Single vote: cols = EV, Wähler, Gültige, CDU, SPD, FDP, GRÜNE,
        # Sonstige(agg), ödp, ABG
        valid_v <- vals[3]
        odp_v <- if (length(vals) >= 9) vals[9] else NA_real_
        abg_v <- if (length(vals) >= 10) vals[10] else NA_real_
        mapped_sum <- sum(vals[4:7], na.rm = TRUE) +
          sum(c(odp_v, abg_v), na.rm = TRUE)
        other_n <- max(valid_v - mapped_sum, 0, na.rm = TRUE)
        result <- tibble(
          ags = ags, election_year = as.integer(year), state = "03",
          election_date = as.Date(ni_comp_dates[as.character(year)]),
          eligible_voters = vals[1], number_voters = vals[2],
          valid_votes = valid_v, invalid_votes = vals[2] - valid_v,
          turnout = vals[2] / vals[1],
          cdu = vals[4] / valid_v, spd = vals[5] / valid_v,
          fdp = vals[6] / valid_v,
          gruene = if (!is.na(vals[7])) vals[7] / valid_v else NA_real_,
          odp = if (!is.na(odp_v)) odp_v / valid_v else NA_real_,
          other = other_n / valid_v
        )
        result$cdu_csu <- result$cdu
        ni_results[[paste0(year, "_", ags)]] <- result
      } else {
        prev_i_data <- list(ev = vals[1], voters = vals[2], year = year)
      }
    } else if (label == "II" && !is.null(prev_i_data)) {
      year <- prev_i_data$year
      if (year == 1998) { prev_i_data <- NULL; next }
      # II row: vals[1:2] = NA (empty EV/Wähler), vals[3]=Gültige, vals[4:7]=parties
      valid_v <- vals[3]
      odp_v <- if (length(vals) >= 9) vals[9] else NA_real_
      abg_v <- if (length(vals) >= 10) vals[10] else NA_real_
      mapped_sum <- sum(vals[4:7], na.rm = TRUE) +
        sum(c(odp_v, abg_v), na.rm = TRUE)
      other_n <- max(valid_v - mapped_sum, 0, na.rm = TRUE)
      result <- tibble(
        ags = ags, election_year = as.integer(year), state = "03",
        election_date = as.Date(ni_comp_dates[as.character(year)]),
        eligible_voters = prev_i_data$ev,
        number_voters = prev_i_data$voters,
        valid_votes = valid_v,
        invalid_votes = prev_i_data$voters - valid_v,
        turnout = prev_i_data$voters / prev_i_data$ev,
        cdu = vals[4] / valid_v, spd = vals[5] / valid_v,
        fdp = vals[6] / valid_v, gruene = vals[7] / valid_v,
        odp = if (!is.na(odp_v)) odp_v / valid_v else NA_real_,
        other = other_n / valid_v
      )
      result$cdu_csu <- result$cdu
      ni_results[[paste0(year, "_", ags)]] <- result
      prev_i_data <- NULL
    }
  }
}

all_states[["ni"]] <- bind_rows(ni_results)

## Remove Samtgemeinde Sottrum aggregate row (03357406)
## SG-level votes are separate from member Gemeinden; this AGS doesn't exist
## in any crosswalk year and cannot be harmonized.
sg_mask <- all_states[["ni"]]$ags == "03357406"
if (any(sg_mask)) {
  cat("  Removing", sum(sg_mask), "SG Sottrum rows (03357406)\n")
  all_states[["ni"]] <- all_states[["ni"]][!sg_mask, ]
}

cat("Niedersachsen total:", nrow(all_states[["ni"]]), "rows\n\n")


###############################################################################
####                          Berlin (11)                                  ####
###############################################################################

cat("=== BERLIN ===\n")

be_raw_path <- file.path(raw_path, "Berlin")

be_map_party <- function(pname) normalise_party(pname)

be_dates <- c(
  "1990" = "1990-12-02", "1995" = "1995-10-22", "1999" = "1999-10-10",
  "2001" = "2001-10-21", "2006" = "2006-09-17", "2011" = "2011-09-18",
  "2016" = "2016-09-18", "2021" = "2021-09-26", "2023" = "2023-02-12"
)

be_results <- list()
be_files <- list.files(be_raw_path, pattern = "\\.(xlsx|xls)$", full.names = TRUE)
be_files <- be_files[grepl("AGH|1990_AGH", basename(be_files)) &
                       !grepl("1992", basename(be_files))]

for (f in sort(be_files)) {
  yr <- regmatches(basename(f), regexpr("[0-9]{4}", basename(f)))
  cat("BE", yr, "...")

  sheets <- excel_sheets(f)
  sheet <- if ("AGH_W2" %in% sheets) "AGH_W2" else "Zweitstimme"
  raw <- suppressWarnings(read_excel(f, sheet = sheet))
  cols <- names(raw)

  ev_col      <- grep("Wahlberechtigte.*insgesamt", cols, value = TRUE)[1]
  voter_col   <- grep("^W.hler$|^W.hlende$", cols, value = TRUE)[1]
  valid_col   <- grep("G.ltige Stimmen", cols, value = TRUE)[1]
  invalid_col <- grep("Ung.ltige Stimmen", cols, value = TRUE)[1]

  # Briefwahl rows have EV = 0; include them for votes but not EV
  wba_col <- grep("Wahlbezirksart", cols, value = TRUE)[1]
  if (!is.na(wba_col)) {
    is_brief <- grepl("Brief|^B$", as.character(raw[[wba_col]]))
  } else {
    is_brief <- rep(FALSE, nrow(raw))
  }

  ev_vals  <- as.numeric(raw[[ev_col]])
  eligible <- sum(ev_vals[!is_brief & ev_vals > 0], na.rm = TRUE)
  voters   <- sum(as.numeric(raw[[voter_col]]), na.rm = TRUE)
  valid_v  <- sum(as.numeric(raw[[valid_col]]), na.rm = TRUE)
  invalid  <- sum(as.numeric(raw[[invalid_col]]), na.rm = TRUE)

  # Party columns: everything after the last voting meta column
  meta_positions <- na.omit(match(c(ev_col, voter_col, valid_col, invalid_col), cols))
  skip_cols <- grep("Wahlberechtigte A|W.hler B|W.hlende B", cols)
  party_col_indices <- setdiff((max(meta_positions) + 1):length(cols), skip_cols)
  party_col_names <- cols[party_col_indices]

  ## Skip aggregate columns (Sonstige, Summe, Insgesamt)
  party_col_names <- party_col_names[
    !grepl("^Sonstige$|^Summe$|^Insgesamt$|^Übrige$", party_col_names, ignore.case = TRUE)
  ]

  mapped_votes <- list()
  for (pc in party_col_names) {
    std <- be_map_party(pc)
    v <- sum(as.numeric(raw[[pc]]), na.rm = TRUE)
    if (std %in% names(mapped_votes)) {
      mapped_votes[[std]] <- mapped_votes[[std]] + v
    } else {
      mapped_votes[[std]] <- v
    }
  }

  mapped_sum <- sum(unlist(mapped_votes), na.rm = TRUE)
  other_v <- max(valid_v - mapped_sum, 0)

  result <- tibble(
    ags = "11000000", election_year = as.integer(yr), state = "11",
    election_date = as.Date(be_dates[yr]),
    eligible_voters = eligible, number_voters = voters,
    valid_votes = valid_v, invalid_votes = invalid,
    turnout = voters / eligible
  )
  for (std_name in names(mapped_votes)) {
    result[[std_name]] <- mapped_votes[[std_name]] / valid_v
  }
  result$other <- other_v / valid_v
  result$cdu_csu <- result$cdu

  cat(nrow(result), "row, EV=", eligible, "\n")
  be_results[[yr]] <- result
}

all_states[["be"]] <- bind_rows(be_results)
cat("Berlin total:", nrow(all_states[["be"]]), "rows\n\n")


###############################################################################
####  Bremen (HB, state 04)  — 6 elections: 1999-2023, 2 municipalities   ####
###############################################################################
##
## Bremen has 2 municipalities: Stadt Bremen (04011000) and Bremerhaven (04012000).
## Multiple format eras:
##  - 2015 & 2019: CSV files, 5-vote Kumulieren/Panaschieren system.
##       Separate files for Bremen and BHV. Party votes in Dx_SUMME_LISTE_KANDIDATEN.
##       valid_votes = D2 column = total Personenstimmen.
##  - 1999, 2003, 2007: XLS files with single-vote system. Combined Bremen+BHV in
##       one file, split by "WAHLBEREICH BREMERHAVEN" marker row. OT-level data
##       aggregated per municipality.
##  - 2023: XLSX summary files (one per municipality). German number format (dots as
##       thousands separators). Explicit "Gültige Stimmen" row for valid votes.
##  - Pre-1995 files are BIFF format (unreadable). 2011 only available as PDF.
##
## NOTE: Since 2011, Bremen uses a 5-vote system where valid_votes = total
## Personenstimmen, which greatly exceeds the number of voters (5× ballots).

hb_map_party <- function(pname) {
  p <- trimws(pname)
  ## Skip meta rows and aggregate labels
  if (grepl("^Wahlberechtigte|^W.hler|^Ung.ltige|^G.ltige|^Wahl zur|^Stimmen|^Enderg",
            p, ignore.case = TRUE)) return(NA_character_)
  normalise_party(p)
}

hb_german_num <- function(x) {
  x <- as.character(x)
  x <- gsub("[.]", "", x)   # remove thousands separator
  x <- gsub(",", ".", x)     # convert decimal comma to point
  suppressWarnings(as.numeric(x))
}

hb_safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

hb_raw_path <- "data/state_elections/raw/Landtagswahlen/Bremen"
hb_results <- list()

## ---- 1946-1995 converted XLSX (percentages, no cell headers) ----
## Source: Converted BIFF→XLSX files. Data is vote share percentages at Ortsteil
## level. Column headers are stored as graphical objects, not cell values.
## We use hardcoded party column assignments derived from the drawing XML.
## Strategy: extract aggregate "Stadt Bremen" and "Bremerhaven" summary rows,
## which already contain city-level totals (EV absolute, turnout %, party %).
## Note: valid_votes and invalid_votes are NA (only percentages available).

hb_pre99_config <- list(
  "1946" = list(
    date = "1946-10-13",
    party_names = c("spd", "cdu", "bdv", "kpd", "unabhangige"),
    main_pattern = "1946\\.xlsx$", bhv_pattern = NULL
  ),
  "1947" = list(
    date = "1947-10-12",
    party_names = c("spd", "cdu", "bdv", "kpd", "fdp", "dp", "rsf", "unabhangige"),
    main_pattern = "1947\\.xlsx$", bhv_pattern = NULL
  ),
  "1951" = list(
    date = "1951-10-07",
    party_names = c("spd", "cdu", "fdp", "kpd", "dp", "bhe", "fsu", "srp", "wahlergemeinschaft"),
    main_pattern = "1951\\.xlsx$", bhv_pattern = NULL
  ),
  "1955" = list(
    date = "1955-10-09",
    party_names = c("spd", "cdu", "fdp", "dp", "kpd", "other"),
    main_pattern = "1955\\.xlsx$", bhv_pattern = NULL
  ),
  "1959" = list(
    date = "1959-10-11",
    party_names = c("spd", "cdu", "fdp", "dp", "drp", "other"),
    main_pattern = "1959\\.xlsx$", bhv_pattern = NULL
  ),
  "1963" = list(
    date = "1963-09-29",
    party_names = c("spd", "cdu", "fdp", "dp", "dfu", "gdp"),
    main_pattern = "1963\\.xlsx$", bhv_pattern = NULL
  ),
  "1967" = list(
    date = "1967-10-01",
    party_names = c("spd", "cdu", "fdp", "dp", "dfu", "npd"),
    main_pattern = "1967\\.xlsx$", bhv_pattern = NULL
  ),
  "1971" = list(
    date = "1971-10-10",
    party_names = c("spd", "cdu", "fdp", "npd", "dkp"),
    main_pattern = "1971\\.xlsx$", bhv_pattern = NULL
  ),
  "1975" = list(
    date = "1975-10-12",
    party_names = c("spd", "cdu", "fdp", "dkp", "npd", "other"),
    main_pattern = "1975\\.xlsx$", bhv_pattern = NULL
  ),
  "1979" = list(
    date = "1979-10-07",
    party_names = c("spd", "cdu", "fdp", "gruene", "gruene", "dkp", "other"),
    main_pattern = "1979\\.xlsx$", bhv_pattern = NULL
  ),
  "1983" = list(
    date = "1983-09-25",
    party_names = c("spd", "cdu", "fdp", "gruene", "gruene", "other"),
    main_pattern = "1983\\.xlsx$", bhv_pattern = NULL
  ),
  "1987" = list(
    date = "1987-09-13",
    party_names = c("spd", "cdu", "gruene", "fdp", "liste_d", "other"),
    main_pattern = "1987\\.xlsx$", bhv_pattern = NULL
  ),
  "1991" = list(
    date = "1991-09-29",
    party_names = c("spd", "cdu", "gruene", "fdp", "dvu", "rep", "other"),
    main_pattern = "1991\\.xlsx$", bhv_pattern = NULL
  ),
  "1995" = list(
    date = "1995-05-14",
    ## "Sonstige" (col 9) includes DVU+AfB as subcategories ("darunter").
    ## Mark "other" as _sonstige_incl to compute: other = Sonstige - DVU - AfB
    party_names = c("spd", "cdu", "gruene", "fdp", "_sonstige_incl", "dvu", "afb"),
    main_pattern = "1995_02\\.xlsx$",   # Stadt Bremen total is in _02 file
    bhv_pattern = "1995 BHV\\.xlsx$"
  )
)

for (yr in names(hb_pre99_config)) {
  cfg <- hb_pre99_config[[yr]]
  cat("HB", yr, "...")

  ## --- Helper to extract aggregate row from an xlsx ---
  hb_extract_aggregate <- function(fpath, search_pattern) {
    raw <- read_excel(fpath, col_names = FALSE, .name_repair = "minimal")
    labels <- as.character(raw[[2]])
    # Find matching aggregate row
    idx <- grep(search_pattern, labels, ignore.case = TRUE)
    if (length(idx) == 0) return(NULL)
    # Use the LAST match (city total is usually after subtotals)
    row_idx <- idx[length(idx)]
    vals <- as.character(raw[row_idx, ])
    return(vals)
  }

  ## --- Read Stadt Bremen aggregate ---
  hb_main <- list.files(hb_raw_path, pattern = cfg$main_pattern, full.names = TRUE)
  if (length(hb_main) == 0) { cat("SKIP (no file)\n"); next }

  hb_row <- hb_extract_aggregate(hb_main[1], "Stadt Bremen")
  if (is.null(hb_row)) { cat("SKIP (no aggregate row)\n"); next }

  hb_ev <- hb_safe_num(gsub(" ", "", hb_row[3]))    # EV (may have space in number)
  hb_to <- hb_safe_num(gsub(",", ".", hb_row[4]))    # Turnout %
  hb_voters <- round(hb_ev * hb_to / 100)

  result_hb <- tibble(
    ags = "04011000", election_year = as.integer(yr), state = "04",
    election_date = as.Date(cfg$date),
    eligible_voters = hb_ev, number_voters = hb_voters,
    valid_votes = NA_real_, invalid_votes = NA_real_,
    turnout = hb_to / 100
  )

  # Extract party percentages from cols 5+
  sonstige_incl_pct <- 0
  for (pi in seq_along(cfg$party_names)) {
    col_idx <- 4 + pi
    pname <- cfg$party_names[pi]
    pval <- hb_safe_num(gsub(",", ".", gsub(" ", "", hb_row[col_idx])))
    if (pname == "_sonstige_incl") {
      # "Sonstige" that includes subcategories (e.g. DVU+AfB in 1995)
      sonstige_incl_pct <- ifelse(is.na(pval), 0, pval)
      next
    }
    if (is.na(pval) || pval == 0 || pname == "other") {
      if (pname == "other") result_hb$other <- ifelse(is.na(pval), 0, pval / 100)
      next
    }
    if (pname %in% names(result_hb)) {
      result_hb[[pname]] <- result_hb[[pname]] + pval / 100
    } else {
      result_hb[[pname]] <- pval / 100
    }
  }
  # If _sonstige_incl was used, compute other = sonstige - named subcategories
  if (sonstige_incl_pct > 0) {
    named_sub_pct <- sum(sapply(cfg$party_names, function(p) {
      if (p %in% c("_sonstige_incl", "other") || !p %in% names(result_hb)) return(0)
      # Only count subcategories that come AFTER _sonstige_incl in the config
      si_pos <- which(cfg$party_names == "_sonstige_incl")
      p_pos <- which(cfg$party_names == p)
      if (any(p_pos > si_pos)) return(result_hb[[p]][1] * 100) else return(0)
    }))
    result_hb$other <- max(sonstige_incl_pct - named_sub_pct, 0) / 100
  }
  # Compute "other" if not explicitly provided
  if (!"other" %in% names(result_hb)) {
    existing_pcols <- intersect(cfg$party_names[!cfg$party_names %in% c("other", "_sonstige_incl")], names(result_hb))
    party_sum <- if (length(existing_pcols) > 0) sum(unlist(result_hb[existing_pcols]), na.rm = TRUE) else 0
    result_hb$other <- max(1 - party_sum, 0)
  }
  result_hb$cdu_csu <- result_hb$cdu
  cat(sprintf(" HB:EV=%.0f", hb_ev))
  hb_results[[paste0(yr, "_04011000")]] <- result_hb

  ## --- Read Bremerhaven aggregate ---
  if (!is.null(cfg$bhv_pattern)) {
    bhv_file <- list.files(hb_raw_path, pattern = cfg$bhv_pattern, full.names = TRUE)
    bhv_row <- if (length(bhv_file) > 0) {
      hb_extract_aggregate(bhv_file[1], "Stadt Bremerhaven|Bremerhaven")
    } else NULL
  } else {
    # BHV is in the main file (1947-1991)
    bhv_row <- hb_extract_aggregate(hb_main[1], "Bremerhaven")
  }

  if (!is.null(bhv_row) && yr != "1946") {
    bhv_ev <- hb_safe_num(gsub(" ", "", bhv_row[3]))
    bhv_to <- hb_safe_num(gsub(",", ".", bhv_row[4]))
    bhv_voters <- round(bhv_ev * bhv_to / 100)

    result_bhv <- tibble(
      ags = "04012000", election_year = as.integer(yr), state = "04",
      election_date = as.Date(cfg$date),
      eligible_voters = bhv_ev, number_voters = bhv_voters,
      valid_votes = NA_real_, invalid_votes = NA_real_,
      turnout = bhv_to / 100
    )

    sonstige_incl_pct_bhv <- 0
    for (pi in seq_along(cfg$party_names)) {
      col_idx <- 4 + pi
      pname <- cfg$party_names[pi]
      pval <- hb_safe_num(gsub(",", ".", gsub(" ", "", bhv_row[col_idx])))
      if (pname == "_sonstige_incl") {
        sonstige_incl_pct_bhv <- ifelse(is.na(pval), 0, pval)
        next
      }
      if (is.na(pval) || pval == 0 || pname == "other") {
        if (pname == "other") result_bhv$other <- ifelse(is.na(pval), 0, pval / 100)
        next
      }
      if (pname %in% names(result_bhv)) {
        result_bhv[[pname]] <- result_bhv[[pname]] + pval / 100
      } else {
        result_bhv[[pname]] <- pval / 100
      }
    }
    if (sonstige_incl_pct_bhv > 0) {
      named_sub_pct_bhv <- sum(sapply(cfg$party_names, function(p) {
        if (p %in% c("_sonstige_incl", "other") || !p %in% names(result_bhv)) return(0)
        si_pos <- which(cfg$party_names == "_sonstige_incl")
        p_pos <- which(cfg$party_names == p)
        if (any(p_pos > si_pos)) return(result_bhv[[p]][1] * 100) else return(0)
      }))
      result_bhv$other <- max(sonstige_incl_pct_bhv - named_sub_pct_bhv, 0) / 100
    }
    if (!"other" %in% names(result_bhv)) {
      existing_pcols_bhv <- intersect(cfg$party_names[!cfg$party_names %in% c("other", "_sonstige_incl")], names(result_bhv))
    party_sum <- if (length(existing_pcols_bhv) > 0) sum(unlist(result_bhv[existing_pcols_bhv]), na.rm = TRUE) else 0
      result_bhv$other <- max(1 - party_sum, 0)
    }
    result_bhv$cdu_csu <- result_bhv$cdu
    cat(sprintf(" BHV:EV=%.0f", bhv_ev))
    hb_results[[paste0(yr, "_04012000")]] <- result_bhv
  }
  cat("\n")
}

## ---- 2015 & 2019 CSV (5-vote Kumulieren) ----
hb_csv_party_map <- list(
  "2015" = c(D1="spd", D2="gruene", D3="cdu", D4="linke_pds",
             D5=NA, D6="fdp", D7=NA, D9="afd", D10=NA, D11=NA),
  "2019" = c(D1="spd", D2="cdu", D3="gruene", D4="linke_pds",
             D5="fdp", D6="afd", D7=NA, D8=NA, D9=NA, D10=NA,
             D11=NA, D12=NA, D13=NA, D14=NA, D15=NA, D16=NA)
)
hb_csv_dates <- c("2015" = "2015-05-10", "2019" = "2019-05-26")

for (yr in c("2015", "2019")) {
  cat("HB", yr, "...")
  party_map <- hb_csv_party_map[[yr]]
  hb_file <- list.files(hb_raw_path, pattern = paste0(yr, "[.]csv$"), full.names = TRUE)
  bhv_file <- list.files(hb_raw_path, pattern = paste0(yr, ".*BHV[.]csv$"), full.names = TRUE)

  for (muni in list(
    list(file = hb_file, ags = "04011000", name = "Bremen"),
    list(file = bhv_file, ags = "04012000", name = "BHV"))) {

    df <- read.csv2(muni$file, fileEncoding = "latin1", stringsAsFactors = FALSE)
    eligible <- sum(hb_safe_num(df$A), na.rm = TRUE)
    voters  <- sum(hb_safe_num(df$B), na.rm = TRUE)
    invalid <- sum(hb_safe_num(df$C), na.rm = TRUE)
    total_stimmen <- sum(hb_safe_num(df$D2), na.rm = TRUE)   # D2 = total Personenstimmen

    mapped_votes <- list()
    slk_cols <- grep("_SUMME_LISTE_KANDIDATEN$", names(df), value = TRUE)
    for (dc in slk_cols) {
      d_code <- sub("_SUMME_LISTE_KANDIDATEN$", "", dc)
      if (d_code %in% names(party_map) && !is.na(party_map[d_code])) {
        std <- party_map[d_code]
        v <- sum(hb_safe_num(df[[dc]]), na.rm = TRUE)
        if (std %in% names(mapped_votes)) mapped_votes[[std]] <- mapped_votes[[std]] + v
        else mapped_votes[[std]] <- v
      }
    }

    mapped_sum <- sum(unlist(mapped_votes), na.rm = TRUE)
    other_v <- max(total_stimmen - mapped_sum, 0)

    result <- tibble(
      ags = muni$ags, election_year = as.integer(yr), state = "04",
      election_date = as.Date(hb_csv_dates[yr]),
      eligible_voters = eligible, number_voters = voters,
      valid_votes = total_stimmen, invalid_votes = invalid,
      turnout = voters / eligible    )
    for (s in names(mapped_votes)) result[[s]] <- mapped_votes[[s]] / total_stimmen
    result$other <- other_v / total_stimmen
    result$cdu_csu <- result$cdu
    cat(sprintf(" %s:EV=%.0f", muni$name, eligible))
    hb_results[[paste0(yr, "_", muni$ags)]] <- result
  }
  cat("\n")
}

## ---- 2011 (5-vote system, data from official Faltblatt PDF) ----
## Source: Statistisches Landesamt Bremen, Faltblatt Bürgerschaft 2011
## No downloadable file available; wahlen-bremen.de returned 503.
## Data extracted from PDF: EV, voters, invalid, gültige Stimmen (Gesamtstimmen),
## and per-party Gesamtstimmen shares (L+P combined).
## Amtsblatt: Bremen EV=408435, Wähler=232883, ungültig=7262, gültig=225621,
##            gültige Stimmen=1115686
##            Bremerhaven EV=85732, Wähler=41240, ungültig=1877, gültig=39363,
##            gültige Stimmen=193669
cat("HB 2011 ...")

hb_2011_data <- list(
  list(ags = "04011000", ev = 408435, voters = 232883, invalid = 7262,
       valid_stimmen = 1115686,
       parties = c(spd = 0.393, cdu = 0.204, gruene = 0.226,
                   linke_pds = 0.058, fdp = 0.022, biw = 0.031)),
  list(ags = "04012000", ev = 85732, voters = 41240, invalid = 1877,
       valid_stimmen = 193669,
       parties = c(spd = 0.343, cdu = 0.201, gruene = 0.218,
                   linke_pds = 0.046, fdp = 0.031, biw = 0.071))
)

for (muni in hb_2011_data) {
  result <- tibble(
    ags = muni$ags, election_year = 2011L, state = "04",
    election_date = as.Date("2011-05-22"),
    eligible_voters = muni$ev, number_voters = muni$voters,
    valid_votes = muni$valid_stimmen, invalid_votes = muni$invalid,
    turnout = muni$voters / muni$ev
  )
  party_sum <- 0
  for (pn in names(muni$parties)) {
    result[[pn]] <- muni$parties[pn]
    party_sum <- party_sum + muni$parties[pn]
  }
  result$other <- max(1 - party_sum, 0)
  result$cdu_csu <- result$cdu
  cat(sprintf(" %s:EV=%.0f", muni$ags, muni$ev))
  hb_results[[paste0("2011_", muni$ags)]] <- result
}
cat("\n")

## ---- 1999, 2003, 2007 XLS (single vote, combined Bremen+BHV) ----
## Per-year column layout configs (columns differ by year)
hb_xls_config <- list(
  "2007" = list(
    date = "2007-05-13",
    data_start = 11,    # first OT data row
    code_col = 1,       # column with 3-digit OT codes
    ev_col = 7, voter_col = 8, invalid_col = 11, valid_col = 12,
    party_abs_start = 13, party_abs_step = 2,   # absolute votes at 13, 15, 17, ...
    party_map = c(D1 = "spd", D2 = "cdu", D3 = "gruene", D4 = "fdp", D5 = NA,
                  D6 = NA, D7 = NA, D8 = NA, D9 = NA, D10 = "linke_pds",
                  D11 = NA, D12 = NA, D13 = NA)
  ),
  "2003" = list(
    date = "2003-05-25",
    data_start = 9,
    code_col = 1,
    ev_col = 6, voter_col = 7, invalid_col = 10, valid_col = 11,
    party_abs_start = 12, party_abs_step = 2,
    party_map = c(D1 = "spd", D2 = "cdu", D3 = "gruene", D4 = NA, D5 = NA,
                  D6 = NA, D7 = NA, D8 = NA, D9 = NA, D10 = "fdp",
                  D11 = NA, D12 = "linke_pds", D13 = NA, D14 = NA)
  ),
  "1999" = list(
    date = "1999-06-06",
    data_start = 8,
    code_col = 2,       # 1999: col 1 = sequential nr, col 2 = OT code
    ev_col = 7, voter_col = 8, invalid_col = 11, valid_col = 12,
    party_abs_start = 13, party_abs_step = 2,
    party_map = c(D1 = "spd", D2 = "cdu", D3 = "gruene", D4 = NA, D5 = NA,
                  D6 = "fdp", D7 = NA, D8 = NA, D9 = "linke_pds")
  )
)

for (yr in c("2007", "2003", "1999")) {
  cat("HB", yr, "...")
  cfg <- hb_xls_config[[yr]]
  f <- list.files(hb_raw_path, pattern = paste0(yr, "[.]xls$"), full.names = TRUE)
  raw <- read_excel(f, col_names = FALSE, .name_repair = "minimal")

  data <- raw[cfg$data_start:nrow(raw), ]
  codes <- trimws(as.character(data[[cfg$code_col]]))
  is_ot <- !is.na(codes) & grepl("^[0-9]{3}$", codes)
  ot_data <- data[is_ot, ]

  # Split Bremen/BHV: search ALL first 5 cols for "BREMERHAVEN" marker
  bhv_start <- nrow(raw) + 1
  for (j in 1:min(5, ncol(raw))) {
    bhv_rows <- which(grepl("BREMERHAVEN|Bremerhaven", as.character(raw[[j]])))
    if (length(bhv_rows) > 0) bhv_start <- min(bhv_start, bhv_rows[1])
  }

  ot_global <- which(is_ot) + cfg$data_start - 1
  is_hb_ot  <- ot_global < bhv_start
  is_bhv_ot <- ot_global >= bhv_start

  for (muni in list(
    list(mask = is_hb_ot,  ags = "04011000", name = "Bremen"),
    list(mask = is_bhv_ot, ags = "04012000", name = "BHV"))) {

    sub <- ot_data[muni$mask, ]
    if (nrow(sub) == 0) next

    eligible <- sum(hb_safe_num(sub[[cfg$ev_col]]),      na.rm = TRUE)
    voters   <- sum(hb_safe_num(sub[[cfg$voter_col]]),    na.rm = TRUE)
    invalid  <- sum(hb_safe_num(sub[[cfg$invalid_col]]),  na.rm = TRUE)
    valid_v  <- sum(hb_safe_num(sub[[cfg$valid_col]]),    na.rm = TRUE)

    mapped_votes <- list()
    d_idx <- 1; col_idx <- cfg$party_abs_start
    while (col_idx <= ncol(sub)) {
      d_code <- paste0("D", d_idx)
      if (d_code %in% names(cfg$party_map) && !is.na(cfg$party_map[d_code])) {
        std <- cfg$party_map[d_code]
        v <- sum(hb_safe_num(sub[[col_idx]]), na.rm = TRUE)
        if (std %in% names(mapped_votes)) mapped_votes[[std]] <- mapped_votes[[std]] + v
        else mapped_votes[[std]] <- v
      }
      d_idx <- d_idx + 1
      col_idx <- col_idx + cfg$party_abs_step
    }

    mapped_sum <- sum(unlist(mapped_votes), na.rm = TRUE)
    other_v <- max(valid_v - mapped_sum, 0)

    result <- tibble(
      ags = muni$ags, election_year = as.integer(yr), state = "04",
      election_date = as.Date(cfg$date),
      eligible_voters = eligible, number_voters = voters,
      valid_votes = valid_v, invalid_votes = invalid,
      turnout = voters / eligible    )
    for (s in names(mapped_votes)) result[[s]] <- mapped_votes[[s]] / valid_v
    result$other <- other_v / valid_v
    result$cdu_csu <- result$cdu
    cat(sprintf(" %s:EV=%.0f", muni$name, eligible))
    hb_results[[paste0(yr, "_", muni$ags)]] <- result
  }
  cat("\n")
}

## ---- 2023 XLSX (city-level summary, German number format) ----
cat("HB 2023 ...")
for (muni in list(
  list(pattern = "2023[.]xlsx$",       ags = "04011000", name = "Bremen"),
  list(pattern = "2023.*BHV[.]xlsx$",  ags = "04012000", name = "BHV"))) {

  f23 <- list.files(hb_raw_path, pattern = muni$pattern, full.names = TRUE)
  if (length(f23) == 0) next
  raw23 <- read_excel(f23, col_names = FALSE, .name_repair = "minimal")

  labels <- as.character(raw23[[1]])
  values <- hb_german_num(as.character(raw23[[2]]))

  ev_row      <- grep("Wahlberechtigte",  labels, ignore.case = TRUE)[1]
  voter_row   <- grep("hler",             labels, ignore.case = TRUE)[1]
  invalid_row <- grep("Ung.ltige",        labels, ignore.case = TRUE)[1]
  valid_row   <- grep("G.ltige Stimmen",  labels, ignore.case = TRUE)[1]

  eligible <- values[ev_row]
  voters   <- values[voter_row]
  invalid  <- values[invalid_row]
  valid_v  <- values[valid_row]   # explicit "Gültige Stimmen" row

  # Map party rows (skip meta rows)
  mapped_votes <- list()
  meta_pat <- "Wahlberechtigte|hler|Ung.ltige|G.ltige|Wahl zur|Stimmen tabel|Enderg"
  for (i in seq_along(labels)) {
    if (is.na(labels[i]) || is.na(values[i])) next
    std <- hb_map_party(labels[i])
    if (!is.na(std)) {
      if (std %in% names(mapped_votes)) mapped_votes[[std]] <- mapped_votes[[std]] + values[i]
      else mapped_votes[[std]] <- values[i]
    }
  }

  mapped_sum <- sum(unlist(mapped_votes), na.rm = TRUE)
  other_v <- max(valid_v - mapped_sum, 0)

  result <- tibble(
    ags = muni$ags, election_year = 2023L, state = "04",
    election_date = as.Date("2023-05-14"),
    eligible_voters = eligible, number_voters = voters,
    valid_votes = valid_v, invalid_votes = invalid,
    turnout = voters / eligible  )
  for (s in names(mapped_votes)) result[[s]] <- mapped_votes[[s]] / valid_v
  result$other <- other_v / valid_v
  result$cdu_csu <- result$cdu
  cat(sprintf(" %s:EV=%.0f", muni$name, eligible))
  hb_results[[paste0("2023_", muni$ags)]] <- result
}
cat("\n")

all_states[["hb"]] <- standardise(bind_rows(hb_results))
cat("Bremen total:", nrow(all_states[["hb"]]), "rows\n\n")


###############################################################################
####  Schleswig-Holstein (SH, state 01)  — 8 elections: 1983-2022         ####
###############################################################################
##
## All files are at Wahlbezirk (ballot district) level → aggregate to municipality.
## The "Statistische Kennziffer" (col 1) encodes: digits 1-2 = Kreis within SH,
## digits 3-5 = Gemeinde, digits 6-8 = Wahlbezirk. Standard 8-digit AGS =
## "01" + 3-digit Kreis (zero-padded) + 3-digit Gemeinde.
##
## Format eras:
##  - 2012, 2017, 2022: XLSX/XLS with 3 header rows (title, names, codes).
##       Column codes row (row 3) has A/B/E/F codes. Use F columns (Zweitstimmen).
##  - 2000, 2005, 2009: XLS with NO header rows. Data starts at row 1.
##       Column positions known from Infodat documentation.
##  - 1996: XLSX, no header rows. Only Erststimmen (no Zweitstimmen split).
##
## NOTE: 2012 codes are sometimes 7 digits (missing leading 0 for kreisfreie
## Städte). Left-padded to 8 digits before processing.

sh_map_party <- function(pname) {
  p <- trimws(pname)
  p <- gsub("-\n", "", p)
  p <- gsub("\n", " ", p)
  p <- gsub("\\s*(Erst|Zweit)stimmen$", "", p, ignore.case = TRUE)
  p <- trimws(p)
  normalise_party(p)
}

sh_safe_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

sh_make_ags <- function(code8) {
  kz <- as.integer(substr(code8, 1, 2))
  gm <- substr(code8, 3, 5)
  paste0("01", sprintf("%03d", kz), gm)
}

sh_raw_path <- "data/state_elections/raw/Landtagswahlen/Schleswig-Holstein"
sh_results <- list()

## ---- 2012, 2017, 2022: header rows 1-3, data from row 4, F cols = ZS ----
sh_new_config <- list(
  "2012" = list(file = "Schleswig-Holstein_2012_Landtag.xls",
                date = "2012-05-06"),
  "2017" = list(file = "Schleswig-Holstein_2017_Landtag.xlsx",
                date = "2017-05-07"),
  "2022" = list(file = "Schleswig-Holstein_2022_Landtag.xlsx",
                date = "2022-05-08")
)

for (yr in c("2012", "2017", "2022")) {
  cat("SH", yr, "...")
  cfg <- sh_new_config[[yr]]
  f <- file.path(sh_raw_path, cfg$file)
  raw <- read_excel(f, sheet = 1, col_names = FALSE, .name_repair = "minimal")

  # Row 2 = header names, Row 3 = column codes (A1, B, F, F1...)
  code_row <- sapply(1:ncol(raw), function(j) as.character(raw[[j]][3]))
  name_row <- sapply(1:ncol(raw), function(j) {
    v <- as.character(raw[[j]][2])
    if (is.na(v)) "" else gsub("-\n", "", gsub("\n", " ", v))
  })

  col_A  <- which(code_row == "A")[1]     # eligible voters total
  col_B  <- which(code_row == "B")[1]     # total voters
  col_E  <- which(code_row == "E")[1]     # invalid Zweitstimmen
  col_F  <- which(code_row == "F")[1]     # valid Zweitstimmen total
  f_cols <- which(grepl("^F[0-9]+$", code_row))

  party_map <- list()
  for (j in f_cols) {
    std <- sh_map_party(name_row[j])
    if (!is.na(std)) party_map[[as.character(j)]] <- std
  }

  data <- raw[4:nrow(raw), ]
  raw_codes <- as.character(data[[1]])
  raw_codes <- ifelse(!is.na(raw_codes) & nchar(raw_codes) == 7,
                      paste0("0", raw_codes), raw_codes)
  valid_rows <- !is.na(raw_codes) & grepl("^[0-9]{8}$", raw_codes)
  data <- data[valid_rows, ]
  raw_codes <- raw_codes[valid_rows]
  ags8 <- sh_make_ags(raw_codes)

  agg <- tibble(
    ags = ags8,
    eligible = sh_safe_num(data[[col_A]]),
    voters   = sh_safe_num(data[[col_B]]),
    invalid  = sh_safe_num(data[[col_E]]),
    valid_v  = sh_safe_num(data[[col_F]])
  )
  party_names <- c()
  for (jc in names(party_map)) {
    j <- as.integer(jc)
    agg[[party_map[[jc]]]] <- sh_safe_num(data[[j]])
    party_names <- c(party_names, party_map[[jc]])
  }
  party_names <- unique(party_names)

  agg_muni <- agg |>
    group_by(ags) |>
    summarise(
      eligible = sum(eligible, na.rm = TRUE),
      voters   = sum(voters, na.rm = TRUE),
      invalid  = sum(invalid, na.rm = TRUE),
      valid_v  = sum(valid_v, na.rm = TRUE),
      across(all_of(party_names), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  ## Briefwahl allocation: Amt-level Briefwahl rows (Gemeinde suffix >= 900)
  ## produce phantom municipalities (EV=0, voters>0). Allocate their votes
  ## proportionally to real municipalities in the same Kreis.
  agg_muni$gem3 <- substr(agg_muni$ags, 6, 8)
  agg_muni$kreis <- substr(agg_muni$ags, 3, 5)
  is_brief <- as.integer(agg_muni$gem3) >= 900
  brief_rows <- agg_muni[is_brief, ]
  real_rows  <- agg_muni[!is_brief, ]

  if (nrow(brief_rows) > 0) {
    vote_cols <- c("voters", "invalid", "valid_v", party_names)
    for (kr in unique(brief_rows$kreis)) {
      kr_brief <- brief_rows |> filter(kreis == kr)
      kr_real  <- real_rows  |> filter(kreis == kr)
      if (nrow(kr_real) == 0) next
      weights <- kr_real$eligible / sum(kr_real$eligible, na.rm = TRUE)
      weights[is.na(weights)] <- 0
      for (vc in vote_cols) {
        brief_total <- sum(kr_brief[[vc]], na.rm = TRUE)
        idx <- which(real_rows$kreis == kr)
        real_rows[[vc]][idx] <- real_rows[[vc]][idx] + brief_total * weights
      }
    }
    agg_muni <- real_rows
    cat(sprintf(" (allocated %d Brief rows)", nrow(brief_rows)))
    # Cap voters at eligible after Briefwahl allocation:
    # proportional allocation can overshoot for small municipalities
    overcap <- !is.na(agg_muni$voters) & !is.na(agg_muni$eligible) &
      agg_muni$eligible > 0 & agg_muni$voters > agg_muni$eligible
    if (any(overcap)) {
      cat(sprintf(" (%d munis capped: voters > eligible)", sum(overcap)))
      agg_muni$voters[overcap] <- agg_muni$eligible[overcap]
    }
  }
  agg_muni <- agg_muni |> select(-gem3, -kreis)

  for (i in 1:nrow(agg_muni)) {
    r <- agg_muni[i, ]
    mapped_sum <- sum(sapply(party_names, function(p) r[[p]]), na.rm = TRUE)
    other_v <- max(r$valid_v - mapped_sum, 0)
    result <- tibble(
      ags = r$ags, election_year = as.integer(yr), state = "01",
      election_date = as.Date(cfg$date),
      eligible_voters = r$eligible, number_voters = r$voters,
      valid_votes = r$valid_v, invalid_votes = r$invalid,
      turnout = r$voters / r$eligible    )
    for (p in party_names) result[[p]] <- r[[p]] / r$valid_v
    result$other <- other_v / r$valid_v
    result$cdu_csu <- result$cdu
    sh_results[[paste0(yr, "_", r$ags)]] <- result
  }
  cat(sprintf(" %d munis, EV=%.0f\n", nrow(agg_muni), sum(agg_muni$eligible)))
}

## ---- 2000, 2005, 2009: no headers, data from row 1, ZS positions from docs ----
sh_old_config <- list(
  "2009" = list(
    file = "Schleswig-Holstein_2009_Landtag.xls",
    date = "2009-09-27",
    ev_col = 16, voter_col = 20,
    zs_inv = 36, zs_val = 37,
    party_cols = list(
      "38" = "cdu", "39" = "spd", "40" = "fdp", "41" = "gruene",
      "42" = NA, "43" = NA, "44" = NA, "45" = "linke_pds",
      "46" = NA, "47" = NA, "48" = NA, "49" = NA, "50" = NA
    )
  ),
  "2005" = list(
    file = "Schleswig-Holstein_2005_Landtagswahl.xls",
    date = "2005-02-20",
    ev_col = 16, voter_col = 20,
    zs_inv = 32, zs_val = 33,
    party_cols = list(
      "34" = "spd", "35" = "cdu", "36" = "fdp", "37" = "gruene",
      "38" = NA, "39" = "linke_pds", "40" = NA, "41" = NA,
      "42" = NA, "43" = NA, "44" = NA, "45" = NA, "46" = NA
    )
  ),
  "2000" = list(
    file = "Schleswig-Holstein_2000_Landtagswahl.xls",
    date = "2000-02-27",
    ev_col = 16, voter_col = 20,
    zs_inv = 36, zs_val = 37,
    party_cols = list(
      "38" = "spd", "39" = "cdu", "40" = "gruene", "41" = "fdp",
      "42" = NA, "43" = NA, "44" = NA, "45" = NA, "46" = NA,
      "47" = "linke_pds", "48" = NA
    )
  )
)

for (yr in c("2009", "2005", "2000")) {
  cat("SH", yr, "...")
  cfg <- sh_old_config[[yr]]
  f <- file.path(sh_raw_path, cfg$file)
  raw <- read_excel(f, sheet = 1, col_names = FALSE, .name_repair = "minimal")

  raw_codes <- as.character(raw[[1]])
  raw_codes <- ifelse(!is.na(raw_codes) & nchar(raw_codes) == 7,
                      paste0("0", raw_codes), raw_codes)
  valid_rows <- !is.na(raw_codes) & grepl("^[0-9]{8}$", raw_codes)
  data <- raw[valid_rows, ]
  raw_codes <- raw_codes[valid_rows]
  ags8 <- sh_make_ags(raw_codes)

  agg <- tibble(
    ags = ags8,
    eligible = sh_safe_num(data[[cfg$ev_col]]),
    voters   = sh_safe_num(data[[cfg$voter_col]]),
    invalid  = sh_safe_num(data[[cfg$zs_inv]]),
    valid_v  = sh_safe_num(data[[cfg$zs_val]])
  )

  party_names <- c()
  for (jc in names(cfg$party_cols)) {
    std <- cfg$party_cols[[jc]]
    if (!is.na(std)) {
      j <- as.integer(jc)
      agg[[std]] <- sh_safe_num(data[[j]])
      party_names <- c(party_names, std)
    }
  }
  party_names <- unique(party_names)

  agg_muni <- agg |>
    group_by(ags) |>
    summarise(
      eligible = sum(eligible, na.rm = TRUE),
      voters   = sum(voters, na.rm = TRUE),
      invalid  = sum(invalid, na.rm = TRUE),
      valid_v  = sum(valid_v, na.rm = TRUE),
      across(all_of(party_names), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  for (i in 1:nrow(agg_muni)) {
    r <- agg_muni[i, ]
    mapped_sum <- sum(sapply(party_names, function(p) r[[p]]), na.rm = TRUE)
    other_v <- max(r$valid_v - mapped_sum, 0)
    result <- tibble(
      ags = r$ags, election_year = as.integer(yr), state = "01",
      election_date = as.Date(cfg$date),
      eligible_voters = r$eligible, number_voters = r$voters,
      valid_votes = r$valid_v, invalid_votes = r$invalid,
      turnout = r$voters / r$eligible    )
    for (p in party_names) result[[p]] <- r[[p]] / r$valid_v
    result$other <- other_v / r$valid_v
    result$cdu_csu <- result$cdu
    sh_results[[paste0(yr, "_", r$ags)]] <- result
  }
  cat(sprintf(" %d munis, EV=%.0f\n", nrow(agg_muni), sum(agg_muni$eligible)))
}

## ---- 1996: only Erststimmen (no Zweitstimmen split) ----
cat("SH 1996 ...")
f96 <- file.path(sh_raw_path, "Schleswig-Holstein_1996_Landtagswahl_Infodat.xlsx")
raw96 <- read_excel(f96, sheet = 1, col_names = FALSE, .name_repair = "minimal")

raw_codes <- as.character(raw96[[1]])
raw_codes <- ifelse(!is.na(raw_codes) & nchar(raw_codes) == 7,
                    paste0("0", raw_codes), raw_codes)
valid_rows <- !is.na(raw_codes) & grepl("^[0-9]{8}$", raw_codes)
data96 <- raw96[valid_rows, ]
raw_codes <- raw_codes[valid_rows]
ags8 <- sh_make_ags(raw_codes)

# 1996: col 16=EV, col 20=voters, col 21=invalid, col 22=valid Erststimmen
# Parties: 23=SPD, 24=CDU, 25=DVU, 26=F.D.P., 27=GRÜNE, 28=SSW, 29+=others
party_96 <- list("23" = "spd", "24" = "cdu", "25" = "dvu", "26" = "fdp",
                 "27" = "gruene", "28" = "ssw")

agg96 <- tibble(
  ags = ags8,
  eligible = sh_safe_num(data96[[16]]),
  voters   = sh_safe_num(data96[[20]]),
  invalid  = sh_safe_num(data96[[21]]),
  valid_v  = sh_safe_num(data96[[22]])
)

pnames96 <- c()
for (jc in names(party_96)) {
  std <- party_96[[jc]]
  if (!is.na(std)) {
    agg96[[std]] <- sh_safe_num(data96[[as.integer(jc)]])
    pnames96 <- c(pnames96, std)
  }
}
pnames96 <- unique(pnames96)

agg_muni96 <- agg96 |>
  group_by(ags) |>
  summarise(
    eligible = sum(eligible, na.rm = TRUE),
    voters   = sum(voters, na.rm = TRUE),
    invalid  = sum(invalid, na.rm = TRUE),
    valid_v  = sum(valid_v, na.rm = TRUE),
    across(all_of(pnames96), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

for (i in 1:nrow(agg_muni96)) {
  r <- agg_muni96[i, ]
  mapped_sum <- sum(sapply(pnames96, function(p) r[[p]]), na.rm = TRUE)
  other_v <- max(r$valid_v - mapped_sum, 0)
  result <- tibble(
    ags = r$ags, election_year = 1996L, state = "01",
    election_date = as.Date("1996-03-24"),
    eligible_voters = r$eligible, number_voters = r$voters,
    valid_votes = r$valid_v, invalid_votes = r$invalid,
    turnout = r$voters / r$eligible  )
  for (p in pnames96) result[[p]] <- r[[p]] / r$valid_v
  result$other <- other_v / r$valid_v
  result$cdu_csu <- result$cdu
  sh_results[[paste0("1996_", r$ags)]] <- result
}
cat(sprintf(" %d munis, EV=%.0f\n", nrow(agg_muni96), sum(agg_muni96$eligible)))

## ---- 1983: text-layer-extracted CSV (see 00_sh_1983_extract.py) ----
cat("SH 1983 ...")
ocr83 <- read.csv(file.path(sh_raw_path, "sh_1983_extracted.csv"),
                  colClasses = "character")

## Map OCR column names to standard party names
sh83_party_map <- c(
  cdu = "cdu", spd = "spd", fdp = "fdp", ssw = "ssw",
  dkp = "dkp", dgl = "dgl", gruene = "gruene",
  fp = "fp", fsu = "fsu", einzelbewerber = "einzelbewerber"
)

result83 <- tibble(
  ags             = ocr83$ags,
  eligible_voters = as.numeric(ocr83$eligible_voters),
  number_voters   = as.numeric(ocr83$number_voters),
  invalid_votes   = as.numeric(ocr83$invalid_votes),
  valid_votes     = as.numeric(ocr83$valid_votes)
)

for (ocr_col in names(sh83_party_map)) {
  std_name <- sh83_party_map[[ocr_col]]
  votes <- as.numeric(ocr83[[ocr_col]])
  votes[is.na(votes)] <- 0
  result83[[paste0(std_name, "_n")]] <- votes
  result83[[std_name]] <- votes / result83$valid_votes
}

mapped_n_cols <- paste0(unique(sh83_party_map), "_n")
mapped_n_cols <- mapped_n_cols[mapped_n_cols %in% names(result83)]
result83 <- result83 |>
  mutate(
    other_n = valid_votes - rowSums(across(all_of(mapped_n_cols)), na.rm = TRUE),
    other_n = pmax(other_n, 0, na.rm = TRUE),
    other   = other_n / valid_votes
  )

result83 <- result83 |>
  mutate(
    election_year = 1983L,
    state = "01",
    election_date = as.Date("1983-03-13"),
    turnout = number_voters / eligible_voters,
    cdu_csu = cdu
  ) |>
  select(-ends_with("_n"))

## Drop records with 0 valid votes (OCR artifacts)
result83 <- result83 |> filter(valid_votes > 0)

for (i in 1:nrow(result83)) {
  r <- result83[i, ]
  sh_results[[paste0("1983_", r$ags)]] <- r
}
cat(sprintf(" %d munis, VV=%.0f\n", nrow(result83), sum(result83$valid_votes)))

all_states[["sh"]] <- standardise(bind_rows(sh_results))
cat("Schleswig-Holstein total:", nrow(all_states[["sh"]]), "rows\n\n")

###############################################################################
####                         BAYERN (09)                                   ####
###############################################################################
## 19 elections: 1946-1990 (12 elections from "Selbst recherchiert" files),
##               1994-2013 (5 elections from Landesamt "Stimmabgabe" files),
##               2018, 2023 (from Stimmbezirk-level files)
##
## Data sources:
##   1946-1990: Individual XLSX per year with Ergebnis (party votes, Gesamt-
##              stimmen = Erst+Zweit) and Wahlbeteiligung (EV, voters).
##              Consistent 16-col layout: AGS, name, valid_total, CSU, SPD,
##              FW, GRUENE, FDP, sonstige (Gesamtstimmen cols 3-9).
##   1994-2023: Landesamt "Stimmabgabe" files: party-level Gesamtstimmen
##              broken by Urnenwahl/Briefwahl. Gesamtstimmen rows used.
##              "Art d Wahl" file provides voter counts (all 19 years).
##              EV not available for 1994-2013 (set to NA).
##   2018/2023: Stimmbezirk XLSX with EV (A), voters (B), Erst+Zweitstimmen.
##              Aggregated to Gemeinde via AGS extracted from 13-digit key.
##              Kreisfreie Staedte identified by X6X Kreis pattern.
##
## Note: Bavaria uses CSU instead of CDU. cdu_csu = csu for all BY rows.
##
## IMPORTANT: BY uses Gesamtstimmen (Erst+Zweit summed) as the basis for
## proportional seat allocation and the 5% threshold. Unlike Bundestagswahlen,
## Erststimmen of losing candidates are NOT discarded — they count toward party
## totals. Therefore valid_votes = gültige_Erst + gültige_Zweit, and
## invalid_votes = ungültige_Erst + ungültige_Zweit. Since each voter casts
## 2 ballots: valid_votes + invalid_votes = number_voters × 2 (for 1950+).
## Exception: 1946 was single-ballot (no Erst/Zweit split in source data).

cat("=== BAYERN ===\n")

by_raw_path <- file.path(raw_path, "Bayern")
by_sr_path  <- file.path(by_raw_path, "Selbst recherchiert")
by_la_path  <- file.path(by_raw_path, "Vom Statistischen Landesamt bekommen")

by_safe_num <- function(x) {
  x <- as.character(x)
  x[x == "-" | x == "x" | x == "" | x == "\u2013" | x == "."] <- NA
  suppressWarnings(as.numeric(x))
}

by_map_party <- function(pname) {
  p <- trimws(pname)
  p <- gsub("-\n", "", p)
  ## Fix UTF-8 mojibake from Bayern 2018 XLSX (Latin-1 double-encoded)
  p <- gsub("\u00c3\u0084|\u00c3\u201e", "\u00c4", p)   # Ã„ or Ã„ → Ä
  p <- gsub("\u00c3\u0096|\u00c3\u2013", "\u00d6", p)   # Ã– → Ö
  p <- gsub("\u00c3\u009c|\u00c3\u0153", "\u00dc", p)   # Ãœ → Ü
  p <- gsub("\u00c3\u00a4", "\u00e4", p)                 # Ã¤ → ä
  p <- gsub("\u00c3\u00b6", "\u00f6", p)                 # Ã¶ → ö
  p <- gsub("\u00c3\u00bc", "\u00fc", p)                 # Ã¼ → ü
  p <- gsub("\u00c3\u009f|\u00c3\u0178", "\u00df", p)   # ÃŸ → ß
  normalise_party(p)
}

by_results <- list()

## Election dates for all 19 Bavarian state elections
by_dates <- c(
  "1946" = "1946-12-01", "1950" = "1950-11-26", "1954" = "1954-11-28",
  "1958" = "1958-11-23", "1962" = "1962-11-25", "1966" = "1966-11-20",
  "1970" = "1970-11-22", "1974" = "1974-10-27", "1978" = "1978-10-15",
  "1982" = "1982-10-10", "1986" = "1986-10-12", "1990" = "1990-10-14",
  "1994" = "1994-09-25", "1998" = "1998-09-13", "2003" = "2003-09-21",
  "2008" = "2008-09-28", "2013" = "2013-09-15", "2018" = "2018-10-14",
  "2023" = "2023-10-08"
)

## ---------------------------------------------------------------------------
## Phase 1: 1946-1990  (Selbst recherchiert: Ergebnis + Wahlbeteiligung XLSX)
## ---------------------------------------------------------------------------
## Each year has two files per subdirectory BY_LTW_YYYY/:
##   *_Ergebnis.xlsx:        8 header rows, data from row 9
##     Col 1=AGS, 2=name, 3=Gesamtstimmen total,
##     4=CSU, 5=SPD, 6=FW, 7=GRUENE, 8=FDP, 9=sonstige (Gesamtstimmen)
##     Cols 10-16 repeat for Erststimmen (not used).
##   *_Wahlbeteiligung.xlsx: 5 header rows, data from row 6
##     Col 1=AGS, 2=name, 3=Wahlberechtigte, 4=Waehler, 5=Wahlbeteiligung(%)

by_early_years <- c("1946", "1950", "1954", "1958", "1962", "1966",
                     "1970", "1974", "1978", "1982", "1986", "1990")

for (yr in by_early_years) {
  cat("BY", yr, "...")
  yr_dir <- file.path(by_sr_path, paste0("BY_LTW_", yr))

  ## -- Read Ergebnis (party votes) --
  erg_file <- list.files(yr_dir, pattern = "Ergebnis", full.names = TRUE)
  erg_raw  <- read_excel(erg_file, col_names = FALSE, .name_repair = "minimal")

  ## -- Read Wahlbeteiligung (EV, voters) --
  wb_file <- list.files(yr_dir, pattern = "Wahlbeteiligung", full.names = TRUE)
  wb_raw  <- read_excel(wb_file, col_names = FALSE, .name_repair = "minimal")

  ## -- Filter to municipality rows (8-digit AGS starting with "09") --
  ags_erg <- as.character(erg_raw[[1]])
  keep_erg <- grepl("^09[0-9]{6}$", ags_erg)
  erg_data <- erg_raw[keep_erg, ]
  ags_vec  <- ags_erg[keep_erg]

  ags_wb <- as.character(wb_raw[[1]])
  keep_wb <- grepl("^09[0-9]{6}$", ags_wb)
  wb_data <- wb_raw[keep_wb, ]

  ## -- Extract vote counts (Gesamtstimmen = cols 3-9) --
  ## Col 3=total, 4=CSU, 5=SPD, 6=FW, 7=GRÜNE, 8=FDP, 9=sonstige(aggregate)
  valid_v <- by_safe_num(erg_data[[3]])
  csu_v   <- by_safe_num(erg_data[[4]])
  spd_v   <- by_safe_num(erg_data[[5]])
  fw_v    <- by_safe_num(erg_data[[6]])
  gru_v   <- by_safe_num(erg_data[[7]])
  fdp_v   <- by_safe_num(erg_data[[8]])

  ## -- Extract EV and voters from Wahlbeteiligung --
  eligible <- by_safe_num(wb_data[[3]])
  voters   <- by_safe_num(wb_data[[4]])

  ## Safety: Wahlbeteiligung AGS must match Ergebnis AGS
  ags_wb_vec <- ags_wb[keep_wb]
  stopifnot(all(ags_vec == ags_wb_vec))

  ## -- Build mapped votes --
  mapped_sum <- rowSums(cbind(
    ifelse(is.na(csu_v), 0, csu_v),
    ifelse(is.na(spd_v), 0, spd_v),
    ifelse(is.na(fw_v), 0, fw_v),
    ifelse(is.na(gru_v), 0, gru_v),
    ifelse(is.na(fdp_v), 0, fdp_v)
  ))
  other_v <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)
  ## BY uses Gesamtstimmen (Erst+Zweit combined); each voter casts 2 ballots
  ## (except 1946 which was single-ballot). invalid = total_ballots - valid.
  if (yr == "1946") {
    invalid_v <- voters - valid_v          # single ballot: 1 vote per voter
  } else {
    invalid_v <- voters * 2L - valid_v     # dual ballot: 2 votes per voter
  }

  result <- tibble(
    ags            = ags_vec,
    election_year  = as.integer(yr),
    state          = "09",
    election_date  = as.Date(by_dates[yr]),
    eligible_voters = eligible,
    number_voters  = voters,
    valid_votes    = valid_v,
    invalid_votes  = invalid_v,
    turnout        = voters / eligible,
    csu            = csu_v / valid_v,
    spd            = spd_v / valid_v,
    freie_wahler   = ifelse(is.na(fw_v), NA_real_, fw_v / valid_v),
    gruene         = ifelse(is.na(gru_v), NA_real_, gru_v / valid_v),
    fdp            = fdp_v / valid_v,
    other          = other_v / valid_v,
    cdu_csu        = csu_v / valid_v
  )

  cat(nrow(result), "munis\n")
  by_results[[yr]] <- result
}

## ---------------------------------------------------------------------------
## Phase 2: 1994-2013  (Landesamt Stimmabgabe + Art d Wahl)
## ---------------------------------------------------------------------------
## Stimmabgabe files contain Gesamtstimmen by party (Insgesamt/Urnenwahl/
## Briefwahl triplets). Each file holds 1-2 elections.
## Structure: rows alternate AGS-row (col3=Erststimmen) / col3=Gesamtstimmen.
## We keep Gesamtstimmen rows. Party names in row 6, "Insgesamt" cols = first
## of each 3-column triplet.
##
## Art d Wahl file: voter counts (Abstimmvermerk + Wahlscheine + Briefwahl)
## for all 19 elections, 3 cols per year starting at col 3.

## -- Read Art d Wahl (voter counts for all years) --
aw_file <- file.path(by_la_path, "Bayern_LTW_ Art d Wahl_1946 - 2023.xlsx")
aw_raw  <- read_excel(aw_file, col_names = FALSE, .name_repair = "minimal")

## Row 4 has election dates; map each to a column offset
aw_r4    <- as.character(aw_raw[4, ])
aw_date_cols <- which(grepl("[0-9]{2}[.][0-9]{2}[.][0-9]{4}", aw_r4, perl = TRUE))
aw_dates_str <- aw_r4[aw_date_cols]

## Map date strings to election years
aw_date_to_year <- function(ds) {
  as.integer(sub(".*([0-9]{4})$", "\\1", ds))
}
aw_year_map <- setNames(aw_date_cols,
                         as.character(sapply(aw_dates_str, aw_date_to_year)))

## Data rows start at row 7; AGS in col 1
aw_ags <- as.character(aw_raw[[1]])
aw_muni <- grepl("^09[0-9]{6}$", aw_ags)
aw_data <- aw_raw[aw_muni, ]
aw_ags_vec <- aw_ags[aw_muni]

## Function to get voters for a given year from Art d Wahl
by_get_voters <- function(year_char) {
  start_col <- aw_year_map[year_char]
  v1 <- by_safe_num(aw_data[[start_col]])      # Abstimmvermerk
  v2 <- by_safe_num(aw_data[[start_col + 1]])   # Wahlscheine
  v3 <- by_safe_num(aw_data[[start_col + 2]])   # Briefwahl
  rowSums(cbind(
    ifelse(is.na(v1), 0, v1),
    ifelse(is.na(v2), 0, v2),
    ifelse(is.na(v3), 0, v3)
  ))
}

## -- Stimmabgabe files: parse party votes --
stimmabgabe_files <- list(
  list(file = "LTW_WV_Art d Stimmabgabe_1994 -1998.xlsx",
       years = c("1994", "1998")),
  list(file = "LTW_WV_Art d Stimmabgabe_2003 - 2008.xlsx",
       years = c("2003", "2008")),
  list(file = "LTW_WV_Art d Stimmabgabe_2013 - 2018.xlsx",
       years = c("2013"))
)

for (sa_info in stimmabgabe_files) {
  sa_file <- file.path(by_la_path, sa_info$file)
  sa_raw  <- read_excel(sa_file, col_names = FALSE, .name_repair = "minimal")

  ## Find election date positions in row 4
  sa_r4 <- as.character(sa_raw[4, ])
  sa_date_cols <- which(grepl("[0-9]{2}[.][0-9]{2}[.][0-9]{4}", sa_r4, perl = TRUE))
  sa_date_vals <- sa_r4[sa_date_cols]
  sa_date_years <- as.character(sapply(sa_date_vals, aw_date_to_year))

  ## Party names in row 6
  sa_r6 <- as.character(sa_raw[6, ])
  sa_party_positions <- which(!is.na(sa_r6) &
                                seq_along(sa_r6) >= sa_date_cols[1])

  ## AGS in col 1, vote type in col 3
  sa_ags  <- as.character(sa_raw[[1]])
  sa_col3 <- as.character(sa_raw[[3]])

  ## Filter: keep Gesamtstimmen rows only
  gesamt_rows <- which(sa_col3 == "Gesamtstimmen")

  ## The AGS for each Gesamtstimmen row is in the preceding row
  erst_rows <- gesamt_rows - 1
  ags_vec <- sa_ags[erst_rows]
  keep_muni <- grepl("^09[0-9]{6}$", ags_vec)

  gesamt_data <- sa_raw[gesamt_rows[keep_muni], ]
  ags_vec     <- ags_vec[keep_muni]

  ## Process each target election year in this file
  for (yr in sa_info$years) {
    ## Find which date column corresponds to this year
    yi <- which(sa_date_years == yr)
    if (length(yi) == 0) next

    cat("BY", yr, "(Stimmabgabe) ...")

    ## Determine party columns for this election year
    col_start <- sa_date_cols[yi]
    col_end   <- if (yi < length(sa_date_cols)) sa_date_cols[yi + 1] - 1 else ncol(sa_raw)

    ## Find party names within this column range
    yr_party_pos <- sa_party_positions[sa_party_positions >= col_start &
                                         sa_party_positions <= col_end]
    yr_party_names <- sa_r6[yr_party_pos]

    ## Map parties; each party has 3 cols, we want the first (Insgesamt)
    mapped_votes <- list()
    for (pi in seq_along(yr_party_pos)) {
      pname <- yr_party_names[pi]
      std   <- by_map_party(pname)
      col_idx <- yr_party_pos[pi]  # "Insgesamt" column
      v <- by_safe_num(gesamt_data[[col_idx]])

      if (!is.na(std)) {
        if (std %in% names(mapped_votes)) {
          mapped_votes[[std]] <- mapped_votes[[std]] + ifelse(is.na(v), 0, v)
        } else {
          mapped_votes[[std]] <- v
        }
      }
    }

    ## Valid votes = sum of all party votes (including unmapped ones)
    all_party_cols <- yr_party_pos
    valid_v <- rep(0, nrow(gesamt_data))
    for (col_idx in all_party_cols) {
      v <- by_safe_num(gesamt_data[[col_idx]])
      valid_v <- valid_v + ifelse(is.na(v), 0, v)
    }

    ## Voters from Art d Wahl
    voters_aw <- by_get_voters(yr)
    ## Match AGS between Art d Wahl and Stimmabgabe
    aw_match <- match(ags_vec, aw_ags_vec)
    voters <- voters_aw[aw_match]

    ## Mapped sum for "other" calculation
    mapped_sum <- rep(0, nrow(gesamt_data))
    for (std_name in names(mapped_votes)) {
      mapped_sum <- mapped_sum + ifelse(is.na(mapped_votes[[std_name]]), 0,
                                        mapped_votes[[std_name]])
    }
    other_v <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)

    ## BY Gesamtstimmen: dual ballot, invalid = 2*voters - valid
    result <- tibble(
      ags            = ags_vec,
      election_year  = as.integer(yr),
      state          = "09",
      election_date  = as.Date(by_dates[yr]),
      eligible_voters = NA_real_,
      number_voters  = voters,
      valid_votes    = valid_v,
      invalid_votes  = voters * 2L - valid_v,
      turnout        = NA_real_
    )

    ## Add party shares
    for (std_name in names(mapped_votes)) {
      result[[std_name]] <- mapped_votes[[std_name]] / valid_v
    }
    result$other   <- other_v / valid_v
    result$cdu_csu <- result$csu

    cat(nrow(result), "munis\n")
    by_results[[yr]] <- result
  }
}

## ---------------------------------------------------------------------------
## Phase 3: 2018 & 2023  (Stimmbezirk-level files, aggregated to Gemeinde)
## ---------------------------------------------------------------------------
## Data at Stimmbezirk level with 13-digit key: SK(3)+Kreis(3)+Gem(3)+StBez(4).
## AGS = "09" + Kreis(3) + Gem(3).
## Kreisfreie Staedte (Kreis pattern X6X) -> Gem forced to "000".
## Landkreise (X7X+) -> Gem used as-is.
##
## Columns (2018 CSV-Rohdaten / 2023 Rohdaten):
##   Col 1 = Schluessel, Col 2 = Art (0=Urne, 3=Brief)
##   Cols 3-6: A1,A2,A3,A (eligible voters; A=total)
##   Cols 7-10: B1,B2,B3,B (voters; B=total)
##   Erst party cols, D/C/E (valid/invalid/total Erst)
##   Zweit party cols, D/C/E (valid/invalid/total Zweit)
##
## 2018: Erst cols 11-28, D=29,C=30,E=31; Zweit 32-49, D=50,C=51,E=52
## 2023: Erst cols 11-25, valid=26,invalid=27,total=28;
##        Zweit cols 29-43, valid=44,invalid=45,total=46

by_stbez_info <- list(
  "2018" = list(
    file  = file.path(by_sr_path, "Bayern_2018_Landtagswahl.xlsx"),
    sheet = "CSV-Rohdaten StBez Ergebnisse",
    erst_party = 11:28, erst_valid = 29, erst_invalid = 30,
    zweit_party = 32:49, zweit_valid = 50, zweit_invalid = 51,
    eligible_col = 6, voters_col = 10
  ),
  "2023" = list(
    file  = file.path(by_sr_path, "Bayern_2023_Landtagswahl.xlsx"),
    sheet = "Rohdaten StBez Ergebnisse",
    erst_party = 11:25, erst_valid = 26, erst_invalid = 27,
    zweit_party = 29:43, zweit_valid = 44, zweit_invalid = 45,
    eligible_col = 6, voters_col = 10
  )
)

for (yr in c("2018", "2023")) {
  cat("BY", yr, "(Stimmbezirk) ...")
  info <- by_stbez_info[[yr]]

  stb_raw <- read_excel(info$file, sheet = info$sheet,
                         col_names = FALSE, .name_repair = "minimal")

  ## Row 1 = headers, data from row 2
  hdr <- as.character(stb_raw[1, ])
  stb_data <- stb_raw[-1, ]

  ## Extract key and remove surrounding quotes
  keys <- as.character(stb_data[[1]])
  keys <- gsub("'", "", keys)

  ## Extract AGS from 13-digit key
  kreis_code <- substr(keys, 4, 6)
  gem_code   <- substr(keys, 7, 9)
  ## Kreisfreie Staedte: second digit of Kreis is "6" -> Gem = "000"
  is_kreisfrei <- grepl("^.6.$", kreis_code)
  gem_code[is_kreisfrei] <- "000"
  ags_vec <- paste0("09", kreis_code, gem_code)

  ## Eligible voters (A) and voters (B)
  eligible <- by_safe_num(stb_data[[info$eligible_col]])
  voters   <- by_safe_num(stb_data[[info$voters_col]])

  ## Valid/invalid votes: sum Erst + Zweit
  valid_erst    <- by_safe_num(stb_data[[info$erst_valid]])
  invalid_erst  <- by_safe_num(stb_data[[info$erst_invalid]])
  valid_zweit   <- by_safe_num(stb_data[[info$zweit_valid]])
  invalid_zweit <- by_safe_num(stb_data[[info$zweit_invalid]])
  valid_v   <- ifelse(is.na(valid_erst), 0, valid_erst) +
               ifelse(is.na(valid_zweit), 0, valid_zweit)
  invalid_v <- ifelse(is.na(invalid_erst), 0, invalid_erst) +
               ifelse(is.na(invalid_zweit), 0, invalid_zweit)

  ## Party votes: sum Erst + Zweit for each party position
  ## Clean party names from Erst headers (remove prefix for 2023)
  n_parties <- length(info$erst_party)
  erst_hdr  <- hdr[info$erst_party]
  clean_hdr <- gsub("^Erststimmen\\s+", "", erst_hdr)

  ## Map each party
  mapped_votes <- list()
  for (pi in 1:n_parties) {
    pname <- clean_hdr[pi]
    std   <- by_map_party(pname)
    v_erst  <- by_safe_num(stb_data[[info$erst_party[pi]]])
    v_zweit <- by_safe_num(stb_data[[info$zweit_party[pi]]])
    v <- ifelse(is.na(v_erst), 0, v_erst) + ifelse(is.na(v_zweit), 0, v_zweit)

    if (!is.na(std)) {
      if (std %in% names(mapped_votes)) {
        mapped_votes[[std]] <- mapped_votes[[std]] + v
      } else {
        mapped_votes[[std]] <- v
      }
    }
  }

  ## Aggregate to Gemeinde level
  agg_df <- tibble(
    ags     = ags_vec,
    eligible = eligible,
    voters  = voters,
    valid_v = valid_v,
    invalid = invalid_v
  )
  for (std_name in names(mapped_votes)) {
    agg_df[[std_name]] <- mapped_votes[[std_name]]
  }

  std_names <- names(mapped_votes)
  agg_muni <- agg_df |>
    group_by(ags) |>
    summarise(
      eligible = sum(eligible, na.rm = TRUE),
      voters   = sum(voters, na.rm = TRUE),
      valid_v  = sum(valid_v, na.rm = TRUE),
      invalid  = sum(invalid, na.rm = TRUE),
      across(all_of(std_names), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  ## Compute shares
  mapped_sum <- rowSums(agg_muni[std_names], na.rm = TRUE)
  other_v <- pmax(agg_muni$valid_v - mapped_sum, 0, na.rm = TRUE)

  result <- tibble(
    ags            = agg_muni$ags,
    election_year  = as.integer(yr),
    state          = "09",
    election_date  = as.Date(by_dates[yr]),
    eligible_voters = agg_muni$eligible,
    number_voters  = agg_muni$voters,
    valid_votes    = agg_muni$valid_v,
    invalid_votes  = agg_muni$invalid,
    turnout        = agg_muni$voters / agg_muni$eligible
  )
  for (std_name in std_names) {
    result[[std_name]] <- agg_muni[[std_name]] / agg_muni$valid_v
  }
  result$other   <- other_v / agg_muni$valid_v
  result$cdu_csu <- result$csu

  cat(nrow(result), "munis\n")
  by_results[[yr]] <- result
}

all_states[["by"]] <- standardise(bind_rows(by_results))
cat("Bayern total:", nrow(all_states[["by"]]), "rows\n\n")


###############################################################################
####                      Rheinland-Pfalz (07)                             ####
###############################################################################

cat("=== RHEINLAND-PFALZ ===\n")

rp_raw_path <- file.path(raw_path, "Rheinland-Pfalz")

rp_safe_num <- function(x) {
  x <- as.character(x)
  x[x == "-" | x == "" | x == "\u2013"] <- NA
  suppressWarnings(as.numeric(x))
}

rp_results <- list()

## ---------- 1979-2016 from LW_RLP_1979_2021.xlsx ----------
## Source: Landeswahlleiter RLP, municipality-level Landesstimmen 1979-2021
## Format: 12-digit AGS (col 1), name (col 2), Stichtag (col 3)
##   - Party headers in row 3, sub-headers in row 4
##   - Landesstimmen in even columns (4,6,...,106); WKS in odd (5,7,...,107)
##   - Gesamtsumme LS in col 106 = valid_votes
##   - Forward-fill needed: AGS appears only on first row of each municipality block
##   - No eligible_voters/number_voters/invalid_votes in this file
##   - We process 1979-2016 here; 2021 uses LW_2021_GESAMT.xlsx (has turnout data)
## Note on WKS: RLP only had single vote before 1991; WKS=0 for 1979/1983/1987

rp_multi_xlsx <- file.path(rp_raw_path, "LW_RLP_1979_2021.xlsx")
rp_multi_raw <- read_excel(rp_multi_xlsx, col_names = FALSE, .name_repair = "minimal")

## --- Parse Landesstimme party columns from row 3 ---
## Even columns (4,6,8,...,104) are Landesstimmen
rp_m_header <- as.character(rp_multi_raw[3, ])
rp_m_parties <- list()
for (ci in seq(4, 104, by = 2)) {
  pname <- trimws(rp_m_header[ci])
  if (is.na(pname) || pname == "" || pname == "Gesamtsumme") next
  # Skip individual WKS-only candidates (names with commas or "parteilos/parteiunabhängig")
  if (grepl(",", pname) || grepl("^partei(los|unabh)", pname, ignore.case = TRUE) ||
      grepl("^SIGGI", pname, ignore.case = TRUE)) next
  std <- normalise_party(pname)
  rp_m_parties[[as.character(ci)]] <- std
}
cat("  1979-2016 file: mapped", length(rp_m_parties), "Landesstimme party columns\n")

## --- Extract data rows (row 5 onward; rows 1-4 are title/headers) ---
rp_m_ags12 <- as.character(rp_multi_raw[[1]])[5:nrow(rp_multi_raw)]
rp_m_names <- as.character(rp_multi_raw[[2]])[5:nrow(rp_multi_raw)]
rp_m_dates <- as.character(rp_multi_raw[[3]])[5:nrow(rp_multi_raw)]
rp_m_data  <- rp_multi_raw[5:nrow(rp_multi_raw), ]

## --- Forward-fill AGS and name ---
for (i in seq_along(rp_m_ags12)) {
  if (is.na(rp_m_ags12[i]) || rp_m_ags12[i] == "NA") {
    if (i > 1) {
      rp_m_ags12[i] <- rp_m_ags12[i - 1]
      rp_m_names[i] <- rp_m_names[i - 1]
    }
  }
}

## --- Convert 12-digit AGS to 8-digit ---
## Format: first 5 digits (state+kreis) + last 3 digits (gemeinde)
rp_m_ags8 <- paste0(substr(rp_m_ags12, 1, 5), substr(rp_m_ags12, 10, 12))

## --- Parse election dates ---
rp_m_iso_dates <- as.Date(rp_m_dates, format = "%d.%m.%Y")
rp_m_years <- as.integer(format(rp_m_iso_dates, "%Y"))

## --- Gesamtsumme LS (col 106) = valid_votes ---
rp_m_valid <- rp_safe_num(rp_m_data[[106]])

## --- Process each election year (1979-2016 only) ---
rp_target_years <- c(1979L, 1983L, 1987L, 1991L, 1996L, 2001L, 2006L, 2011L, 2016L)

for (yr in rp_target_years) {
  yr_mask <- rp_m_years == yr & !is.na(rp_m_years)
  cat("  RP", yr, ":", sum(yr_mask), "rows\n")

  yr_ags   <- rp_m_ags8[yr_mask]
  yr_date  <- rp_m_iso_dates[yr_mask][1]
  yr_valid <- rp_m_valid[yr_mask]
  yr_data  <- rp_m_data[yr_mask, ]

  result <- tibble(
    ags             = yr_ags,
    election_year   = yr,
    state           = "07",
    election_date   = yr_date,
    eligible_voters = NA_real_,
    number_voters   = NA_real_,
    valid_votes     = yr_valid,
    invalid_votes   = NA_real_
  )

  ## --- Extract party vote counts ---
  mapped_sum <- rep(0, nrow(result))
  party_names_seen <- c()
  for (ci_str in names(rp_m_parties)) {
    ci  <- as.integer(ci_str)
    std <- rp_m_parties[[ci_str]]
    v   <- rp_safe_num(yr_data[[ci]])
    col_n <- paste0(std, "_n")
    if (col_n %in% names(result)) {
      result[[col_n]] <- ifelse(is.na(result[[col_n]]), 0, result[[col_n]]) +
                         ifelse(is.na(v), 0, v)
    } else {
      result[[col_n]] <- ifelse(is.na(v), 0, v)
    }
    mapped_sum <- mapped_sum + ifelse(is.na(v), 0, v)
    party_names_seen <- c(party_names_seen, std)
  }
  party_names_seen <- unique(party_names_seen)
  result$other_n <- pmax(yr_valid - mapped_sum, 0, na.rm = TRUE)

  result <- result |> filter(!is.na(ags))

  ## --- Convert to vote shares ---
  result <- result |> mutate(turnout = NA_real_)
  for (std_name in c(party_names_seen, "other")) {
    result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
  }
  result$cdu_csu <- result$cdu
  result <- result |> select(-ends_with("_n"))

  rp_results[[as.character(yr)]] <- result
}

## ---------- 2021 from LW_2021_GESAMT.xlsx ----------
## 2021 election from official state statistics office
## Source: LW_2021_GESAMT.xlsx, sheet "LW_GESAMT_daten20210329_1228"
## Format: 13-digit ID, GUW rows (G=Gesamt, U=Urnenwahl, W=Briefwahl)
##   - Municipality rows: pos 9-11 != "000" AND pos 12-13 == "00" AND prefix != "000"
##   - Kreisfreie Städte: prefix "000" with "Kreisfreie Stadt" in name
##   - AGS = "07" + substr(ID, 4, 6) + substr(ID, 9, 11)
## Landesstimme (Zweitstimme) in cols 50-85; Erststimme in cols 14-49

rp_xlsx <- file.path(rp_raw_path, "LW_2021_GESAMT.xlsx")
rp_raw <- read_excel(rp_xlsx, sheet = "LW_GESAMT_daten20210329_1228",
                     col_names = FALSE, .name_repair = "minimal")

## --- Parse Landesstimme party columns from header (row 1) ---
## Landesstimme party counts are in even columns 54, 56, ..., 84
rp_header <- as.character(rp_raw[1, ])
rp_parties <- list()
for (ci in seq(54, 84, by = 2)) {
  pname <- trimws(rp_header[ci])
  if (is.na(pname) || pname == "" || pname == "%") next
  if (grepl("^Gesamtsumme$|^Sonstige$|^Insgesamt$|^Gültige$|^ungültige$", pname, ignore.case = TRUE)) next
  std <- normalise_party(pname)
  rp_parties[[as.character(ci)]] <- std
}
cat("  Mapped", length(rp_parties), "Landesstimme party columns\n")

## --- Extract data rows (row 2 onward, row 1 is header) ---
rp_ids   <- as.character(rp_raw[[1]])[2:nrow(rp_raw)]
rp_names <- as.character(rp_raw[[3]])[2:nrow(rp_raw)]
rp_guw   <- as.character(rp_raw[[4]])[2:nrow(rp_raw)]
rp_data  <- rp_raw[2:nrow(rp_raw), ]

## --- Filter to G (Gesamt) rows only ---
g_mask  <- rp_guw == "G"
rp_ids   <- rp_ids[g_mask]
rp_names <- rp_names[g_mask]
rp_data  <- rp_data[g_mask, ]

## --- Select municipality-level rows ---
## Municipalities: pos 9-11 != "000" AND pos 12-13 == "00" AND prefix != "000"
muni_mask <- substr(rp_ids, 9, 11) != "000" &
             substr(rp_ids, 12, 13) == "00" &
             substr(rp_ids, 1, 3) != "000"
## Kreisfreie Städte: prefix "000", name contains "Kreisfreie Stadt"
kfs_mask <- substr(rp_ids, 1, 3) == "000" &
            grepl("Kreisfreie Stadt", rp_names)
row_mask <- muni_mask | kfs_mask

rp_ids   <- rp_ids[row_mask]
rp_names <- rp_names[row_mask]
rp_data  <- rp_data[row_mask, ]

cat("  Selected", sum(muni_mask[row_mask | TRUE][1:length(row_mask)]), "municipality +",
    sum(kfs_mask[row_mask | TRUE][1:length(row_mask)]), "kreisfreie Stadt rows\n")

## --- Compute AGS ---
rp_ags <- paste0("07", substr(rp_ids, 4, 6), substr(rp_ids, 9, 11))

## --- Extract vote metadata (Landesstimme) ---
eligible_v <- rp_safe_num(rp_data[[6]])   # Wahlberechtigte
voters     <- rp_safe_num(rp_data[[10]])  # Wähler
invalid_v  <- rp_safe_num(rp_data[[50]])  # ungültige Zweitstimmen
valid_v    <- rp_safe_num(rp_data[[52]])  # Gültige Zweitstimmen

result <- tibble(
  ags             = rp_ags,
  election_year   = 2021L,
  state           = "07",
  election_date   = as.Date("2021-03-14"),
  eligible_voters = eligible_v,
  number_voters   = voters,
  valid_votes     = valid_v,
  invalid_votes   = invalid_v
)

## --- Extract party vote counts ---
mapped_sum <- rep(0, nrow(result))
party_names_seen <- c()
for (ci_str in names(rp_parties)) {
  ci  <- as.integer(ci_str)
  std <- rp_parties[[ci_str]]
  v   <- rp_safe_num(rp_data[[ci]])
  col_n <- paste0(std, "_n")
  if (col_n %in% names(result)) {
    result[[col_n]] <- ifelse(is.na(result[[col_n]]), 0, result[[col_n]]) +
                       ifelse(is.na(v), 0, v)
  } else {
    result[[col_n]] <- ifelse(is.na(v), 0, v)
  }
  mapped_sum <- mapped_sum + ifelse(is.na(v), 0, v)
  party_names_seen <- c(party_names_seen, std)
}
party_names_seen <- unique(party_names_seen)
result$other_n <- pmax(valid_v - mapped_sum, 0, na.rm = TRUE)

result <- result |> filter(!is.na(ags))

## --- Convert to vote shares ---
result <- result |> mutate(turnout = ifelse(eligible_voters > 0, number_voters / eligible_voters, NA_real_))
for (std_name in c(party_names_seen, "other")) {
  result[[std_name]] <- result[[paste0(std_name, "_n")]] / result$valid_votes
}
result$cdu_csu <- result$cdu
result <- result |> select(-ends_with("_n"))

cat("  RP 2021:", nrow(result), "munis\n")
rp_results[["2021"]] <- result

all_states[["rp"]] <- standardise(bind_rows(rp_results))
cat("Rheinland-Pfalz total:", nrow(all_states[["rp"]]), "rows\n\n")


###############################################################################
####                     FINAL: Bind and write                             ####
###############################################################################

state_unharm <- bind_rows(all_states)

# Remove HE county-level aggregate rows that coexist with their constituent municipalities
# 06439000 (Schwalm-Eder 1946, VV=57) and 06535000 (Werra-Meissner 1958, VV=0)
# are gemeindefreie Gebiete/county aggregates mixed with municipality-level data
he_agg_rows <- state_unharm$ags %in% c("06439000", "06535000")
if (any(he_agg_rows)) {
  cat(sprintf("Removing %d HE county-aggregate rows (AGS 06439000, 06535000)\n",
              sum(he_agg_rows)))
  state_unharm <- state_unharm[!he_agg_rows, ]
}

# Flag (but keep) rows with valid_votes == 0 (gemeindefreie Gebiete, empty municipalities)
# These are real administrative units with persistent AGS codes — needed for balanced panels
# Note: valid_votes = NA is kept unflagged (e.g. HB pre-1999, RP 1979-2016 where only pcts available)
state_unharm$flag_no_valid_votes <- ifelse(
  !is.na(state_unharm$valid_votes) & state_unharm$valid_votes == 0, 1L, 0L
)
cat(sprintf("Flagged %d rows with valid_votes == 0\n", sum(state_unharm$flag_no_valid_votes)))

# Flag (but keep) Briefwahl-only entities: eligible_voters=0 but votes>0
# These are real municipalities with mail-in vote misallocation or source gaps
# (e.g., BB 1990, SH 1983 garbled PDF, NRW 1966 major cities)
state_unharm$flag_briefwahl_only <- ifelse(
  !is.na(state_unharm$eligible_voters) & state_unharm$eligible_voters == 0 &
  !is.na(state_unharm$valid_votes) & state_unharm$valid_votes > 0, 1L, 0L
)
n_brief <- sum(state_unharm$flag_briefwahl_only)
if (n_brief > 0) {
  cat(sprintf("Flagged %d rows as Briefwahl-only entities (EV=0, VV>0)\n", n_brief))
  # Neutralize Briefwahl-only rows: EV/NV are not meaningful (EV=0 is source artifact,
  # e.g. MV Amt-level Briefwahl aggregates). VV and party columns are preserved so that
  # vote shares remain correct after harmonization. na.rm=TRUE in harm scripts'
  # weighted sum naturally skips NA EV/NV, preventing turnout inflation.
  idx_brief <- state_unharm$flag_briefwahl_only == 1
  state_unharm$eligible_voters[idx_brief] <- NA_real_
  state_unharm$number_voters[idx_brief] <- NA_real_
  state_unharm$turnout[idx_brief] <- NA_real_
  cat(sprintf("Set EV/NV/turnout to NA for %d Briefwahl-only rows\n", sum(idx_brief)))
}

# HE 1958/1962: number_voters not reported for non-kreisfreie municipalities
# Source XLSX has empty Wähler column → recode 0 to NA (data unavailable, not zero)
he_nv_fix <- state_unharm$state == "06" &
  state_unharm$election_year %in% c(1958L, 1962L) &
  !is.na(state_unharm$number_voters) & state_unharm$number_voters == 0
if (any(he_nv_fix)) {
  cat(sprintf("Recoding %d HE 1958/1962 number_voters from 0 to NA (source gap)\n", sum(he_nv_fix)))
  state_unharm$number_voters[he_nv_fix] <- NA_real_
  state_unharm$turnout[he_nv_fix] <- NA_real_
}

# Clamp negative invalid_votes to 0 (Briefwahl allocation rounding artifacts)
n_neg_iv <- sum(state_unharm$invalid_votes < 0, na.rm = TRUE)
if (n_neg_iv > 0) {
  cat(sprintf("Clamping %d rows with negative invalid_votes to 0\n", n_neg_iv))
  state_unharm$invalid_votes <- pmax(state_unharm$invalid_votes, 0, na.rm = TRUE)
}

# Flag turnout > 1 before capping (mirrors federal pipeline flag)
state_unharm <- state_unharm |>
  mutate(
    flag_naive_turnout_above_1 = ifelse(!is.na(turnout) & is.finite(turnout) & turnout > 1, 1, 0)
  )
n_turnout_above_1 <- sum(state_unharm$flag_naive_turnout_above_1, na.rm = TRUE)
if (n_turnout_above_1 > 0) {
  cat(sprintf("Flagged %d rows with turnout > 1 (flag_naive_turnout_above_1)\n", n_turnout_above_1))
}

# Safety net: cap turnout at plausible range; set Inf/NaN to NA
state_unharm <- state_unharm |>
  mutate(
    turnout = ifelse(is.finite(turnout), turnout, NA_real_),
    turnout = ifelse(!is.na(turnout) & turnout > 1.5, NA_real_, turnout)
  )
n_bad_turnout <- sum(is.na(state_unharm$turnout) & !is.na(state_unharm$eligible_voters))
if (n_bad_turnout > 0) {
  cat(sprintf("Note: %d rows have NA turnout (non-finite or >150%%)\n", n_bad_turnout))
}

# CDU/CSU consistency: ensure cdu_csu = combined CDU+CSU family vote
# Bayern uses CSU only; other states typically CDU only, but DSU (CSU sister)
# ran in some East German states in 1990, so always sum both when present.
state_unharm <- state_unharm |>
  mutate(
    cdu_csu = case_when(
      state == "09" ~ csu,                                        # Bavaria: CSU only
      !is.na(cdu) & !is.na(csu) ~ cdu + csu,                     # Both present (e.g. MV 1990 DSU)
      !is.na(cdu)   ~ cdu,                                        # CDU only
      TRUE           ~ cdu_csu                                     # Fallback
    )
  )

# Standardise column ordering: meta_cols, sorted party cols, other, cdu_csu
state_unharm <- standardise(state_unharm)

cat("\n=== Final dataset ===\n")
cat("Total rows:", nrow(state_unharm), "\n")
cat("Number of party columns:", sum(!names(state_unharm) %in% c(meta_cols, "other", "cdu_csu")), "\n")
cat("State-year combinations:\n")
print(table(state_unharm$state, state_unharm$election_year))

# Replace Inf with NA (division by valid_votes=0 produces Inf shares)
state_unharm <- state_unharm |>
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.), NA_real_, .)))

# Drop always-zero party columns (party existed in raw data but never had votes)
party_cols_all <- setdiff(names(state_unharm), c(meta_cols, "other", "cdu_csu",
  "flag_naive_turnout_above_1", "flag_no_valid_votes", "flag_briefwahl_only"))
always_zero <- sapply(party_cols_all, function(col) {
  all(state_unharm[[col]] == 0 | is.na(state_unharm[[col]]), na.rm = FALSE)
})
if (any(always_zero)) {
  drop_cols <- names(which(always_zero))
  cat(sprintf("Dropping %d always-zero party columns: %s\n",
              length(drop_cols), paste(head(drop_cols, 10), collapse = ", ")))
  if (length(drop_cols) > 10) cat(sprintf("  ... and %d more\n", length(drop_cols) - 10))
  state_unharm <- state_unharm |> select(-all_of(drop_cols))
}

# Relocate: move flag_* and perc_total_votes_incongruence to the front
# so data-quality flags are visible near the metadata (requested in GitHub #7).
front_meta <- intersect(
  c("ags", "election_year", "election_date", "state", "state_name",
    "ags_name_21", "ags_name",
    "eligible_voters", "number_voters", "valid_votes", "invalid_votes",
    "turnout"),
  names(state_unharm)
)
front_flags <- grep("^flag_|^perc_total_votes", names(state_unharm), value = TRUE)
other_cols  <- setdiff(names(state_unharm), c(front_meta, front_flags))
state_unharm <- state_unharm |>
  dplyr::relocate(dplyr::all_of(c(front_meta, front_flags, other_cols)))

# Write output
fwrite(state_unharm, "data/state_elections/final/state_unharm.csv")
write_rds(state_unharm, "data/state_elections/final/state_unharm.rds")
cat("Written to data/state_elections/final/state_unharm.{csv,rds}\n")
