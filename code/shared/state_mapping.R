# Source-specific facts used by all three state-election boundary targets.

gerda_fix_st_2011_burg <- function(raw, year) {
  if (as.integer(year) != 2011L) return(raw)
  hit <- which(!is.na(raw[[3]]) & raw[[3]] == "15080156")
  if (!length(hit)) return(raw) # a corrected upstream workbook needs no patch
  # The official file puts Burg's postal district 994 under a malformed AGS.
  # Its other 17 polling districts and the BBSR crosswalk use 15086015.
  stopifnot(length(hit) == 1L, raw[[9]][hit] == "994",
            raw[[10]][hit] == "Burg, Stadt", raw[[11]][hit] == "Brief",
            any(raw[[3]] == "15086015", na.rm = TRUE))
  raw[[3]][hit] <- "15086015"
  raw
}

gerda_state_exclusions <- function(df, party_vars, label) {
  # These are county totals for uninhabited extra-municipal territories, not
  # municipality elections. Do not manufacture valid_votes=1 for empty rows.
  territory <- df$state == "09" & grepl("^09[0-9]{3}444$", df$ags)
  cols <- intersect(c("eligible_voters", "number_voters", "valid_votes", "invalid_votes", party_vars), names(df))
  empty <- Reduce(`&`, lapply(df[cols], function(v) is.na(v) | v == 0))
  if (any(territory & !empty)) stop("Bavarian territory row carries electoral data; investigate before exclusion")
  excluded <- df[territory, intersect(c("ags", "election_year", "state", "eligible_voters", "number_voters", "valid_votes", "invalid_votes"), names(df))]
  excluded$reason <- rep("Bavarian extra-municipal aggregate; no election counts or party results", nrow(excluded))
  excluded$source <- rep("Bayern official source: county AGS + 444 (gemeindefreie Gebiete)", nrow(excluded))
  gerda_audit_write(excluded, label, "exclusions")
  df[!territory, , drop = FALSE]
}

gerda_add_geckler_backmap <- function(cw, target, years) {
  stopifnot(target %in% c("ags_21", "ags_23"))
  if (!length(years)) return(cw)
  stopifnot(!anyNA(years), all(is.finite(years)))
  # The RLP historical source is on later boundaries. Reuse the documented
  # population allocation already used for municipal and county elections:
  # Niedergeckler 0.05 / Obergeckler 0.15 thousand inhabitants in BBSR 2020.
  targets <- c("07232089", "07232096")
  stopifnot(all(targets %in% cw[[target]]))
  patch <- expand.grid(election_year = sort(unique(years)), target = targets,
                       stringsAsFactors = FALSE)
  names(patch)[names(patch) == "target"] <- target
  patch$ags <- "07232503"
  patch$pop_cw <- ifelse(patch[[target]] == targets[1], 0.25, 0.75)
  patch$area_cw <- ifelse(patch[[target]] == targets[1], 1.39 / 7.75, 6.36 / 7.75)
  # Leave population/employment covariates NA: the patch supplies allocation
  # weights, not historical measurements of the combined source municipality.
  if (any(cw$ags == "07232503" & cw$election_year %in% years)) {
    stop("Obergeckler is now in the upstream crosswalk; review the backward-map patch")
  }
  dplyr::bind_rows(cw, patch)
}
