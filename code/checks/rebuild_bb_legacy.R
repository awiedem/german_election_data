# Compatibility rebuild of the deprecated population-weighted state_harm series.
# Run from the repository root:
# Rscript --vanilla code/checks/rebuild_bb_legacy.R BASELINE_STATE_HARM_RDS OUTPUT_DIR
library(dplyr)
library(data.table)
library(digest)
set.seed(20260928)
source("code/shared/harmonization_audit.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
out <- args[[2]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
legacy <- readRDS(args[[1]])
current <- readRDS("data/state_elections/final/state_unharm.rds")

# Extracted without modification from state_unharm.rds at commit cf9abee.
# Original Git LFS SHA256: 211ad9bea2add5a0c9077301c659a68c06e744d8534f64417983817962cf7193.
# Its BB 1990/1994 rows reproduce every published legacy recipient cell.
snapshot <- "data/state_elections/derived/bb_1990_1994/legacy_bb_source_cf9abee.rds"
stopifnot(digest(snapshot, algo = "sha256", file = TRUE) ==
            "6a60770e17cdbc19f3f16bb8f1d290bba6526da6a689ded0521caca200786c3f")
old <- readRDS(snapshot)
fixed <- current |> filter(state == "12", election_year %in% c(1990L, 1994L))
stopifnot(all(old$state == "12"), all(old$election_year %in% c(1990, 1994)))
key <- function(d) paste(d$ags, d$election_year, sep = "/")
stopifnot(!anyDuplicated(key(old)), !anyDuplicated(key(fixed)))
cw <- fread("data/crosswalks/final/ags_crosswalks.csv",
            colClasses = c(ags = "character", ags_21 = "character")) |>
  as_tibble() |> rename(election_year = year)
pad <- function(x) ifelse(nchar(x) == 7L, paste0("0", x), x)
cw <- cw |> mutate(ags = pad(ags), ags_21 = pad(ags_21))
meta <- c("ags", "election_year", "state", "election_date",
          "eligible_voters", "number_voters", "valid_votes", "invalid_votes")
shares <- setdiff(names(old), c(meta, "ags_name"))
# Preserve the legacy schema; unexpected new substantive parties require review.
for (nm in setdiff(names(fixed), names(old))) {
  stopifnot(nm == "ags_name" || all(is.na(fixed[[nm]]) | fixed[[nm]] == 0))
}
for (nm in setdiff(names(old), names(fixed))) {
  stopifnot(all(is.na(old[[nm]]) | old[[nm]] == 0))
  fixed[[nm]] <- NA_real_
}
fixed <- fixed[names(old)]

run <- function(src, label) {
  joined <- left_join(src, cw, by = c("ags", "election_year"),
                      relationship = "many-to-many")
  unmatched <- joined |> filter(is.na(ags_21))
  fwrite(unmatched, file.path(out, paste0("legacy_unmatched_", label, ".csv")))
  # Preserve the inherited direct-crosswalk coverage explicitly. Unmatched
  # units remain documented exclusions from this deprecated series only.
  mapped <- joined |> filter(!is.na(ags_21))
  input <- src[key(src) %in% key(mapped), ]
  options(gerda.audit_dir = file.path(out, paste0("audit_", label)))
  gerda_audit_mapping(input, mapped, c("ags", "election_year"), "ags_21",
                      paste0("legacy_bb_", label), target_codes = unique(cw$ags_21))
  targets <- paste(mapped$ags_21, mapped$election_year, sep = "/")
  # A BB-only rebuild is valid only if no other state's source contributes to
  # these recipients. Check against all contemporary source keys and the CW.
  foreign <- cw[substr(cw$ags, 1, 2) != "12" &
                  paste(cw$ags_21, cw$election_year, sep = "/") %in% targets, ]
  stopifnot(!any(key(foreign) %in% key(current)))
  result <- mapped |>
    mutate(weights = pop_cw * population) |>
    group_by(ags_21, election_year) |>
    summarise(across(all_of(shares), ~ weighted.mean(.x, weights, na.rm = TRUE)),
              eligible_voters = round(sum(eligible_voters * pop_cw, na.rm = TRUE)),
              number_voters = round(sum(number_voters * pop_cw, na.rm = TRUE)),
              valid_votes = round(sum(valid_votes * pop_cw, na.rm = TRUE)),
              .groups = "drop") |>
    rename(ags = ags_21) |>
    mutate(state = substr(ags, 1, 2),
           state_name = case_when(state == "12" ~ "Brandenburg",
                                  state == "13" ~ "Mecklenburg-Vorpommern"),
           election_date = as.Date(NA), flag_unsuccessful_naive_merge = 0,
           total_vote_share = round(rowSums(across(all_of(setdiff(shares,
                                      c("turnout", "cdu_csu")))), na.rm = TRUE), 8),
           flag_total_votes_incongruent = ifelse(total_vote_share > 1, 1, 0)) |>
    arrange(ags, election_year)
  stopifnot(!anyNA(result$state_name))
  summary <- src |> count(election_year, name = "source_rows") |>
    left_join(input |> count(election_year, name = "matched_source_rows"), by = "election_year") |>
    left_join(result |> count(election_year, name = "target_rows"), by = "election_year") |>
    mutate(stage = label, unmatched_source_rows = source_rows - matched_source_rows)
  fwrite(summary, file.path(out, paste0("legacy_counts_", label, ".csv")))
  result
}
before <- run(old, "before")
ix <- match(key(before), key(legacy))
stopifnot(!anyNA(ix), setequal(names(before), names(legacy)))
for (nm in names(legacy)) {
  a <- legacy[[nm]][ix]; b <- before[[nm]]
  stopifnot(identical(is.na(a), is.na(b)))
  if (is.numeric(a)) {
    stopifnot(isTRUE(all.equal(a, b, tolerance = 1e-12, check.attributes = FALSE)))
  } else stopifnot(identical(as.character(a), as.character(b)))
}
after <- run(fixed, "after")
affected <- union(key(before), key(after))
replace <- key(legacy) %in% affected
result <- bind_rows(legacy[!replace, ], after[names(legacy)])
result <- result[order(match(key(result), key(legacy)), na.last = TRUE), ]
stopifnot(identical(legacy[!replace, ], result[!key(result) %in% affected, ]),
          identical(legacy[is.na(legacy$ags), ], result[is.na(result$ags), ]),
          !anyDuplicated(key(result)))
saveRDS(before, file.path(out, "legacy_before_reproduced.rds"), compress = "gzip")
saveRDS(after, file.path(out, "legacy_after_focused.rds"), compress = "gzip")
saveRDS(result, file.path(out, "state_harm.rds"), compress = "gzip")
fwrite(result, file.path(out, "state_harm.csv"))
csv <- fread(file.path(out, "state_harm.csv"),
             colClasses = c(ags = "character", state = "character"),
             na.strings = c("", "NA"), data.table = FALSE)
for (nm in names(result)) {
  stopifnot(identical(is.na(result[[nm]]), is.na(csv[[nm]])))
  if (is.numeric(result[[nm]])) {
    stopifnot(isTRUE(all.equal(as.numeric(result[[nm]]), as.numeric(csv[[nm]]),
                               tolerance = 1e-12)))
  } else stopifnot(identical(as.character(result[[nm]]), as.character(csv[[nm]])))
}
cat(sprintf("PASS: baseline reproduced; %d before / %d after recipient rows; %d unrelated rows unchanged.\n",
            nrow(before), nrow(after), sum(!replace)))
