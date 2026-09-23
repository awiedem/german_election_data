# Source-to-export regression checks for the September 2026 missingness repair.
# Run at the repository root after rebuilding state_unharm and state_harm_21/23/25:
# Rscript --vanilla code/checks/check_state_missingness.R [baseline-RDS-directory]
# The optional baseline checks every cell outside invalid_votes for regression.
source("code/shared/state_missingness.R")
stopifnot(is.na(gerda_sum_known(c(NA_real_, NA_real_))),
          identical(gerda_sum_known(c(0, NA_real_)), 0),
          identical(gerda_sum_known(c(2, NA_real_, 3)), 5))

out <- "data/data_checks/state_missingness"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stems <- c("state_unharm", "state_harm_21", "state_harm_23", "state_harm_25")
datasets <- setNames(lapply(stems, function(nm) as.data.frame(readRDS(
  paste0("data/state_elections/final/", nm, ".rds")))), stems)
s <- datasets$state_unharm
raw_path <- "data/state_elections/raw/Landtagswahlen"

# Independently read the HE source columns (row 3 headers, row 5 state totals).
# Check each modern GKZ, not just a statewide sum over the few known cities.
he_file <- list.files(file.path(raw_path, "Hessen"), "xlsx$", full.names = TRUE)
stopifnot(length(he_file) == 1L)
source_checks <- list()
for (yr in c(1958L, 1962L)) {
  raw <- readxl::read_excel(he_file, sheet = as.character(yr), col_names = FALSE,
                           .name_repair = "minimal")
  headers <- gsub("[[:space:]]+", " ", as.character(raw[3, ]))
  ev <- which(grepl("^Insgesamt", headers))
  iv <- which(grepl("ung.ltig", headers, ignore.case = TRUE))
  stopifnot(length(ev) == 1L, length(iv) == 1L)
  raw <- raw[6:nrow(raw), ]
  take <- grepl("^[0-9]{3,6}$", as.character(raw[[1]]))
  raw <- raw[take, ]
  ags <- sprintf("06%06d", as.integer(raw[[1]]))
  observed <- s[s$state == "06" & s$election_year == yr, ]
  for (field in c("number_voters", "invalid_votes")) {
    values <- as.numeric(raw[[if (field == "number_voters") ev + 1L else iv]])
    # Deliberately independent of gerda_sum_known(): empty groups stay unknown.
    expected <- vapply(split(values, ags), function(x) {
      known <- x[!is.na(x)]
      if (length(known)) sum(known) else NA_real_
    }, numeric(1))
    # The parser removes these county aggregates, not municipality observations.
    expected <- expected[!names(expected) %in% c("06439000", "06535000")]
    matched <- expected[match(observed$ags, names(expected))]
    stopifnot(!anyDuplicated(observed$ags),
              setequal(observed$ags, names(expected)),
              identical(is.na(observed[[field]]), is.na(unname(matched))),
              isTRUE(all.equal(unname(matched), observed[[field]],
                               check.attributes = FALSE, tolerance = 1e-10)))
    source_checks[[length(source_checks) + 1L]] <- data.frame(
      state = "06", election_year = yr, field = field, rows = nrow(observed),
      known = sum(!is.na(unname(matched))), missing = sum(is.na(unname(matched))),
      sum_known = gerda_sum_known(matched))
  }
}

rp_years <- c(1979, 1983, 1987, 1991, 1996, 2001, 2006, 2011, 2016)
rp <- s[s$state == "07" & s$election_year %in% rp_years, ]
stopifnot(setequal(unique(rp$election_year), rp_years),
          all(is.na(rp$eligible_voters)), all(is.na(rp$number_voters)),
          all(is.na(rp$invalid_votes)), all(is.na(rp$turnout)),
          any(rp$valid_votes > 0, na.rm = TRUE))
# RP valid-vote totals must still agree with the source's Landesstimmen column.
rp_raw <- readxl::read_excel(file.path(raw_path, "Rheinland-Pfalz",
  "LW_RLP_1979_2021.xlsx"), col_names = FALSE, .name_repair = "minimal")
stopifnot(as.character(rp_raw[3, 106]) == "Gesamtsumme")
rp_dates <- as.Date(as.character(rp_raw[[3]]), "%d.%m.%Y")
for (yr in rp_years) {
  take <- !is.na(rp_dates) & format(rp_dates, "%Y") == yr
  stopifnot(abs(sum(as.numeric(rp_raw[[106]][take]), na.rm = TRUE) -
                sum(rp$valid_votes[rp$election_year == yr], na.rm = TRUE)) < 1e-7)
}

# Known invalid counts remain nonnegative. Source-reported zeros are checked
# individually against the MV workbook below.
stopifnot(any(s$invalid_votes == 0, na.rm = TRUE),
          !any(s$invalid_votes < 0, na.rm = TRUE))
# Guard against tempting but incorrect party/source and multi-vote changes.
mv <- s[s$state == "13" & s$election_year == 1990, ]
mv_raw <- readxl::read_excel(file.path(raw_path, "Mecklenburg-Vorpommern",
  "Mecklenburg-Vorpommern_1990_Landtagswahl.xls"), sheet = "LW90-GEM",
  col_names = FALSE, .name_repair = "minimal")
stopifnot(mv_raw[[13]][4] == "CSU", mv_raw[[15]][4] == "DSU")
mv_rows <- !is.na(mv_raw[[2]]) & grepl("^13[0-9]{6}$", mv_raw[[2]])
mv_ags <- as.character(mv_raw[[2]][mv_rows])
mv_invalid <- as.numeric(mv_raw[[6]][mv_rows])
stopifnot(mv_raw[[6]][4] == "ungültig", !anyDuplicated(mv_ags),
          !anyDuplicated(mv$ags), setequal(mv_ags, mv$ags),
          sum(mv_invalid == 0) == 7L,
          identical(mv_invalid, mv$invalid_votes[match(mv_ags, mv$ags)]))
stopifnot(sum(as.numeric(mv_raw[[13]][mv_rows]), na.rm = TRUE) == 9663,
          sum(as.numeric(mv_raw[[15]][mv_rows]), na.rm = TRUE) == 6499)
stopifnot(abs(sum(mv$csu * mv$valid_votes, na.rm = TRUE) - 9663) < 1e-7,
          abs(sum(mv$dsu * mv$valid_votes, na.rm = TRUE) - 6499) < 1e-7)
by <- s[s$state == "09" & s$election_year %in% c(1994, 1998, 2003, 2008, 2013), ]
stopifnot(all(is.na(by$eligible_voters)), all(!is.na(by$number_voters)))
# Pin the documented unresolved SH extraction, without treating it as valid.
# A source repair must deliberately update this check and the limitation table.
sh <- s[s$state == "01" & s$election_year == 1983, ]
paired <- !is.na(sh$number_voters) & !is.na(sh$valid_votes)
stopifnot(nrow(sh) == 1079L, sum(is.na(sh$eligible_voters)) == 135L,
          sum(paired) == 944L, sum(sh$valid_votes[paired]) == 1329758,
          sum(sh$number_voters[paired]) == 1286010,
          sum(sh$valid_votes[paired] > sh$number_voters[paired]) == 253L)

comparisons <- changes <- list()
baseline <- commandArgs(trailingOnly = TRUE)
schema <- read.csv("data/state_elections/metadata/column_schema.csv")
for (nm in stems) {
  x <- datasets[[nm]]
  definitions <- schema[schema$dataset == nm, ]
  stopifnot(!anyDuplicated(definitions$column),
            identical(definitions$column, names(x)))
  share_cols <- definitions$column[definitions$role %in%
                                    c("party_share", "residual_share", "derived_share")]
  stopifnot(all(vapply(x[share_cols], is.numeric, logical(1))),
            all(definitions$role[grepl("^ags_name", definitions$column)] == "identifier"))
  csv <- data.table::fread(paste0("data/state_elections/final/", nm, ".csv"),
    colClasses = list(character = intersect(c("ags", "state", "county"), names(x))),
    na.strings = c("", "NA"), data.table = FALSE)
  stopifnot(identical(names(x), names(csv)), nrow(x) == nrow(csv))
  for (v in names(x)) {
    a <- x[[v]]; b <- csv[[v]]
    equal <- identical(unname(is.na(a)), unname(is.na(b))) &&
      if (is.numeric(a) && !inherits(a, "Date")) {
        isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = 1e-12))
      } else identical(unname(as.character(a)), unname(as.character(b)))
    comparisons[[length(comparisons) + 1L]] <- data.frame(dataset = nm, field = v,
                                                        equal = equal)
    if (!equal) stop("CSV/RDS mismatch: ", nm, " / ", v)
  }
  r <- x[x$state == "07" & x$election_year %in% rp_years, ]
  stopifnot(nrow(r) > 0L, all(is.na(r$invalid_votes)))
  if (length(baseline)) {
    old <- as.data.frame(readRDS(file.path(baseline[1], paste0(nm, ".rds"))))
    stopifnot(identical(names(x), names(old)), nrow(x) == nrow(old))
    for (v in setdiff(names(x), "invalid_votes")) {
      if (!isTRUE(all.equal(unname(x[[v]]), unname(old[[v]]), tolerance = 1e-12))) {
        stop("Unexpected baseline change: ", nm, " / ", v, ": ",
             paste(all.equal(unname(x[[v]]), unname(old[[v]]), tolerance = 1e-12), collapse = "; "))
      }
    }
    changed <- is.na(x$invalid_votes) != is.na(old$invalid_votes) |
      (!is.na(x$invalid_votes) & !is.na(old$invalid_votes) &
         abs(x$invalid_votes - old$invalid_votes) > 1e-10)
    stopifnot(all(is.na(x$invalid_votes[changed])),
              all(old$invalid_votes[changed] == 0))
    d <- x[changed, c("ags", "state", "election_year", "invalid_votes")]
    d$dataset <- nm
    d$previous_invalid_votes <- old$invalid_votes[changed]
    changes[[nm]] <- d
  }
}
write.csv(do.call(rbind, source_checks), file.path(out, "he_source_checks.csv"),
          row.names = FALSE, na = "")
write.csv(do.call(rbind, comparisons), file.path(out, "csv_rds_checks.csv"),
          row.names = FALSE)
if (length(changes)) write.csv(do.call(rbind, changes),
  file.path(out, "changed_invalid_votes.csv"), row.names = FALSE, na = "")
if (length(changes)) {
  d <- do.call(rbind, changes)
  d$invalid_votes_zero_to_na <- 1L
  summary <- aggregate(invalid_votes_zero_to_na ~ dataset + state + election_year,
                       data = d, FUN = sum)
  summary <- summary[order(summary$dataset, summary$state, summary$election_year), ]
  write.csv(summary, file.path(out, "change_summary.csv"), row.names = FALSE)
}
cat("Source missingness, unchanged vote conventions, and",
    length(comparisons), "CSV/RDS column comparisons passed.\n")
