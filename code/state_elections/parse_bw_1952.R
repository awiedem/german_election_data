# Wahl zur Verfassunggebenden Landesversammlung, 9 March 1952.
# Source geography: 1 January 1979, NOT election-day municipalities.
# See docs/sources/bw_1952.md for the electorate restriction and source differences.

read_bw_1952 <- function(raw_root = "data/state_elections/raw/Landtagswahlen") {
  # Search by basename so composed/decomposed Unicode directory names both work.
  path <- list.files(raw_root, pattern = "^Gemeindetabelle_LW1952[.]xlsx$",
                     recursive = TRUE, full.names = TRUE)
  if (length(path) != 1L) stop("BW 1952: expected exactly one source workbook")
  if (!identical(readxl::excel_sheets(path), "Tabelle1")) {
    stop("BW 1952: unexpected workbook sheets")
  }
  raw <- readxl::read_excel(path, sheet = "Tabelle1", col_names = FALSE,
                           col_types = "text", .name_repair = "minimal")
  # Fail on a changed layout/source instead of silently ignoring rows or footnotes.
  stopifnot(nrow(raw) == 1120L, ncol(raw) == 30L,
            grepl("Verfassunggebenden Landesversammlung", raw[[1]][1]),
            grepl("9. Marz 1952", raw[[1]][1], fixed = TRUE),
            grepl("Wahlschein (insgesamt 49 310", raw[[1]][1119], fixed = TRUE),
            grepl("laut Wahlliste", raw[[1]][1120], fixed = TRUE))
  party_columns <- seq.int(10L, 28L, 2L)
  source_parties <- c("CDU", "SPD", "FDP/DVP", "BHE", "KPD", "DG-BHE",
                      "SRP", "Zentrum", "UWG", "DG")
  parties <- c("cdu", "spd", "fdp", "bhe", "kpd", "dg_bhe", "srp",
               "zentrum", "uwg", "dg")
  stopifnot(identical(unname(unlist(raw[3, party_columns])), source_parties))

  rows <- which(grepl("^[1-4][0-9]{5}$", raw[[1]]))
  stopifnot(identical(rows, 5:1115), !anyDuplicated(raw[[1]][rows]),
            identical(raw[[1]][rows], raw[[30]][rows]),
            !anyNA(raw[[2]][rows]), all(nzchar(raw[[2]][rows])))
  # Row 1116 is an electorate-only total, followed by blanks and footnotes.
  stopifnot(is.na(raw[[1]][1116]), as.numeric(raw[[3]][1116]) == 4332807,
            all(is.na(unlist(raw[1116, -3]))),
            all(is.na(unlist(raw[1117:1118, ]))))

  count_columns <- c(3L, 4L, 6L, 8L, party_columns)
  count_names <- c("eligible_voters_wahlliste_only", "number_voters",
                   "invalid_votes", "valid_votes", parties)
  counts <- as.data.frame(lapply(raw[rows, count_columns], as.numeric))
  names(counts) <- count_names
  # This particular source contains complete integer counts (including real zeros).
  # Missing or nonnumeric cells in a replacement source must trigger fresh review.
  stopifnot(!anyNA(counts), all(is.finite(as.matrix(counts))),
            all(as.matrix(counts) >= 0),
            all(as.matrix(counts) == floor(as.matrix(counts))),
            all(counts$number_voters == counts$valid_votes + counts$invalid_votes),
            all(rowSums(counts[parties]) == counts$valid_votes),
            sum(counts$eligible_voters_wahlliste_only) == as.numeric(raw[[3]][1116]))
  # Also validate cached source percentages; these are never used as party shares.
  pct_columns <- c(5L, 7L, 9L, party_columns + 1L)
  pct_expected <- cbind(
    counts$number_voters / counts$eligible_voters_wahlliste_only,
    counts$invalid_votes / counts$number_voters,
    counts$valid_votes / counts$number_voters,
    as.matrix(counts[parties]) / counts$valid_votes
  ) * 100
  pct_source <- as.matrix(as.data.frame(lapply(raw[rows, pct_columns], as.numeric)))
  stopifnot(!anyNA(pct_source), all(abs(pct_source - pct_expected) < 1e-9))

  # Independently published official totals: StaLA, B VII S - 2020,
  # 4235 20001, Table 1, printed pages 4, 6, 8 and 10 (1952 column).
  # Preserve the workbook's KPD +1 / SRP -1; no municipal correction is known.
  official <- c(eligible_voters_wahlliste_only = 4382117, number_voters = 2789872,
                invalid_votes = 59052, valid_votes = 2730820,
                cdu = 982727, spd = 765032, fdp = 491711, bhe = 170751,
                kpd = 119604, dg_bhe = 84026, srp = 65787,
                zentrum = 23356, uwg = 22393, dg = 5433)
  expected_difference <- setNames(rep(0, length(official)), names(official))
  expected_difference[c("eligible_voters_wahlliste_only", "kpd", "srp")] <-
    c(-49310, 1, -1)
  totals <- colSums(counts[names(official)])
  stopifnot(identical(unname(totals - official), unname(expected_difference)))
  official_url <- paste0("https://www.statistik-bw.de/fileadmin/user_upload/",
                         "Service/Veroeff/Statistische_Berichte/423520001.pdf")
  reconciliation <- data.frame(
    field = names(official), source_sum = unname(totals),
    official_total = unname(official), difference = unname(totals - official),
    documented_difference = unname(expected_difference),
    printed_page = c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 8L, 6L, 8L, 10L, 10L, 6L),
    official_source = official_url
  )
  source_counts <- data.frame(
    source_row = rows, source_key = raw[[1]][rows],
    ags = paste0("08", raw[[1]][rows]), municipality_name = raw[[2]][rows],
    election_year = 1952L, state = "08", election_date = as.Date("1952-03-09"),
    geography_date = as.Date("1979-01-01"), counts,
    turnout_wahlliste_only = pct_source[, 1] / 100,
    check.names = FALSE
  )
  # The full municipal electorate cannot be recovered from the statewide footnote.
  # Keep the restricted denominator and its rate exclusively in source_counts.
  result <- tibble::tibble(
    ags = source_counts$ags, election_year = 1952L, state = "08",
    election_date = as.Date("1952-03-09"), eligible_voters = NA_real_,
    number_voters = counts$number_voters, valid_votes = counts$valid_votes,
    invalid_votes = counts$invalid_votes, turnout = NA_real_
  )
  for (party in parties) result[[party]] <- counts[[party]] / counts$valid_votes
  result$other <- (counts$valid_votes - rowSums(counts[parties])) / counts$valid_votes
  result$cdu_csu <- result$cdu
  coverage <- data.frame(
    dataset = "state_unharm", state = "08", election_year = 1952L,
    election_date = as.Date("1952-03-09"),
    election_label = "Wahl zur Verfassunggebenden Landesversammlung",
    geography_level = "municipality", geography_date = as.Date("1979-01-01"),
    geography_basis = "Retrospective aggregation by Statistisches Landesamt",
    rows = nrow(result), unique_ags = length(unique(result$ags)),
    vote_unit = "single vote", share_denominator = "valid_votes",
    eligible_voters_known = 0L, turnout_known = 0L,
    voters_known = nrow(result), valid_votes_known = nrow(result),
    invalid_votes_known = nrow(result),
    eligible_voters_wahlliste_only = sum(counts$eligible_voters_wahlliste_only),
    excluded_wahlschein_holders = 49310L,
    official_eligible_voters = unname(official[1]),
    official_number_voters = unname(official[2]),
    official_turnout = unname(official[2] / official[1]),
    turnout_treatment = "NA: municipal electorate excludes unallocated Wahlschein holders",
    harmonized = FALSE,
    harmonization_reason = "Before annual crosswalk coverage (1990)",
    source_workbook = basename(path), source_sheet = "Tabelle1",
    source_note = "docs/sources/bw_1952.md"
  )
  list(data = result, source_counts = source_counts, reconciliation = reconciliation,
       coverage = coverage, source_path = path, parties = parties)
}

write_bw_1952_provenance <- function(parsed,
                                    directory = "data/state_elections/derived/bw_1952") {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (name in c("source_counts", "reconciliation", "coverage")) {
    data.table::fwrite(parsed[[name]], file.path(directory, paste0(name, ".csv")), na = "NA")
  }
  invisible(directory)
}
