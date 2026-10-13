# Official Brandenburg polling-station inputs.
# Counts stay separate from shares so source arithmetic can be checked first.
set.seed(20260928)
library(readxl)

gerda_bb_station_source <- function(year) {
  stopifnot(year %in% c(1990L, 1994L))
  path <- file.path("data/state_elections/raw/Landtagswahlen_Wahlkreis/Brandenburg",
                    sprintf("BB_%s_Landtagswahl_Wahlkreis.xlsx", year))
  sheet <- if (year == 1990L) "Zweitstimmen" else "Zweitstimme"
  raw <- as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text"))
  names(raw) <- trimws(names(raw))
  stopifnot(all(raw$Stimmart == "Zweitstimme"),
            all(raw$Wahlbezirksart %in% c("Urnenwahlbezirk", "Briefwahlbezirk")))
  party_map <- if (year == 1990L) c(
    SPD = "spd", CDU = "cdu", "PDS/LL" = "linke_pds", "Bündnis 90" = "buendnis_90",
    "Grüne" = "gruene", FDP = "fdp", LIGA = "chr_l", DBU = "dbu", DFP = "dfp",
    DSU = "dsu", REP = "rep", Domowina = "domowina", NPD = "npd", CSU = "csu",
    FABU = "fabu", LVP = "lvp", FORUM = "forum", GRAUE = "graue"
  ) else c(
    SPD = "spd", CDU = "cdu", FDP = "fdp", BFWG = "freie_wahler",
    "GRÜNE/B90" = "gruene", "Bürger" = "buerger", DSU = "dsu", GRAUE = "graue",
    REP = "rep", "ÖDP" = "odp", PDS = "linke_pds", UWVB = "uwvb"
  )
  numeric_map <- c("Wahlberechtigte insgesamt" = "eligible_voters", "Wähler" = "number_voters",
                   "Gültige Stimmen" = "valid_votes", "Ungültige Stimmen" = "invalid_votes",
                   party_map)
  stopifnot(all(names(numeric_map) %in% names(raw)))
  # No silently omitted party fields: all columns after valid votes are mapped.
  stopifnot(setequal(names(raw)[seq.int(match("Gültige Stimmen", names(raw)) + 1L, ncol(raw))],
                     names(party_map)))
  x <- data.frame(source_row = seq_len(nrow(raw)) + 1L, source_ags = raw$AGS,
                  source_name = raw$Gemeindename, wkr = raw[["Landtags-wahlkreis"]],
                  station = raw$Wahlbezirk, postal = raw$Wahlbezirksart == "Briefwahlbezirk",
                  stringsAsFactors = FALSE)
  for (nm in names(numeric_map)) {
    value <- suppressWarnings(as.numeric(raw[[nm]]))
    if (year == 1990L && nm %in% c("LIGA", "DFP", "Domowina", "CSU", "FABU", "LVP", "FORUM", "GRAUE")) {
      # Erläuterungen: transferred MV precincts had a different ballot. Blanks
      # in these columns mean the party did not contest that state's election.
      mv_ballot <- !is.na(raw$CSU)
      expected_blank <- if (nm %in% c("LIGA", "DFP", "Domowina")) mv_ballot else !mv_ballot
      stopifnot(identical(is.na(value), expected_blank))
      value[expected_blank] <- 0
    }
    if (anyNA(value) || any(value < 0 | value != floor(value))) {
      stop("Unexpected missing/noninteger/negative source counts: ", year, " ", nm)
    }
    x[[numeric_map[[nm]]]] <- value
  }
  stopifnot(!anyDuplicated(paste(x$source_ags, x$source_name, x$wkr, x$station, x$postal)),
            all(rowSums(x[unname(party_map)]) == x$valid_votes),
            all(x$valid_votes + x$invalid_votes == x$number_voters),
            all(x$eligible_voters[x$postal] == 0))
  attr(x, "party_columns") <- unname(party_map)
  attr(x, "source_path") <- path
  x
}

gerda_bb_1994_verified <- function() {
  raw <- gerda_bb_station_source(1994L)
  parties <- attr(raw, "party_columns")
  # Postal districts have no municipal attribution. Preserve in-person coverage.
  x <- raw[!raw$postal, ]
  stopifnot(all(grepl("^12[0-9]{8}$", x$source_ags)))
  # The final two source digits identify the Amt. City subareas use district
  # codes inside the municipality field; aggregate them to their city AGS.
  x$ags <- ifelse(substr(x$source_ags, 1L, 5L) %in% paste0("120", 51:54),
                  paste0(substr(x$source_ags, 1L, 5L), "000"), substr(x$source_ags, 1L, 8L))
  fields <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes", parties)
  counts <- aggregate(x[fields], list(ags = x$ags), sum)
  stopifnot(nrow(counts) == 1693L, !anyDuplicated(counts$ags),
            all(colSums(counts[fields]) == colSums(x[fields])))
  result <- counts
  for (party in parties) result[[party]] <- counts[[party]] / counts$valid_votes
  # Retain the established 1994 published party grouping. Source audit tables
  # still retain each of these parties separately as integer counts.
  residual <- c("buerger", "dsu", "graue", "odp", "uwvb")
  result$other <- rowSums(counts[residual]) / counts$valid_votes
  result[residual] <- NULL
  result$einzelbewerber <- 0 # Only first-vote candidates, absent from this sheet.
  result$cdu_csu <- result$cdu
  result$election_year <- 1994L
  result$state <- "12"
  result$election_date <- as.Date("1994-09-11")
  # This is the in-person participation rate, excluding pooled postal voters.
  result$turnout <- counts$number_voters / counts$eligible_voters
  list(data = result, counts = counts, stations = raw)
}

gerda_bb_1990_verified <- function() {
  raw <- gerda_bb_station_source(1990L)
  fields <- c("eligible_voters", "number_voters", "valid_votes", "invalid_votes")
  parties <- c("spd", "cdu", "linke_pds", "buendnis_90", "gruene", "fdp",
               "chr_l", "dbu", "dfp", "dsu", "rep", "domowina", "npd")
  provenance <- "data/state_elections/derived/bb_1990_1994"
  mapping <- read.delim(file.path(provenance, "bb1990_election_day_geography.tsv"),
                        colClasses = "character", check.names = FALSE)
  key <- function(ags, name) paste(ags, name, sep = "|")
  mk <- key(mapping$source_ags, mapping$source_municipality)
  x <- raw[!raw$postal, ]
  idx <- match(key(x$source_ags, x$source_name), mk)
  stopifnot(!anyDuplicated(mk), !anyDuplicated(mapping$election_day_ags),
            !anyNA(idx), setequal(unique(idx), seq_len(nrow(mapping))))
  x$ags <- mapping$election_day_ags[idx]
  exclude <- substr(x$ags, 1L, 2L) == "13"
  # The workbook's extra MV-only parties must occur only on MV ballots.
  mv_parties <- c("csu", "fabu", "lvp", "forum", "graue")
  stopifnot(length(unique(x$ags[exclude])) == 22L, sum(exclude) == 35L,
            all(as.matrix(x[!exclude, mv_parties]) == 0),
            all(mapping$disposition[idx][exclude] == "exclude_MV_1990_ballot"))
  counts <- aggregate(x[!exclude, c(fields, parties)], list(ags = x$ags[!exclude]), sum)
  extra <- read.delim(file.path(provenance, "bb1990_original_transferred_municipalities.tsv"),
                      colClasses = c(ags = "character", ddr_code = "character"))
  names(extra)[match(c("pds_ll", "chrl"), names(extra))] <- c("linke_pds", "chr_l")
  stopifnot(!any(extra$ags %in% counts$ags), nrow(extra) == 2L,
            all(rowSums(extra[parties]) == extra$valid_votes),
            all(extra$valid_votes + extra$invalid_votes == extra$number_voters))
  counts <- rbind(counts, extra[c("ags", fields, parties)])
  counts <- counts[order(counts$ags), ]
  rownames(counts) <- NULL
  stopifnot(nrow(counts) == 1792L, !anyDuplicated(counts$ags),
            all(grepl("^12[0-9]{6}$", counts$ags)),
            all(rowSums(counts[parties]) == counts$valid_votes))
  result <- counts
  for (party in parties) result[[party]] <- counts[[party]] / counts$valid_votes
  result$other <- 0
  result$cdu_csu <- result$cdu
  result$election_year <- 1990L
  result$state <- "12"
  result$election_date <- as.Date("1990-10-14")
  result$turnout <- counts$number_voters / counts$eligible_voters
  list(data = result, counts = counts, stations = raw, mapping = mapping)
}
