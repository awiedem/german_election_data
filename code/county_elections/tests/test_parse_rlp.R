set.seed(20260730)

source("code/county_elections/parsers/parse_rlp.R")

test_root <- tempfile("test-parse-rlp-")
dir.create(file.path(test_root, "Kreistagswahlen", "Rheinland-Pfalz"), recursive = TRUE)
on.exit(unlink(test_root, recursive = TRUE, force = TRUE), add = TRUE)

zip_source <- "data/county_elections/raw/local_elections_rlp.zip"
html_source <- paste0(
  "data/county_elections/raw/Kreistagswahlen/",
  "Rheinland-Pfalz/1999_html"
)
pdf_source <- paste0(
  ".git/lfs/objects/3d/02/",
  "3d02a61930c0ee26cf1b733eb015c4794bd73830bcc178b561beff251887c762"
)
stopifnot(file.exists(zip_source), dir.exists(html_source), file.exists(pdf_source))
stopifnot(file.copy(zip_source, file.path(test_root, "local_elections_rlp.zip")))
html_target <- file.path(
  test_root,
  "Kreistagswahlen",
  "Rheinland-Pfalz",
  "1999_html"
)
dir.create(html_target)
html_files <- list.files(html_source, pattern = "\\.html$", full.names = TRUE)
stopifnot(length(html_files) == 36L)
stopifnot(all(file.copy(html_files, html_target)))
stopifnot(file.copy(
  pdf_source,
  file.path(
    test_root,
    "Kreistagswahlen",
    "Rheinland-Pfalz",
    "Rheinland-Pfalz_2024_Ergebnisse_Kreisebene.pdf"
  )
))

result <- parse_rlp_county_elections(test_root)

stopifnot(nrow(result) == 216L)
stopifnot(nrow(unique(result[c("ags", "election_year")])) == nrow(result))
stopifnot(identical(
  sort(unique(result$election_year)),
  c(1999L, 2004L, 2009L, 2014L, 2019L, 2024L)
))
stopifnot(all(table(result$election_year) == 36L))
stopifnot(all(table(result$election_year, result$contest_type)[, "kreisfreie_city_council"] == 12L))
stopifnot(all(table(result$election_year, result$contest_type)[, "kreistag"] == 24L))
stopifnot(all(result$result_level == "county"))
stopifnot(all(result$event_scope == "statewide"))
stopifnot(all(result$state == "07"))
stopifnot(all(result$county == substr(result$ags, 1, 5)))
stopifnot(all(result$eligible_voters > 0))
stopifnot(all(result$number_voters <= result$eligible_voters))
stopifnot(all(result$valid_votes + result$invalid_votes == result$number_voters))
stopifnot(all(result$turnout >= 0 & result$turnout <= 1))

count_columns <- grep("^vote_count_", names(result), value = TRUE)
for (count_column in count_columns) {
  party <- sub("^vote_count_", "", count_column)
  stopifnot(party %in% names(result))
  expected_share <- result[[count_column]] / result$valid_votes
  comparable <- !is.na(expected_share) & !is.na(result[[party]])
  stopifnot(all(abs(result[[party]][comparable] - expected_share[comparable]) < 1e-12))
  stopifnot(all(result[[count_column]][!is.na(result[[count_column]])] >= 0))
}

party_columns <- sub("^vote_count_", "", count_columns)
share_sum <- rowSums(result[party_columns], na.rm = TRUE)
stopifnot(all(share_sum > 0.995 & share_sum < 1.005))

result_1999 <- result[result$election_year == 1999L, ]
stopifnot(nrow(result_1999) == 36L)
stopifnot(length(unique(result_1999$ags)) == 36L)
stopifnot(sum(result_1999$eligible_voters) == 3071058)
stopifnot(sum(result_1999$number_voters) == 1932234)
stopifnot(sum(result_1999$valid_votes) == 1868025)
stopifnot(sum(result_1999$invalid_votes) == 64209)
stopifnot(sum(result_1999$vote_count_spd, na.rm = TRUE) == 673586)
stopifnot(sum(result_1999$vote_count_cdu, na.rm = TRUE) == 860852)
stopifnot(sum(result_1999$vote_count_gruene, na.rm = TRUE) == 93795)
stopifnot(sum(result_1999$vote_count_fdp, na.rm = TRUE) == 76780)
stopifnot(sum(result_1999$vote_count_linke_pds, na.rm = TRUE) == 1111)

# The raw archive consists of official Landeswahlleiter pages. Each local copy
# retains both the office marker and its original official-site result path.
html_contents <- vapply(
  html_files,
  function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
  character(1)
)
stopifnot(all(grepl("Statistisches Landesamt", html_contents, fixed = TRUE)))
stopifnot(all(grepl(
  "/kw/wahlen/2004/kreistagswahlen/ergebnisse/",
  html_contents,
  fixed = TRUE
)))

result_2024 <- result[result$election_year == 2024L, ]
stopifnot(sum(result_2024$eligible_voters) == 3194181)
stopifnot(sum(result_2024$number_voters) == 2022139)
stopifnot(sum(result_2024$valid_votes) == 1970218)
stopifnot(sum(result_2024$invalid_votes) == 51921)
stopifnot(sum(result_2024$vote_count_spd, na.rm = TRUE) == 398393)
stopifnot(sum(result_2024$vote_count_cdu, na.rm = TRUE) == 620635)

cat(
  "RLP parser tests passed:",
  nrow(result), "rows;",
  paste(names(table(result$election_year)), table(result$election_year), sep = "=", collapse = ", "),
  "\n"
)
