# Run from repository root: Rscript --vanilla data/state_elections/derived/bb_1990_1994/bb1990_audit_geography.R
set.seed(20260928)
library(readxl)

base <- "data/state_elections/derived/bb_1990_1994"
source <- read_excel("data/state_elections/raw/Landtagswahlen_Wahlkreis/Brandenburg/BB_1990_Landtagswahl_Wahlkreis.xlsx", sheet = "Zweitstimmen")
map <- read.delim(file.path(base, "bb1990_election_day_geography.tsv"), colClasses = "character", check.names = FALSE)
extra <- read.delim(file.path(base, "bb1990_original_transferred_municipalities.tsv"), colClasses = "character", check.names = FALSE)
key <- function(ags, name) paste(ags, name, sep = "|")
groups <- unique(key(source$AGS[!is.na(source$AGS)], source$Gemeindename[!is.na(source$AGS)]))
mapkey <- key(map$source_ags, map$source_municipality)
stopifnot(length(groups) == 1812L, nrow(map) == 1812L, !anyDuplicated(mapkey), !anyDuplicated(map$election_day_ags), setequal(groups, mapkey), all(grepl("^[0-9]{8}$", map$election_day_ags)), all(nzchar(map$evidence)))
stopifnot(sum(map$disposition == "exclude_MV_1990_ballot") == 22L, sum(map$disposition == "retain_BB_1990_ballot") == 1790L)
stopifnot(all(substr(map$election_day_ags, 1, 2) == ifelse(map$disposition == "exclude_MV_1990_ballot", "13", "12")))
joined <- match(key(source$AGS, source$Gemeindename), mapkey)
# The five additional party columns identify ballots cast in Mecklenburg-Vorpommern.
stopifnot(sum(!is.na(source$CSU)) == 35L, all(map$disposition[joined[!is.na(source$CSU)]] == "exclude_MV_1990_ballot"))
stopifnot(setequal(extra$ags, c("12037110", "12037160")), !any(extra$ags %in% map$election_day_ags))
parties <- c("spd", "cdu", "pds_ll", "buendnis_90", "fdp", "chrl", "dbu", "dfp", "dsu", "gruene", "rep", "domowina", "npd")
votes <- as.data.frame(lapply(extra[parties], as.numeric))
stopifnot(all(rowSums(votes) == as.numeric(extra$valid_votes)), all(as.numeric(extra$number_voters) == as.numeric(extra$valid_votes) + as.numeric(extra$invalid_votes)))
stopifnot(sum(votes$spd) == 6, sum(votes$cdu) == 17, sum(rowSums(votes)) == 31)
# These precincts generated two false municipality codes in the old PDF extraction.
eisen <- source[which(source$Gemeindename == "Eisenhüttenstad" & source$Wahlbezirk == "035"), ]
schwedt <- source[which(source$Gemeindename == "Schwedt/Oder, S" & source$Wahlbezirk == "029"), ]
stopifnot(nrow(eisen) == 1L, nrow(schwedt) == 1L, eisen$`Gültige Stimmen` == 557, schwedt$`Gültige Stimmen` == 502, eisen$SPD == 172, eisen$CDU == 190, eisen$`PDS/LL` == 62, eisen$`Bündnis 90` == 51, eisen$FDP == 45, schwedt$DSU == 5, schwedt$Grüne == 30, schwedt$REP == 8)
stopifnot(map$election_day_ags[match(key(eisen$AGS, eisen$Gemeindename), mapkey)] == "12003000", map$election_day_ags[match(key(schwedt$AGS, schwedt$Gemeindename), mapkey)] == "12006000")
cat("PASS: complete unique source-name mapping; 1790 retained + 2 restored municipalities; 22 MV groups excluded; original-source additions reconcile; both formerly misidentified city precincts retained.\n")
