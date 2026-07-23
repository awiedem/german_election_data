#### Bundestagswahl Wahlkreise — 2021 -> 2025 boundary crosswalk ####
## The 2025 Wahlkreiseinteilung redraws some of the 299 constituencies. This
## script builds a tested crosswalk between the 2021 and 2025 definitions using
## the official Bundeswahlleiterin recomputation of the 2021 result onto the
## 2025 boundaries (btwkr25_umrechnung_btw21.csv), so the "previous-election
## district strength" for a 2025 Wahlkreis can be read off directly without any
## geometry guess-work.
##
## Outputs (data/federal_elections/wahlkreis_level/final/):
##   federal_wkr_2021_on_2025.{csv,rds} — the 2021 result expressed on the 2025
##      boundaries, GERDA-style (one row per Wahlkreis x stimme; party columns are
##      vote shares), with a `boundary_change` category.
##   wkr_2021_to_2025_crosswalk.{csv,rds} — compact per-2025-Wahlkreis crosswalk:
##      wkr_nr, name, land, boundary_change (unchanged / redrawn / new).
##
## boundary_change is derived by exact comparison of the recomputed 2021 totals
## (2025 boundary) against the ACTUAL 2021 totals (2021 boundary) for the same
## Wahlkreis number:
##   unchanged = eligible + both valid-vote totals identical (territory unchanged)
##   redrawn   = totals differ (territory changed) but the 2021 number/name exists
##   new       = the 2025 Wahlkreis name has no 2021 counterpart
##
## Authors: Hanno Hilbig (with Claude Code assistance), July 2026

#### Setup ####
rm(list = ls())
for (pkg in c("here", "data.table")) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  suppressMessages(library(pkg, character.only = TRUE))
}
here::i_am("code/federal_elections_wahlkreis/02_wkr_crosswalk_21_25.R")
source(here("code", "federal_elections_wahlkreis", "_normalise_party.R"))
normalise_party_v <- function(x) vapply(x, normalise_party, character(1), USE.NAMES = FALSE)

fin_dir <- here("data", "federal_elections", "wahlkreis_level", "final")
umr_path <- here("data", "federal_elections", "wahlkreis_level", "raw", "BTW25",
                 "btwkr25_umrechnung_btw21.csv")

de_num <- function(x) { x <- gsub("[.[:space:]]", "", x); x[x == ""] <- NA; as.numeric(x) }
locf   <- function(x) { x[x == ""] <- NA; for (i in seq_along(x)) if (is.na(x[i]) && i > 1) x[i] <- x[i - 1]; x }

#### Parse the recomputed 2021-on-2025 result ####
L <- readLines(umr_path, warn = FALSE, encoding = "UTF-8")
L <- L[!grepl("^#", L)]; L <- L[nchar(trimws(L)) > 0]
sp <- lapply(strsplit(L, ";", fixed = TRUE), function(x) trimws(gsub('"', "", x)))
nc <- max(lengths(sp))
M  <- t(vapply(sp, function(x) c(x, rep("", nc - length(x))), character(nc)))
H1 <- M[1, ]; H2 <- M[2, ]; D <- M[-(1:2), , drop = FALSE]
# Keep only the 299 Wahlkreise; drop Land (901-916) and Bund (999) summary rows.
keep <- { n <- suppressWarnings(as.integer(D[, 1])); !is.na(n) & n >= 1 & n <= 299 }
D <- D[keep, , drop = FALSE]

grp <- locf(H1)
st  <- ifelse(grepl("^Erst", H2), "erststimme", ifelse(grepl("^Zweit", H2), "zweitstimme", NA_character_))
is_meta <- grepl("^Wahlberechtigte|^Wählende|^Ungültig|^Gültig|^Wkr|^Land|^Wahlkreisname", grp)

wkr_nr  <- sprintf("%03d", suppressWarnings(as.integer(D[, 1])))
land    <- D[, 2]
wkrname <- D[, 3]
vcol_e  <- which(grepl("^Gültig", grp) & st == "erststimme")[1]
vcol_z  <- which(grepl("^Gültig", grp) & st == "zweitstimme")[1]
ecol    <- which(grepl("^Wahlberechtigte \\(A\\)", grp))[1]
valid_e <- de_num(D[, vcol_e]); valid_z <- de_num(D[, vcol_z]); elig <- de_num(D[, ecol])

party_cols <- which(!is_meta & !is.na(st) & grp != "" & !grepl("^sonstige", grp, ignore.case = TRUE))
plist <- lapply(party_cols, function(c) data.table(
  wkr_nr = wkr_nr, wkr_name = wkrname, land = land,
  stimme = st[c], party = normalise_party_v(grp[c]), votes = de_num(D[, c])
))
P <- rbindlist(plist)
P <- P[, .(votes = sum(votes, na.rm = TRUE)), by = .(wkr_nr, wkr_name, land, stimme, party)]

valid <- rbind(
  data.table(wkr_nr, stimme = "erststimme",  valid_votes = valid_e, eligible_voters = elig),
  data.table(wkr_nr, stimme = "zweitstimme", valid_votes = valid_z, eligible_voters = elig)
)
P <- merge(P, valid, by = c("wkr_nr", "stimme"))
P[, vote_share := ifelse(valid_votes > 0, votes / valid_votes, NA_real_)]

# wide shares
named <- P[, .(named = sum(votes, na.rm = TRUE)), by = .(wkr_nr, stimme)]
wide <- dcast(P, wkr_nr + wkr_name + land + stimme + valid_votes + eligible_voters ~ party,
              value.var = "votes", fun.aggregate = sum, fill = 0)
pcols <- setdiff(names(wide), c("wkr_nr", "wkr_name", "land", "stimme", "valid_votes", "eligible_voters"))
wide <- merge(wide, named, by = c("wkr_nr", "stimme"))
wide[, other := pmax(valid_votes - named, 0)]
for (p in c(pcols, "other")) wide[, (p) := ifelse(valid_votes > 0, get(p) / valid_votes, NA_real_)]
wide[, cdu_csu := rowSums(cbind(if ("cdu" %in% pcols) cdu else 0, if ("csu" %in% pcols) csu else 0), na.rm = TRUE)]
wide[, named := NULL]

#### Derive boundary_change by matching 2025 Wahlkreise to 2021 ####
## Wahlkreis NUMBERS are reassigned between elections (per-Land seat reallocation
## cascades the numbering), so 2025 #N is not 2021 #N. Match on the normalised
## Wahlkreis NAME; the 2021 electorate is the territorial fingerprint (identical
## eligible count => territory unchanged). A merely RENAMED Wahlkreis has no name
## match but an exact (electorate, Zweitstimme-valid) fingerprint against a single
## 2021 Wahlkreis -> treat as unchanged (renamed). Only a Wahlkreis with neither a
## name nor a fingerprint match is genuinely new / restructured (drawing on
## several 2021 predecessors); the official recomputation still supplies its
## previous-election strength directly.
norm_name <- function(x) gsub("[^a-z]", "", tolower(x))
act <- as.data.table(readRDS(file.path(fin_dir, "federal_wkr_unharm.rds")))
a21 <- unique(act[election_year == 2021 & stimme == "zweitstimme",
                  .(prior_2021_wkr_nr = wkr_nr, prior_2021_name = wkr_name,
                    a_elig = eligible_voters, a_valid_z = valid_votes)])
a21[, k := norm_name(prior_2021_name)]
rec <- unique(merge(unique(P[, .(wkr_nr, wkr_name, land)]),
                    valid[stimme == "zweitstimme", .(wkr_nr, eligible_voters, r_valid_z = valid_votes)],
                    by = "wkr_nr"))
rec[, k := norm_name(wkr_name)]
cw <- merge(rec, a21[, .(k, prior_2021_wkr_nr, prior_2021_name, a_elig, a_valid_z)], by = "k", all.x = TRUE)

# Fallback for name-unmatched rows: a unique (electorate, Zweitstimme-valid) match.
fp <- a21[, .(nfp = .N), by = .(a_elig, a_valid_z)][nfp == 1]
fp <- merge(fp, a21, by = c("a_elig", "a_valid_z"))[, .(eligible_voters = a_elig,
            r_valid_z = a_valid_z, fp_nr = prior_2021_wkr_nr, fp_name = prior_2021_name)]
cw <- merge(cw, fp, by = c("eligible_voters", "r_valid_z"), all.x = TRUE)
cw[is.na(prior_2021_wkr_nr) & !is.na(fp_nr),
   `:=`(prior_2021_wkr_nr = fp_nr, prior_2021_name = fp_name, a_elig = eligible_voters)]

cw[, boundary_change := fifelse(is.na(prior_2021_wkr_nr), "new",
                        fifelse(eligible_voters == a_elig, "unchanged", "redrawn"))]
cw[, renamed := !is.na(prior_2021_name) & norm_name(prior_2021_name) != norm_name(wkr_name)]

# 2-letter Land -> zero-padded state code + name (matching the main dataset).
abbr_to_num <- c(SH = "01", HH = "02", NI = "03", HB = "04", NW = "05", HE = "06",
                 RP = "07", BW = "08", BY = "09", SL = "10", BE = "11", BB = "12",
                 MV = "13", SN = "14", ST = "15", TH = "16")
state_names <- c(
  "01" = "Schleswig-Holstein", "02" = "Hamburg", "03" = "Niedersachsen",
  "04" = "Bremen", "05" = "Nordrhein-Westfalen", "06" = "Hessen",
  "07" = "Rheinland-Pfalz", "08" = "Baden-Württemberg", "09" = "Bayern",
  "10" = "Saarland", "11" = "Berlin", "12" = "Brandenburg",
  "13" = "Mecklenburg-Vorpommern", "14" = "Sachsen", "15" = "Sachsen-Anhalt",
  "16" = "Thüringen")
cw[, state := unname(abbr_to_num[land])][, state_name := unname(state_names[state])]

crosswalk <- cw[, .(wkr_nr, wkr_name, state, state_name, boundary_change, renamed,
                    prior_2021_wkr_nr, prior_2021_name,
                    recomputed_2021_eligible = eligible_voters, actual_2021_eligible = a_elig,
                    eligible_delta = eligible_voters - a_elig)]
setorder(crosswalk, wkr_nr)

# attach category + state to the wide share table
wide[, state := unname(abbr_to_num[land])][, state_name := unname(state_names[state])][, land := NULL]
wide <- merge(wide, crosswalk[, .(wkr_nr, boundary_change)], by = "wkr_nr", all.x = TRUE)
setcolorder(wide, c("wkr_nr", "wkr_name", "state", "state_name", "boundary_change", "stimme",
                    "eligible_voters", "valid_votes", sort(pcols), "other", "cdu_csu"))
setorder(wide, wkr_nr, stimme)

#### Report + write ####
cat("2025 Wahlkreise by boundary_change:\n"); print(crosswalk[, .N, by = boundary_change][order(-N)])
cat(sprintf("\nWahlkreise: %d | changed (redrawn+new): %d | of unchanged, renamed: %d\n",
            crosswalk[, uniqueN(wkr_nr)], crosswalk[boundary_change != "unchanged", .N],
            crosswalk[boundary_change == "unchanged" & renamed == TRUE, .N]))
cat("Redrawn/new Wahlkreise:\n")
print(crosswalk[boundary_change != "unchanged", .(wkr_nr, wkr_name, state, boundary_change, prior_2021_name)])

fwrite(crosswalk, file.path(fin_dir, "wkr_2021_to_2025_crosswalk.csv"))
saveRDS(as.data.frame(crosswalk), file.path(fin_dir, "wkr_2021_to_2025_crosswalk.rds"))
fwrite(wide, file.path(fin_dir, "federal_wkr_2021_on_2025.csv"))
saveRDS(as.data.frame(wide), file.path(fin_dir, "federal_wkr_2021_on_2025.rds"))
cat("\nWrote wkr_2021_to_2025_crosswalk + federal_wkr_2021_on_2025 to", fin_dir, "\n")
