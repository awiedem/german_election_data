# Run from the repository root: Rscript --vanilla code/checks/test_harmonization_audit.R
source("code/shared/harmonization_audit.R")
source("code/shared/state_mapping.R")
options(gerda.audit_dir = tempfile("gerda-mapping-tests-"))
dir.create(getOption("gerda.audit_dir"))
checks <- 0L
ok <- function(x) { stopifnot(isTRUE(x)); checks <<- checks + 1L }
fails <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  ok(inherits(error, "error") && grepl(pattern, conditionMessage(error), fixed = TRUE))
}
s <- data.frame(ags = c("01001000", "01002000"), year = c(2000, 2000),
                valid_votes = c(100, 50), number_voters = c(110, 60))
m <- s[c(1, 1, 2), ]
m$target <- c("01001001", "01001002", "01002000")
m$pop_cw <- c(.25, .75, 1)
audit <- function(x = m, input = s, ...) gerda_audit_mapping(input, x, c("ags", "year"),
  "target", "fixture", target_codes = c("01001001", "01001002", "01002000"), ...)
ok(nrow(audit()) == 2L)
fails(audit(m[-3, ]), "invalid/incomplete")                       # fully lost row
fails(audit(m[-1, ]), "invalid/incomplete")                       # one branch of a split
fails(audit(rbind(m, m[1, ])), "invalid/incomplete")               # duplicated edge
z <- m; z$target[1] <- NA; fails(audit(z), "invalid/incomplete")
z <- m; z$target[1] <- "99999999"; fails(audit(z), "invalid/incomplete")
z <- m; z$ags[1] <- "01099999"; fails(audit(z), "invalid/incomplete")
for (value in c(NA, Inf, -.25, 0, .5)) {
  z <- m; z$pop_cw[1] <- value; fails(audit(z), "invalid/incomplete")
}
z <- m; z$valid_votes[1] <- 90; fails(audit(z), "invalid/incomplete")
z <- m; z$valid_votes[1] <- NA; fails(audit(z), "invalid/incomplete")
fails(audit(input = rbind(s, s[1, ])), "source keys are not unique")
z <- m; z$ags[1] <- NA; fails(audit(z), "Missing/empty source key")
empty <- s; empty$valid_votes <- empty$number_voters <- 0
z <- m; z$valid_votes <- z$number_voters <- 0; z$pop_cw <- c(1, 1, 1)
fails(audit(z, empty), "invalid/incomplete")
ok(nrow(audit(z, empty, allow_zero_weight_sum = TRUE)) == 2L)
z$target[1] <- NA
fails(audit(z, empty, allow_zero_weight_sum = TRUE), "invalid/incomplete")
ok(file.exists(file.path(getOption("gerda.audit_dir"), "fixture_failures.csv")))
audit()
ok(nrow(data.table::fread(file.path(getOption("gerda.audit_dir"), "fixture_failures.csv"))) == 0L)

v <- gerda_weighted_counts(m, c("target", "year"), c("valid_votes", "number_voters"), "pop_cw")
ok(identical(v$valid_votes, c(25, 75, 50)))
gerda_audit_totals(s, v, "year", c("valid_votes", "number_voters"), "totals")
z <- v; z$valid_votes[1] <- z$valid_votes[1] + 1
fails(gerda_audit_totals(s, z, "year", "valid_votes", "totals"), "counts not conserved")
z <- v; z$year <- 2001
fails(gerda_audit_totals(s, z, "year", "valid_votes", "totals"), "entire group")
n <- m; n$valid_votes <- NA_real_
v <- gerda_weighted_counts(n, c("target", "year"), "valid_votes", "pop_cw")
ok(all(is.na(v$valid_votes)))
n <- s; n$valid_votes <- NA_real_
fails(gerda_audit_totals(n, transform(v, valid_votes = 0), "year", "valid_votes", "totals"), "counts not conserved")

# Regression against the original official workbook, not a copied expected row.
raw <- suppressMessages(readxl::read_excel(
  "data/state_elections/raw/Landtagswahlen/Sachsen-Anhalt/Sachsen-Anhalt_2011_Landtagswahl.xlsx",
  col_names = FALSE, col_types = "text"))
hit <- which(raw[[3]] == "15080156")
ok(length(hit) == 1L && as.numeric(raw[[29]][hit]) == 1217)
fixed <- gerda_fix_st_2011_burg(raw, 2011)
ok(fixed[[3]][hit] == "15086015")
ok(identical(raw[-3], fixed[-3]))
ok(identical(gerda_fix_st_2011_burg(fixed, 2011), fixed))
z <- raw; z[[10]][hit] <- "Other town"
fails(gerda_fix_st_2011_burg(z, 2011), "not TRUE")
cw <- data.frame(ags = c("07232089", "07232096"), election_year = 2020,
                 ags_21 = c("07232089", "07232096"), pop_cw = 1)
back <- gerda_add_geckler_backmap(cw, "ags_21", c(1991, 1996))
ok(nrow(back) == 6L)
ok(all(tapply(back$pop_cw[back$ags == "07232503"], back$election_year[back$ags == "07232503"], sum) == 1))
fails(gerda_add_geckler_backmap(back, "ags_21", 1991), "review the backward-map patch")

# Edge cases from full-data runs and adversarial mutations.
ok(nrow(audit(m[0, ], s[0, ])) == 0L)
z <- m; z$target <- as.integer(z$target)
fails(audit(z), "is.character")
z <- m; z$pop_cw[1] <- NaN
fails(audit(z), "invalid/incomplete")
z <- v; z$valid_votes <- Inf
fails(gerda_audit_totals(n, z, "year", "valid_votes", "totals"), "counts not conserved")
ok(identical(gerda_add_geckler_backmap(cw, "ags_21", numeric()), cw))
paths <- data.frame(ags = "01001000", year = 2000, target = "01001001",
                    pop_cw = c(.2, .8), area_cw = c(.3, .7))
collapsed <- gerda_collapse_crosswalk(paths)
ok(nrow(collapsed) == 1L && collapsed$pop_cw == 1 && collapsed$area_cw == 1)
fails(gerda_collapse_crosswalk(rbind(paths, paths[1, ])), "Identical duplicate")
fails(gerda_require_mapped(data.frame(ags = c("01001000", NA)), "ags", "lookup"), "unresolved")
fails(gerda_require_join_coverage(s, s[1, ], "ags", "postal"), "discard")
ex <- data.frame(ags = "01001000", year = 2000, reason = "Reviewed historical case", evidence = "Fixture")
left <- gerda_exclude_documented(rbind(s, transform(s[1, ], year = 2001)), ex, c("ags", "year"), "exclusion")
ok(nrow(left) == 2L && any(left$ags == "01001000" & left$year == 2001))
fails(gerda_exclude_documented(s, rbind(ex, ex), c("ags", "year"), "exclusion"), "anyDuplicated")

source("code/shared/mayoral_mapping.R")
history <- data.frame(ags = "15151011", year = c(1994, 1995),
                      ags_21 = "15082430", pop_cw = 1, population = 1)
u <- data.frame(ags = c("15151011", "15000000"), election_year = 2008,
                 ags_21 = NA_character_, population = NA_real_)
r <- gerda_recover_mayoral_mapping(u, history)
ok(r$ags_21[1] == "15082430" && is.na(r$ags_21[2]) && r$crosswalk_year[1] == 1995)
history$ags_21[2] <- "15082431"
ok(all(is.na(gerda_recover_mayoral_mapping(u, history)$ags_21)))
history$ags_21[2] <- "15082430"; history$pop_cw[2] <- .9
ok(all(is.na(gerda_recover_mayoral_mapping(u, history)$ags_21)))
# Retrospective code recovery must never spill over to unrelated years.
history <- data.frame(ags = "16056000", year = 2018, ags_21 = "16056000", pop_cw = 1, population = 1)
e <- data.frame(ags = "16063105", election_year = 2017:2018, ags_21 = NA_character_)
r <- gerda_recover_mayoral_mapping(e, history)
ok(is.na(r$ags_21[1]) && r$ags_21[2] == "16056000")
# A retrospective AGS can also duplicate an election already present under its
# historical code. Never recover that copy as additional votes.
e <- data.frame(ags=c("16056000", "16063105"), election_date=as.Date("2018-04-15"),
                round="hauptwahl", election_type=c("Oberbürgermeisterwahl", "Bürgermeisterwahl"),
                eligible_voters=34930, number_voters=16745, valid_votes=16564,
                invalid_votes=181, winner_party="DIE LINKE", winner_votes=7854)
ok(nrow(gerda_exclude_eisenach_duplicates(e)) == 1L)
e$valid_votes[2] <- 16000
fails(gerda_exclude_eisenach_duplicates(e), "not TRUE")
fails(gerda_exclude_eisenach_duplicates(e[2, ]), "not TRUE")
nearest_cw <- data.frame(ags="01001000", election_year=c(1999,2001),
                         target="01001001", pop_cw=1, area_cw=1)
keys <- data.frame(ags=c("01001000","01002000"), election_year=2000)
ok(gerda_nearest_crosswalk_year(keys, nearest_cw, "target", "nearest")$cw_year == 2001)
nearest_cw$target[2] <- "01001002"
fails(gerda_nearest_crosswalk_year(keys, nearest_cw, "target", "nearest"), "equally close")

# Input keys must fail before downstream year filters erase a missing value.
input_file <- tempfile(fileext = ".rds")
input <- data.frame(ags = "01001000", election_year = NA_real_)
saveRDS(input, input_file)
fails(gerda_read_election_source(input_file), "unresolved source rows")
input$election_year <- 2000; input$ags <- ""
saveRDS(input, input_file)
fails(gerda_read_election_source(input_file), "unresolved source rows")
unlink(input_file)
ok(nrow(gerda_weighted_counts(m[0, ], c("target", "year"), "valid_votes", "pop_cw")) == 0L)
ok(identical(names(gerda_weighted_counts(m, c("target", "year"), character(), "pop_cw")), c("target", "year")))
cat("PASS:", checks, "mapping, conservation, missingness and source-regression checks\n")
