# Regression checks for the October 2026 state fixes (Hanno's notes of
# 2026-10-16/18): far-right parties hidden under unmapped labels or in `other`,
# far_right = 0 where the source cannot tell, and postal votes in
# Mecklenburg-Vorpommern and Sachsen-Anhalt.
# Run at the repository root after rebuilding state_unharm and state_harm_21/23/25:
# Rscript --vanilla code/checks/check_state_farright_postal.R [final-directory]
# Calibrated 2026-10-19: 31 of the 32 checks of sections 1-5 fail on the build
# before the patch ("no pooled postal row left" is a guard that passed before);
# 8 of the 9 checks of section 6 (the audit round) fail on the first patched
# build; the voters = valid + invalid check guards what that build fixed. The
# far_left NA and far-right classification checks (third round, same day) fail
# on the second-round build, and those of section 7 (Hanno's suggestions) on
# the third-round build.
suppressMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[[1]] else "data/state_elections/final"
u <- as.data.table(readRDS(file.path(dir, "state_unharm.rds")))
harm <- lapply(c(h21 = "state_harm_21", h23 = "state_harm_23", h25 = "state_harm_25"),
               function(nm) as.data.table(readRDS(file.path(dir, paste0(nm, ".rds")))))

failed <- character(0)
check <- function(name, ok) {
  ok <- isTRUE(ok)
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", name))
  if (!ok) failed <<- c(failed, name)
}
share <- function(d, col) {
  if (!col %in% names(d) || all(is.na(d[[col]]))) return(NA_real_)
  100 * sum(d[[col]] * d$valid_votes, na.rm = TRUE) / sum(d$valid_votes, na.rm = TRUE)
}
near <- function(x, target, tol = 0.06) !is.na(x) && abs(x - target) <= tol

# 1. Bayern 1994-2013: REP and NPD ("Die Heimat (HEIMAT)") in rep/npd, not under
#    fallback labels. Official Gesamtstimmen shares, Landeswahlleiter.
check("BY: no fallback columns for REP/NPD/BP/Franken",
      !any(c("die_republikaner_rep", "die_heimat_heimat", "bayernpartei_bp",
             "partei_fuer_franken_die_franken") %in% names(u)))
by_rep <- c(`1994` = 3.9, `1998` = 3.6, `2003` = 2.2, `2008` = 1.4, `2013` = 1.0)
for (yr in names(by_rep))
  check(sprintf("BY %s: REP share %.1f %%", yr, by_rep[[yr]]),
        near(share(u[state == "09" & election_year == as.integer(yr)], "rep"), by_rep[[yr]]))
check("BY 2008: NPD 1.2 %", near(share(u[state == "09" & election_year == 2008], "npd"), 1.2))

# 2. Schleswig-Holstein 2000-2009 and Bremen 1999-2007: every list read, so
#    nothing is left in `other`; SSW and NPD/DVU at their official shares.
sh_npd <- c(`2000` = 1.0, `2005` = 1.9, `2009` = 0.9)
sh_ssw <- c(`2000` = 4.1, `2005` = 3.6, `2009` = 4.3)
for (yr in names(sh_npd)) {
  d <- u[state == "01" & election_year == as.integer(yr)]
  check(sprintf("SH %s: NPD %.1f %%, SSW %.1f %%, other 0", yr, sh_npd[[yr]], sh_ssw[[yr]]),
        near(share(d, "npd"), sh_npd[[yr]]) && near(share(d, "ssw"), sh_ssw[[yr]]) &&
          all(d$other == 0, na.rm = TRUE))
}
hb_dvu <- c(`1999` = 3.0, `2003` = 2.2, `2007` = 2.7)
for (yr in names(hb_dvu)) {
  d <- u[state == "04" & election_year == as.integer(yr)]
  check(sprintf("HB %s: DVU %.1f %%, other 0", yr, hb_dvu[[yr]]),
        near(share(d, "dvu"), hb_dvu[[yr]], 0.1) && all(d$other == 0, na.rm = TRUE))
}

# 3. far_right in the harmonized files: NA where the source names no far-right
#    party but books votes as `other`; 0 only where the source lists every party.
fr_na_expected <- c("03 1990", "03 1994", "04 2011", "09 1990")
fl_na_expected <- c("01 1996", "03 1990", "03 1994", "04 2011", "04 2015", "04 2019",
                    "05 1990", "05 1995", "05 2000", "05 2005", "09 1990", "10 2022",
                    "12 1994", "12 1999")
for (nm in names(harm)) {
  h <- harm[[nm]]
  all_na <- h[, .(all_na = all(is.na(far_right))), by = .(state, election_year)][all_na == TRUE]
  check(sprintf("%s: far_right NA exactly in %s", nm, paste(fr_na_expected, collapse = ", ")),
        setequal(paste(all_na$state, all_na$election_year), fr_na_expected))
  fl_na <- h[, .(all_na = all(is.na(far_left)) & all(is.na(far_left_w_linke))),
             by = .(state, election_year)][all_na == TRUE]
  check(sprintf("%s: far_left (and far_left_w_linke) NA exactly in the %d state-years without a named far-left party but with votes in other",
                nm, length(fl_na_expected)),
        setequal(paste(fl_na$state, fl_na$election_year), fl_na_expected))
  pos <- h[(state == "09" & election_year %in% c(1994, 1998, 2003, 2008, 2013)) |
             (state == "01" & election_year %in% c(2000, 2005, 2009)) |
             (state == "04" & election_year %in% c(1999, 2003, 2007)),
           .(m = mean(far_right, na.rm = TRUE)), by = .(state, election_year)]
  check(sprintf("%s: far_right > 0 in BY 1994-2013, SH 2000-2009, HB 1999-2007", nm),
        nrow(pos) == 11L && all(pos$m > 0))
  check(sprintf("%s: ST 2002 far_right = FDVP (0.84 %%)", nm),
        near(share(h[state == "15" & election_year == 2002], "far_right"), 0.84, 0.02))
  # Classification of 2026-10-19: pro NRW / pro Deutschland and the DLVH count,
  # the DSU does not.
  check(sprintf("%s: far_right counts pro NRW (NRW 2010 2.40 %%) and pro Deutschland (Berlin 2011 3.36 %%)", nm),
        near(share(h[state == "05" & election_year == 2010], "far_right"), 2.40, 0.02) &&
          near(share(h[state == "11" & election_year == 2011], "far_right"), 3.36, 0.02))
  check(sprintf("%s: far_right counts the DLVH (BW 1992 12.27 %%), not the DSU (Sachsen 1990 0.67 %%)", nm),
        near(share(h[state == "08" & election_year == 1992], "far_right"), 12.27, 0.02) &&
          near(share(h[state == "14" & election_year == 1990], "far_right"), 0.67, 0.02))
}

# 4. Mecklenburg-Vorpommern: every pooled postal row distributed and flagged,
#    integer counts, voters = valid + invalid, statewide totals as published.
mv <- u[state == "13" & election_year >= 1994]
mv_tot <- mv[, .(valid = sum(valid_votes), voters = sum(number_voters),
                 flag = sum(flag_postal_allocated)), keyby = election_year]
mv_valid <- c(`1994` = 977867, `1998` = 1084611, `2002` = 970031, `2006` = 818061,
              `2011` = 681375, `2016` = 806419, `2021` = 913863)
check("MV 1994-2021: statewide valid votes as published (pools conserved)",
      identical(setNames(mv_tot$valid, mv_tot$election_year), mv_valid[as.character(mv_tot$election_year)]))
check("MV 1994-2021: flag_postal_allocated set in every election",
      all(mv_tot$flag > 600))
check("MV 1994-2021: integer counts", all(mv$valid_votes == round(mv$valid_votes)))
check("MV 1994-2021: number_voters = valid_votes + invalid_votes",
      all(mv$number_voters == mv$valid_votes + mv$invalid_votes))
check("MV: no pooled postal row left as a municipality",
      !any(u$state == "13" & u$eligible_voters %in% 0 & u$valid_votes > 0))
# 2016/2021 against the source's own statewide row (independent of the parse).
mv_file <- "data/state_elections/raw/Landtagswahlen/Mecklenburg-Vorpommern/Mecklenburg-Vorpommern_2021_2016_Landtagswahl.xlsx"
if (file.exists(mv_file)) {
  raw <- readxl::read_xlsx(mv_file, sheet = "3.12 Zweitst. n. Gemeinden",
                           col_names = FALSE, col_types = "text", skip = 10)
  # The name is printed on the first of a unit's rows only (2021 Anzahl).
  nm <- trimws(raw[[1]])
  for (i in seq_along(nm)[-1]) if (is.na(nm[i])) nm[i] <- nm[i - 1]
  land <- raw[nm %in% "Mecklenburg-Vorpommern" & raw[[3]] %in% "Anzahl", ]
  for (yr in c(2016L, 2021L)) {
    row <- land[land[[2]] == as.character(yr), ]
    check(sprintf("MV %d: voters and valid votes equal the source's Land row", yr),
          nrow(row) == 1L &&
            as.numeric(row[[5]]) == mv_tot[election_year == yr, voters] &&
            as.numeric(row[[7]]) == mv_tot[election_year == yr, valid])
  }
}

# 5. Sachsen-Anhalt 2011/2016: Brücken-Hackpfüffel keeps its postal district;
#    named pools reach only the Gemeinden they name.
bh <- u[ags == "15087101" & election_year == 2011]
check("ST 2011: Brücken-Hackpfüffel 480 voters, 468 valid (postal district 999)",
      nrow(bh) == 1L && bh$number_voters == 480 && bh$valid_votes == 468)
qlb <- u[ags == "15085235" & election_year %in% c(2011, 2016)]
check("ST 2011/2016: Quedlinburg gets no share of the Ballenstedt/Huy pools",
      nrow(qlb) == 2L && all(qlb$flag_postal_allocated == 0) &&
        identical(sort(qlb$number_voters), c(11813, 12272)))
st_voters <- c(`2011` = 1017502, `2016` = 1147498)   # sum of every row of the source
st_tot <- u[state == "15" & election_year %in% c(2011, 2016),
            .(voters = sum(number_voters)), keyby = election_year]
check("ST 2011/2016: statewide voters conserved",
      identical(setNames(st_tot$voters, st_tot$election_year), st_voters))

# 6. Audit round (2026-10-19). Wherever pooled votes were allocated by
#    allocate_pooled_counts(), a row's party votes add up to its valid votes and
#    its voters to valid + invalid (rounding each column separately broke both).
alloc <- u[flag_postal_allocated == 1 & (state %in% c("03", "13", "14", "15") |
             (state == "12" & election_year %in% c(2009, 2019)))]
meta <- c("election_year", "eligible_voters", "number_voters", "valid_votes",
          "invalid_votes", "turnout", "cdu_csu")
pcols <- setdiff(names(u)[vapply(u, is.numeric, logical(1))],
                 c(meta, grep("^flag", names(u), value = TRUE)))
psum <- rowSums(as.matrix(alloc[, ..pcols]), na.rm = TRUE)
check(sprintf("allocated rows (%d): party shares add up to 1", nrow(alloc)),
      all(abs(psum - 1) < 1e-6))
check("allocated rows: number_voters = valid_votes + invalid_votes",
      all(abs(alloc$number_voters - alloc$valid_votes - alloc$invalid_votes) < 1e-6))   # SN 2019/2024 are fractional
voters <- function(a, y) u[ags == a & election_year == y, number_voters]
# Sachsen-Anhalt: members weigh with their electorate in the pool's Wahlkreis.
check("ST 2011: Thale 8,656 voters (pool serves only OT Allrode), Jessen 6,707",
      identical(voters("15085330", 2011), 8656) && identical(voters("15091145", 2011), 6707))
check("ST 2016: Petersberg 5,730 / Landsberg 8,331 (OT Brachstedt in the Landsberg pools)",
      identical(voters("15088295", 2016), 5730) && identical(voters("15088195", 2016), 8331))
# Mecklenburg-Vorpommern 1994/1998: pools by Wahlkreis, across the Kreis line.
check("MV 1994/1998: Stralsund 38,400 / 38,582 voters (its WK-25 part votes in Nordvorpommern's pool)",
      identical(voters("13005000", 1994), 38400) && identical(voters("13005000", 1998), 38582))
check("MV 2002: Neukalen counts its own postal votes only (1,316 voters, not flagged)",
      identical(voters("13052055", 2002), 1316) &&
        u[ags == "13052055" & election_year == 2002, flag_postal_allocated] == 0)
check("MV 2011: 760 receivers (the source's Amt code, merged Gemeinden included)",
      u[state == "13" & election_year == 2011, sum(flag_postal_allocated)] == 760)
# Bremen: Wahlbereich total rows, which add up to the source's Land row.
hb_land <- u[state == "04" & election_year %in% c(1999, 2003, 2007),
             .(valid = sum(valid_votes), voters = sum(number_voters),
               cdu = sum(cdu * valid_votes) / sum(valid_votes)), keyby = election_year]
check("HB 1999/2003/2007: Land valid votes 291,091 / 291,766 / 276,022 as in the source",
      identical(hb_land$valid, c(291091, 291766, 276022)) &&
        identical(hb_land$voters, c(293794, 295391, 279855)))
check("HB 2003: CDU 29.76 % (Stadtteile no longer counted twice)",
      near(100 * hb_land[election_year == 2003, cdu], 29.76, 0.01))

# 7. Hanno's suggestions (2026-10-19): every allocation flagged and documented in
#    the metadata tables, receivers capped at their electorate.
meta_dir <- file.path(dirname(dirname(normalizePath(dir))), "state_elections", "metadata")
if (!dir.exists(meta_dir)) meta_dir <- "data/state_elections/metadata"
rf <- file.path(meta_dir, "state_postal_receivers.csv")
pf <- file.path(meta_dir, "state_postal_pools.csv")
lf <- file.path(meta_dir, "postal_allocation_rules.csv")
cf <- file.path(meta_dir, "state_party_columns.csv")
have <- all(file.exists(c(rf, pf, lf, cf)))
check("metadata: postal rules, pools, receivers and party-columns tables exist", have)
if (have) {
  rec <- fread(rf, colClasses = c(state = "character", ags = "character"))
  pls <- fread(pf, colClasses = c(state = "character"))
  rul <- fread(lf, colClasses = c(state = "character"))
  pcl <- fread(cf, colClasses = c(state = "character"))
  fl <- u[flag_postal_allocated == 1, .(state, election_year, ags)]
  check(sprintf("receivers table = the %d flagged rows", nrow(fl)),
        nrow(rec) == nrow(fl) && nrow(fsetdiff(fl, rec[, .(state, election_year, ags)])) == 0)
  per_pool <- rec[, .(ags = unlist(strsplit(postal_pool_ids, ";"))), by = .(state, election_year, a = ags)]
  check("every pool's receivers appear in the receivers table",
        all(pls$pool_id %in% per_pool$ags) && sum(pls$n_receivers) == nrow(per_pool))
  check("receivers' allocated voters add up to the pools' voters (state-year totals)",
        all(abs(merge(rec[, .(r = sum(postal_voters_allocated)), by = .(state, election_year)],
                      pls[, .(p = sum(pool_voters)), by = .(state, election_year)])[, r - p]) < 1e-6))
  check("SH 2017/2022 and BB 2004/2014/2024 allocations are flagged",
        all(u[(state == "01" & election_year %in% c(2017, 2022)) |
                (state == "12" & election_year %in% c(2004, 2014, 2024)),
              sum(flag_postal_allocated), by = .(state, election_year)]$V1 > 0))
  cap_rows <- merge(u[flag_postal_allocated == 1 & !state %in% "16" &
                        !(state == "14" & election_year %in% c(2019, 2024)),
                      .(state, election_year, ags, number_voters, eligible_voters)],
                    rec, by = c("state", "election_year", "ags"))
  check("no allocation pushes a receiver above its electorate (beyond a 2-vote rounding margin)",
        nrow(cap_rows[number_voters > eligible_voters + 2 &
                        number_voters - postal_voters_allocated <= eligible_voters]) == 0)
  check("rules table: one row per state-election, 4 in-person-only elections",
        nrow(rul) == nrow(unique(u[, .(state, election_year)])) &&
          sum(rul$postal_votes == "not in the data (in-person votes only)") == 4)
  check("party-columns table: ST 2002 names FDVP and Schill, not DVU; BY 1994 names REP",
        all(c("fdvp", "schill") %in% pcl[state == "15" & election_year == 2002, column]) &&
          !"dvu" %in% pcl[state == "15" & election_year == 2002, column] &&
          "rep" %in% pcl[state == "09" & election_year == 1994, column])
}
sh_file <- "data/state_elections/raw/Landtagswahlen/Schleswig-Holstein/Schleswig-Holstein_%d_Landtag.xlsx"
for (yr in c(2017L, 2022L)) {
  f <- sprintf(sh_file, yr)
  if (!file.exists(f)) next
  raw <- readxl::read_excel(f, sheet = 1, col_names = FALSE, .name_repair = "minimal")
  code_row <- vapply(seq_along(raw), function(j) as.character(raw[[j]][3]), character(1))
  b <- suppressWarnings(as.numeric(as.character(raw[[which(code_row == "B")[1]]][-(1:3)])))
  kz <- as.character(raw[[1]][-(1:3)])
  src <- sum(b[!is.na(kz) & grepl("^[0-9]{7,8}$", kz)], na.rm = TRUE)
  check(sprintf("SH %d: statewide voters equal the source's Wahlbezirk rows (%s)", yr, format(src, big.mark = ",")),
        u[state == "01" & election_year == yr, sum(number_voters)] == src)
}

if (length(failed)) {
  cat(length(failed), "check(s) failed.\n")
  quit(status = 1)
}
cat("All far-right and postal-allocation checks passed.\n")
