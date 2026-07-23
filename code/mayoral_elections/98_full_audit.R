### Extensive mayoral-pipeline audit — all states, all datasets.
#
# Complements 99_audit.R (which guards specific known regressions). This script
# runs broad integrity / arithmetic / cross-dataset / coverage checks across
# mayoral_unharm, mayoral_candidates, mayoral_harm, mayor_panel and the landrat
# split, applying documented per-state exceptions. Run:
#     Rscript code/mayoral_elections/98_full_audit.R
# Exit status is non-zero if any ERROR-level finding is detected.

suppressMessages(pacman::p_load(tidyverse, data.table))
options(width = 200)
setwd(here::here())
F <- "data/mayoral_elections/final/"
sn <- c("01"="SH","03"="NI","05"="NRW","06"="HE","07"="RLP","08"="BW","09"="BY",
        "10"="SL","12"="BB","13"="MV","14"="SN","15"="ST","16"="TH")
mtypes <- c("Bürgermeisterwahl","Oberbürgermeisterwahl","VG-Bürgermeisterwahl","SG-Bürgermeisterwahl")
NERR <- 0; NWARN <- 0
rep <- function(sev, check, msg, det = NULL) {
  if (sev == "ERROR") NERR <<- NERR + 1 else if (sev == "WARN") NWARN <<- NWARN + 1
  cat(sprintf("[%s] %-34s %s\n", sev, check, msg))
  if (!is.null(det) && nrow(det) > 0) print(head(as.data.frame(det), 8))
}
ok <- function(check, msg) cat(sprintf("[ OK ] %-34s %s\n", check, msg))

mu <- as.data.table(readRDS(paste0(F, "mayoral_unharm.rds")))
mc <- as.data.table(readRDS(paste0(F, "mayoral_candidates.rds")))
mh <- as.data.table(readRDS(paste0(F, "mayoral_harm.rds")))
mp <- as.data.table(readRDS(paste0(F, "mayor_panel.rds")))
lu <- as.data.table(readRDS("data/landrat_elections/final/landrat_unharm.rds"))
for (d in list(mu, mc, mh, mp, lu)) d[, ags := as.character(ags)]

cat("\n========== A. mayoral_unharm (winner-level) ==========\n")
cat(sprintf("rows=%d  states=%s\n", nrow(mu), paste(sort(unique(substr(mu$ags,1,2))), collapse=",")))
if (!all(nchar(mu$ags) == 8)) rep("ERROR","unharm.ags_len","AGS not all 8 chars", mu[nchar(ags)!=8]) else ok("unharm.ags_len","all AGS 8 chars")
if (!all(substr(mu$ags,1,2) == mu$state)) rep("ERROR","unharm.ags_prefix","AGS prefix != state", mu[substr(ags,1,2)!=state,.(ags,state)]) else ok("unharm.ags_prefix","AGS prefix matches state")
if (any(is.na(mu$ags)|is.na(mu$election_date)|is.na(mu$round))) rep("ERROR","unharm.na_keys","NA in ags/date/round") else ok("unharm.na_keys","no NA keys")
if (nrow(mu[!election_type %in% mtypes])) rep("ERROR","unharm.type","non-mayoral type present", mu[!election_type %in% mtypes,.N,election_type]) else ok("unharm.type","only mayoral types (Landrat split out)")
if (nrow(mu[!round %in% c("hauptwahl","stichwahl")])) rep("ERROR","unharm.round","bad round") else ok("unharm.round","round in {hauptwahl,stichwahl}")
if (nrow(mu[!is.na(winner_votes)&!is.na(valid_votes)&winner_votes>valid_votes])) rep("ERROR","unharm.winner_le_valid","winner_votes>valid", mu[winner_votes>valid_votes,.(ags,election_date,winner_votes,valid_votes)]) else ok("unharm.winner_le_valid","winner_votes<=valid_votes")
if (nrow(mu[!is.na(winner_voteshare)&(winner_voteshare< -0.001|winner_voteshare>1.01)])) rep("ERROR","unharm.share_range","winner_voteshare out of [0,1.01]") else ok("unharm.share_range","winner_voteshare in [0,1.01]")
to <- mu[!is.na(turnout)&(turnout<=0|turnout>1.05)]; if (nrow(to)) rep("WARN","unharm.turnout","turnout out of (0,1.05]", to[,.(ags,election_date,state,turnout)]) else ok("unharm.turnout","turnout in (0,1.05]")
dup <- mu[,.N,.(ags,election_date,round)][N>1]; if (nrow(dup)) rep("ERROR","unharm.dup","duplicate (ags,date,round)", dup) else ok("unharm.dup","no duplicate (ags,date,round)")
sc <- mu[!is.na(winner_voteshare)&!is.na(winner_votes)&!is.na(valid_votes)&valid_votes>0][abs(winner_votes/valid_votes-winner_voteshare)>0.02]
if (nrow(sc)) rep("ERROR","unharm.share_calc",sprintf("%d rows winner_voteshare != votes/valid",nrow(sc)), sc[,.(ags,election_date,state,winner_votes,valid_votes,winner_voteshare)]) else ok("unharm.share_calc","winner_voteshare = votes/valid")

cat("\n========== B. mayoral_candidates ==========\n")
cat(sprintf("rows=%d  states=%s\n", nrow(mc), paste(sort(unique(substr(mc$ags,1,2))), collapse=",")))
if (!all(nchar(mc$ags)==8)) rep("ERROR","cand.ags_len","AGS not 8 chars") else ok("cand.ags_len","all AGS 8 chars")
# exactly one winner per election (ags, date, election_type)
w <- mc[,.(nw=sum(is_winner,na.rm=TRUE)),.(ags,election_date,election_type)]
if (nrow(w[nw>1])) rep("ERROR","cand.multi_winner",sprintf("%d elections with >1 winner",nrow(w[nw>1])), w[nw>1][order(-nw)]) else ok("cand.one_winner","<=1 winner per election")
if (nrow(w[nw==0])) rep("WARN","cand.zero_winner",sprintf("%d elections with 0 winner",nrow(w[nw==0])), w[nw==0]) else ok("cand.zero_winner","every election has a winner")
# per-candidate share consistency
mc[, dev_sh := ifelse(!is.na(candidate_votes_hw)&!is.na(valid_votes)&valid_votes>0, abs(candidate_votes_hw/valid_votes - candidate_voteshare_hw), NA_real_)]
bsh <- mc[!is.na(dev_sh)&dev_sh>0.02]; if (nrow(bsh)) rep("ERROR","cand.share_calc",sprintf("%d candidate rows share!=votes/valid",nrow(bsh)), bsh[order(-dev_sh)][,.(ags,election_date,candidate_party,candidate_votes_hw,valid_votes,candidate_voteshare_hw)]) else ok("cand.share_calc","candidate share = votes/valid")
# sum(HW votes) vs valid. Expected undercounts (NOT flagged):
#   - single-candidate elections (nc==1): a lone candidate is a Ja/Nein vote
#     (gültige = Ja + Nein) or, for BW, the winner-only record — so the candidate's
#     votes are legitimately below gültige Stimmen.
#   - BY (scattered votes), NI (only top candidates listed), BW (winner-only).
# We therefore flag only MULTI-candidate (nc>1) undersums in the remaining states,
# which would indicate genuinely missing candidates / an extraction gap.
agg <- mc[!is.na(candidate_votes_hw),.(s=sum(candidate_votes_hw),v=first(valid_votes),nc=.N),.(ags,election_date)][!is.na(v)&v>0]
agg[, bad := abs(s-v) > pmax(5, v*0.02)][, st := sn[substr(ags,1,2)]]
badsum <- agg[bad & nc > 1 & !st %in% c("BY","NI","BW")]
if (nrow(badsum)) rep("WARN","cand.sum_hw",sprintf("%d multi-candidate elections sum(HW)!=valid (excl. BY/NI/BW)",nrow(badsum)), badsum[order(-abs(s-v))][,.(ags,st,nc,s,v)]) else ok("cand.sum_hw","multi-candidate sum(HW)=valid (single-cand Ja/Nein & BY/NI/BW expected)")

cat("\n========== C. unharm winner == candidates winner (cross-dataset) ==========\n")
hw <- mc[!is.na(candidate_votes_hw),.(cmax=max(candidate_votes_hw)),.(ags,election_date)][,round:="hauptwahl"]
sw <- mc[has_stichwahl==TRUE & !is.na(candidate_votes_sw),.(cmax=max(candidate_votes_sw)),.(ags,election_date=election_date_sw)][,round:="stichwahl"]
mwin <- merge(mu[!is.na(winner_votes),.(ags,election_date,round,st=sn[substr(ags,1,2)],winner_votes)], rbind(hw,sw), by=c("ags","election_date","round"))
# NI (top-candidates-only) & SL legitimately differ; everything else must match
crit <- mwin[abs(winner_votes-cmax)>2 & !st %in% c("NI","SL")]
if (nrow(crit)) rep("ERROR","recon.winner_votes",sprintf("%d unharm winner_votes != candidates max",nrow(crit)), crit[,.N,st]) else ok("recon.winner_votes","unharm winner_votes = candidates max (excl. NI/SL top-only)")

cat("\n========== D. mayoral_harm ==========\n")
cat(sprintf("rows=%d states=%s\n", nrow(mh), paste(sort(unique(substr(mh$ags,1,2))), collapse=",")))
if (!all(nchar(mh$ags)==8)) rep("ERROR","harm.ags_len","harm AGS not 8 chars") else ok("harm.ags_len","harm AGS all 8 chars")
if (nrow(mh[!is.na(winner_voteshare)&(winner_voteshare>1.01|winner_voteshare<0)])) rep("WARN","harm.share_range","harm share out of range") else ok("harm.share_range","harm share in range")

cat("\n========== E. mayor_panel ==========\n")
cat(sprintf("rows=%d states=%s persons=%d\n", nrow(mp), paste(sort(unique(substr(mp$ags,1,2))),collapse=","), uniqueN(mp$person_id)))
pms <- mp[,.(nst=uniqueN(substr(ags,1,2))),person_id][nst>1]
if (nrow(pms)) rep("ERROR","panel.person_multistate",sprintf("%d person_ids span >1 state",nrow(pms)), pms) else ok("panel.person_multistate","no person spans multiple states")

cat("\n========== G. mayoral <-> landrat split ==========\n")
if (nrow(lu[election_type!="Landratswahl"])) rep("ERROR","split.landrat_type","non-Landrat in landrat_unharm") else ok("split.landrat_type","landrat_unharm all Landratswahl")
if (nrow(mu[election_type=="Landratswahl"])) rep("ERROR","split.mayoral_landrat","Landratswahl in mayoral_unharm") else ok("split.mayoral_landrat","no Landratswahl in mayoral")

cat("\n========== H. flag_superseded (Bayern annulled / superseded rounds) ==========\n")
# flag_superseded marks Bayern rounds that did NOT seat the mayor: annulled
# ("... ungültig") rounds + a Hauptwahl superseded by a later Hauptwahl (Neuwahl)
# for the same office within 120 days. It must exist, be a non-NA logical, be
# TRUE only for Bayern, and never appear in the Landrat split.
for (nm in c("mayoral_unharm","mayoral_candidates")) {
  d <- if (nm == "mayoral_unharm") mu else mc
  if (!"flag_superseded" %in% names(d)) { rep("ERROR","flag.present",paste(nm,"missing flag_superseded")); next }
  if (!is.logical(d$flag_superseded) || any(is.na(d$flag_superseded))) rep("ERROR","flag.type",paste(nm,"flag_superseded not non-NA logical")) else ok("flag.type",paste(nm,"flag_superseded is non-NA logical"))
  nbad <- nrow(d[flag_superseded == TRUE & substr(ags,1,2) != "09"])
  if (nbad) rep("ERROR","flag.bayern_only",sprintf("%s: %d non-Bayern rows flagged",nm,nbad), d[flag_superseded==TRUE & substr(ags,1,2)!="09",.N,.(st=substr(ags,1,2))]) else ok("flag.bayern_only",paste(nm,"flagged rows all Bayern"))
  nflag <- nrow(d[flag_superseded == TRUE])
  if (nflag == 0) rep("ERROR","flag.nonzero",paste(nm,"has 0 flagged rows (expected >0)")) else ok("flag.nonzero",sprintf("%s: %d rows flagged superseded",nm,nflag))
}
if ("flag_superseded" %in% names(lu)) rep("ERROR","flag.landrat_absent","flag_superseded leaked into landrat_unharm") else ok("flag.landrat_absent","flag_superseded absent from landrat_unharm (scoped to mayoral)")
# flag_superseded is an election-level attribute: it must be constant within every
# (ags, election_date, election_type) group in the candidate-level data (guards the
# wide-pivot SW-only/orphaned branch from carrying the Stichwahl row's flag).
mixflag <- mc[, .(u = uniqueN(flag_superseded)), .(ags, election_date, election_type)][u > 1]
if (nrow(mixflag)) rep("ERROR","flag.consistent",sprintf("%d elections have inconsistent flag_superseded",nrow(mixflag)), mixflag) else ok("flag.consistent","flag_superseded constant within each election")
# Fixtures. (a) 5 superseded failed/annulled BY rounds must be TRUE. (b) the
# single-ballot Memmingen 1946 OB (indirect council vote) must be FALSE. (c) two
# high-majority Hauptwahlen followed by a within-months by-election must be FALSE —
# they SEATED a mayor and must NOT be flagged (guards the majority test against a
# regression to a pure date-gap rule, which wrongly flagged ~60 such rows).
flagged_true <- mu[paste(ags,election_date) %in%
  c("09274118 2008-03-02","09777118 2008-03-02","09776118 1978-03-05",
    "09374163 1990-03-18","09577133 1993-01-17"), all(flag_superseded)]
memmingen_false <- mu[ags=="09764000" & election_date=="1946-01-27", all(!flag_superseded)]
majority_false <- mu[paste(ags,election_date) %in%
  c("09571181 1972-06-11","09574117 1972-06-11"), all(!flag_superseded)]
if (isTRUE(flagged_true) && isTRUE(memmingen_false) && isTRUE(majority_false)) ok("flag.fixtures","5 superseded TRUE; Memmingen 1946 + 2 majority by-election predecessors FALSE") else rep("ERROR","flag.fixtures",sprintf("fixture mismatch (superseded=%s, memmingen_false=%s, majority_false=%s)",flagged_true,memmingen_false,majority_false))

cat("\n========== I. Sachsen-Anhalt StaLA integration ==========\n")
# The May-2025 Sachsen-Anhalt StaLA bmbm.csv is the primary source (2019-2026,
# 218 Gemeinden). Portal supplements 5 post-cutoff 2026 elections + Genthin
# its 8th candidate. These checks pin down the integration end-to-end.
st_mu <- mu[substr(ags,1,2) == "15"]
st_mc <- mc[substr(ags,1,2) == "15"]

# Coverage: at least 200 unique AGS (the StaLA universe of 218 is upper bound).
if (uniqueN(st_mu$ags) < 200) rep("ERROR","st.coverage",
  sprintf("only %d ST Gemeinden in mayoral_unharm (expected ~223)",uniqueN(st_mu$ags))) else
  ok("st.coverage", sprintf("%d ST Gemeinden in mayoral_unharm",uniqueN(st_mu$ags)))

# 38 distinct AGS appear as OB across the 1994-2026 series: ~24 cities counted
# under BOTH their pre-2007-Kreisreform code and their current one (Halle
# 15202000 + 15002000, Magdeburg 15303000 + 15003000, Dessau 15101000 +
# 15001000, …) plus cities that held an OB election historically but no longer
# do (Wolfen, Burg, Merseburg). 19 was the count in the old 2019-2026 snapshot.
st_ob_ags <- unique(st_mu[election_type == "Oberbürgermeisterwahl"]$ags)
if (length(st_ob_ags) != 38) rep("ERROR","st.ob_count",
  sprintf("%d OB cities (expected 38)",length(st_ob_ags))) else
  ok("st.ob_count","38 ST OB AGS across 1994-2026 (historical + current codes)")

# 3 kreisfreie Städte hard fixture
kfs <- c("15001000","15002000","15003000")
if (!all(kfs %in% st_ob_ags)) rep("ERROR","st.kfs_ob","kfS missing from OB set") else
  ok("st.kfs_ob","all 3 kreisfreie Städte classified as OB")

# One winner per election
st_winners <- st_mc[, .(w = sum(is_winner, na.rm=TRUE)), .(ags, election_date)]
if (any(st_winners$w != 1)) rep("ERROR","st.one_winner",
  sprintf("%d ST elections with ≠ 1 winner",sum(st_winners$w != 1)),
  st_winners[w != 1]) else
  ok("st.one_winner", sprintf("exactly 1 winner per election (%d elections)",nrow(st_winners)))

# Winner votes = candidate max (winner is the top scorer of the decisive round)
st_recon <- merge(
  st_mu[!is.na(winner_votes), .(ags, election_date, round, winner_votes)],
  st_mc[, .(cand_max = max(candidate_votes_hw, candidate_votes_sw, na.rm=TRUE),
            cand_max_sw = suppressWarnings(max(candidate_votes_sw, na.rm=TRUE)),
            cand_max_hw = suppressWarnings(max(candidate_votes_hw, na.rm=TRUE))),
        .(ags, election_date)],
  by = c("ags","election_date"), all.x = TRUE
)
# For SW rounds match cand_max_sw; for HW rounds match cand_max_hw
st_recon[, expected := ifelse(round == "stichwahl", cand_max_sw, cand_max_hw)]
mm <- st_recon[!is.na(expected) & !is.na(winner_votes) & winner_votes != expected]
if (nrow(mm)) rep("ERROR","st.winner_reconcile",
  sprintf("%d ST rows: unharm winner_votes != candidates max",nrow(mm)),
  head(mm[, .(ags, election_date, round, winner_votes, expected)],5)) else
  ok("st.winner_reconcile","ST unharm winner_votes = candidates max per round")

# Named-source integration fixtures (winner surname must match). Fixture dates
# are HW dates — mayoral_candidates is HW-keyed after the 01b wide pivot.
# Fixtures independently verified against Wikipedia / official Landeswahlleiter
# / local newspapers (July 2026 adversarial pass, 20/20 winners).
st_fixtures <- data.table(
  ags = c("15001000","15002000","15003000","15082180","15084355","15085135",
          "15085370","15087370","15089015","15089030","15086040","15083040",
          "15081455","15084550","15091375","15088355","15084250"),
  gemeinde = c("Dessau-Roßlau","Halle","Magdeburg","Köthen","Naumburg","Halberstadt",
               "Wernigerode","Sangerhausen","Aschersleben","Bernburg","Genthin",
               "Barleben","Salzwedel","Weißenfels","Wittenberg","Steigra","Karsdorf"),
  election_date = as.Date(c("2021-06-06","2025-02-02","2022-04-24","2023-03-19",
                            "2021-04-11","2020-07-05","2022-04-03","2024-04-14",
                            "2022-05-08","2021-09-26","2024-11-10","2025-05-25",
                            "2022-11-06","2022-04-24","2022-04-24","2026-06-07",
                            "2026-03-22")),
  winner = c("Reck","Vogt","Borris","Buchheim","Müller","Szarata",
             "Kascha","Schweiger","Amme","Ristow","Turian","Nase",
             "Meining","Papke","Zugehör","Stockhaus","Schumann")
)
w_check <- st_mc[
  st_fixtures[, .(ags, election_date)],
  on = c("ags","election_date")][
    is_winner == TRUE, .(ags, election_date, got = candidate_last_name)]
w_check <- merge(st_fixtures, w_check, by = c("ags","election_date"), all.x = TRUE)
bad_fx <- w_check[is.na(got) | got != winner]
if (nrow(bad_fx)) rep("ERROR","st.fixtures",
  sprintf("%d ST fixture winner mismatches",nrow(bad_fx)), bad_fx) else
  ok("st.fixtures", sprintf("%d ST named-source fixtures verified (Reck/Vogt/Borris/…)", nrow(st_fixtures)))

# Genthin regression: 8 candidates, the 96-vote record present, Turian wins
gen <- st_mc[ags == "15086040" & election_date == as.Date("2024-11-10")]
# The recovered 8th candidate lost, so their name is stripped under the StaLA
# licence; pin the row by its vote count (96) instead of by name.
gen_ok <- nrow(gen) == 8 && 96 %in% gen$candidate_votes_hw &&
  gen[is_winner==TRUE, .N] == 1 && gen[is_winner==TRUE]$candidate_last_name[1] == "Turian"
if (!gen_ok) rep("ERROR","st.genthin_regression",
  sprintf("Genthin 2024 regression: %d cands, wöhling=%s, winner=%s",
          nrow(gen), 96 %in% gen$candidate_votes_hw,
          paste(gen[is_winner==TRUE]$candidate_last_name, collapse=","))) else
  ok("st.genthin_regression","Genthin 2024: 8 cands, 96-vote record recovered, Turian wins")

# Election-year window: 1994-2026 (StaLA historical file "BM-Wahl ab 1994")
if (min(st_mu$election_year) != 1994 || max(st_mu$election_year) > 2026)
  rep("ERROR","st.year_window",
      sprintf("ST years %d-%d outside 1994-2026",min(st_mu$election_year),max(st_mu$election_year))) else
  ok("st.year_window", sprintf("ST elections span 1994-%d",max(st_mu$election_year)))

# No Landratswahl leaked (ST Landrat is a separate dataset/pipeline)
if (nrow(st_mu[election_type == "Landratswahl"])) rep("ERROR","st.no_landrat",
  "Landratswahl leaked into ST mayoral") else
  ok("st.no_landrat","no Landratswahl in ST mayoral (Landrat is separate)")

# Hard numeric fixtures: pin winner_votes for 5 landmark OB Stichwahlen
# (Dessau-Roßlau 2021, Halle 2025, Magdeburg 2022, Köthen 2023, Sangerhausen 2024).
# Surname fixtures above guard identity; these guard the vote-count extraction path.
st_vote_fx <- data.table(
  ags           = c("15001000","15002000","15003000","15082180","15087370"),
  election_date = as.Date(c("2021-06-27","2025-02-23","2022-05-08","2023-04-02","2024-04-28")),
  gemeinde      = c("Dessau-Roßlau","Halle","Magdeburg","Köthen","Sangerhausen"),
  winner        = c("Reck","Vogt","Borris","Buchheim","Schweiger"),
  votes         = c(14856L, 60758L, 39210L, 4207L, 5341L))
vx <- merge(st_vote_fx,
            st_mu[round == "stichwahl", .(ags, election_date, got_votes = winner_votes)],
            by = c("ags","election_date"), all.x = TRUE)
bad_vx <- vx[is.na(got_votes) | got_votes != votes]
if (nrow(bad_vx)) rep("ERROR","st.vote_fixtures",
  sprintf("%d ST vote-count fixture mismatches", nrow(bad_vx)), bad_vx) else
  ok("st.vote_fixtures","5 landmark OB Stichwahlen match pinned winner_votes")

# Round pairing: every Stichwahl must follow a Hauptwahl at the same AGS that
# failed to seat a mayor outright. Guards against orphaned SW rows from a
# merge-rule regression.
#   * Pair each Stichwahl with the NEAREST PRECEDING Hauptwahl, not with any
#     same-year one — over a 1994-2026 series a Gemeinde can hold several
#     elections in one year (and the source gives some 2008 Gemeinden a shared
#     AGS), so same-year matching pairs unrelated rounds.
#   * A runoff is required when no one exceeds 50%, so hw_share == 0.5 is a
#     legitimate trigger; only hw_share > 0.5 is an error.
#   * hw_share is NA for many 1994 rounds (winner-only records, no vote counts)
#     and cannot be tested.
sw <- st_mu[round == "stichwahl", .(ags, sw_date = election_date)]
hw <- st_mu[round == "hauptwahl", .(ags, hw_date = election_date,
                                    hw_share = winner_voteshare)]
cand <- merge(sw, hw, by = "ags", allow.cartesian = TRUE)[hw_date <= sw_date]
pair <- cand[order(ags, sw_date, -as.numeric(hw_date))][, .SD[1], by = .(ags, sw_date)]
no_hw <- sw[!pair, on = c("ags","sw_date")]
orph  <- rbind(no_hw[, .(ags, sw_date, hw_date = as.Date(NA), hw_share = NA_real_)],
               pair[!is.na(hw_share) & hw_share > 0.5,
                    .(ags, sw_date, hw_date, hw_share)])
if (nrow(orph)) rep("ERROR","st.round_pair",
  sprintf("%d Stichwahl rounds with no preceding HW or a HW that already won outright",
          nrow(orph)), orph) else
  ok("st.round_pair", sprintf("%d Stichwahl rounds all follow a HW that failed to seat a mayor", nrow(sw)))

# Candidate voteshare completeness: for multi-candidate elections the sum of
# candidate_voteshare_hw should be ≈1.0. Single-candidate Ja/Nein rounds have
# a valid partial share (Ja/gültige) and are excluded.
sh <- st_mc[!is.na(candidate_voteshare_hw),
            .(sh_sum = sum(candidate_voteshare_hw), nc = .N),
            .(ags, election_date)][nc > 1]
bad_sh <- sh[sh_sum < 0.995 | sh_sum > 1.005]
# Three documented exceptions, all source defects rather than pipeline faults:
#  * 15154013 (2001-05-06) and 15159006 (2001-06-10) — the source gives two
#    DIFFERENT Gemeinden the same AGS on the same day (Großzöberitz/Heideloh and
#    Cosa/Diebzig). They are flagged `flag_shared_ags` by 00_st_hist_parse.py;
#    stage 01 reduces each pair to one row, so the shares sum to 2.0.
#  * 15261033 Leuna 1994-06-12 — arithmetic error in the source itself: the
#    candidate votes sum to 4010 against 3910 Gültige Stimmen.
st_share_known <- data.table(
  ags           = c("15154013","15159006","15261033"),
  election_date = as.Date(c("2001-05-06","2001-06-10","1994-06-12")))
bad_sh <- bad_sh[!st_share_known, on = c("ags","election_date")]
if (nrow(bad_sh)) rep("ERROR","st.share_sum",
  sprintf("%d multi-cand ST elections with sum(share_hw) outside [0.995,1.005]",
          nrow(bad_sh)), bad_sh) else
  ok("st.share_sum",
     sprintf("%d multi-candidate ST elections: candidate shares sum to 1", nrow(sh)))

# Portal-supplement provenance: st_stala_parsed.csv (StaLA historical file +
# bmbm) is the primary source. Anything in ST mayoral NOT covered by it would be
# a Landeswahlleiter-portal supplement. Since the historical file (Stand
# 13.07.2026) reaches 2026-06-07 it now covers all five formerly portal-only
# elections (Zerbst, Karsdorf, Stößen, Steigra, Teutschenthal), so the expected
# supplement set is EMPTY. Note Zerbst: StaLA dates it 2026-02-08 while the
# portal page carries the byte-identical result as 2026-04-12 — 01/01b drop that
# portal round via the fingerprint guard. A non-empty set here means either the
# merge regressed or the portal gained a genuinely new election.
portal_expected <- data.table(
  ags = character(0), election_date = as.Date(character(0)))
stla_raw <- suppressWarnings(fread(
  "data/mayoral_elections/raw/sachsen_anhalt/st_stala_parsed.csv",
  colClasses = list(character = "ags")))
stla_keys <- unique(stla_raw[, .(ags, election_date = as.character(as.Date(election_date)))])
st_keys   <- unique(st_mu[, .(ags, election_date = as.character(election_date))])
portal_only <- st_keys[!stla_keys, on = c("ags","election_date")]
exp_keys <- paste(portal_expected$ags, portal_expected$election_date)
got_keys <- paste(portal_only$ags, portal_only$election_date)
if (!setequal(exp_keys, got_keys)) rep("ERROR","st.portal_prov",
  sprintf("portal-supplement set drifted: got %d elections, expected %d (%s)",
          length(got_keys), length(exp_keys),
          paste(setdiff(got_keys, exp_keys), collapse=" ; ")),
  portal_only) else
  ok("st.portal_prov", "no portal supplements needed — StaLA historical file covers all ST elections")

# PARTEI value sanity: no leading/trailing whitespace, no control characters
# (Windows-1252 CRLF artifacts), non-NA. Empty string is valid (=Einzelbewerber).
# Truncated coalition names (containing a comma, length exactly 20) are OK and
# documented in [[st-stala-mayoral-upgrade]].
if (any(is.na(st_mc$candidate_party))) rep("ERROR","st.party_sanity",
  "candidate_party contains NA (should be '' for Einzelbewerber)") else {
  pv <- unique(st_mc$candidate_party)
  whs <- pv[nzchar(pv) & grepl("^\\s|\\s$", pv)]
  ctl <- pv[grepl("[[:cntrl:]]", pv)]
  if (length(whs) || length(ctl)) rep("ERROR","st.party_sanity",
    sprintf("%d whitespace-bounded / %d control-char PARTEI values",
            length(whs), length(ctl)),
    data.table(bad = c(whs, ctl))) else
    ok("st.party_sanity",
       sprintf("%d distinct PARTEI values, all clean (no whitespace/ctrl artifacts)",
               length(pv[nzchar(pv)])))
}

cat("\n========== F. Coverage by state ==========\n")
print(mu[election_type %in% mtypes, .(elec=uniqueN(paste(ags,election_date)), munis=uniqueN(ags),
        yrs=paste0(min(election_year),"-",max(election_year))), .(st=sn[substr(ags,1,2)])][order(st)])

cat(sprintf("\n========== SUMMARY: %d ERROR, %d WARN ==========\n", NERR, NWARN))
if (NERR > 0) quit(status = 1)
