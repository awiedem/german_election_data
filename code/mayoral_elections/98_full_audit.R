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
sn <- c("01"="SH","03"="NI","05"="NRW","07"="RLP","08"="BW","09"="BY","10"="SL",
        "13"="MV","14"="SN","16"="TH")
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

cat("\n========== F. Coverage by state ==========\n")
print(mu[election_type %in% mtypes, .(elec=uniqueN(paste(ags,election_date)), munis=uniqueN(ags),
        yrs=paste0(min(election_year),"-",max(election_year))), .(st=sn[substr(ags,1,2)])][order(st)])

cat(sprintf("\n========== SUMMARY: %d ERROR, %d WARN ==========\n", NERR, NWARN))
if (NERR > 0) quit(status = 1)
