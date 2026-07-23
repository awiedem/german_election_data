#### Stage-1 Wahlkreis dataset — comprehensive internal audit ####
## Deterministic integrity checks over raw -> intermediate -> final.
## Run: Rscript code/state_elections_wahlkreis/99_audit.R
rm(list = ls())
suppressMessages({library(here); library(data.table)})
here::i_am("code/state_elections_wahlkreis/99_audit.R")

INTER <- here("data","state_elections","processed","wahlkreis")
FIN   <- here("data","state_elections","final")
fail <- 0L; warn <- 0L
ok  <- function(m) cat(sprintf("  [PASS] %s\n", m))
bad <- function(m){ fail <<- fail+1L; cat(sprintf("  [FAIL] %s\n", m)) }
wn  <- function(m){ warn <<- warn+1L; cat(sprintf("  [WARN] %s\n", m)) }
sec <- function(m) cat(sprintf("\n=== %s ===\n", m))

sc <- c("01"="SH","02"="HH","03"="NI","04"="HB","05"="NW","06"="HE","07"="RP",
        "08"="BW","09"="BY","10"="SL","11"="BE","12"="BB","13"="MV","14"="SN","15"="ST","16"="TH")
city <- c("02","04")  # HH, HB multi-vote: valid_votes may exceed number_voters

long <- fread(file.path(FIN,"ltw_wkr_unharm_long.csv"), colClasses=list(character=c("wkr_nr","state")))
wide <- fread(file.path(FIN,"ltw_wkr_unharm.csv"),       colClasses=list(character=c("wkr_nr","state")))
meta <- c("flag_no_valid_votes","flag_naive_turnout_above_1","state","election_year","election_date",
          "wkr_nr","wkr_name","stimme","eligible_voters","number_voters","valid_votes","invalid_votes",
          "turnout","other","cdu_csu")
pc <- setdiff(names(wide), meta)

sec("1. SCHEMA & TYPES")
need_long <- c("state","state_abbr","election_year","election_date","wkr_nr","wkr_name","stimme",
               "eligible_voters","number_voters","valid_votes","invalid_votes","turnout","party","votes","vote_share")
if(all(need_long %in% names(long))) ok("long has all expected columns") else bad(paste("long missing:",paste(setdiff(need_long,names(long)),collapse=",")))
if(is.character(long$wkr_nr) && is.character(wide$wkr_nr)) ok("wkr_nr is character (leading zeros preserved)") else bad("wkr_nr not character")
if(!any(is.na(as.Date(wide$election_date)))) ok("all election_date parse as Date") else bad("some election_date unparseable")

sec("2. STATE CODES")
if(all(wide$state %in% names(sc))) ok("all state codes valid (01-16)") else bad("invalid state codes present")
if(uniqueN(wide$state)==16) ok("all 16 states present") else wn(sprintf("only %d states present",uniqueN(wide$state)))

sec("3. DUPLICATES")
dW <- nrow(wide)-nrow(unique(wide[,.(state,election_year,wkr_nr,stimme)]))
if(dW==0) ok("no duplicate (state,year,wkr_nr,stimme) in wide") else bad(sprintf("%d dup keys in wide",dW))
dL <- nrow(long)-nrow(unique(long[,.(state,election_year,wkr_nr,stimme,party)]))
if(dL==0) ok("no duplicate (state,year,wkr_nr,stimme,party) in long") else bad(sprintf("%d dup keys in long",dL))

sec("4. VOTE INTEGRITY (long: Sum party votes == valid_votes)")
ig <- long[!is.na(votes) & !is.na(valid_votes) & valid_votes>0,
           .(s=sum(votes), v=valid_votes[1]), by=.(state,election_year,wkr_nr,stimme)]
ig[, d:=abs(s-v)]
if(nrow(ig[d>1])==0) ok(sprintf("all %d (wkr,stimme) groups reconcile (max |diff|=%g)",nrow(ig),max(ig$d))) else bad(sprintf("%d groups fail integrity (max diff %g)",nrow(ig[d>1]),max(ig$d)))

sec("5. SHARES (wide: Sum party shares == 1)")
ss <- rowSums(as.matrix(wide[,..pc]), na.rm=TRUE)
nbad <- sum(!(ss>0.995 & ss<1.005))
if(nbad==0) ok(sprintf("all %d wide rows sum to ~1 (range %.4f-%.4f)",nrow(wide),min(ss),max(ss))) else bad(sprintf("%d wide rows do not sum to ~1",nbad))

sec("6. SHARE RANGE & vote_share consistency")
if(all(long$vote_share>=-1e-9 & long$vote_share<=1+1e-9, na.rm=TRUE)) ok("all long vote_share in [0,1]") else bad("vote_share out of [0,1]")
vs <- long[!is.na(votes)&!is.na(valid_votes)&valid_votes>0]; vs[,calc:=votes/valid_votes]
if(max(abs(vs$vote_share-vs$calc),na.rm=TRUE)<1e-6) ok("long vote_share == votes/valid_votes") else bad("vote_share != votes/valid_votes")

sec("7. TURNOUT LOGIC")
if(sum(wide$turnout>1, na.rm=TRUE)==0) ok("no turnout > 1") else bad(sprintf("%d rows turnout>1",sum(wide$turnout>1,na.rm=TRUE)))
tt <- wide[!is.na(eligible_voters)&eligible_voters>0]; tt[,calc:=number_voters/eligible_voters]
if(max(abs(tt$turnout-tt$calc),na.rm=TRUE)<1e-6) ok("turnout == number_voters/eligible_voters") else bad("turnout mismatch")
cat(sprintf("    turnout range: %.3f - %.3f ; NA: %d\n", min(wide$turnout,na.rm=TRUE), max(wide$turnout,na.rm=TRUE), sum(is.na(wide$turnout))))

sec("8. VOTER HIERARCHY (number_voters <= eligible_voters)")
vh <- wide[!is.na(eligible_voters)&!is.na(number_voters) & number_voters>eligible_voters+1]
if(nrow(vh)==0) ok("number_voters <= eligible_voters everywhere") else bad(sprintf("%d rows number_voters>eligible",nrow(vh)))
# valid+invalid vs number_voters: meaningful for single-ballot (non-city, einzel/erst).
# NB: in two-vote states a voter may turn out but leave the Erststimme blank (not counted as
# valid OR invalid), so number_voters >= valid+invalid by a small margin (BY/BE, <0.2%). Only a
# LARGE gap (>1% of voters) indicates a real problem.
sb <- wide[!(state %in% city) & stimme!="zweitstimme" & !is.na(valid_votes)&!is.na(invalid_votes)&!is.na(number_voters) & number_voters>0]
sb[, pct:=abs(valid_votes+invalid_votes-number_voters)/number_voters]
fr <- sb[pct>0.01]
if(nrow(fr)==0) ok("valid+invalid ~ number_voters (single-ballot; small blank-Erststimme gaps only)") else { wn(sprintf("%d single-ballot rows where |valid+invalid - number_voters| > 1%% of voters (inspect):",nrow(fr))); print(fr[order(-pct)][1:min(10,nrow(fr)),.(state,election_year,wkr_nr,number_voters,valid_votes,invalid_votes,pct=round(pct,3))]) }

sec("9. NEGATIVE / MISSING")
if(sum(long$votes<0,na.rm=TRUE)==0) ok("no negative votes") else bad("negative votes present")
if(sum(wide$valid_votes<0,na.rm=TRUE)==0 && sum(wide$eligible_voters<0,na.rm=TRUE)==0) ok("no negative counts") else bad("negative counts")
nv <- sum(is.na(wide$valid_votes)|wide$valid_votes==0)
if(nv==0) ok("no NA/zero valid_votes") else wn(sprintf("%d rows NA/zero valid_votes",nv))

sec("10. einzelstimme / erst / zweit CONSISTENCY")
mix <- wide[,.(st=paste(sort(unique(stimme)),collapse=",")), by=.(state,election_year)][grepl("einzelstimme",st)&grepl("stimme,",st)]
if(nrow(mix)==0) ok("no state-year mixes einzelstimme with erst/zweit") else bad(sprintf("%d state-years mix einzelstimme & two-vote",nrow(mix)))
# einzelstimme is correct for single-vote systems: BW (<=2021), SL, and NW (<=2005, pre-2010 reform)
ez_ok <- c("08","10","05")
ezy <- unique(wide[stimme=="einzelstimme",.(state,election_year)])
bad_ez <- ezy[!(state %in% ez_ok) |
              (state=="05" & election_year>2005) | (state=="08" & election_year>2021)]
if(nrow(bad_ez)==0) ok("einzelstimme only in single-vote systems (BW<=2021, SL, NW<=2005)") else { wn("einzelstimme in unexpected state-years:"); print(bad_ez) }
# erststimme count == zweitstimme count per two-vote state-year? (should match unless source gap)
ew <- wide[stimme %in% c("erststimme","zweitstimme"), .N, by=.(state,election_year,stimme)]
ew <- dcast(ew, state+election_year~stimme, value.var="N")
asym <- ew[!is.na(erststimme)&!is.na(zweitstimme)&erststimme!=zweitstimme]
if(nrow(asym)==0) ok("erst & zweit Wahlkreis counts match per two-vote state-year") else { wn(sprintf("%d state-years with erst != zweit WK count (expected for known source gaps):",nrow(asym))); print(asym) }

sec("11. cdu_csu CONSISTENCY")
chk <- copy(wide); chk[, c_:=ifelse(is.na(get("cdu")),0,get("cdu")) + ifelse(is.na(get("csu")),0,get("csu"))]
chk[, c_:=ifelse(c_==0,NA,c_)]
m <- chk[!is.na(cdu_csu)&!is.na(c_) & abs(cdu_csu-c_)>1e-6]
if(nrow(m)==0) ok("cdu_csu == cdu+csu") else bad(sprintf("%d rows cdu_csu != cdu+csu",nrow(m)))
by_cdu <- wide[state=="09" & !is.na(cdu)]
if(nrow(by_cdu)==0) ok("Bayern uses csu (no cdu)") else wn("Bayern has cdu rows (unexpected)")

sec("12. PARTY COLUMN SANITY")
allna <- pc[sapply(pc, function(p) all(is.na(wide[[p]])))]
if(length(allna)==0) ok("no all-NA party columns") else bad(paste("all-NA party cols:",paste(allna,collapse=",")))
# suspicious near-duplicate normalized names (possible residual splits)
susp <- c("grune","gruene","linke","linke_pds","die_linke","cdu","c_d_u","spd","s_p_d","fdp","f_d_p","npd","n_p_d","rep","r_e_p","gal","fw","fwg","freie_wahler","die_republikaner","bue90_gruene")
present <- intersect(susp, pc)
splits <- list(green=intersect(c("grune","gal","bue90_gruene"),pc), linke=intersect(c("linke","die_linke"),pc),
               cdu=intersect(c("c_d_u"),pc), spd=intersect(c("s_p_d"),pc), fdp=intersect(c("f_d_p"),pc),
               rep=intersect(c("r_e_p","die_republikaner"),pc), npd=intersect(c("n_p_d"),pc))
splits <- splits[sapply(splits,length)>0]
if(length(splits)==0) ok("no residual major-party split columns") else { wn("possible residual splits (verify):"); print(splits) }
cat(sprintf("    total party columns: %d\n", length(pc)))

sec("13. COVERAGE (Wahlkreis count per state-year)")
cov <- unique(wide[,.(state,election_year,wkr_nr)])[,.(nwkr=uniqueN(wkr_nr)), by=.(state,election_year)]
cov[, ab:=sc[state]]
print(cov[order(ab,election_year)], nrow=200)

sec("14. ELECTION DATE PLAUSIBILITY")
dd <- wide[,.(nd=uniqueN(election_date), yr=election_year[1], dt=as.Date(election_date[1])), by=.(state,election_year)]
if(all(dd$nd==1)) ok("one distinct election_date per state-year") else bad("multiple dates within a state-year")
mism <- dd[!is.na(dt) & format(dt,"%Y")!=as.character(yr)]
if(nrow(mism)==0) ok("election_date year matches election_year") else bad(sprintf("%d state-years: date year != election_year",nrow(mism)))

sec("15. LONG <-> WIDE CONSISTENCY")
kl <- unique(long[,.(state,election_year,wkr_nr,stimme)]); kw <- unique(wide[,.(state,election_year,wkr_nr,stimme)])
if(nrow(fsetdiff(kl,kw))==0 && nrow(fsetdiff(kw,kl))==0) ok("same (state,year,wkr,stimme) keys in long & wide") else bad("key set differs between long & wide")

sec("16. RAW INTERMEDIATE -> presence")
nfiles <- length(list.files(INTER, pattern="_ltw_wkr_long.csv$"))
if(nfiles==16) ok("16 per-state intermediates present") else wn(sprintf("%d intermediates (expected 16)",nfiles))

cat(sprintf("\n=================  AUDIT SUMMARY: %d FAIL, %d WARN  =================\n", fail, warn))
quit(status = if(fail>0) 1 else 0)
