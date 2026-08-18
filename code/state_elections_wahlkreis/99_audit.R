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
meta <- c("flag_no_valid_votes","flag_naive_turnout_above_1","flag_wkr_boundaries_recomputed",
          "state","election_year","election_date",
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

sec("17. WAHLKREIS NAMES (no placeholders, unique within a state-year)")
# Calibration: before the June/August-2026 fix this fired on 704 Brandenburg rows
# ("Landtagswahlkreis 01" ...) and 360 Mecklenburg-Vorpommern rows (NA, 1994-2011).
ph <- wide[is.na(wkr_name) | !nzchar(trimws(wkr_name)) |
             grepl("^(Landtags)?[Ww]ahlkreis ?[0-9]+$", trimws(wkr_name))]
if(nrow(ph)==0) ok("no NA / placeholder wkr_name") else {
  bad(sprintf("%d rows have a placeholder or missing wkr_name",nrow(ph)))
  print(unique(ph[,.(state,election_year)]))
}
nm <- wide[, .(n_wkr=uniqueN(wkr_nr), n_names=uniqueN(wkr_name)), by=.(state,election_year)]
if(nrow(nm[n_wkr!=n_names])==0) ok("wkr_name is one-to-one with wkr_nr in every state-year") else {
  bad("Wahlkreis names are not unique within some state-year"); print(nm[n_wkr!=n_names])
}
# per-year name tables must still exist and agree with what was published
for(f in c("BB_wkr_names.csv","MV_wkr_names.csv")){
  pth <- file.path(INTER,"wkr_names",f)
  if(!file.exists(pth)){ bad(paste("missing name table",f)); next }
  nt <- fread(pth, colClasses=list(character=c("wkr_nr","wkr_name")))
  st <- if(startsWith(f,"BB")) "12" else "13"
  jn <- merge(wide[state==st, .(election_year, wkr_int=as.integer(wkr_nr), published=wkr_name)],
              nt[, .(election_year=as.integer(election_year), wkr_int=as.integer(wkr_nr), src=wkr_name)],
              by=c("election_year","wkr_int"))
  mism <- jn[published!=src]
  if(nrow(mism)==0) ok(sprintf("%s: every published name matches the extracted table (%d rows)",f,nrow(jn)))
  else { bad(sprintf("%s: %d published names differ from the name table",f,nrow(mism))); print(head(mism,5)) }
}

sec("18. BRANDENBURG / MV NAME FIXTURES (per-year, not inherited)")
bbf <- list(list(1990,"011","Oranienburg I"), list(1994,"011","Havelland I"),
            list(1999,"011","Havelland I"),   list(2004,"011","Uckermark I"),
            list(2024,"011","Uckermark I"),   list(1990,"001","Perleberg I"),
            list(2014,"001","Prignitz I"),    list(1999,"034","Frankfurt (Oder) I"))
for(f in bbf){
  got <- unique(wide[state=="12" & election_year==f[[1]] & wkr_nr==f[[3-1]]]$wkr_name)
  if(identical(got,f[[3]])) ok(sprintf("BB %d WK %s = '%s'",f[[1]],f[[2]],f[[3]]))
  else bad(sprintf("BB %d WK %s = %s (expected '%s')",f[[1]],f[[2]],paste(got,collapse="/"),f[[3]]))
}
# BB renumbered twice: the same WK number must NOT carry the same name throughout
bb11 <- unique(wide[state=="12" & wkr_nr=="011", .(election_year, wkr_name)])
if(uniqueN(bb11$wkr_name)>=3){ ok(sprintf("BB WK 11 changes name across the renumberings (%d distinct)",uniqueN(bb11$wkr_name)))
} else bad("BB WK 11 has too few distinct names - a per-year table may have been copied")
mvf <- list(list(1994,"01","Greifswald"), list(1998,"21","Mecklenburg-Strelitz I/Müritz II"),
            list(2002,"21","Mecklenburg-Strelitz I/Müritz II"), list(2006,"21","Mecklenburg-Strelitz I"),
            list(2011,"36","Uecker-Randow II"))
for(f in mvf){
  mvw <- wide[state=="13" & election_year==f[[1]]]
  got <- unique(mvw[as.integer(wkr_nr)==as.integer(f[[2]])]$wkr_name)
  if(identical(got,f[[3]])) ok(sprintf("MV %d WK %s = '%s'",f[[1]],f[[2]],f[[3]]))
  else bad(sprintf("MV %d WK %s = %s (expected '%s')",f[[1]],f[[2]],paste(got,collapse="/"),f[[3]]))
}

sec("19. HESSEN 2013/2018 (constituency series added August 2026)")
he_years <- sort(unique(wide[state=="06"]$election_year))
if(identical(he_years,c(2013L,2018L,2023L))){ ok("HE covers 2013, 2018, 2023")
} else bad(paste("HE years:",paste(he_years,collapse=",")))
hec <- wide[state=="06", .N, by=.(election_year,stimme)]
if(all(hec$N==55) && nrow(hec)==6){ ok("HE: 55 Wahlkreise x 2 Stimmen in each of the 3 years")
} else { bad("HE Wahlkreis/stimme counts are off"); print(hec) }
# statewide fixtures straight out of Table 1 of B VII 2-4 - 5j/18 (which is also
# the official published result: 2018 Landesstimmen CDU 27.0, GRUENE 19.8,
# SPD 19.8, AfD 13.1, FDP 7.5, LINKE 6.3 per cent).  Summing the 55 Wahlkreise
# must reproduce them exactly; a column mix-up in the PDF parse cannot survive this.
he_fix <- data.table(
  year   = c(rep(2018L,7), rep(2018L,7), rep(2013L,7), rep(2013L,7)),
  stimme = c(rep("erststimme",7), rep("zweitstimme",7), rep("erststimme",7), rep("zweitstimme",7)),
  party  = rep(c("cdu","spd","gruene","linke_pds","fdp","afd","die_partei"),4),
  votes  = c(843068,670637,517904,164535,205384,362210,12007,
             776910,570446,570512,181332,215946,378692,18334,
             1329746,1092125,289830,160531,93098,42721,1786,
             1199633,961896,348661,161488,157451,126906,15109))
hel <- long[state=="06", .(v=sum(votes,na.rm=TRUE)), by=.(election_year,stimme,party)]
he_cmp <- merge(he_fix, hel, by.x=c("year","stimme","party"),
                by.y=c("election_year","stimme","party"), all.x=TRUE)
he_cmp[is.na(v), v := -1]
if(nrow(he_cmp[votes!=v])==0){ ok(sprintf("HE: all %d statewide party fixtures reproduced exactly",nrow(he_cmp)))
} else { bad("HE statewide party totals differ from the official report"); print(he_cmp[votes!=v]) }
he_tot <- long[state=="06" & party=="cdu", .(elig=sum(eligible_voters), vot=sum(number_voters),
                                             val=sum(valid_votes)), by=.(election_year,stimme)]
he_exp <- data.table(election_year=c(2018L,2018L,2013L,2013L),
                     stimme=c("erststimme","zweitstimme","erststimme","zweitstimme"),
                     elig=c(4372788,4372788,4392213,4392213),
                     vot=c(2942846,2942846,3216206,3216206),
                     val=c(2873070,2881261,3112596,3130781))
he_j <- merge(he_tot, he_exp, by=c("election_year","stimme"), suffixes=c("",".exp"))
if(nrow(he_j[elig!=elig.exp | vot!=vot.exp | val!=val.exp])==0){
  ok("HE: Wahlberechtigte / Waehler / gueltige Stimmen match the report statewide")
} else { bad("HE turnout aggregates differ"); print(he_j) }

# Direktmandate: an INDEPENDENT structural check, taken from the report's prose
# ("in 40 der 55 hessischen Wahlkreise ... CDU. Die SPD war in 10 Wahlkreisen
# erfolgreich und die GRUENEN in 5"), not from the tables that were parsed.
hew <- long[state=="06" & stimme=="erststimme" & election_year==2018 & !is.na(votes)]
hew <- hew[hew[, .I[which.max(votes)], by=wkr_nr]$V1][, .N, by=party]
dm <- setNames(hew$N, hew$party)
exp_dm <- c(cdu=40L, spd=10L, gruene=5L)
if(identical(sort(names(dm)), sort(names(exp_dm))) &&
   all(dm[names(exp_dm)] == exp_dm)){
  ok("HE 2018: Direktmandate CDU 40 / SPD 10 / GRUENE 5, as the report's text states")
} else { bad("HE 2018 Direktmandat counts differ from the report"); print(dm) }
# 2013 is on 2018 boundaries, so the direct wins are NOT the 41 CDU / 14 SPD
# actually returned in 2013: exactly one constituency changes hands under the
# re-cut. Pinned so the boundary effect stays visible rather than being "fixed".
hew13 <- long[state=="06" & stimme=="erststimme" & election_year==2013 & !is.na(votes)]
hew13 <- hew13[hew13[, .I[which.max(votes)], by=wkr_nr]$V1][, .N, by=party]
d13 <- setNames(hew13$N, hew13$party)
if(isTRUE(d13[["cdu"]]==42L) && isTRUE(d13[["spd"]]==13L) && sum(d13)==55L){
  ok("HE 2013 on 2018 boundaries: CDU 42 / SPD 13 (2013 actually returned 41/14 - the one-seat difference IS the boundary re-cut)")
} else { bad("HE 2013 Direktmandat counts moved"); print(d13) }

sec("20. flag_wkr_boundaries_recomputed")
fl <- wide$flag_wkr_boundaries_recomputed
if(!is.null(fl) && all(fl %in% c(0L,1L))){ ok("flag present and strictly 0/1")
} else bad("flag_wkr_boundaries_recomputed missing or not 0/1")
fr <- unique(wide[flag_wkr_boundaries_recomputed==1, .(state,election_year)])
if(nrow(fr)==1 && fr$state=="06" && fr$election_year==2013L){
  ok("only Hessen 2013 is back-cast onto a later Wahlkreiseinteilung")
} else { bad("unexpected state-years flagged as recomputed"); print(fr) }
# Frankfurt am Main I / IV keep their own 2013 boundaries in B VII 2-4, so they
# are deliberately NOT flagged; everything else in HE 2013 is.
f34 <- wide[state=="06" & election_year==2013 & wkr_nr %in% c("34","37")]
f_other <- wide[state=="06" & election_year==2013 & !(wkr_nr %in% c("34","37"))]
if(all(f34$flag_wkr_boundaries_recomputed==0) && nrow(f34)==4 &&
   all(f_other$flag_wkr_boundaries_recomputed==1) && nrow(f_other)==106){
  ok("HE 2013: 106 rows recomputed, the 4 Frankfurt WK34/WK37 rows on their own boundaries")
} else bad(sprintf("HE 2013 flag pattern wrong (WK34/37 rows=%d flagged=%d; others=%d flagged=%d)",
                 nrow(f34), sum(f34$flag_wkr_boundaries_recomputed),
                 nrow(f_other), sum(f_other$flag_wkr_boundaries_recomputed)))
if(sum(wide[state!="06"]$flag_wkr_boundaries_recomputed)==0){ ok("no non-Hessen row is flagged")
} else bad("a non-Hessen row is flagged as recomputed")

sec("21. CROSS-PIPELINE: Wahlkreis vs GEMEINDE-level state elections")
# The strongest check available: state_unharm is built from entirely different
# source files by a different pipeline (code/state_elections/) and stores
# LANDESSTIMMEN, so summing both to the state total must agree to the vote.
# This is what independently confirms the Hessen 2013/2018 PDF parse.
#
# The broad sweep is INFORMATIONAL, not a pass/fail on this pipeline - the two
# datasets do not always measure the same thing. Known and expected:
#   Bayern    ratio ~0.50 - the Gemeinde-level files hold GESAMTSTIMMEN (Erst +
#             Zweit, the official Bavarian measure); the Stimmkreis files keep
#             the two ballots apart, so the Zweitstimmen are half.
#   RP        2001-2016 reproduces every party EXACTLY but has no Gemeinde-level
#             turnout block at all.
#   BB/ST/NI  ratio 1.01-1.42 - the Gemeinde-level files omit pooled Briefwahl in
#             some years (cf. flag_briefwahl_only), so the constituency totals are
#             higher.
# All of these are properties of the Gemeinde-level pipeline. Hessen, checked
# hard below, is exact.
mp <- file.path(FIN, "state_unharm.rds")
if (!file.exists(mp)) {
  wn("state_unharm.rds not found - cross-pipeline check skipped")
} else {
  mun <- as.data.table(readRDS(mp))
  pz <- intersect(c("cdu","spd","gruene","afd","fdp","linke_pds","npd","freie_wahler"),
                  intersect(names(mun), pc))
  roll <- function(d) d[, c(list(elig = sum(eligible_voters, na.rm = TRUE),
                                 vot  = sum(number_voters,  na.rm = TRUE),
                                 val  = sum(valid_votes,    na.rm = TRUE)),
                            lapply(.SD, function(x) round(sum(x * valid_votes, na.rm = TRUE)))),
                        by = .(state, election_year), .SDcols = pz]
  aw <- roll(wide[stimme == "zweitstimme"]); am <- roll(mun)
  cmpx <- merge(aw, am, by = c("state","election_year"), suffixes = c(".w",".m"))

  # (a) HARD: Hessen must be exact on every quantity in every year
  qty <- c("elig","vot","val", pz)
  he <- cmpx[state == "06"]
  he_bad <- unlist(lapply(qty, function(q) {
    d <- he[[paste0(q,".w")]] - he[[paste0(q,".m")]]
    if (any(abs(d) > 0.5)) sprintf("%s: %s", q, paste(round(d), collapse=",")) else NULL
  }))
  if (nrow(he) == 3 && length(he_bad) == 0) {
    ok("HESSEN 2013/2018/2023: every quantity and party reproduces the Gemeinde-level pipeline EXACTLY")
  } else { bad("Hessen differs from the Gemeinde-level pipeline"); print(he_bad) }

  # (b) BROAD: party votes, only where both pipelines actually report the party
  rows <- list()
  for (q in pz) {
    a <- cmpx[[paste0(q,".w")]]; b <- cmpx[[paste0(q,".m")]]
    keep <- a > 0 & b > 0
    rows[[q]] <- data.table(state = cmpx$state[keep], election_year = cmpx$election_year[keep],
                            party = q, rel = abs(a[keep]-b[keep]) / b[keep],
                            ratio = a[keep] / b[keep])
  }
  pr <- rbindlist(rows)
  off <- pr[rel > 0.005]
  if (nrow(off) == 0) {
    ok(sprintf("all %d state-year x party comparisons agree within 0.5%% across the two pipelines",
               nrow(pr)))
  } else {
    wn(sprintf("%d of %d state-year x party comparisons differ by >0.5%% between the two pipelines - expected, see the note above (BY Gesamtstimmen, BB/ST/NI Briefwahl):",
               nrow(off), nrow(pr)))
    print(off[, .(comparisons = .N, median_wkr_over_gemeinde = round(median(ratio), 3)),
              by = state][order(state)])
  }
  cat(sprintf("    turnout block exact in %d of %d shared state-years (RP 2001-2016 has no Gemeinde-level turnout)\n",
              nrow(cmpx[abs(elig.w-elig.m) < 0.5 & abs(vot.w-vot.m) < 0.5 & abs(val.w-val.m) < 0.5]),
              nrow(cmpx)))
}

cat(sprintf("\n=================  AUDIT SUMMARY: %d FAIL, %d WARN  =================\n", fail, warn))
quit(status = if(fail>0) 1 else 0)
