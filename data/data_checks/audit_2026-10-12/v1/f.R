suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
cw <- readRDS("data/crosswalks/final/ags_crosswalks.rds")
cw$ags8 <- sprintf("%08.0f", cw$ags)
na <- u |> filter(is.na(ags_name)) |> distinct(ags, election_year)
cat("NA rows in cw by ags+year:", nrow(inner_join(na, cw |> transmute(ags=ags8, election_year=year), by=c("ags","election_year"))), " of ", nrow(na), "\n")
# F7
mh <- readRDS("data/municipal_elections/final/municipal_harm.rds")
cat("no-space comma names:", n_distinct(mh$ags_name[grepl(",[^ ]", mh$ags_name)]), "\n"); print(head(unique(mh$ags_name[grepl(",[^ ]", mh$ags_name)]),5))
print(unique(mh$ags_name[grepl("Clausthal|Bodenwerder", mh$ags_name)]))
f <- readRDS("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds"); print(unique(f$ags_name[grepl("Clausthal|Bodenwerder", f$ags_name)]))
sh <- readRDS("data/state_elections/final/state_harm_21.rds"); print(grep("ags_name",names(sh),value=T))
# F8
for (p in c("federal_elections/municipality_level/final/federal_muni_unharm","mayoral_elections/final/mayoral_unharm","landrat_elections/final/landrat_unharm")) { fn<-paste0("data/",p,".rds"); if(file.exists(fn)){d<-readRDS(fn); cat(p,"\n"); print(unique(d$state_name))} else cat("missing",fn,"\n")}
# F10
h <- readRDS("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds")
a <- u |> filter(election_year==1990) |> group_by(state) |> summarise(ev=sum(eligible_voters_orig,na.rm=T), nv=sum(number_voters,na.rm=T), vv=sum(valid_votes,na.rm=T))
b <- h |> filter(election_year==1990) |> group_by(state) |> summarise(ev=sum(eligible_voters_orig,na.rm=T), nv=sum(number_voters,na.rm=T), vv=sum(valid_votes,na.rm=T))
print(inner_join(a,b,by="state") |> mutate(rev=ev.y/ev.x-1, rnv=nv.y/nv.x-1, rvv=vv.y/vv.x-1) |> as.data.frame())
