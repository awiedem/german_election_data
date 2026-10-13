suppressMessages({library(dplyr);library(tidyr)})
setwd("/Users/hanno/Documents/GitHub/german_election_data")
S<-"/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/state-hist"
x<-readRDS("data/state_elections/final/state_unharm.rds")
bw<-readRDS(file.path(S,"bl_wide.rds")); bl<-readRDS(file.path(S,"bl_long.rds"))
map<-c(BW="08",BY="09",BE="11",BB="12",HB="04",HH="02",HE="06",MV="13",NI="03",NW="05",RP="07",SL="10",SN="14",ST="15",SH="01",TH="16")
bl$state<-map[bl$land]
pm<-c(SPD="spd",CDU="cdu",CSU="csu",FDP="fdp","FDP/DVP"="fdp",`GRÜNE`="gruene",`DIE LINKE`="linke_pds",PDS="linke_pds",AfD="afd",NPD="npd",REP="rep",SSW="ssw",`FREIE WÄHLER`="freie_wahler",PIRATEN="piraten",DKP="dkp",KPD="kpd",Zentrum="zentrum",DP="dp","GB/BHE"="gb_bhe",DRP="drp",SRP=NA,BP=NA,`STATT Partei`="statt_partei",WASG="wasg")
bl$col<-unname(pm[bl$party])
bl<-bl|>filter(!is.na(col))
# check cols exist
cols<-unique(bl$col); cols<-cols[cols %in% names(x)]; bl<-bl|>filter(col %in% cols)
# GERDA state-election party votes
long<-x|>select(state,election_date,election_year,valid_votes,all_of(cols))|>pivot_longer(all_of(cols),names_to="col",values_to="sh")|>filter(!is.na(sh))|>group_by(state,election_date,col)|>summarise(g_votes=sum(sh*valid_votes,na.rm=TRUE),.groups="drop")
tot<-x|>group_by(state,election_date)|>summarise(g_valid=sum(valid_votes,na.rm=TRUE),.groups="drop")
long<-long|>left_join(tot,by=c("state","election_date"))|>mutate(g_share=100*g_votes/g_valid)
bl<-bl|>group_by(state,date,col,valid)|>summarise(bl_votes=sum(votes),.groups="drop")|>mutate(bl_share=100*bl_votes/valid)
bl2<-bl|>rename(election_date=date)
m<-full_join(long,bl2,by=c("state","election_date","col"))
# restrict to elections present in both
ge<-unique(paste(tot$state,tot$election_date)); be<-unique(paste(bl2$state,bl2$election_date))
m<-m|>filter(paste(state,election_date)%in%ge,paste(state,election_date)%in%be)
m<-m|>mutate(g_share=ifelse(is.na(g_share),0,g_share),bl_share=ifelse(is.na(bl_share),0,bl_share),err=g_share-bl_share)
write.csv(m,file.path(S,"party_compare.csv"),row.names=FALSE)
cat("n elections compared:",nrow(distinct(m,state,election_date)),"\n")
print(m|>filter(abs(err)>1)|>arrange(desc(abs(err)))|>mutate(across(where(is.numeric),~round(.,2)))|>select(state,election_date,col,g_share,bl_share,err,g_votes,bl_votes)|>as.data.frame(),row.names=FALSE)
cat("\n0.5-1pp\n")
print(m|>filter(abs(err)>0.5,abs(err)<=1)|>arrange(desc(abs(err)))|>mutate(across(where(is.numeric),~round(.,2)))|>select(state,election_date,col,g_share,bl_share,err)|>as.data.frame(),row.names=FALSE)
