suppressMessages({library(dplyr);library(tidyr)})
setwd("/Users/hanno/Documents/GitHub/german_election_data")
S<-"/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/state-hist"
x<-readRDS("data/state_elections/final/state_unharm.rds")
bl<-readRDS(file.path(S,"bl_long.rds"))
map<-c(BW="08",BY="09",BE="11",BB="12",HB="04",HH="02",HE="06",MV="13",NI="03",NW="05",RP="07",SL="10",SN="14",ST="15",SH="01",TH="16")
bl$state<-map[bl$land]
# GERDA column groups
grp<-list(spd="spd",cdu="cdu",csu="csu",fdp="fdp",gruene=c("gruene","b90","b90_gr","grue_nf"),linke=c("linke_pds"),afd="afd",npd="npd",rep=c("rep","die_republikaner_rep"),ssw="ssw",fw=c("freie_wahler","partei_freie_waehler"),piraten="piraten",dkp="dkp",kpd=c("kpd","kpd_ml","kpd_rz"),zentrum="zentrum",dp="dp",gb_bhe=c("gb_bhe","gdpbhe","gpdbhe","dg_bhe","bhe"),drp="drp",statt="statt_partei",wasg="wasg",schill=c("schill","pro_dm_schill"))
blmap<-c(SPD="spd",CDU="cdu",CSU="csu",FDP="fdp","FDP/DVP"="fdp","FDP/DPS"="fdp",`GRÜNE`="gruene","GRÜNE/B 90"="gruene","GRÜNE/AL"="gruene","GRÜNE/GAL"="gruene",`BÜ90/GRÜNE`="gruene",`GRÜNE Saar`="gruene",`GRÜ-NF`="gruene","DIE LINKE"="linke","DIE LINKE."="linke","Die Linke."="linke",PDS="linke",AfD="afd",NPD="npd",REP="rep",SSW="ssw","FREIE WÄHLER"="fw",FW="fw","FW FREIE WÄHLER"="fw","FBI/Freie Wähler"="fw","BVB/FREIE WÄHLER"="fw",PIRATEN="piraten",DKP="dkp",KPD="kpd",Zentrum="zentrum",DP="dp","GB/BHE"="gb_bhe",BHE="gb_bhe",DRP="drp","STATT Partei"="statt",WASG="wasg",Schill="schill")
bl$g<-unname(blmap[bl$party]); bl<-bl|>filter(!is.na(g))
# weights: valid_votes else number_voters else eligible
x$w<-ifelse(!is.na(x$valid_votes)&x$valid_votes>0,x$valid_votes,ifelse(!is.na(x$number_voters),x$number_voters,x$eligible_voters))
out<-list()
for(g in names(grp)){cs<-intersect(grp[[g]],names(x)); if(!length(cs)) next
 sh<-rowSums(x[cs],na.rm=TRUE); allna<-rowSums(!is.na(x[cs]))==0
 d<-data.frame(state=x$state,election_date=x$election_date,w=x$w,v=ifelse(allna,NA,sh*x$w),stringsAsFactors=FALSE)
 r<-d|>group_by(state,election_date)|>summarise(g_votes=sum(v,na.rm=TRUE),n_nonNA=sum(!is.na(v)),nrows=n(),.groups="drop")|>mutate(g=g); out[[g]]<-r}
G<-bind_rows(out)
tot<-x|>group_by(state,election_date)|>summarise(g_w=sum(w,na.rm=TRUE),.groups="drop")
G<-G|>left_join(tot,by=c("state","election_date"))|>mutate(g_share=100*g_votes/g_w)
B<-bl|>group_by(state,date,g,valid)|>summarise(bl_votes=sum(votes),.groups="drop")|>mutate(bl_share=100*bl_votes/valid)|>rename(election_date=date)
ge<-unique(paste(tot$state,tot$election_date)); be<-unique(paste(B$state,B$election_date))
M<-full_join(G,B,by=c("state","election_date","g"))|>filter(paste(state,election_date)%in%ge,paste(state,election_date)%in%be)
M<-M|>mutate(g_share=ifelse(is.na(g_share)|n_nonNA%in%0,0,g_share),bl_share=ifelse(is.na(bl_share),0,bl_share),err=g_share-bl_share)
write.csv(M,file.path(S,"party_compare2.csv"),row.names=FALSE)
cat("elections compared:",nrow(distinct(M,state,election_date)),"\n")
M|>filter(abs(err)>1)|>arrange(state,election_date,desc(abs(err)))|>mutate(across(where(is.numeric),~round(.,2)))|>select(state,election_date,g,g_share,bl_share,err,n_nonNA,nrows)|>as.data.frame()|>print(row.names=FALSE)
cat("\n0.5-1\n")
M|>filter(abs(err)>0.5,abs(err)<=1)|>arrange(state,election_date)|>mutate(across(where(is.numeric),~round(.,2)))|>select(state,election_date,g,g_share,bl_share,err)|>as.data.frame()|>print(row.names=FALSE)
