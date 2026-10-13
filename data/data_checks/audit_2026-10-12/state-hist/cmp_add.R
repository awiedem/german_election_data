suppressMessages({library(readxl);library(dplyr);library(tidyr)})
setwd("/Users/hanno/Documents/GitHub/german_election_data")
S<-"/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/state-hist"
z<-as.data.frame(read_excel(file.path(S,"dl/additional_elecdata.xlsx")))
map<-c(BW="08",BY="09",BE="11",BB="12",HB="04",HH="02",HE="06",MV="13",NI="03",NW="05",RP="07",SL="10",SN="14",ST="15",SH="01",TH="16")
z$state<-map[z$state]; z$election_date<-as.Date(z$election_date)
x<-readRDS("data/state_elections/final/state_unharm.rds")
g<-x|>group_by(state,election_date)|>summarise(n=n(),elig=sum(eligible_voters,na.rm=T),voters=sum(number_voters,na.rm=T),valid=sum(valid_votes,na.rm=T),.groups="drop")
t<-z|>distinct(state,election_date,electorate,number_of_voters,valid_votes)|>left_join(g,by=c("state","election_date"))|>mutate(d_elig=100*(elig/electorate-1),d_vot=100*(voters/number_of_voters-1),d_valid=100*(valid/valid_votes-1))
print(t|>mutate(across(starts_with("d_"),~round(.,3))),width=200)
# party
pm<-c(CDU="cdu",AfD="afd",SPD="spd",Linke="linke_pds",FDP="fdp",`Grüne`="gruene",CSU="csu",`FW-ST`="freie_wahler",BSW="bsw")
print(table(z$partyname_short))
cat("\n--- party compare (additional) ---\n")
pm<-c(CDU="cdu",CSU="csu",AfD="afd",SPD="spd",Linke="linke_pds",FDP="fdp",`Grüne`="gruene",BSW="bsw",SSW="ssw",`FW-ST`="freie_wahler",`FW-BB`="freie_wahler",`FW-BY`="freie_wahler",`FW-HE`="freie_wahler",`FW-TH`="freie_wahler",`FW-SN`="freie_wahler",`FW-NI`="freie_wahler",`FW-MV`="freie_wahler",`FW-BE`="freie_wahler",`FW-NW`="freie_wahler",`FW-SH`="freie_wahler",`FW-SL`="freie_wahler",`FW-HH`="freie_wahler",Piraten="piraten",Volt="volt",dieBasis="diebasis",Rep="rep",NPD="npd",`NPD (2023)`="npd",`Die Tierschutzpartei`=NA)
z$col<-unname(pm[z$partyname_short]); zz<-z|>filter(!is.na(col),col%in%names(x))
rows<-list()
for(i in seq_len(nrow(zz))){r<-zz[i,]; d<-x[x$state==r$state&x$election_date==r$election_date,]; if(!nrow(d)) next
 sh<-100*sum(d[[r$col]]*d$valid_votes,na.rm=TRUE)/sum(d$valid_votes,na.rm=TRUE)
 rows[[i]]<-data.frame(state=r$state,date=r$election_date,party=r$partyname_short,col=r$col,bl=100*r$party_vote_count/r$valid_votes,g=sh,nonNA=sum(!is.na(d[[r$col]])),nrow=nrow(d))}
R<-bind_rows(rows)|>mutate(err=g-bl)
print(R|>filter(abs(err)>0.3)|>mutate(across(where(is.numeric),~round(.,2))),width=200)
cat("n compared",nrow(R),"max abs err",max(abs(R$err)),"\n")
