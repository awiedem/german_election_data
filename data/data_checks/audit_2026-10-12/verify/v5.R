suppressMessages({library(dplyr);library(bundeslaendeR)}); options(width=200)
u <- readRDS("data/state_elections/final/state_unharm.rds")
h25 <- readRDS("data/state_elections/final/state_harm_25.rds")
bl <- as.data.frame(ltw_elections) %>% mutate(election_date=as.Date(election_date))
for(x in list(c("BW","08",1992),c("HE","06",1991))){d<-u%>%filter(state==x[2],election_year==x[3]);sh<-sum(d$gruene*d$valid_votes,na.rm=T)/sum(d$valid_votes,na.rm=T)
 b<-bl%>%filter(state==x[1],format(election_date,"%Y")==x[3],grepl("Gr",partyname_short))%>%select(partyname_short,party_vshare);cat(x,"gerda",sh,"\n");print(b)}
# SH-04 samples
chk<-function(st,code,y,col,pat){d<-u%>%filter(state==code,election_year==y);sh<-if(col%in%names(d)) sum(d[[col]]*d$valid_votes,na.rm=T)/sum(d$valid_votes,na.rm=T) else NA
 b<-bl%>%filter(state==st,format(election_date,"%Y")==y,grepl(pat,partyname_short))%>%select(partyname_short,party_vshare);cat(st,y,col,"gerda",sh,"| bl:",paste(b$partyname_short,round(b$party_vshare,4)),"| other",sum(d$other*d$valid_votes,na.rm=T)/sum(d$valid_votes,na.rm=T),"\n")}
chk("HB","04",2003,"dvu","DVU");chk("HB","04",2015,"biw","BIW");chk("NI","03",1994,"rep","REP");chk("SL","10",2022,"tierschutz","Tier");chk("HB","04",2019,"biw","BIW")
print(grep("dvu|biw|tierschutz|schill|pro_",names(u),value=T))
