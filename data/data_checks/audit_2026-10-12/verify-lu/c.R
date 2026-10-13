suppressMessages(library(dplyr))
r<-function(p)readRDS(p)
a<-r("data/covars_municipality/final/ags_area_pop_emp.rds");cat("pop0",sum(a$population_ags==0,na.rm=T),"\n")
# LU-07
for(p in c("municipal_elections/final/municipal_unharm","municipal_elections/final/municipal_harm","municipal_elections/final/municipal_harm_25","state_elections/final/state_harm")){x<-r(paste0("data/",p,".rds"));cat(p,nrow(x),sum(is.na(x$election_date)),class(x$election_date),"\n")}
mh<-r("data/municipal_elections/final/municipal_harm.rds");print(table(mh$election_year[!is.na(mh$election_date)]))
for(p in c("county_elections/final/county_elec_unharm","county_elections/final/county_elec_harm_21_muni","county_elections/final/county_elec_harm_21_cty","county_elections/final/county_council_seats")) cat(p,"election_date" %in% names(r(paste0("data/",p,".rds"))),"\n")
# LU-08
s<-r("data/state_elections/final/state_harm_25.rds")
s%>%filter(state_name=="Sachsen-Anhalt"|state=="15")%>%group_by(election_year)%>%summarise(n=n(),gruene=sum(!is.na(gruene)),grue_nf=sum(!is.na(grue_nf)),bue90=sum(!is.na(bue90_gruene)),fw=sum(!is.na(freie_wahler)),fw2=sum(!is.na(freiewaehler)))%>%print(n=20)
ce<-r("data/county_elections/final/county_elec_unharm.rds");cat(sum(!is.na(ce$freie_w_hler)),sum(!is.na(ce$freie_waehler)),"\n")
for(n in c("municipal_unharm","state_unharm")){}
print(grep("freie",names(s),value=T)); print(grep("freie",names(r("data/municipal_elections/final/municipal_unharm.rds")),value=T))
# LU-12
sh<-r("data/state_elections/final/state_harm.rds");cat(sum(is.na(sh$ags)),sum(sh$eligible_voters==0,na.rm=T),sum(sh$number_voters==0,na.rm=T),max(sh$total_vote_share,na.rm=T),"\n")
f<-sh$flag_naive_turnout_above_1;cat(sum(!f%in%c(0,1)&!is.na(f)),"\n")
# LU-13
su<-r("data/state_elections/final/state_unharm.rds");print(table(su$flag_briefwahl_only,useNA="a"));cat(sum(is.na(su$eligible_voters)&su$valid_votes>0,na.rm=T),"\n")
h<-r("data/federal_elections/municipality_level/final/federal_muni_harm_21.rds");print(table(h$flag_unsuccessful_naive_merge,useNA="a"))
# LU-15
cat(sum(duplicated(su[,c("ags","election_year")])),"\n"); print(grep("state_name|ags_name",names(su),value=T))
print(su[duplicated(su[,c("ags","election_year")])|duplicated(su[,c("ags","election_year")],fromLast=T),c("ags","election_year","election_date")])
print(grep("incog|incon",names(r("data/federal_elections/county_level/final/federal_cty_harm.rds")),value=T));print(grep("incog|incon",names(su),value=T))
