suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds")); nm<-names(u); parts<-setdiff(nm[which(nm=="cdu"):which(nm=="zentrum")],"cdu_csu")
u$ps<-rowSums(u[,parts],na.rm=T); u$r<-u$ps-u$valid_votes/u$number_voters
x<-u|>filter(valid_votes>0,r< -0.05)
print(x|>select(ags,ags_name,election_year,number_voters,valid_votes,ps,r,unique_mailin)|>arrange(r)|>head(12)|>as.data.frame())
cat("under rows with small voters(<100):",sum(x$number_voters<100),"of",nrow(x),"\n")
# count rows with abs deviation in votes (>3 votes) 
u$dv<-round((u$ps*u$number_voters)-u$valid_votes)
print(u|>group_by(election_year)|>summarise(n_gt3=sum(abs(dv)>3,na.rm=T),maxabs=max(abs(dv),na.rm=T),sum=sum(dv,na.rm=T))|>as.data.frame())
print(u|>filter(abs(dv)>20)|>select(ags,ags_name,election_year,number_voters,valid_votes,dv)|>head(10)|>as.data.frame())
print(sum(is.na(u$ags_name)));print(table(u$election_year[is.na(u$ags_name)]))
