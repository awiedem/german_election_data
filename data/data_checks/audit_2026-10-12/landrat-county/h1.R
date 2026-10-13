suppressMessages(library(data.table)); setwd("/Users/hanno/Documents/GitHub/german_election_data")
chk <- function(d,f,key){ r<-as.data.frame(readRDS(sprintf("data/%s/final/%s.rds",d,f))); c<-fread(sprintf("data/%s/final/%s.csv",d,f),colClasses=list(character=key)); cat(f,"dim rds",dim(r),"csv",dim(c),"names equal",identical(names(r),names(c)),"\n")
 for(k in key) cat(" key",k,"equal",all(r[[k]]==c[[k]],na.rm=T),"nchar csv",paste(unique(nchar(c[[k]])),collapse=","),"\n")
 num<-names(r)[sapply(r,is.numeric)]; mx<-sapply(num,function(v){a<-r[[v]];b<-c[[v]]; if(!is.numeric(b)) return(NA); max(abs(a-b),na.rm=T)}); cat(" max numeric diff",max(mx,na.rm=T)," nonnumeric csv cols:",sum(is.na(mx)),"\n")}
chk("landrat_elections","landrat_unharm","ags"); chk("landrat_elections","landrat_candidates","ags")
chk("county_elections","county_council_seats","county"); chk("county_elections","county_elec_harm_21_cty",c("county_code")); chk("county_elections","county_elec_harm_21_muni",c("ags","county")); chk("county_elections","county_elec_unharm",c("ags","county"))
