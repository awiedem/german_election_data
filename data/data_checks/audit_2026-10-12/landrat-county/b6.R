suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
u <- as_tibble(readRDS("data/county_elections/final/county_elec_unharm.rds"))
for (v in c("krs_nr","landkreis","bemerkungen","nichtwaehler","sonstige_wg","andere","einzelbe","w_hlergruppen_isgesamt","waehlervereinigungen","weitere_listen_vereinigungen","freie_w_hler","freie_waehler","fw","fwg","freie","wirtso","piraten")) if(v %in% names(u)) {
 x<-u[[v]]; cat(v,": nonNA",sum(!is.na(x)),"nonzero",sum(x>0,na.rm=T),"states:",paste(unique(u$state[!is.na(x)&x>0]),collapse=","),"yrs:",paste(range(u$election_year[!is.na(x)&x>0]),collapse="-"),"max",suppressWarnings(max(x,na.rm=T)),"\n")}
# coexistence of fw variants
fwv <- c("freie_w_hler","freie_waehler","fw","fwg","freie_w_hlergemeinschaft","freie_w_hlerv","fwe","fwo","fwp","fws","fwv","fww","fwn")
fwv <- fwv[fwv%in%names(u)]
print(sapply(u[fwv],function(x)sum(!is.na(x)&x>0)))
print(u%>%filter(rowSums(!is.na(u[fwv])&u[fwv]>0)>1)%>%nrow())
# nonwaehler > 0
print(u%>%filter(nichtwaehler>0)%>%select(ags,ags_name,election_year,nichtwaehler,turnout,sonstige_wg)%>%head(5))
# party columns: how many columns present per state (nonzero)
nm<-names(u); 
cat("flag cols:",grep("^flag",nm,value=T),"\n"); 
for(f in grep("^flag",nm,value=T)) {print(f);print(table(u[[f]],useNA="a"))}
