suppressMessages({library(dplyr);library(data.table)})
D<-"data/mayoral_elections/final/"
ph<-readRDS(paste0(D,"mayor_panel_harm.rds")); setDT(ph)
ah<-readRDS(paste0(D,"mayor_panel_annual_harm.rds")); setDT(ah)
p<-readRDS(paste0(D,"mayor_panel.rds")); setDT(p)
cat("panel_harm dup (ags_21,election_date):",sum(duplicated(ph[,.(ags_21,election_date)])),"; dup (ags,election_date):",sum(duplicated(ph[,.(ags,election_date)])),"\n")
cat("annual_harm dup (ags_21,year):",sum(duplicated(ah[,.(ags_21,year)])),"; dup (ags,year):",sum(duplicated(ah[,.(ags,year)])),"\n")
d<-ah[duplicated(ah[,.(ags_21,year)])|duplicated(ah[,.(ags_21,year)],fromLast=T)]
cat("rows in dup groups",nrow(d),"\n"); print(d[,.N,by=state])
print(d[order(ags_21,year)][1:12,.(ags,ags_21,year,person_id,election_date,term_number)])
cat("dup groups with different persons:",d[,uniqueN(person_id),by=.(ags_21,year)][V1>1,.N],"of",nrow(unique(d[,.(ags_21,year)])),"\n")
# panel_harm dups
d2<-ph[duplicated(ph[,.(ags_21,election_date)])|duplicated(ph[,.(ags_21,election_date)],fromLast=T)]; cat("panel_harm dup rows",nrow(d2),"\n")
cat("panel_harm: ags_21 state prefix mismatch:",sum(substr(ph$ags_21,1,2)!=ph$state),"; ags_21 NA:",sum(is.na(ph$ags_21)),"\n")
cat("panel rows lost in harm:",nrow(p)-nrow(ph),"; by state:\n"); print(merge(p[,.N,by=state],ph[,.N,by=state],by="state",suffixes=c("_p","_h"))[,d:=N_h-N_p][d!=0])
# term_number monotone in harm by person within ags_21
setorder(ph,person_id,ags_21,election_date); ph[,dt:=term_number-shift(term_number),by=.(person_id,ags_21)]
cat("harm term_number non-increasing steps:",sum(ph$dt<=0,na.rm=T),"\n")
