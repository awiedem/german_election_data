suppressMessages({library(dplyr);library(data.table)})
D<-"data/mayoral_elections/final/"
a<-readRDS(paste0(D,"mayor_panel_annual.rds")); setDT(a)
p<-readRDS(paste0(D,"mayor_panel.rds")); setDT(p)
cat("annual rows",nrow(a),"; dup (ags,year):",sum(duplicated(a[,.(ags,year)])),"\n")
d<-a[duplicated(a[,.(ags,year)])|duplicated(a[,.(ags,year)],fromLast=T)]
cat("rows in dup groups:",nrow(d),"\n"); print(d[,.N,by=state]); print(head(d[order(ags,year)][,.(ags,year,person_id,election_date,term_number,winner_party,years_since_election)],10))
cat("year range",range(a$year),"\n")
cat("year < election_year:",sum(a$year<a$election_year),"; years_since_election<0:",sum(a$years_since_election<0,na.rm=T),"\n")
cat("year - election_year != years_since:",sum((a$year-a$election_year)!=a$years_since_election,na.rm=T),"\n")
cat("max years_since:",max(a$years_since_election,na.rm=T),"; >12:",sum(a$years_since_election>12,na.rm=T),"\n")
print(a[years_since_election>10,.N,by=state])
print(a[years_since_election>10][1:5,.(ags,year,election_year,years_since_election,years_to_next_election)])
cat("year > 2026:",sum(a$year>2026),"\n")
# every panel election in annual?
k<-p[,.(ags,election_date)]; k2<-unique(a[,.(ags,election_date)])
cat("panel elections missing in annual:",nrow(k[!k2,on=.(ags,election_date)]),"; annual elections not in panel:",nrow(k2[!k,on=.(ags,election_date)]),"\n")
# annual covers span gaps? for each ags, count of years between first and last
g<-a[,.(n=.N,span=max(year)-min(year)+1),by=ags]; cat("ags with gaps in annual (n<span):",sum(g$n<g$span),"\n")
# annual with next election overlap: year >= next election year but still assigned old
a<-merge(a,p[,.(ags,election_date,nxt=NA)],by=c("ags","election_date"),all.x=T)
setorder(p,ags,election_date); p[,nextdate:=shift(election_date,-1),by=ags]
b<-merge(a[,.(ags,year,election_date)],p[,.(ags,election_date,nextdate)],by=c("ags","election_date"))
cat("annual year > year(next election date):",sum(b$year>as.integer(format(b$nextdate,"%Y")),na.rm=T),"\n")
cat("electoral_cycle_pos range:",range(a$electoral_cycle_pos,na.rm=T),"\n")
cat("annual panel last years: elections that are in Landrat? etc\n")
print(a[,.N,by=year][order(-year)][1:5])
