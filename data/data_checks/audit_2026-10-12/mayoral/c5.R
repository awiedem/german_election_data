suppressMessages({library(dplyr);library(data.table)})
D<-"data/mayoral_elections/final/"
a<-readRDS(paste0(D,"mayor_panel_annual.rds")); setDT(a)
p<-readRDS(paste0(D,"mayor_panel.rds")); setDT(p)
u<-readRDS(paste0(D,"mayoral_unharm.rds")); setDT(u)
cat("panel rows vs unharm HW-decisive elections by state\n")
uh<-unique(u[,.(ags,election_date,election_type,state)])
x<-merge(uh[,.(unharm_elections=uniqueN(paste(ags,election_date)) ),by=state],p[,.(panel_elections=.N),by=state],by="state",all=T); x[,cov:=round(panel_elections/unharm_elections,2)]; print(x[order(state)])
# annual: rows after the max year of last unharm election + 12
last<-u[,.(last_u=max(election_date)),by=ags]
b<-merge(a[,.(ags,year,election_year,state,years_since_election)],last,by="ags")
cat("annual rows with years_since>=10 and no later election in unharm (same ags):\n")
print(b[years_since_election>=10,.(N=.N,later=sum(as.integer(format(last_u,"%Y"))>election_year)),by=state][order(state)])
cat("max term length by state\n"); print(a[,.(maxys=max(years_since_election),p95=quantile(years_since_election,.95)),by=state][order(state)])
print(a[state=="15"&years_since_election>=20][1:4,.(ags,year,election_year,person_id)])
cat("ST: ags that have elections in unharm 1994-2008 only\n")
print(u[state=="15",.(last=max(election_year)),by=ags][,.N,by=last][order(last)])
