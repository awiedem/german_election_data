suppressMessages({library(dplyr);library(data.table)})
D<-"data/mayoral_elections/final/"
p<-readRDS(paste0(D,"mayor_panel.rds")); setDT(p)
cat("panel rows",nrow(p),"; unique (ags,election_date):",uniqueN(p[,.(ags,election_date)]),"; dup:",sum(duplicated(p[,.(ags,election_date)])),"\n")
print(p[duplicated(p[,.(ags,election_date)])|duplicated(p[,.(ags,election_date)],fromLast=T)][order(ags,election_date)][1:8,.(person_id,ags,election_date,term_number,winner_party)])
cat("person_id NA:",sum(is.na(p$person_id)),"; persons",uniqueN(p$person_id),"\n")
# person across multiple ags
pa<-p[,.(nags=uniqueN(ags),states=uniqueN(state)),by=person_id]; cat("persons in >1 ags:",sum(pa$nags>1)," >1 state:",sum(pa$states>1),"\n")
print(p[person_id%in%pa[nags>1]$person_id][,.(n=uniqueN(ags),meth=person_id_method[1]),by=person_id][,.N,by=meth])
# term_number monotone within person-ags ordered by date
setorder(p,person_id,ags,election_date)
p[,dterm:=term_number-shift(term_number),by=.(person_id,ags)]
cat("term_number decreases/zero step:",sum(p$dterm<=0,na.rm=T),"; step>1:",sum(p$dterm>1,na.rm=T),"\n")
print(p[dterm<=0][1:6,.(person_id,ags,election_date,term_number,consecutive_terms,n_terms)])
cat("term_number<1:",sum(p$term_number<1,na.rm=T),"; NA:",sum(is.na(p$term_number)),"\n")
cat("first obs term_number>1 by person-ags (left-censored):",p[,.SD[1],by=.(person_id,ags)][term_number>1,.N],"\n")
cat("consecutive_terms > term_number:",sum(p$consecutive_terms>p$term_number,na.rm=T),"\n")
cat("n_terms < term_number:",sum(p$n_terms<p$term_number,na.rm=T),"\n")
# overlapping: two persons same ags with same election_date? or different persons: same ags, election_date within 2 years
setorder(p,ags,election_date)
p[,gap:=as.numeric(election_date-shift(election_date)),by=ags]
cat("gap<365 days within ags (different row):",sum(p$gap<365,na.rm=T),"; gap<30:",sum(p$gap<30,na.rm=T),"\n")
print(p[gap<30][1:8,.(person_id,ags,election_date,gap,term_number)])
# is_incumbent consistency: is_incumbent==1 implies same person as previous row at ags
p[,prev_pid:=shift(person_id),by=ags]
cat("is_incumbent=1 but prev person differs:",sum(p$is_incumbent==1&p$prev_pid!=p$person_id,na.rm=T),"; incumbent=1 & first row:",sum(p$is_incumbent==1&is.na(p$prev_pid),na.rm=T),"\n")
cat("term_number>1 but prev person differs:",sum(p$term_number>1&p$prev_pid!=p$person_id,na.rm=T),"\n")
cat("same person as prev but is_incumbent==0:",sum(p$is_incumbent==0&p$prev_pid==p$person_id,na.rm=T),"\n")
cat("winner_voteshare range:",range(p$winner_voteshare,na.rm=T),"; NA:",sum(is.na(p$winner_voteshare)),"\n")
cat("years_in_office<0",sum(p$years_in_office<0,na.rm=T),"; >40:",sum(p$years_in_office>40,na.rm=T)," total_tenure>60:",sum(p$total_tenure_years>60,na.rm=T),"\n")
cat("tenure_start > election_year:",sum(p$tenure_start>p$election_year,na.rm=T),"; term_start_date != election_date:",sum(p$term_start_date!=p$election_date,na.rm=T),"\n")
cat("party_switch dist\n"); print(table(p$party_switch,useNA="a"))
cat("party_switch=1 with same party:",sum(p$party_switch==1&p$winner_party==shift(p$winner_party),na.rm=T),"\n")
saveRDS(p,paste0("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/mayoral/p.rds"))
