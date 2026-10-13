suppressMessages({library(dplyr);library(data.table)})
c<-readRDS("data/mayoral_elections/final/mayoral_candidates.rds"); setDT(c)
u<-readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); setDT(u)
p<-function(...)cat(...,"\n")
c[,eid:=paste(ags,election_date,election_type)]
p("elections:",uniqueN(c$eid)," rows:",nrow(c))
p("share bounds hw", sum(c$candidate_voteshare_hw<0|c$candidate_voteshare_hw>1,na.rm=T)," sw",sum(c$candidate_voteshare_sw<0|c$candidate_voteshare_sw>1,na.rm=T))
e<-c[,.(state=state[1],yr=election_year[1],s_hw=sum(candidate_voteshare_hw,na.rm=T),nna=sum(is.na(candidate_voteshare_hw)),n=.N,nc=n_candidates_hw[1],
  votes_hw=sum(candidate_votes_hw,na.rm=T),valid=valid_votes[1],nwin=sum(is_winner,na.rm=T),nwinNA=sum(is.na(is_winner)),
  s_sw=sum(candidate_voteshare_sw,na.rm=T),nsw=sum(!is.na(candidate_votes_sw)|!is.na(candidate_voteshare_sw)),has_sw=has_stichwahl[1],sup=flag_superseded[1]),by=eid]
p("HW share sum dist (non-NA all)"); print(summary(e[nna==0]$s_hw))
p("HW share sum off by >0.02 by state"); print(e[nna==0&abs(s_hw-1)>0.02,.N,by=state]); 
print(e[nna==0&abs(s_hw-1)>0.02][order(-abs(s_hw-1))][1:12,.(eid,state,s_hw,n)])
p("HW share sum off by >0.05"); print(e[nna==0&abs(s_hw-1)>0.05,.N,by=state])
p("votes sum != valid (rel>1%)"); e[,rel:=(votes_hw-valid)/valid]; print(e[votes_hw>0&abs(rel)>0.01,.N,by=state]); print(e[votes_hw>0&abs(rel)>0.01][order(-abs(rel))][1:10,.(eid,votes_hw,valid,rel)])
p("n_candidates_hw != rows w/ hw"); print(c[!is.na(candidate_votes_hw)|!is.na(candidate_voteshare_hw),.(n=.N,nc=n_candidates_hw[1]),by=eid][n!=nc,.N])
p("nwin dist"); print(table(e$nwin,e$nwinNA))
p("n winners >1"); print(e[nwin>1,.N,by=state]); print(e[nwin>1][1:6,.(eid,nwin)])
p("n winners 0 (non NA)"); print(e[nwin==0&nwinNA==0,.N,by=state]); print(e[nwin==0&nwinNA==0&!sup][1:6,.(eid,state,yr,s_hw)])
p("SW share sum !=1 (has sw)"); print(e[has_sw==T&nsw>0&abs(s_sw-1)>0.02,.N,by=state])
print(e[has_sw==T&nsw>0&abs(s_sw-1)>0.02][1:10,.(eid,s_sw,nsw)])
p("has_sw TRUE but no SW votes"); print(e[has_sw==T&nsw==0,.N,by=state])
p("winner==rank1 in decisive round")
c[,rk:=ifelse(has_stichwahl & !is.na(candidate_rank_sw),candidate_rank_sw,candidate_rank_hw)]
w<-c[is_winner==TRUE]; p("winner with rank>1:",sum(w$rk>1,na.rm=T)); print(w[rk>1,.N,by=state]); print(w[rk>1][1:8,.(eid,candidate_name,candidate_party,candidate_voteshare_hw,candidate_voteshare_sw,candidate_rank_hw,candidate_rank_sw)])
p("hw rank dup ties"); 
# unharm winner share vs candidates
m<-merge(u[round=="hauptwahl",.(ags,election_date,election_type,wvs=winner_voteshare,wp=winner_party,wv=winner_votes)], c[candidate_rank_hw==1,.(ags,election_date,election_type,cvs=candidate_voteshare_hw,cp=candidate_party,cv=candidate_votes_hw)],by=c("ags","election_date","election_type"))
p("merged HW",nrow(m)," share mismatch >0.005:",sum(abs(m$wvs-m$cvs)>0.005,na.rm=T)," votes mismatch:",sum(m$wv!=m$cv,na.rm=T)," party mismatch:",sum(m$wp!=m$cp,na.rm=T))
print(m[abs(wvs-cvs)>0.005][1:8])
print(m[!is.na(wp)&!is.na(cp)&wp!=cp][1:10])
p("unharm HW not in candidates:", nrow(u[round=="hauptwahl"][!c[,.(ags,election_date,election_type)],on=c("ags","election_date","election_type")]))
print(u[round=="hauptwahl"][!c[,.(ags,election_date,election_type)],on=c("ags","election_date","election_type"),.N,by=state])
p("candidates not in unharm:", nrow(unique(c[,.(ags,election_date,election_type)])[!u,on=c("ags","election_date","election_type")]))
saveRDS(e,file.path("/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/mayoral","e.rds"))
