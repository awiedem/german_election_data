x<-readRDS("data/state_elections/final/state_unharm.rds")
S<-"/private/tmp/claude-501/-Users-hanno-Documents-GitHub-german-election-data/adfc9cc1-5d71-47c2-9e9e-f58b91334ee3/scratchpad/audit/state-hist"
sel<-c("b90","b90_gr","bue90_gruene","buendnis_90","grue_nf","gruene","bp","bayernpartei_bp","freiewaehler","freie_wahler","partei_freie_waehler","fdvp","dvp","dg_oder_dvp","die_republikaner_rep","rep","dl","dlw","gb_bhe","bhe","dg_bhe","gdpbhe","gpdbhe","kpd","kpd_ml","kpd_rz","ssw","npd","drp","linke_pds","wasg")
sel<-intersect(sel,names(x))
w<-ifelse(!is.na(x$valid_votes),x$valid_votes,x$number_voters)
key<-paste(x$state,x$election_date)
rows<-list()
for(p in sel){ok<-!is.na(x[[p]]); if(!any(ok)) next
 v<-tapply(ifelse(ok,x[[p]]*w,0),key,sum); ww<-tapply(w,key,sum,na.rm=TRUE); nn<-tapply(ok,key,sum)
 d<-data.frame(col=p,key=names(v),sh=100*as.numeric(v)/as.numeric(ww[names(v)]),nn=as.numeric(nn[names(v)])); rows[[p]]<-d[d$nn>0,]}
R<-do.call(rbind,rows); R$state<-substr(R$key,1,2); R$yr<-substr(R$key,4,7)
agg<-aggregate(sh~col+state,R,function(z) round(max(z),1)); yy<-aggregate(yr~col+state,R,function(z) paste(sort(unique(z)),collapse=","))
mm<-merge(agg,yy); mm<-mm[order(mm$col,mm$state),]
write.csv(mm,file.path(S,"alias_cols.csv"),row.names=FALSE)
for(cc in sel){z<-mm[mm$col==cc,]; if(nrow(z)) cat(cc,":",paste0(z$state,"[max",z$sh,"|",z$yr,"]",collapse="; "),"\n\n")}
