x<-readRDS("data/state_elections/final/state_unharm.rds")
pc<-setdiff(names(x)[13:ncol(x)],c("cdu_csu"))
cand<-grep("gr(u|ü)n|b90|bue|bund|linke|pds|sed|^l$|wasg|dl|fdp|dvp|f_d_p|cdu|csu|spd|afd|npd|dvu|rep|republik|freie|^fw|pirat|ssw|schill|statt|bp|bayern",pc,value=TRUE)
w<-ifelse(!is.na(x$valid_votes),x$valid_votes,x$number_voters)
res<-do.call(rbind,lapply(cand,function(p){ok<-!is.na(x[[p]]); if(!any(ok)) return(NULL); d<-aggregate(cbind(v=ifelse(ok,x[[p]]*w,0),w=w)~state+election_date,x,sum); d$col<-p; d$sh<-100*d$v/d$w; d$nonNA<-as.vector(tapply(ok,paste(x$state,x$election_date),sum)[paste(d$state,d$election_date)]); d[d$nonNA>0,]}))
res$year<-format(res$election_date,"%Y")
s<-aggregate(sh~col+state,res,function(z) paste0(round(min(z),1),"-",round(max(z),1)))
n<-aggregate(year~col+state,res,function(z) paste(range(z),collapse="-"))
m<-merge(s,n); m<-m[order(m$col,m$state),]; print(m,row.names=FALSE)
sel<-c("b90","b90_gr","bue90_gruene","buendnis_90","grue_nf","gruene","bp","bayernpartei_bp","freiewaehler","freie_wahler","partei_freie_waehler","fdvp","dvp","dg_oder_dvp","die_republikaner_rep","rep","dl","dlw","gb_bhe","bhe","dg_bhe","gdpbhe","gpdbhe","kpd","kpd_ml","kpd_rz","ssw","npd","drp")
r2<-res[res$col%in%sel,]
agg<-aggregate(cbind(sh=sh)~col+state,r2,function(z) round(max(z),1)); yy<-aggregate(year~col+state,r2,function(z) paste(sort(unique(z)),collapse=","))
mm<-merge(agg,yy); mm<-mm[order(mm$col,mm$state),]
options(width=250); write.csv(mm,file.path("'$S'","alias_cols.csv"),row.names=FALSE)
for(cc in sel) {z<-mm[mm$col==cc,]; if(nrow(z)) cat(cc,":",paste0(z$state,"[max",z$sh,"|",z$year,"]",collapse="; "),"\n")}
