suppressMessages(library(dplyr))
options(width=250)
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
h <- readRDS("data/municipal_elections/final/municipal_harm.rds"); h25 <- readRDS("data/municipal_elections/final/municipal_harm_25.rds")
print(names(h)); 
for(nm in c("h","h25")){ x<-get(nm); cat("\n==",nm,nrow(x),"\n"); k<-intersect(c("ags_21","ags_25"),names(x)); k<-k[1]; cat("dup key:",sum(duplicated(x[,c(k,"election_year")])),"\n"); print(table(nchar(x[[k]]),useNA="a")); cat("prefix mismatch:",sum(substr(x[[k]],1,2)!=as.character(x$state),na.rm=T),"\n"); cat("NA key:",sum(is.na(x[[k]])),"\n")
 cat("turnout range:",range(x$turnout,na.rm=T),"NA",sum(is.na(x$turnout)),"\n"); print(table(x$election_type,useNA="a"))
 pc <- c("cdu_csu","spd","linke_pds","gruene","afd","piraten","fdp","die_partei","freie_wahler","bsw","other"); pc<-intersect(pc,names(x)); s<-rowSums(x[,pc],na.rm=T); cat("sumshares out of [.97,1.03]:",sum(s<.97|s>1.03),"\n"); 
 if(any(s<.97|s>1.03)) print(x[s<.97|s>1.03,] %>% count(state,election_year) %>% head(20))
 cat("eligible<voters:",sum(x$number_voters>x$eligible_voters*1.0001,na.rm=T)," valid NA:",sum(is.na(x$valid_votes)),"\n")
 cat("year range:"); print(range(x$election_year))
 print(x %>% group_by(election_year) %>% summarise(n=n()) %>% as.data.frame %>% t)
}
# totals compare unharm vs harm (valid_votes, eligible) per state-year
ut <- u %>% filter(!state %in% c("02","04","11")) %>% group_by(state=as.character(state),election_year) %>% summarise(e_u=sum(eligible_voters,na.rm=T),v_u=sum(valid_votes,na.rm=T),.groups="drop")
for(nm in c("h","h25")){ x<-get(nm); ht <- x %>% group_by(state=as.character(state),election_year) %>% summarise(e_h=sum(eligible_voters,na.rm=T),v_h=sum(valid_votes,na.rm=T),.groups="drop"); d <- inner_join(ut,ht,by=c("state","election_year")) %>% mutate(de=e_h/e_u-1,dv=v_h/v_u-1); cat("\n",nm,"state-years with |dev|>0.5%\n"); print(as.data.frame(d %>% filter(abs(de)>0.005|abs(dv)>0.005)),digits=3); anti <- ut %>% anti_join(ht,by=c("state","election_year")); cat("unharm state-years absent in harm:\n"); print(as.data.frame(anti %>% filter(election_year>=1990)))}
