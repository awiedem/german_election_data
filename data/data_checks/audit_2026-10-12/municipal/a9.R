suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
options(width=220)
pc <- c("cdu_csu","spd","linke_pds","gruene","afd","piraten","fdp","die_partei","freie_wahler","bsw","other")
u$sumsh <- rowSums(u[,pc],na.rm=T)
u$nsh <- rowSums(!is.na(u[,pc]))
u$vv <- u$valid_votes/u$number_voters
s <- u %>% filter(!state %in% c("02","04","11")) %>% group_by(state,election_year) %>% summarise(n=n(),elig=sum(eligible_voters,na.rm=T)/1e3,tot_to=sum(number_voters,na.rm=T)/sum(eligible_voters,na.rm=T),medvv=median(vv,na.rm=T),
 na_el=sum(is.na(eligible_voters)),na_nv=sum(is.na(number_voters)),na_vv=sum(is.na(valid_votes)),na_to=sum(is.na(turnout)),
 gt1=sum(number_voters>eligible_voters,na.rm=T),vgtn=sum(vv<0.99,na.rm=T),vgt_hi=sum(vv>200,na.rm=T), shlow=sum(sumsh<0.97),shhigh=sum(sumsh>1.03),zero=sum(valid_votes==0|eligible_voters==0,na.rm=T),.groups="drop")
print(as.data.frame(s),digits=3)
