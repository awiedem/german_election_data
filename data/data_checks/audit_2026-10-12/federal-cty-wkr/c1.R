suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
cat("years:\n"); print(table(u$year))
cat("dup key:", sum(duplicated(u[,c("ags","year")])),"\n")
cat("ags nchar:\n"); print(table(nchar(u$ags)))
cat("state mismatch:", sum(substr(u$ags,1,2)!=u$state),"\n")
print(table(u$state, u$year>=1990)[,])
# date vs year
print(u %>% distinct(year, election_date) %>% arrange(year) %>% as.data.frame)
# totals
u$chk1 <- u$eligible_voters - u$number_voters
u$chk2 <- u$number_voters - u$valid_votes - u$invalid_votes
cat("elig<voters:", sum(u$chk1<0,na.rm=T), " valid+inv!=voters:", sum(abs(u$chk2)>0,na.rm=T), " NA:", sum(is.na(u$chk2)),"\n")
print(u %>% filter(chk1<0 | abs(chk2)>0) %>% select(ags,year,eligible_voters,number_voters,valid_votes,invalid_votes) %>% head(30) %>% as.data.frame)
cat("turnout summary by year\n")
print(u %>% group_by(year) %>% summarise(n=n(), tmin=min(turnout,na.rm=T), tmax=max(turnout,na.rm=T), nNAelig=sum(is.na(eligible_voters)|eligible_voters==0)) %>% as.data.frame)
tn <- u$number_voters/u$eligible_voters
cat("turnout recomputed mismatch:", sum(abs(tn-u$turnout)>1e-6,na.rm=T),"\n")
print(u %>% filter(abs(tn-u$turnout)>1e-6) %>% select(ags,year,turnout,number_voters,eligible_voters) %>% head(10) %>% as.data.frame)
