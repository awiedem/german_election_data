suppressMessages(library(dplyr))
s <- readRDS("data/state_elections/final/state_harm.rds")
for (f in c("flag_briefwahl_only","flag_naive_turnout_above_1","flag_no_valid_votes","flag_unsuccessful_naive_merge","flag_total_votes_incongruent")) { v <- s[[f]]; cat(f, "non-0/1 values:", sum(!is.na(v) & !(v %in% c(0,1))), " ones:", sum(v==1,na.rm=T), "\n") }
print(s |> filter(!is.na(flag_naive_turnout_above_1), !(flag_naive_turnout_above_1 %in% c(0,1))) |> select(ags, election_year, turnout, flag_naive_turnout_above_1) |> head(5))
u <- readRDS("data/state_elections/final/state_unharm.rds"); print(table(u$flag_briefwahl_only, useNA="a")); print(sum(u$eligible_voters==0 & u$valid_votes>0, na.rm=T)); print(sum(is.na(u$eligible_voters)&u$valid_votes>0,na.rm=T))
