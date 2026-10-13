suppressMessages(library(dplyr));setwd("/Users/hanno/Documents/GitHub/german_election_data")
fu<-readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
x<-fu%>%mutate(nt=number_voters/eligible_voters_orig, ct=turnout)
print(as.data.frame(x%>%group_by(election_year)%>%summarise(n_nt_gt1=sum(nt>1,na.rm=T),n_nt_gt1.2=sum(nt>1.2,na.rm=T),n_turn_eq1=sum(turnout>=0.9999,na.rm=T),n_turnNA=sum(is.na(turnout)))))
print(as.data.frame(x%>%filter(nt>1.5)%>%select(election_year,ags,ags_name,eligible_voters_orig,eligible_voters,number_voters_orig,number_voters,turnout,unique_mailin)%>%arrange(desc(number_voters/eligible_voters_orig))%>%head(12)))
cat("rows nt>1.5:",sum(x$nt>1.5,na.rm=T),"\n")
# does turnout column = number_voters/eligible_voters (alloc) or /orig?
print(summary(abs(x$turnout - x$number_voters/x$eligible_voters)),digits=3);print(summary(abs(x$turnout-x$number_voters/x$eligible_voters_orig)))
