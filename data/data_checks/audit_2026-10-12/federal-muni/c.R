suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
for(n in c("unharm","harm_21","harm_25","raw")){
u<-readRDS(paste0(d,"federal_muni_",n,".rds"))
cat("\n==",n,"\n")
pc<-intersect(c("cdu","csu","spd","gruene","grune","fdp","linke_pds","afd","die_linke","pds","b90_gr"),names(u))
r<-u|>group_by(election_year)|>summarise(n=n(),elig=sum(eligible_voters,na.rm=T),vot=sum(number_voters,na.rm=T),val=sum(valid_votes,na.rm=T),inv=sum(invalid_votes,na.rm=T),
 across(any_of(c("cdu","csu","spd","afd","fdp")),~sum(.x*number_voters,na.rm=T)),.groups="drop")
print(as.data.frame(r))
}
