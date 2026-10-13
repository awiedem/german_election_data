suppressMessages(library(dplyr))
for (n in c("state_harm","state_harm_21","state_harm_23","state_harm_25","state_unharm")) { d <- readRDS(file.path("data/state_elections/final", paste0(n,".rds")))
 d <- d[!is.na(d$ags),]
 cat(n, nrow(d), " elig==0:", sum(d$eligible_voters==0,na.rm=T), " nv==0:", sum(d$number_voters==0,na.rm=T), " nvNA:", sum(is.na(d$number_voters)), " valid==0:", sum(d$valid_votes==0,na.rm=T), " turnout NA:", sum(is.na(d$turnout)), " turn>1:", sum(d$turnout>1,na.rm=T), "\n")
 print(d |> group_by(election_year) |> summarise(nv0=mean(number_voters==0, na.rm=TRUE), e0=mean(eligible_voters==0,na.rm=T)) |> filter(nv0>0.5|e0>0.5) |> as.data.frame() |> head(30))
}
