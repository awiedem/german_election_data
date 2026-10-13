suppressMessages(library(dplyr))
d <- readRDS("data/state_elections/final/state_harm_25.rds")
g <- d |> mutate(g=!is.na(gruene)&gruene>0, b=!is.na(b90_gr)&b90_gr>0, bb=!is.na(buendnis_90)&buendnis_90>0, fw=!is.na(freie_wahler)&freie_wahler>0, fw2=!is.na(freiewaehler)&freiewaehler>0)
print(g |> group_by(state_name, election_year) |> summarise(n=n(), gruene=sum(g), b90_gr=sum(b), buendnis_90=sum(bb), fw=sum(fw), freiewaehler=sum(fw2), .groups="drop") |> filter(b90_gr>0|buendnis_90>0|freiewaehler>0) |> as.data.frame())
# both populated in same row?
print(g |> summarise(both_g_b=sum(g&b), both_g_bb=sum(g&bb), both_fw=sum(fw&fw2)))
# do shares include them
d$sumg <- rowSums(d[,c("gruene","b90_gr","buendnis_90")], na.rm=TRUE)
print(d |> filter(election_year<=1994) |> group_by(state_name, election_year) |> summarise(g=mean(gruene,na.rm=T), b=mean(b90_gr,na.rm=T), bb=mean(buendnis_90,na.rm=T), .groups="drop") |> as.data.frame())
