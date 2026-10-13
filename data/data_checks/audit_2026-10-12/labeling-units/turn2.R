suppressMessages(library(dplyr))
u <- readRDS("data/federal_elections/municipality_level/final/federal_muni_unharm.rds")
u$t_elig <- u$number_voters/u$eligible_voters; u$t_orig <- u$number_voters_orig/u$eligible_voters_orig; u$t_mix <- u$number_voters/u$eligible_voters_orig
for (nm in c("t_elig","t_orig","t_mix")) cat(nm, "max abs diff to turnout:", max(abs(u[[nm]]-u$turnout), na.rm=TRUE), " rows diff>0.001:", sum(abs(u[[nm]]-u$turnout)>0.001, na.rm=T), "\n")
print(u |> mutate(d=t_elig-turnout) |> filter(abs(d)>0.001) |> summarise(n=n(), med=median(d), p95=quantile(abs(d),.95), p99=quantile(abs(d),.99)))
print(u |> mutate(d=t_elig-turnout) |> filter(abs(d)>0.1) |> select(ags,election_year,eligible_voters,eligible_voters_orig,number_voters,number_voters_orig,turnout,t_elig,unique_mailin) |> head(8))
cat("unique ags-year:", nrow(u), nrow(distinct(u, ags, election_year)), "\n")
s <- readRDS("data/state_elections/final/state_unharm.rds"); s$r <- s$number_voters/s$eligible_voters
x <- s |> filter(is.na(turnout), is.finite(r)); print(summary(x$r)); print(count(x, election_year, state) |> arrange(desc(n)) |> head(8)); print(summary(x[,c("flag_briefwahl_only","flag_naive_turnout_above_1")]))
