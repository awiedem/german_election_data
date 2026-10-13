suppressMessages(library(dplyr))
c <- readRDS("data/federal_elections/county_level/final/federal_cty_unharm.rds")
print(names(c)[!names(c)%in%names(readRDS("data/federal_elections/county_level/final/federal_cty_harm.rds"))][1:10])
print(table(c$flag_briefwahl_agg, useNA="a")); print(c |> filter(flag_briefwahl_agg==1) |> count(year)); print(c |> filter(grepl("999$",ags)) |> count(year, flag_briefwahl_agg))
print(c |> filter(flag_briefwahl_agg==1) |> summarise(elig0=sum(eligible_voters==0, na.rm=T), n=n(), nv=sum(number_voters, na.rm=TRUE)))
# flag typed values
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
for (i in seq_len(nrow(inv))) { d <- readRDS(inv$path[i]); fc <- grep("^flag_", names(d), value=TRUE)
 for (f in fc) { v <- d[[f]]; u <- unique(v); u <- u[!is.na(u)]; cat(sprintf("%-26s %-34s %-9s vals=%s NA=%d\n", inv$ds[i], f, class(v)[1], paste(head(sort(u),5), collapse="/"), sum(is.na(v)))) } }
