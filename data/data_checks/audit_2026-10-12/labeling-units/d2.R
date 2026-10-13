suppressMessages(library(dplyr))
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
for (i in seq_len(nrow(inv))) { d <- readRDS(inv$path[i]); if ("state_name" %in% names(d)) { s <- unique(d$state_name); cat(inv$ds[i], ": ", paste(intersect(s, c("Bavaria","Bayern","Hesse","Hessen","Thuringia","Thüringen","Saxony","Sachsen")), collapse="/"), " nuniq=", length(s), "\n") } }
mh <- readRDS("data/municipal_elections/final/municipal_harm.rds")
print(mh |> group_by(election_year) |> summarise(n=n(), na_date=sum(is.na(election_date))) |> as.data.frame())
mu <- readRDS("data/municipal_elections/final/municipal_unharm.rds"); print(mu |> filter(is.na(election_date)) |> select(ags,election_year,state_name))
sh <- readRDS("data/state_elections/final/state_harm.rds"); print(sh |> group_by(election_year) |> summarise(n=n(), na_date=sum(is.na(election_date)), na_ags=sum(is.na(ags))) |> as.data.frame() |> head(70))
mp <- readRDS("data/mayoral_elections/final/mayoral_unharm.rds"); print(mp |> filter(as.integer(format(election_date,"%Y"))!=election_year) |> select(ags,election_year,election_date,round,state_name))
