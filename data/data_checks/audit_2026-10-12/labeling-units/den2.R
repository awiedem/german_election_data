suppressMessages(library(dplyr))
f <- list(muni_unharm="data/federal_elections/municipality_level/final/federal_muni_unharm.rds", muni_harm21="data/federal_elections/municipality_level/final/federal_muni_harm_21.rds", muni_harm25="data/federal_elections/municipality_level/final/federal_muni_harm_25.rds", cty_unharm="data/federal_elections/county_level/final/federal_cty_unharm.rds", cty_harm="data/federal_elections/county_level/final/federal_cty_harm.rds")
for (n in names(f)) { d <- readRDS(f[[n]]); y <- if ("election_year" %in% names(d)) d$election_year else d$year
  for (yr in c(2013,2017,2021)) { x <- d[y==yr,]
   cat(sprintf("%-13s %d spd*number_voters=%.0f  spd*valid_votes=%.0f  sum(valid)=%.0f sum(nv)=%.0f  (official SPD 2021 zweit=11955434; 2017=9538367; 2013=11252215)\n", n, yr, sum(x$spd*x$number_voters,na.rm=T), sum(x$spd*x$valid_votes,na.rm=T), sum(x$valid_votes,na.rm=T), sum(x$number_voters,na.rm=T))) } }
