suppressMessages(library(dplyr))
mh <- readRDS("data/municipal_elections/final/municipal_harm.rds"); print(mh |> filter(ags=="11000000") |> select(election_year, area, population, area_cw, pop_cw, weights, eligible_voters) |> head(3))
print(summary(mh$population)); print(summary(mh$area))
cw <- readRDS("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds"); print(cw |> filter(ags_25=="11000000", year==2020) |> select(year, area, population))
cw3 <- readRDS("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds"); print(cw3 |> filter(ags_2023=="11000000", year==2020) |> select(year, area, population, employees))
for (n in c("state_harm_21","state_harm_23","state_harm_25")) { s <- readRDS(paste0("data/state_elections/final/",n,".rds")); cat(n, "cov cols:", intersect(c("area_ags","population_ags","employees_ags","pop_density_ags","area","population"), names(s)), "\n"); 
 if ("population_ags" %in% names(s)) print(s |> filter(ags=="11000000", election_year==2016) |> select(area_ags, population_ags, employees_ags, pop_density_ags)) }
mh2 <- readRDS("data/mayoral_elections/final/mayor_panel_harm.rds"); print(names(mh2))
ce <- readRDS("data/county_elections/final/county_elec_harm_21_muni.rds"); print(grep("^(area|pop|emp|weights|.*_cw)", names(ce), value=TRUE))
cy <- readRDS("data/county_elections/final/county_elec_harm_21_cty.rds"); print(grep("^(area|pop|emp|weights|.*_cw)", names(cy), value=TRUE))
