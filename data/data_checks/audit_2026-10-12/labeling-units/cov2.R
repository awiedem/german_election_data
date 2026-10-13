suppressMessages(library(dplyr))
d <- readRDS("data/covars_municipality/final/ags_area_pop_emp.rds")
cat("Inf density:", sum(is.infinite(d$pop_density_ags)), " area==0:", sum(d$area_ags==0), " pop==0:", sum(d$population_ags==0), " NaN:", sum(is.nan(d$pop_density_ags)),"\n")
print(d |> filter(is.infinite(pop_density_ags)|area_ags==0) |> count(year) |> head(40))
print(d |> filter(area_ags==0) |> head(8))
print(d |> filter(is.na(ags_name_21)) |> count(year))
cat("emp NA by year\n"); print(d |> group_by(year) |> summarise(na=mean(is.na(employees_ags))) |> as.data.frame())
print(d |> filter(population_ags==0) |> head())
# key uniqueness
cat("dups:", sum(duplicated(d[,c("ags_21","year")])), "\n")
# pop sum vs Germany total
print(d |> group_by(year) |> summarise(pop=sum(population_ags), area=sum(area_ags)) |> filter(year %in% c(1990,2000,2011,2021)))
