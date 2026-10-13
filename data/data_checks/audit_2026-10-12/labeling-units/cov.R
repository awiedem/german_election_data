suppressMessages(library(dplyr))
for (f in c("data/covars_municipality/final/ags_area_pop_emp.rds","data/covars_municipality/final/ags_area_pop_emp_2023.rds","data/covars_county/final/cty_area_pop_emp.rds")) {
 d <- readRDS(f); cat("\n##", f, "\n"); print(names(d)); print(summary(d[sapply(d,is.numeric)]))
}
d <- readRDS("data/covars_municipality/final/ags_area_pop_emp.rds")
# Berlin 2019 pop and area
print(d |> filter(ags_21=="11000000", year %in% c(2000,2019,2021)))
print(d |> filter(ags_21=="02000000", year %in% c(2000,2019,2021)))
print(d |> filter(ags_21=="09162000", year %in% c(2000,2019,2021)))
d23 <- readRDS("data/covars_municipality/final/ags_area_pop_emp_2023.rds"); print(d23 |> filter(ags_2023=="11000000") |> tail(3)); print(range(d23$year))
c <- readRDS("data/covars_county/final/cty_area_pop_emp.rds"); print(c |> filter(county_code_21 %in% c("11000","09162")) |> tail(4))
