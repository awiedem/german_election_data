suppressMessages(library(dplyr))
for (n in c("state_harm_21","state_harm_23","state_harm_25")) { s <- readRDS(paste0("data/state_elections/final/",n,".rds"))
 s$ratio <- s$pop_density_ags/(s$population_ags/s$area_ags)
 cat(n, "pop_density median:", median(s$pop_density_ags,na.rm=T), " max:", max(s$pop_density_ags,na.rm=T), " n nonNA:", sum(!is.na(s$pop_density_ags)), " ratio dens/(pop/area) median:", median(s$ratio,na.rm=T), " rows with ratio<0.01:", sum(s$ratio<0.01, na.rm=T), "\n") }
d <- readRDS("data/covars_municipality/final/ags_area_pop_emp.rds"); cat("covar: ratio median", median(d$pop_density_ags/(d$population_ags/d$area_ags), na.rm=T), "\n")
s <- readRDS("data/state_elections/final/state_harm_25.rds"); x<-s |> mutate(ratio=pop_density_ags/(population_ags/area_ags)) |> group_by(election_year) |> summarise(med=median(ratio,na.rm=T), n=sum(!is.na(ratio))); print(as.data.frame(x))
