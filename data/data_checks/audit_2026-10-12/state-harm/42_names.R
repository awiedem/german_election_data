suppressMessages({library(dplyr)}); options(width=250)
d <- "data/state_elections/final/"
h21 <- readRDS(paste0(d,"state_harm_21.rds")); h25 <- readRDS(paste0(d,"state_harm_25.rds"))
cat("h25 NA ags_name by year:\n"); print(h25 %>% filter(is.na(ags_name)) %>% count(election_year) %>% as.data.frame())
cat("h21 NA ags_name_21 by state-year (top):\n"); print(h21 %>% filter(is.na(ags_name_21)) %>% count(state,election_year,sort=TRUE) %>% head(12) %>% as.data.frame()); cat("h21 NA ags_name_21 by year:\n"); print(h21 %>% filter(is.na(ags_name_21)) %>% count(election_year) %>% as.data.frame())
cat("h25 covariates NA pop where election_year<2024:", sum(is.na(h25$population_ags) & h25$election_year<2024), "\n")
cat("h21 covariates NA pop by year:\n"); print(h21 %>% filter(is.na(population_ags)) %>% count(election_year) %>% as.data.frame())
