suppressMessages({library(dplyr)}); options(width=250)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds")); du <- u %>% distinct(state,election_year,election_date) %>% group_by(state,election_year) %>% slice(1) %>% ungroup()
for (n in c("21","23","25")) { h <- readRDS(sprintf("%sstate_harm_%s.rds",d,n)); dh <- h %>% distinct(state,election_year,election_date)
  m <- inner_join(dh, du, by=c("state","election_year"), suffix=c("_h","_u")); cat(n,": date mismatches:",sum(m$election_date_h!=m$election_date_u),"; NA ags_name:", sum(is.na(h[[if(n=="21") "ags_name_21" else "ags_name"]])), "; state_name table:", paste(unique(paste(h$state,h$state_name)),collapse="|"),"\n")
  cat(" row counts: ", nrow(h), "\n") }
cat("unharm rows", nrow(u), "\n")
h <- readRDS(paste0(d,"state_harm_25.rds")); cat("harm25 year class:", class(h$election_year), "; unharm:", class(u$election_year), "\n")
cat("harm25 flag_unsuccessful_naive_merge table:\n"); print(table(h$flag_unsuccessful_naive_merge, useNA="ifany"))
print(h %>% filter(flag_unsuccessful_naive_merge==1) %>% count(state,election_year) %>% head(10))
