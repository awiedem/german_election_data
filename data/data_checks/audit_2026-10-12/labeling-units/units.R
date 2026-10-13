suppressMessages(library(dplyr))
ds <- c("county_elec_unharm","county_elec_harm_21_muni","county_elec_harm_21_cty","european_muni_unharm","european_muni_harm","federal_cty_unharm","federal_cty_harm","federal_muni_raw","federal_muni_unharm","federal_muni_harm_21","federal_muni_harm_25","federal_wkr_unharm","federal_wkr_2021_on_2025","municipal_unharm","municipal_harm","municipal_harm_25","state_unharm","state_harm","state_harm_21","state_harm_23","state_harm_25","ltw_wkr_unharm")
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
cnt <- "eligible|number_voters|valid_votes|invalid|voters|weight|^pop|^area|population|employees|seats|year|total_votes|n_pred|^weights|cw$|_orig$|blocked|unique|mail"
for (n in ds) { d <- readRDS(inv$path[inv$ds==n]); num <- names(d)[sapply(d,is.numeric)]; num <- num[!grepl(cnt,num)]
  mx <- sapply(num, function(c) suppressWarnings(max(d[[c]], na.rm=TRUE))); mn <- sapply(num, function(c) suppressWarnings(min(d[[c]], na.rm=TRUE)))
  hi <- num[mx>1.0001 & is.finite(mx)]; lo <- num[mn< -1e-9 & is.finite(mn)]
  cat(sprintf("%-26s numeric share-like cols=%d; >1: %s ; <0: %s\n", n, length(num), paste0(hi,"(",round(mx[hi],2),")",collapse=","), paste0(lo,"(",round(mn[lo],3),")",collapse=","))) }
