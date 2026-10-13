suppressMessages(library(data.table))
chk <- function(base, key) {
 r <- readRDS(paste0(base,".rds")); c <- fread(paste0(base,".csv"), colClasses="character")
 cat("\n",base,"rds",dim(r),"csv",dim(c),"\n")
 cat("names equal:", identical(names(r),names(c)),"\n")
 for (k in key) cat(k, "csv leading-zero-ok:", all(c[[k]]==r[[k]]), " sample:", head(c[[k]],2), "\n")
 num <- names(r)[sapply(r,is.numeric)]
 mx <- sapply(num, function(n) max(abs(as.numeric(c[[n]])-r[[n]]),na.rm=T)); cat("max abs diff numeric:", max(mx), names(which.max(mx)),"\n")
 nadiff <- sapply(num, function(n) sum(is.na(as.numeric(c[[n]]))!=is.na(r[[n]]))); cat("NA pattern diffs:", sum(nadiff),"\n")
}
d <- "data/federal_elections/"
chk(paste0(d,"county_level/final/federal_cty_unharm"), c("ags","state"))
chk(paste0(d,"county_level/final/federal_cty_harm"), c("county_code","state"))
chk(paste0(d,"wahlkreis_level/final/federal_wkr_unharm"), c("wkr_nr","state"))
chk(paste0(d,"wahlkreis_level/final/federal_wkr_2021_on_2025"), c("wkr_nr","state"))
chk(paste0(d,"wahlkreis_level/final/wkr_2021_to_2025_crosswalk"), c("wkr_nr","state"))
