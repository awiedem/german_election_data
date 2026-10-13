for (f in c("county_level/final/federal_cty_unharm","county_level/final/federal_cty_harm","wahlkreis_level/final/federal_wkr_unharm","wahlkreis_level/final/federal_wkr_unharm_long","wahlkreis_level/final/federal_wkr_2021_on_2025","wahlkreis_level/final/wkr_2021_to_2025_crosswalk")) {
 d <- readRDS(paste0("data/federal_elections/",f,".rds")); cat("\n==",f,dim(d),"\n"); print(names(d)); str(d[,1:min(8,ncol(d))]); print(head(as.data.frame(d[,1:min(8,ncol(d))]),3))
}
