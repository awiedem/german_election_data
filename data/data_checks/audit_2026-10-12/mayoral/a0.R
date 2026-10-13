suppressMessages(library(dplyr))
for(f in c("mayoral_unharm","mayoral_harm","mayoral_candidates","mayor_panel","mayor_panel_harm","mayor_panel_annual","mayor_panel_annual_harm")){
 d<-readRDS(paste0("data/mayoral_elections/final/",f,".rds")); cat("\n==",f,nrow(d),ncol(d),"\n"); str(d,give.attr=FALSE,vec.len=2,width=110)}
