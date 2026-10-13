r<-function(p)readRDS(p)
for(p in c("federal_elections/county_level/final/federal_cty_harm","state_elections/final/state_unharm","county_elections/final/county_elec_unharm"))print(grep("incog|incon",names(r(paste0("data/",p,".rds"))),value=T))
f<-list(mh="municipal_elections/final/municipal_harm",mh25="municipal_elections/final/municipal_harm_25",ceu="county_elections/final/county_elec_unharm",ls="landrat_elections/final/landrat_unharm",mu="municipal_elections/final/municipal_unharm",ma="mayoral_elections/final/mayor_panel_annual",mayh="mayoral_elections/final/mayoral_harm",mayu="mayoral_elections/final/mayoral_unharm")
for(n in names(f)){x<-r(paste0("data/",f[[n]],".rds"));cat(n,dim(x),"\n");if("state_name"%in%names(x))cat(" ",head(unique(x$state_name),3),"\n")}
x<-r("data/federal_elections/municipality_level/final/federal_muni_harm_25.rds");cat(sum(is.na(x$ags_25)),nrow(x),"\n")
x<-r("data/mayoral_elections/final/mayoral_harm.rds");cat("mayoral_harm ags_21 NA",sum(is.na(x$ags_21)),"\n")
x<-r("data/municipal_elections/final/municipal_harm_25.rds");cat(sum(is.na(x$ags_25)),"\n")
w<-r("data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.rds");for(f in c("flag_no_valid_votes","flag_naive_turnout_above_1"))if(f%in%names(w))print(table(w[[f]],useNA="a"))
