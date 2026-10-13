suppressMessages({library(dplyr)})
d<-"data/crosswalks/final/"; r<-function(f)readRDS(paste0(d,f,".rds"))
c25<-r("ags_1990_to_2025_crosswalk"); a<-r("crosswalk_ags_2024_to_2025"); b<-r("crosswalk_ags_2023_24_to_2025")
cat("c25 targets",n_distinct(c25$ags_25),"2024->25 targets",n_distinct(a$ags_25),"c25 targets not in 24->25:",length(setdiff(c25$ags_25,a$ags_25)),"\n")
# c25 year 2024: same as a?
x<-c25 %>% filter(year==2024); cat("c25 yr2024 rows",nrow(x),"vs a",nrow(a),"; c25 2023 vs",nrow(r("crosswalk_ags_2023_to_2025")),"\n")
# 2021_to_2023 dup + sums
w<-r("crosswalk_ags_2021_to_2023"); print(w[duplicated(w[,1:2])|duplicated(w[,1:2],fromLast=TRUE),]); print(w %>% filter(ags_2021 %in% c("09374451","16064073")))
w2<-r("crosswalk_ags_2021_2022_to_2023"); print(w2[duplicated(w2[,c("ags","year","ags_2023")])|duplicated(w2[,c("ags","year","ags_2023")],fromLast=TRUE),])
# names per code consistency: ags_name_21 in crosswalks vs covars
c21<-r("ags_crosswalks"); cov<-readRDS("data/covars_municipality/final/ags_area_pop_emp.rds")
nm1<-c21 %>% distinct(ags_21,ags_name_21); cat("ags_21 with >1 name in crosswalk:",sum(duplicated(nm1$ags_21)),"\n")
nm2<-cov %>% distinct(ags_21,ags_name_21); cat("ags_21 with >1 name in covars:",sum(duplicated(nm2$ags_21)),"\n")
j<-inner_join(nm1 %>% distinct(ags_21,.keep_all=T),nm2,by="ags_21",suffix=c("_cw","_cov")); cat("name mismatch crosswalk vs covars:",sum(j$ags_name_21_cw!=j$ags_name_21_cov),"\n"); print(head(j[j$ags_name_21_cw!=j$ags_name_21_cov,],5))
# name casing in source names (uppercase early)
cat("uppercase ags_name rows by year:\n"); print(table(c21$year[c21$ags_name==toupper(c21$ags_name)])[c("1990","1991","1992","1993")])
# c21 ags numeric 7-digit
cat("c21 ags numeric class:",class(c21$ags),"; 7-digit rows",sum(c21$ags<1e7),"\n")
# cty crosswalk
cc<-r("cty_crosswalks"); cat("cty years",range(cc$year),"; n counties/yr 1990,2020:",n_distinct(cc$county_code[cc$year==1990]),n_distinct(cc$county_code[cc$year==2020]),"; targets",n_distinct(cc$county_code_21),"\n")
cty<-readRDS("data/covars_county/final/cty_area_pop_emp.rds"); cat("cty targets not in covars:",length(setdiff(cc$county_code_21,cty$county_code_21)),"\n")
cat("county ags prefix mismatch state:",sum(substr(cc$county_code,1,2)!=substr(cc$county_code_21,1,2)),"\n")
# emp_cw zeros / pop sum per target? NA in 1997+
print(c21 %>% filter(year>=1997) %>% summarise(na=sum(is.na(emp_cw)), n=n()))
print(c21 %>% filter(year>=1997,is.na(emp_cw)) %>% distinct(ags,year) %>% count(year) %>% head(3))
# county crosswalk bad emp
