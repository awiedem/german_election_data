suppressMessages({library(dplyr);library(tidyr)})
cty <- readRDS("data/covars_county/final/cty_area_pop_emp.rds"); m <- readRDS("data/covars_municipality/final/ags_area_pop_emp.rds")
mc <- m %>% mutate(county_code_21=substr(ags_21,1,5)) %>% group_by(county_code_21,year) %>% summarise(pop_m=sum(population_ags),area_m=sum(area_ags),emp_m=sum(employees_ags,na.rm=T),.groups="drop")
j <- inner_join(cty,mc,by=c("county_code_21","year")) %>% mutate(dpop=population_cty-pop_m,darea=area_cty-area_m,demp=employees_cty-emp_m)
cat("county vs sum-of-muni mismatches (|dpop|>1k? i.e. >1 unit =1000 ppl):\n")
print(j %>% filter(abs(dpop)>1) %>% count(year)); 
print(j %>% filter(year==1991,abs(dpop)>1) %>% arrange(desc(abs(dpop))) %>% select(county_code_21,county_name_21,population_cty,pop_m,dpop) %>% head(10))
print(j %>% filter(year %in% c(2017,2020),abs(darea)>1) %>% arrange(desc(abs(darea))) %>% select(county_code_21,county_name_21,year,area_cty,area_m,darea) %>% head(10))
print(j %>% filter(year %in% c(2016,2017,2018,2020),abs(dpop)>5) %>% arrange(desc(abs(dpop))) %>% select(county_code_21,county_name_21,year,population_cty,pop_m,dpop) %>% head(12))
print(j %>% filter(abs(demp)>1) %>% count(year))
print(cty %>% filter(is.na(county_name_21)) %>% distinct(county_code_21))
# muni 2018 area
a <- m %>% filter(year %in% c(2017,2018,2019)) %>% select(ags_21,ags_name_21,year,area_ags) %>% pivot_wider(names_from=year,values_from=area_ags,names_prefix="y")
print(a %>% mutate(d=y2018-y2017) %>% filter(abs(d)>0.5) %>% arrange(d) %>% head(15)); cat("n changed:",sum(abs(a$y2018-a$y2017)>0.5),"\n")
