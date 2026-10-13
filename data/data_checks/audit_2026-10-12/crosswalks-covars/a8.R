suppressMessages({library(dplyr)})
c21 <- readRDS("data/crosswalks/final/ags_crosswalks.rds")
m <- c21 %>% group_by(ags,year) %>% filter(n()>1) %>% summarise(n=n(),npop=n_distinct(population),narea=n_distinct(area),nemp=n_distinct(employees),.groups="drop")
cat("multi-target sources:",nrow(m),"; pop varies across rows:",sum(m$npop>1),"; area varies:",sum(m$narea>1),"; emp varies",sum(m$nemp>1),"\n")
ex <- c21 %>% filter(ags==3159016|ags==1053004) %>% head(0)
s <- m %>% filter(npop>1,n==2) %>% slice(1:2); for(i in 1:nrow(s)) print(c21 %>% filter(ags==s$ags[i],year==s$year[i]) %>% select(ags,year,ags_21,pop_cw,area_cw,population,area,employees))
# sum of population*? : is population apportioned by pop_cw?
x <- c21 %>% group_by(ags,year) %>% filter(n()>1,npop<-n_distinct(population)>1) %>% summarise(r=max(population/pop_cw)/min(population/pop_cw),.groups="drop"); print(summary(x$r))
# country totals under two readings
t1 <- c21 %>% group_by(year) %>% summarise(sum_all=sum(population,na.rm=T),sum_dist=sum(distinct(pick(ags,population))$population,na.rm=T)); print(t1[t1$year %in% c(1990,2005,2020),])
# 2024 vs 2023 identical pop in c25
c25 <- readRDS("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds")
a<-readRDS("data/crosswalks/final/crosswalk_ags_2023_to_2025.rds");b<-readRDS("data/crosswalks/final/crosswalk_ags_2024_to_2025.rds")
j<-inner_join(a,b,by=c("ags","ags_25"),suffix=c("_23","_24")); cat("2023 vs 2024 crosswalk pop identical:",mean(j$population_23==j$population_24,na.rm=T)," area identical:",mean(j$area_23==j$area_24,na.rm=T),"\n")
