suppressMessages({library(dplyr)})
p <- function(x) sprintf("%08d", as.integer(x))
c21 <- readRDS("data/crosswalks/final/ags_crosswalks.rds") %>% mutate(ags=p(ags))
c23 <- readRDS("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")
c25 <- readRDS("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds")
for(y in c(1990,2005,2015,2020)){
 a<-unique(c21$ags[c21$year==y]);b<-unique(c23$ags[c23$year==y]);c<-unique(c25$ags[c25$year==y])
 cat(y,"c21",length(a),"c23",length(b),"c25",length(c),"; in c23 not c21:",length(setdiff(b,a)),"; in c25 not c21:",length(setdiff(c,a)),"; in c21 not c25:",length(setdiff(a,c)),"\n")
 if(y==1990){ e<-setdiff(c,a); print(head(c25 %>% filter(year==y,ags %in% e) %>% select(ags,ags_name,ags_25,pop_cw,population,area),12)); print(table(substr(e,1,2)))}
}
# total population by year in each crosswalk (thousands?)
tot <- function(d,nm) d %>% distinct(ags,year,population,area) %>% group_by(year) %>% summarise(pop=sum(population,na.rm=T),area=sum(area,na.rm=T),n=n()) %>% mutate(src=nm)
r <- bind_rows(tot(c21,"c21"),tot(c23,"c23"),tot(c25,"c25")); print(as.data.frame(r %>% filter(year %in% c(1990,1995,2000,2005,2010,2015,2020,2021,2022,2023,2024)) %>% arrange(year,src)))
