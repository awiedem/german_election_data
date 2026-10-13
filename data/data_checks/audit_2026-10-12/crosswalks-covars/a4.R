suppressMessages({library(dplyr);library(data.table)})
d <- readRDS("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")
dd <- d %>% group_by(ags,year,ags_2023) %>% filter(n()>1) %>% ungroup()
cat("dup rows",nrow(dd),"groups",n_distinct(dd$ags,dd$year,dd$ags_2023),"\n")
print(dd %>% distinct(ags,ags_2023) %>% head(10)); print(table(dd$year))
print(dd %>% arrange(ags,year) %>% head(6) %>% select(ags,ags_name,year,ags_2023,ags_name_23,pop_cw,area_cw,population,area))
cat("zero pop_cw rows:", sum(d$pop_cw==0)," ; rows w/ pop_cw<1e-6:",sum(d$pop_cw<1e-6),"\n")
print(d %>% filter(pop_cw==0) %>% count(year) %>% head(5))
# csv vs rds
for(f in c("ags_crosswalks","ags_1990_to_2023_crosswalk","ags_1990_to_2025_crosswalk","crosswalk_ags_2021_2022_to_2023","crosswalk_ags_2021_to_2023","cty_crosswalks")){
 r <- readRDS(sprintf("data/crosswalks/final/%s.rds",f)); c <- fread(sprintf("data/crosswalks/final/%s.csv",f), colClasses="character")
 cat("\n",f,"rds",nrow(r),ncol(r),"csv",nrow(c),ncol(c),"\n")
 cat(" cols equal:",identical(names(r),names(c)),"\n"); if(!identical(names(r),names(c))) {print(setdiff(names(r),names(c)));print(setdiff(names(c),names(r)))}
 if(nrow(r)==nrow(c)){ for(n in intersect(names(r),names(c))){ a<-r[[n]]; b<-c[[n]]; if(is.numeric(a)){ bb<-as.numeric(b); m<-sum(!(is.na(a)&is.na(bb)) & (is.na(a)!=is.na(bb) | abs(a-bb)>1e-6*pmax(1,abs(a))),na.rm=TRUE)+sum(is.na(a)!=is.na(bb)) } else m<-sum(ifelse(is.na(a),"",a)!=ifelse(is.na(b),"",b)); if(m>0) cat("  diff in",n,":",m,"\n")}}
}
