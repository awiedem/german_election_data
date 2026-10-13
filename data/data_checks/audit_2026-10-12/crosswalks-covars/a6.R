suppressMessages({library(dplyr);library(tidyr)})
p <- function(x) sprintf("%08d", as.integer(x))
c21 <- readRDS("data/crosswalks/final/ags_crosswalks.rds") %>% mutate(ags=p(ags))
c23 <- readRDS("data/crosswalks/final/ags_1990_to_2023_crosswalk.rds")
c25 <- readRDS("data/crosswalks/final/ags_1990_to_2025_crosswalk.rds")
cat("years:", range(c21$year), range(c23$year), range(c25$year), "\n")
cat("n source ags per year (c21/c23/c25) selected:\n"); print(rbind(c21=table(c21$year)[c("1990","2000","2010","2020")], c23=table(c23$year)[c("1990","2000","2010","2020")], c25=table(c25$year)[c("1990","2000","2010","2020")]))
print(table(c23$year)[c("2021","2022","2023")]); print(table(c25$year)[c("2021","2022","2023","2024","2025")])
# target validity: 2021 targets should exist as source in own latest year
a21 <- readRDS("data/covars_municipality/final/ags_area_pop_emp.rds"); cat("covar cols", names(a21), "\n")
t21 <- unique(c21$ags_21); cat("distinct ags_21",length(t21),"\n")
x23 <- readRDS("data/covars_municipality/final/ags_area_pop_emp_2023.rds"); cat("cov23 cols", names(x23), range(x23$year),"\n")
cat("ags_21 targets not in covars 2021:", sum(!t21 %in% a21$ags_21[a21$year==2021]), " covars-2021 not in targets:", sum(!a21$ags_21[a21$year==2021] %in% t21),"\n")
t23 <- unique(c23$ags_2023); cat("distinct ags_2023",length(t23), "not in cov23 2023:", sum(!t23 %in% x23[[1]][x23$year==2023]), "\n")
t25 <- unique(c25$ags_25); cat("distinct ags_25",length(t25),"\n")
print(table(substr(t21,1,2))); 
# state prefix consistency source vs target
for(nm in list(list("c21",c21,"ags","ags_21"),list("c23",c23,"ags","ags_2023"),list("c25",c25,"ags","ags_25"))){d<-nm[[2]]; cat(nm[[1]],"state-prefix mismatch src/tgt:",sum(substr(d[[nm[[3]]]],1,2)!=substr(d[[nm[[4]]]],1,2)),"\n"); print(head(d[substr(d[[nm[[3]]]],1,2)!=substr(d[[nm[[4]]]],1,2),c(nm[[3]],"year",nm[[4]])],5))}
# chain consistency: 2020 source ags in 2020, mapping 21 vs 23 vs 25 for unchanged munis
j <- c21 %>% filter(year==2020) %>% select(ags,ags_21,pop_cw) %>% inner_join(c23 %>% filter(year==2020) %>% select(ags,ags_2023,pop23=pop_cw),by="ags",relationship="many-to-many") %>% inner_join(c25 %>% filter(year==2020) %>% select(ags,ags_25,pop25=pop_cw),by="ags",relationship="many-to-many")
u <- j %>% group_by(ags) %>% filter(n()==1) %>% ungroup(); cat("unique-path 2020 ags:",nrow(u),"; ags_21!=ags_2023:",sum(u$ags_21!=u$ags_2023),"; ags_2023!=ags_25:",sum(u$ags_2023!=u$ags_25),"\n")
print(head(u %>% filter(ags_21!=ags_2023),8)); print(head(u %>% filter(ags_2023!=ags_25),8))
# for all years: share of ags with identity mapping
for(y in c(1990,2000,2010,2020)) cat(y,"ags in c21 not in c23:",sum(!unique(c21$ags[c21$year==y]) %in% c23$ags[c23$year==y]),"; not in c25:",sum(!unique(c21$ags[c21$year==y]) %in% c25$ags[c25$year==y]),"\n")
# c21 vs c23 vs c25 for multi-year source: differences in number of targets
