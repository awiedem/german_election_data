suppressMessages({library(dplyr);library(tidyr)})
cty <- readRDS("data/covars_county/final/cty_area_pop_emp.rds"); m <- readRDS("data/covars_municipality/final/ags_area_pop_emp.rds"); x23<-readRDS("data/covars_municipality/final/ags_area_pop_emp_2023.rds")
print(cty %>% filter(is.na(county_name_21)) %>% count(year))
print(cty %>% filter(county_code_21=="11000") %>% select(year,population_cty,area_cty,employees_cty,pop_density_cty) %>% head(34) %>% as.data.frame)
# yoy jumps muni (>50%) in pop, area
yo <- function(d,k,v,nm){ d %>% arrange(.data[[k]],year) %>% group_by(.data[[k]]) %>% mutate(prev=lag(.data[[v]]),r=.data[[v]]/prev) %>% ungroup() %>% filter(prev>0,.data[[v]]>0,(r>1.5|r<0.5)) %>% mutate(var=nm) }
j <- bind_rows(yo(m,"ags_21","population_ags","pop"),yo(m,"ags_21","area_ags","area"),yo(m,"ags_21","employees_ags","emp"))
print(j %>% count(var,year) %>% as.data.frame)
print(j %>% filter(var=="pop") %>% arrange(desc(abs(log(r)))) %>% select(ags_21,ags_name_21,year,prev,population_ags,r) %>% head(15))
print(j %>% filter(var=="area") %>% arrange(desc(abs(log(r)))) %>% select(ags_21,ags_name_21,year,prev,area_ags,r) %>% head(8))
# big pop with pop>0 : zero pop and which
z <- m %>% filter(population_ags==0) %>% distinct(ags_21,ags_name_21) ; cat("zero pop munis:",nrow(z),"\n"); print(table(substr(z$ags_21,1,2))); print(head(z,5)); print(table(substr(z$ags_21,6,8))[1:6])
print(m %>% filter(is.infinite(pop_density_ags)) %>% count(year)); print(m %>% filter(area_ags==0) %>% head(3))
print(m %>% filter(population_ags>0) %>% group_by(year) %>% summarise(maxdens=max(pop_density_ags)) %>% filter(year %in% c(1990,2000,2020)))
print(m %>% filter(pop_density_ags>8000) %>% distinct(ags_21,ags_name_21) %>% head(10))
# zero-pop with positive pop other years? 
zz <- m %>% group_by(ags_21) %>% summarise(np0=sum(population_ags==0),n=n()) %>% filter(np0>0,np0<n); cat("munis with zero pop in some but not all years:",nrow(zz),"\n"); print(head(zz,10))
# 2023 file NA density
print(x23 %>% filter(is.na(pop_density_ags)) %>% select(ags_2023,ags_name_23,year,area_ags,population_ags))
