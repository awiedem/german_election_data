suppressMessages(library(dplyr))
d <- readRDS("data/crosswalks/final/ags_crosswalks.rds")
s <- d %>% group_by(ags,year) %>% summarise(s=sum(pop_cw),n=n(),.groups="drop")
b <- s %>% filter(abs(s-1)>1e-6)
print(b %>% count(ags,sort=TRUE) %>% head(30)); cat("distinct ags",n_distinct(b$ags),"\n")
print(table(floor(b$ags/1e6)))
print(summary(b$s)); print(table(b$year))
print(d %>% filter(ags==9472444, year==1990) %>% select(ags,ags_name,ags_21,ags_name_21,pop_cw,area_cw,population,area) )
x <- b %>% arrange(desc(s)) %>% slice(1:1)
# example with moderate
y <- b %>% filter(n<=3) %>% slice(1:3); print(y)
for(i in seq_len(nrow(y))) print(d %>% filter(ags==y$ags[i],year==y$year[i]) %>% select(ags,ags_name,ags_21,ags_name_21,pop_cw,area_cw,population,area))
# duplicates in source key to target
dd <- d %>% group_by(ags,year,ags_21) %>% filter(n()>1); cat("dup rows",nrow(dd),"\n")
# weight sum by ags_21 target: sum of pop_cw*population etc
# zero weights
cat("zero pop_cw rows", sum(d$pop_cw==0), "\n")
