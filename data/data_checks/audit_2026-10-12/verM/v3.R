suppressMessages(library(dplyr))
u <- readRDS("data/municipal_elections/final/municipal_unharm.rds")
h <- readRDS("data/municipal_elections/final/municipal_harm.rds")
h25 <- readRDS("data/municipal_elections/final/municipal_harm_25.rds")
u$st <- substr(u$ags,1,2)
# 05
by <- u %>% filter(st=="09") %>% mutate(vv=valid_votes/number_voters)
print(by %>% group_by(election_year) %>% summarise(n=n(),med=median(vv,na.rm=T)))
print(u %>% filter(ags=="09162000") %>% select(election_year,eligible_voters,number_voters,valid_votes))
# 06
print(u %>% filter(st=="01") %>% mutate(sz=eligible_voters<1000) %>% group_by(election_year,sz) %>% summarise(n=n(),cdu=mean(!is.na(cdu_csu)),spd=mean(!is.na(spd)),.groups="drop") %>% filter(election_year>=2013))
# 07
cat("harm ags_21 NA:",sum(is.na(h$ags_21)),"weights NA",sum(is.na(h$weights)),"\n")
print(h %>% summarise(across(c(area_cw,pop_cw,area,population),~sum(is.na(.)))))
print(grep("ags_|name",names(h25),value=T))
cat("h25 ags_25 NA", sum(is.na(h25$ags_25)), "\n")
# 08
cat("h date NA",sum(is.na(h$election_date)),"of",nrow(h),"; h25",sum(is.na(h25$election_date)),"of",nrow(h25),"; u",sum(is.na(u$election_date)),"\n")
print(h %>% group_by(election_year) %>% summarise(nn=sum(!is.na(election_date)),n=n()))
# 09
x <- u %>% filter(ags %in% c("01051053","01053027","01056047","01053117","01053036")) %>% select(ags,election_year,eligible_voters,number_voters)
print(x %>% arrange(ags,election_year) %>% as.data.frame())
j <- u %>% arrange(ags,election_year) %>% group_by(ags) %>% mutate(r=eligible_voters/lag(eligible_voters)) %>% ungroup() %>% filter(!is.na(r), r>2.5|r<0.4)
print(table(j$st,j$election_year))
# 10
print(u %>% filter(ags %in% c("09473159","09373155","09376147","09274145","09181127","09372112","09181131","09372135"), election_year %in% c(2014,2020,2026)) %>% select(ags,election_year,eligible_voters,number_voters,turnout) %>% arrange(ags,election_year) %>% as.data.frame())
# 11
print(u %>% filter(st=="15", election_year>=2019) %>% count(election_year,election_date))
print(u %>% filter(st=="15", is.na(election_date)) %>% select(ags,ags_name,election_year))
print(u %>% filter(election_year==2024) %>% count(st,election_date))
# 12/13
print(table(u$election_type,useNA="a"))
print(table(u$state_name,u$st))
# 14
print(u %>% filter(st %in% c("07","03")) %>% group_by(st) %>% summarise(maxy=max(election_year)))
print(range(h$election_year)); print(range(h25$election_year)); print(sum(h25$election_year<1990))
# 15
s <- u
parties <- c("cdu_csu","spd","linke_pds","gruene","afd","piraten","fdp","die_partei","freie_wahler","bsw")
cnt <- 0
for(p in parties){ k <- sum(s[[paste0("seats_",p)]]>0 & is.na(s[[p]]),na.rm=T); cat(p,k,"\n"); cnt<-cnt+k}
cat("tot",cnt,"\n")
