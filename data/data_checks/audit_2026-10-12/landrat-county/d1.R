suppressMessages(library(dplyr)); options(width=250,tibble.width=Inf)
setwd("/Users/hanno/Documents/GitHub/german_election_data")
h <- as_tibble(readRDS("data/county_elections/final/county_elec_harm_21_muni.rds"))
hc <- as_tibble(readRDS("data/county_elections/final/county_elec_harm_21_cty.rds"))
# muni-> cty aggregation
a <- h%>%group_by(county,election_year)%>%summarise(ev=sum(eligible_voters,na.rm=T),nv=sum(number_voters,na.rm=T),vv=sum(valid_votes,na.rm=T),iv=sum(invalid_votes,na.rm=T),n=n(),.groups="drop")
m <- hc%>%select(county=county_code,election_year,state,ev_c=eligible_voters,nv_c=number_voters,vv_c=valid_votes,iv_c=invalid_votes,turnout_c=turnout)%>%inner_join(a,by=c("county","election_year"))
cat("matched county-years:",nrow(m),"of cty",nrow(hc),"muni-counties",nrow(a),"\n")
d <- m%>%mutate(dvv=vv_c/vv-1,dev=ev_c/ev-1)%>%filter(abs(dvv)>0.001|abs(dev)>0.001); print(nrow(d)); print(d%>%arrange(-abs(dvv))%>%head(30))
print(hc%>%anti_join(a,by=c("county_code"="county","election_year"))%>%count(state,election_year))
print(a%>%anti_join(hc,by=c("county"="county_code","election_year"))%>%count(election_year))
# cty arithmetic
print(hc%>%filter(number_voters>eligible_voters|valid_votes>number_voters)%>%count(state))
print(summary(hc$turnout)); print(hc%>%filter(turnout>1|turnout<0.2)%>%select(county_code,election_year,turnout))
# shares sum
nm <- names(hc); meta <- c("county_code","election_year","state","eligible_voters","number_voters","valid_votes","invalid_votes","turnout","area","population")
pc <- setdiff(nm,c(names(hc)[!sapply(hc,is.numeric)],meta,grep("^flag",nm,value=T),c("waehlergruppen","einzelbewerber","other")))
M <- as.matrix(hc[pc]); s <- rowSums(M,na.rm=T)+rowSums(hc[intersect(c("waehlergruppen","einzelbewerber","other"),nm)],na.rm=T)
hc$s <- s; print(hc%>%group_by(state)%>%summarise(min=min(s),max=max(s),dev=sum(abs(s-1)>0.01)))
print(intersect(c("waehlergruppen","einzelbewerber","other"),nm))
# harm muni
nm <- names(h); pcm <- setdiff(nm,c(names(h)[!sapply(h,is.numeric)],"ags","ags_name_21","county","election_year","state","eligible_voters","number_voters","valid_votes","invalid_votes","turnout","area_ags","population_ags","employees_ags","pop_density_ags","far_right","far_left","far_left_w_linke","total_vote_share","perc_total_votes_incogruence",grep("^flag",nm,value=T),"waehlergruppen","einzelbewerber","other"))
M <- as.matrix(h[pcm]); h$s <- rowSums(M,na.rm=T)+rowSums(h[intersect(c("waehlergruppen","einzelbewerber","other"),nm)],na.rm=T)
print(h%>%filter(valid_votes>0)%>%group_by(state)%>%summarise(n=n(),min=min(s),max=max(s),dev=sum(abs(s-1)>0.01)))
print(summary(h$total_vote_share)); print(summary(h$perc_total_votes_incogruence))
for (f in grep("^flag",nm,value=T)) {cat(f,"\n");print(table(h[[f]],useNA="a"))}
print(h%>%filter(valid_votes==0 | is.na(valid_votes))%>%count(state,election_year))
