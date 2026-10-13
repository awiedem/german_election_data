suppressMessages(library(dplyr))
u<-readRDS("data/european_elections/final/european_muni_unharm.rds")
h<-readRDS("data/european_elections/final/european_muni_harm.rds")
pc<-names(u)[16:85]; pc<-setdiff(pc,c("turnout"))
pc<-names(u)[which(names(u)=="abg"):which(names(u)=="x50plus")]
for(d in list(u,h)){
 t<-d %>% group_by(election_year) %>% summarise(across(all_of(pc),~sum(.x,na.rm=T)))
 tt<-as.data.frame(t(t[-1])); names(tt)<-t$election_year; print(tt[rowSums(tt)>0,]); cat("\n")
}
# NA counts per party/year
print(u %>% group_by(election_year) %>% summarise(across(all_of(pc),~mean(is.na(.x)))) %>% t() %>% as.data.frame() %>% filter(V1>0|V2>0|V3>0|V4>0))
