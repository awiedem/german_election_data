suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
u<-readRDS(paste0(d,"federal_muni_unharm.rds"))
nm<-names(u); i1<-which(nm=="afd");i2<-which(nm=="dsu"); i3<-which(nm=="dkp");i4<-which(nm=="bsa")
cat("order afd..dsu:",nm[i1:i2],"\n"); cat("dkp..bsa:",nm[i3:i4],"\n")
e<-u|>mutate(cc=ifelse(is.na(cdu)&is.na(csu),NA,rowSums(cbind(cdu,csu),na.rm=T)),d=cdu_csu-cc)
print(e|>group_by(election_year)|>summarise(nbad=sum(abs(d)>1e-6,na.rm=T),nNA=sum(is.na(cdu_csu)),maxd=max(abs(d),na.rm=T))|>as.data.frame())
fr<-rowSums(u[,nm[i1:i2]],na.rm=T); print(summary(u$far_right-fr))
fl<-rowSums(u[,nm[i3:i4]],na.rm=T); print(summary(u$far_left-fl))
print(summary(u$far_left_w_linke-(u$far_left+ifelse(is.na(u$linke_pds),0,u$linke_pds))))
# far_right by year nat'l
print(u|>group_by(election_year)|>summarise(fr=sum(far_right*number_voters,na.rm=T)/sum(number_voters),afd=sum(afd*number_voters,na.rm=T)/sum(number_voters),npd=sum(npd*number_voters,na.rm=T)/sum(number_voters),rep=sum(rep*number_voters,na.rm=T)/sum(number_voters),fl=sum(far_left*number_voters,na.rm=T)/sum(number_voters))|>as.data.frame())
