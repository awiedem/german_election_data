suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
for(n in c("unharm","harm_21","harm_25")){
u<-readRDS(paste0(d,"federal_muni_",n,".rds")); nm<-names(u)
i1<-which(nm=="cdu"); i2<-which(nm=="zentrum"); parts<-setdiff(nm[i1:i2],"cdu_csu")
cat("\n==",n,"party cols",length(parts),"last:",nm[i2],"cols after zentrum:",nm[(i2+1):length(nm)],"\n")
ps<-rowSums(u[,parts],na.rm=T)
den<-if(n=="unharm") u$valid_votes/u$number_voters else 1
r<-ps-den
u$r<-r
print(u|>filter(valid_votes>0)|>group_by(election_year)|>summarise(med=median(r),n_over=sum(r>0.005),n_under=sum(r< -0.05),max=max(r),min=min(r))|>as.data.frame())
cc<-u$cdu_csu-rowSums(cbind(u$cdu,u$csu),na.rm=T); cat("cdu_csu mismatch rows:",sum(abs(cc)>1e-6,na.rm=T),"\n")
if(n!="unharm"){ cat("shares>1:",sum(u$cdu>1,na.rm=T),"\n")}
}
