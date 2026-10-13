suppressMessages({library(dplyr);library(data.table)})
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
for(n in c("unharm","harm_21","harm_25","raw")){
 r<-readRDS(paste0(d,"federal_muni_",n,".rds")); c<-fread(paste0(d,"federal_muni_",n,".csv"),colClasses=list(character="ags"))
 cat("\n==",n,"rds",dim(r),"csv",dim(c),"names equal:",identical(names(r),names(c)),"\n")
 cat("ags len csv:",paste(unique(nchar(c$ags)),collapse=","),"\n")
 cat("ags equal:",identical(as.character(r$ags),c$ags),"\n")
 num<-names(r)[sapply(r,is.numeric)]; num<-intersect(num,names(c))
 mx<-sapply(num,function(v){a<-r[[v]];b<-as.numeric(c[[v]]); if(length(a)!=length(b)) return(NA); max(abs(a-b),na.rm=T)})
 print(head(sort(mx,decreasing=T),4)); cat("NA pattern diffs:",sum(sapply(num,function(v) sum(is.na(r[[v]])!=is.na(c[[v]]))),na.rm=T),"\n")
 # default read (no colClasses): ags
 c2<-fread(paste0(d,"federal_muni_",n,".csv"),select="ags"); cat("default fread ags class:",class(c2$ags),"\n")
}
