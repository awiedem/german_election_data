suppressMessages({library(data.table)})
D<-"data/mayoral_elections/final/"
for(f in c("mayoral_unharm","mayoral_harm","mayoral_candidates","mayor_panel","mayor_panel_harm","mayor_panel_annual","mayor_panel_annual_harm")){
 r<-readRDS(paste0(D,f,".rds")); c<-fread(paste0(D,f,".csv"),colClasses=list(character=intersect(c("ags","ags_21","state"),names(r))))
 cat(f,"rows rds/csv",nrow(r),nrow(c),"cols",ncol(r),ncol(c),"names equal",identical(names(r),names(c)),"ags nchar!=8 csv:",sum(nchar(c$ags)!=8,na.rm=T),"\n")
 num<-names(r)[sapply(r,is.numeric)]
 d<-sapply(intersect(num,names(c)),function(n){x<-r[[n]];y=suppressWarnings(as.numeric(c[[n]]));sum(abs(x-y)>1e-6*pmax(1,abs(x)),na.rm=T)+sum(is.na(x)!=is.na(y))})
 if(any(d>0))print(d[d>0])
}
# default read without colClasses: does ags lose zeros?
c<-fread(paste0(D,"mayoral_unharm.csv"),nrows=3); print(class(c$ags)); print(head(c$ags,3))
