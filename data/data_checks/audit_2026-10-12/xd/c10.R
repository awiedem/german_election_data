suppressMessages(library(dplyr))
setwd("/Users/hanno/Documents/GitHub/german_election_data")
ff<-list.files("data",pattern="\\.rds$",recursive=TRUE,full.names=TRUE); ff<-ff[!grepl("_old_|raw|derived",ff)]
meta<-list()
for(p in ff){d<-readRDS(p); if(!is.data.frame(d))next; n<-names(d)
 yc<-intersect(c("election_year","year"),n)[1]
 meta[[p]]<-data.frame(f=sub("data/","",p),n=nrow(d),ags=("ags"%in%n),ags_name=any(grepl("^ags_name",n)),state_name=("state_name"%in%n),edate=("election_date"%in%n),state=("state"%in%n),yc=yc,
  ags_class=if("ags"%in%n)class(d$ags)[1] else NA, ags_nchar=if("ags"%in%n)paste(sort(unique(nchar(as.character(d$ags)))),collapse="/") else NA, edate_na=if("election_date"%in%n) round(mean(is.na(d$election_date)),3) else NA)}
m<-bind_rows(meta);m$f<-basename(m$f);m$n<-NULL;options(width=220);write.table(as.data.frame(m),sep=' | ',quote=F,row.names=F)
