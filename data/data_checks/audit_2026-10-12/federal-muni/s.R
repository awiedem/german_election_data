suppressMessages(library(dplyr))
options(width=250,scipen=99)
d<-"data/federal_elections/municipality_level/final/"
h<-readRDS(paste0(d,"federal_muni_harm_21.rds"));h25<-readRDS(paste0(d,"federal_muni_harm_25.rds"))
cat("ags_21 NA share h21:",mean(is.na(h$ags_21))," h25:",mean(is.na(h25$ags_21)),"\n")
a<-h|>filter(election_year==2017)|>pull(ags); b<-h|>filter(election_year==2021)|>pull(ags)
miss<-setdiff(a,b); cat("in 2017 not 2021:",length(miss),"\n")
print(h|>filter(election_year==2017,ags%in%miss)|>summarise(n=n(),nv=sum(number_voters),vv=sum(valid_votes)))
print(head(h|>filter(election_year==2017,ags%in%miss)|>select(ags,ags_name,number_voters),10)|>as.data.frame())
print(table(substr(miss,1,2)))
# 2021 votes in the rest conserved vs unharm? yes (checked). So where did the votes of 'miss' go in 2021? check neighbors with jump
j<-h|>filter(election_year %in% c(2017,2021))|>select(ags,election_year,number_voters)|>tidyr::pivot_wider(names_from=election_year,values_from=number_voters,names_prefix="y")|>mutate(r=y2021/y2017)
print(j|>filter(!is.na(r),is.finite(r))|>summarise(n=n(),gt2=sum(r>2),lt.5=sum(r<.5)))
print(j|>filter(is.finite(r),r>2|r<.5)|>arrange(desc(r))|>head(10)|>as.data.frame())
# split vote check by 2021 raw: do raw 2021 rows for 'miss' exist?
r<-readRDS(paste0(d,"federal_muni_raw.rds"))
print(r|>filter(election_year==2021,ags%in%miss)|>summarise(n=n(),nv=sum(number_voters)))
