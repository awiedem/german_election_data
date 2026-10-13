suppressMessages({library(dplyr)})
for(f in c("covars_municipality/final/ags_area_pop_emp","covars_municipality/final/ags_area_pop_emp_2023","covars_county/final/cty_area_pop_emp")){
 d <- readRDS(paste0("data/",f,".rds")); k <- names(d)[1]; cat("\n==",f,dim(d),"\n"); print(sapply(d,function(v)class(v)[1]))
 cat("dups key:",sum(duplicated(d[,c(k,"year")])),"; key nchar:"); print(table(nchar(d[[k]]),useNA="a"))
 cat("NA counts:"); print(colSums(is.na(d)))
 nm <- names(d)
 a <- nm[grepl("^area",nm)]; p <- nm[grepl("^population",nm)]; e <- nm[grepl("^employees",nm)]; dd <- nm[grepl("density",nm)]
 t <- d %>% group_by(year) %>% summarise(n=n(),pop=sum(.data[[p]],na.rm=T),area=sum(.data[[a]],na.rm=T),emp=sum(.data[[e]],na.rm=T),empNA=sum(is.na(.data[[e]])),zeroPop=sum(.data[[p]]==0,na.rm=T),zeroArea=sum(.data[[a]]==0,na.rm=T)); print(as.data.frame(t))
 # density check
 r <- d[[dd]]/(d[[p]]/d[[a]]); cat("density ratio to pop/area: "); print(summary(r)); r2 <- d[[dd]]/(1000*d[[p]]/d[[a]]); cat("density ratio to 1000*pop/area: ");print(summary(r2))
 print(summary(d[[dd]]))
}
