suppressMessages({library(data.table)}); options(width=250)
d <- "data/state_elections/final/"
for (n in c("state_unharm","state_harm_21","state_harm_23","state_harm_25","ltw_wkr_unharm","ltw_wkr_unharm_long")) {
  r <- as.data.table(readRDS(paste0(d,n,".rds"))); 
  cc <- fread(paste0(d,n,".csv"), colClasses=list(character=intersect(c("ags","state","state_abbr","wkr_nr"),names(r))), na.strings=c("","NA"), encoding="UTF-8")
  cat("\n##",n,"rds",dim(r),"csv",dim(cc),"; names identical:",identical(names(r),names(cc)),"\n")
  if (n %in% names(r)) next
  if ("ags" %in% names(cc)) cat(" ags nchar csv:", paste(names(table(nchar(cc$ags))),table(nchar(cc$ags)),collapse=" "), " identical ags:", identical(as.character(r$ags), cc$ags), "\n")
  nums <- names(r)[sapply(r,is.numeric)]; bad <- character()
  for (v in nums) { a<-r[[v]]; b<-as.numeric(cc[[v]]); if (!identical(is.na(a),is.na(b)) || any(abs(a-b)>1e-6*pmax(1,abs(a)),na.rm=TRUE)) bad<-c(bad,v) }
  cat(" numeric cols differing:", length(bad), head(bad,10), "\n")
  chars <- names(r)[sapply(r,is.character)]; badc <- chars[!sapply(chars,function(v) identical(r[[v]], as.character(cc[[v]])))]; cat(" char cols differing:", badc, "\n")
  if ("election_date" %in% names(r)) cat(" date identical:", identical(as.character(r$election_date), as.character(cc$election_date)), "\n")
}
