suppressMessages({library(data.table)})
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
for (i in seq_len(nrow(inv))) { rf <- inv$path[i]; cf <- sub("\\.rds$",".csv",rf); n <- inv$ds[i]
 if (!file.exists(cf)) { cat(sprintf("%-32s NO CSV\n", n)); next }
 sz <- file.size(cf); if (sz < 200) { cat(n, "CSV tiny", sz, "bytes (LFS pointer?)\n"); next }
 r <- readRDS(rf); c <- fread(cf, colClasses="character", encoding="UTF-8", na.strings=c("NA",""))
 nm_ok <- identical(names(c), names(r))
 msg <- sprintf("%-32s rds=%dx%d csv=%dx%d names_same=%s", n, nrow(r), ncol(r), nrow(c), ncol(c), nm_ok)
 if (!nm_ok) { msg <- paste0(msg, " rds_only=", paste(head(setdiff(names(r),names(c)),5),collapse="|"), " csv_only=", paste(head(setdiff(names(c),names(r)),5),collapse="|")) }
 # ags comparison
 for (k in intersect(c("ags","county","county_code","state","ags_21","ags_25","ags_2023","ags_2021"), names(c))) {
   if (nrow(c)==nrow(r)) { a <- as.character(r[[k]]); b <- c[[k]]; bad <- sum((is.na(a)!=is.na(b)) | (!is.na(a) & !is.na(b) & a!=b)); if (bad>0) msg <- paste0(msg, sprintf(" [%s mismatch %d e.g. rds=%s csv=%s]", k, bad, a[which((a!=b)|(is.na(a)!=is.na(b)))[1]], b[which((a!=b)|(is.na(a)!=is.na(b)))[1]])) }
 }
 cat(msg, "\n")
 rm(r,c); gc(verbose=FALSE)
}
