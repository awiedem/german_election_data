suppressMessages(library(dplyr))
fs <- list.files("data", pattern="\\.rds$", recursive=TRUE, full.names=TRUE)
fs <- fs[grepl("/final/", fs) & !grepl("_old_regionalstatistik", fs)]
inv <- list(); cols <- list()
for (f in fs) { d <- readRDS(f); n <- sub("\\.rds$","",basename(f))
  inv[[n]] <- data.frame(ds=n, path=f, nr=nrow(d), nc=ncol(d))
  cols[[n]] <- data.frame(ds=n, col=names(d), cls=vapply(d, function(x) class(x)[1], ""), stringsAsFactors=FALSE) }
saveRDS(do.call(rbind,inv), file.path(Sys.getenv("S"),"inv.rds")); saveRDS(do.call(rbind,cols), file.path(Sys.getenv("S"),"cols.rds"))
print(do.call(rbind,inv)[,c(1,3,4)], row.names=FALSE)
