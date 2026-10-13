cols <- readRDS(file.path(Sys.getenv("S"),"cols.rds"))
pat <- "year|date|voter|turnout|eligible|valid|invalid|name|^state|^county|round|wahl|stimme|^ags|id$"
for (d in unique(cols$ds)) { x <- cols[cols$ds==d & grepl(pat, cols$col) & nchar(cols$col)>0,]
  x <- x[!grepl("^flag|^seats|^replaced", x$col),]
  cat(sprintf("%-32s %s\n", d, paste0(x$col,"<",substr(x$cls,1,3),">",collapse=" "))) }
