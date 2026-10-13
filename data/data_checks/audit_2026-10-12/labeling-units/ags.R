suppressMessages(library(dplyr))
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
for (i in seq_len(nrow(inv))) { d <- readRDS(inv$path[i]); n <- inv$ds[i]
 idc <- grep("^(ags|ags_[0-9]+|ags_20[0-9]+|ags_21|ags_25|county|county_code.*|state|cty.*|kreis.*)$", names(d), value=TRUE)
 for (c in idc) { x <- d[[c]]; cls <- class(x)[1]
   xs <- as.character(x[!is.na(x)]); nn <- sum(is.na(x))
   lens <- table(nchar(xs)); 
   cat(sprintf("%-32s %-14s %-9s NA=%d lens=%s\n", n, c, cls, nn, paste(names(lens),lens,sep=":",collapse=",")))
 }
}
