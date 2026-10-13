cols <- readRDS(file.path(Sys.getenv("S"),"cols.rds"))
x <- cols[grepl("incon|incog|^flag_|^replaced|cw$|^weights|n_pred|total_", cols$col),]
print(aggregate(col ~ ds, x, function(v) paste(v, collapse=",")), right=FALSE)
