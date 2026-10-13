cols <- readRDS(file.path(Sys.getenv("S"),"cols.rds")); all <- unique(cols$col)
cb <- readLines("docs/codebook.md")
# only table rows' first cell tokens
rows <- cb[grepl("^\\| `", cb)]
cell <- sub("^\\| (.*?) \\| .*", "\\1", rows)
toks <- unlist(regmatches(cell, gregexpr("`[^`]+`", cell))); toks <- unique(gsub("`","",toks))
miss <- toks[!toks %in% all & grepl("^[a-z0-9_]+$", toks)]
cat("documented-in-table but absent from every dataset:\n"); print(miss)
wild <- toks[grepl("\\*", toks)]; print(wild)
# which datasets lack it
