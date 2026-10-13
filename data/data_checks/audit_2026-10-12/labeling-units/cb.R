cols <- readRDS(file.path(Sys.getenv("S"),"cols.rds"))
cb <- readLines("docs/codebook.md")
tok <- unique(unlist(regmatches(cb, gregexpr("`[^`]+`", cb)))); tok <- gsub("`","",tok)
# expand wildcard-like tokens
txt <- paste(cb, collapse="\n")
for (d in unique(cols$ds)) { x <- cols[cols$ds==d,]
  und <- x$col[!(x$col %in% tok) & !grepl(paste0("(^|[^a-z0-9_])", "x"), x$col)]
  # also check as word anywhere in text
  und2 <- und[!vapply(und, function(c) grepl(paste0("(?<![A-Za-z0-9_])",c,"(?![A-Za-z0-9_])"), txt, perl=TRUE), TRUE)]
  cat("\n##", d, ":", length(x$col), "cols;", length(und), "not in backticks;", length(und2), "not in text anywhere\n")
  if (length(und2)) cat(head(und2, 60), sep=", "); cat("\n")
}
