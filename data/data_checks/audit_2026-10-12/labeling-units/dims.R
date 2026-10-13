inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
txt <- paste(readLines("docs/codebook.md"), collapse=" "); txt <- gsub("\\s+"," ",txt)
m <- gregexpr("`([a-z0-9_]+)`,? (?:\\(|\\()?([0-9,]+) x ([0-9]+)", txt, perl=TRUE)
mm <- regmatches(txt, m)[[1]]
for (s in mm) { p <- regmatches(s, regexec("`([a-z0-9_]+)`,? \\(?([0-9,]+) x ([0-9]+)", s))[[1]]; n <- p[2]; r <- as.numeric(gsub(",","",p[3])); c <- as.numeric(p[4]); i <- which(inv$ds==n)
  if (length(i)) cat(sprintf("%-28s codebook %s x %d ; actual %d x %d %s\n", n, p[3], c, inv$nr[i], inv$nc[i], ifelse(r==inv$nr[i]&c==inv$nc[i],"OK","MISMATCH"))) else cat(n, "not found\n") }
