suppressMessages(library(dplyr))
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
for (i in seq_len(nrow(inv))) { d <- readRDS(inv$path[inv$ds==inv$ds[i]]); n <- inv$ds[i]
 if (all(c("turnout","number_voters","eligible_voters") %in% names(d))) {
  t2 <- d$number_voters/d$eligible_voters; k <- is.finite(t2)&!is.na(d$turnout)
  dif <- abs(t2[k]-d$turnout[k]); 
  idc <- if ("invalid_votes" %in% names(d)) { kk <- !is.na(d$valid_votes)&!is.na(d$invalid_votes)&!is.na(d$number_voters); sum(abs(d$valid_votes[kk]+d$invalid_votes[kk]-d$number_voters[kk])>1) } else NA
  cat(sprintf("%-26s turnout!=nv/elig (>0.001): %d of %d (%.1f%%); max diff %.3f; valid+invalid!=nv(>1): %s; turnout NA with computable: %d\n", n, sum(dif>0.001), sum(k), 100*mean(dif>0.001), max(dif), idc, sum(is.na(d$turnout)&is.finite(t2)))) } }
