suppressMessages(library(dplyr))
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds")); maps <- list()
for (i in seq_len(nrow(inv))) { d <- readRDS(inv$path[i]); n <- inv$ds[i]
  yc <- intersect(c("election_year","year"), names(d))
  if ("election_date" %in% names(d)) {
    dt <- d$election_date; tz <- attr(dt,"tzone"); 
    y <- if ("election_year" %in% names(d)) d$election_year else d$year
    dd <- as.Date(dt); mm <- sum(!is.na(dd) & !is.na(y) & as.integer(format(dd,"%Y"))!=y)
    cat(sprintf("%-30s class=%s tz=%s NAdate=%d (%.1f%%) yr_mismatch=%d\n", n, class(dt)[1], ifelse(is.null(tz),"-",tz), sum(is.na(dt)), 100*mean(is.na(dt)), mm))
    if (class(dt)[1]=="POSIXct") { cat("   time-of-day non-midnight UTC:", sum(format(dt,"%H:%M",tz="UTC")!="00:00", na.rm=TRUE), "\n") }
  } else cat(sprintf("%-30s no election_date; yearcols=%s\n", n, paste(yc,collapse=",")))
  if (all(c("state","state_name") %in% names(d))) maps[[n]] <- distinct(d, state, state_name) |> mutate(ds=n)
}
m <- bind_rows(maps); w <- m |> group_by(state) |> summarise(k=n_distinct(state_name), names=paste(unique(paste0(ds,":",state_name))[1:0], collapse="")) 
print(m |> filter(!is.na(state)) |> group_by(state, state_name) |> summarise(ds=paste(sort(unique(ds)),collapse=","), .groups="drop") |> mutate(ds=substr(ds,1,60)) |> as.data.frame())
