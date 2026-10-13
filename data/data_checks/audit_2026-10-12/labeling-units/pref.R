suppressMessages(library(dplyr))
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
for (i in seq_len(nrow(inv))) { d <- readRDS(inv$path[i]); n <- inv$ds[i]; out <- c()
 if (all(c("ags","state") %in% names(d)) && is.character(d$ags)) { k <- !is.na(d$ags)&!is.na(d$state); b <- sum(substr(d$ags[k],1,2)!=d$state[k]); if (b) out <- c(out, sprintf("state!=ags[1:2]: %d", b)) }
 if (all(c("ags","county") %in% names(d)) && is.character(d$ags) && all(nchar(d$ags[!is.na(d$ags)])==8)) { b <- sum(substr(d$ags,1,5)!=d$county, na.rm=T); if (b) out <- c(out, sprintf("county!=ags[1:5]: %d", b)) }
 if (all(c("county_code","state") %in% names(d))) { b <- sum(substr(d$county_code,1,2)!=d$state,na.rm=T); if (b) out <- c(out, sprintf("state!=county_code[1:2]: %d", b)) }
 if (all(c("state","state_name") %in% names(d))) { m <- d |> filter(!is.na(state)) |> distinct(state,state_name) |> count(state) |> filter(n>1); if (nrow(m)) out <- c(out, paste("state->multiple names:", paste(m$state, collapse=","))); m2 <- d |> distinct(state,state_name) |> count(state_name) |> filter(n>1); if (nrow(m2)) out <- c(out, paste("name->multiple codes:", paste(m2$state_name,collapse=","))) }
 # shares & turnout
 cat(sprintf("%-32s %s\n", n, ifelse(length(out), paste(out, collapse=" ; "), "ok")))
}
