suppressMessages(library(dplyr)); options(width=250, dplyr.print_max=300)
d <- "data/state_elections/final/"
u <- readRDS(paste0(d,"state_unharm.rds"))
H <- list(h21=readRDS(paste0(d,"state_harm_21.rds")),h23=readRDS(paste0(d,"state_harm_23.rds")),h25=readRDS(paste0(d,"state_harm_25.rds")))
s <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm=TRUE)
agg <- function(df) df %>% group_by(state, election_year) %>% summarise(n=n(), ev=s(eligible_voters), nv=s(number_voters), vv=s(valid_votes), iv=s(invalid_votes), .groups="drop")
au <- agg(u)
res <- list()
for (n in names(H)) {
  a <- agg(H[[n]]); m <- inner_join(au, a, by=c("state","election_year"), suffix=c("_u","_h"))
  m$ver <- n
  res[[n]] <- m
  cat("\n####", n, " unharm state-years not in harm:\n"); print(anti_join(au, a, by=c("state","election_year")) %>% filter(election_year>=1990) %>% as.data.frame())
  cat("harm state-years not in unharm:\n"); print(anti_join(a, au, by=c("state","election_year")))
}
r <- bind_rows(res) %>% mutate(d_ev=ev_h/ev_u-1, d_nv=nv_h/nv_u-1, d_vv=vv_h/vv_u-1, d_iv=iv_h/iv_u-1)
saveRDS(r, file.path(dirname(sub("--file=","",grep("--file=",commandArgs(),value=TRUE))),"conserve.rds"))
cat("\n#### >0.1% deviations (any of ev,nv,vv)\n")
bad <- r %>% filter(abs(d_ev)>0.001 | abs(d_nv)>0.001 | abs(d_vv)>0.001 | abs(d_iv)>0.001, election_year>=1990)
print(bad %>% select(ver,state,election_year,n_u,n_h,ev_u,ev_h,d_ev,nv_u,d_nv,vv_u,vv_h,d_vv,d_iv) %>% as.data.frame())
cat("\nmax abs dev by ver\n"); print(r %>% group_by(ver) %>% summarise(across(c(d_ev,d_nv,d_vv,d_iv), ~max(abs(.),na.rm=TRUE))))
cat("\nNA pattern changes (u NA, h non-NA or reverse)\n")
print(r %>% filter(is.na(vv_u)!=is.na(vv_h) | is.na(ev_u)!=is.na(ev_h) | is.na(nv_u)!=is.na(nv_h)) %>% select(ver,state,election_year,ev_u,ev_h,nv_u,nv_h,vv_u,vv_h) %>% as.data.frame())
