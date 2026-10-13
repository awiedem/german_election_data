suppressMessages({library(dplyr)}); options(width=250, dplyr.print_max=500)
h <- readRDS("data/state_elections/final/state_harm_25.rds")
groups <- list(rep=c("rep","die_republikaner_rep"), greens=c("gruene","b90","b90_gr","bue90_gruene","buendnis_90"), fw=c("freie_wahler","freiewaehler","partei_freie_waehler","fbi_fwg"), tier=c("tierschutz","tier_schutz_partei","tierschutz_hier"), schill=c("schill","pro_dm_schill"), b21=c("buendnis_21_rrp","buendnis21"), off=c("offensive","offensive_d"))
for (g in names(groups)) { cols<-intersect(groups[[g]],names(h)); cat("\n##",g,"\n")
 r <- h %>% group_by(state,election_year) %>% summarise(across(all_of(cols), ~ round(100*sum(.x*valid_votes,na.rm=T)/sum(valid_votes,na.rm=T),2)), .groups="drop") %>% filter(if_any(all_of(cols), ~.x>0.3))
 print(as.data.frame(r)) }
cat("\nfar_right check: BY 1994 harm25 far_right/rep/die_rep\n")
print(h %>% filter(state=="09", election_year %in% c(1994,1998,2003)) %>% group_by(election_year) %>% summarise(far_right=100*sum(far_right*valid_votes,na.rm=T)/sum(valid_votes), rep=100*sum(rep*valid_votes,na.rm=T)/sum(valid_votes), drep=100*sum(die_republikaner_rep*valid_votes,na.rm=T)/sum(valid_votes), npd=100*sum(npd*valid_votes,na.rm=T)/sum(valid_votes)))
