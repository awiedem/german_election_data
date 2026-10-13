suppressMessages(library(dplyr))
inv <- readRDS(file.path(Sys.getenv("S"),"inv.rds"))
meta <- "eligible|number_voters|valid_votes|invalid|voters|weight|^pop|^area|population|employees|seats|year|total_|n_pred|^weights|cw$|_orig$|blocked|unique|mail|turnout|^far_|cdu_csu|^other|waehlergruppen|einzelbewerber|nichtw|flag|^replaced|comb$"
chk <- function(n, f=function(d) d) { d <- f(readRDS(inv$path[inv$ds==n])); num <- names(d)[sapply(d,is.numeric)]; pc <- num[!grepl(meta,num)]
  s <- rowSums(as.data.frame(d[,pc]), na.rm=TRUE); r <- d$valid_votes/d$number_voters
  ok <- is.finite(r)&d$number_voters>0
  cat(sprintf("%-26s npartycols=%d median rowsum=%.4f; median valid/voters=%.4f; share rowsum within 0.01 of 1: %.1f%%; within 0.01 of valid/voters: %.1f%%\n", n, length(pc), median(s[ok]), median(r[ok]), 100*mean(abs(s[ok]-1)<0.01), 100*mean(abs(s[ok]-r[ok])<0.01))) }
for (n in c("european_muni_unharm","european_muni_harm","federal_muni_unharm","federal_muni_harm_21","federal_cty_unharm","federal_cty_harm","municipal_unharm","municipal_harm","state_unharm","state_harm_25","county_elec_unharm")) chk(n)
chk("federal_wkr_unharm", function(d) d[d$stimme=="zweitstimme",])
chk("federal_wkr_unharm", function(d) d[d$stimme=="erststimme",])
chk("ltw_wkr_unharm", function(d) d)
