suppressMessages(library(dplyr))
d <- readRDS("data/county_elections/final/county_elec_unharm.rds")
nm <- names(d)
cat("cols with umlaut-dropped pattern (_hler|b_ndnis|_r_|gr_ne|w_hler|_nchen|_rger):", sum(grepl("w_hler|b_ndnis|gr_ne|b_rger|_rger|_hler|f_r_|f_r$|^f_r", nm)), "\n")
fw <- grep("^(freie_w|fw)", nm, value=TRUE); cat("FW-like cols:", length(fw), "\n")
x <- sapply(c("freie_w_hler","freie_waehler","fw","freie_waehlergemeinschaft"), function(c) if (c %in% nm) sum(!is.na(d[[c]])) else NA); print(x)
g <- grep("^(gruene|gr_ne|b_ndnis_90|buendnis_90|b90_gr|bundnis_90|gr_ne.*|die_gr_nen)$", nm, value=TRUE); print(sapply(g, function(c) sum(!is.na(d[[c]]))))
print(grep("gr_n|gruen|gr.ne", nm, value=TRUE))
bun <- grep("b_ndnis|buendnis|bundnis", nm, value=TRUE); cat(length(bun), "Buendnis-like cols\n"); print(head(bun,12))
# CSV encoding check
for (f in c("data/municipal_elections/final/municipal_unharm.csv","data/state_elections/final/state_harm_25.csv","data/mayoral_elections/final/mayoral_unharm.csv","data/federal_elections/wahlkreis_level/final/federal_wkr_unharm.csv","data/county_elections/final/county_elec_unharm.csv","data/crosswalks/final/ags_crosswalks.csv")) { l <- readLines(f, encoding="UTF-8", n=-1, warn=FALSE); bad <- sum(grepl("Ã¼|Ã¤|Ã¶|ÃŸ|Ã\u0084|Ã\u009c|�", l)); inv <- sum(!validUTF8(l)); cat(basename(f), "mojibake lines:", bad, " invalid utf8 lines:", inv, "\n") }
