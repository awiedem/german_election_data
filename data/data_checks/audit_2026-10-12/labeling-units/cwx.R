suppressMessages(library(dplyr))
cw <- readRDS("data/crosswalks/final/ags_crosswalks.rds")
x <- cw[nchar(as.character(cw$ags))==5,]; print(unique(x[,c("ags","ags_name","ags_21")])); print(table(x$year))
cat("sprintf-safe pad:", head(sprintf("%08.0f", unique(x$ags))), "\n")
# other crosswalks: weights sum
for (n in c("ags_crosswalks")) { s <- cw |> group_by(ags, year) |> summarise(p=sum(pop_cw), a=sum(area_cw), .groups="drop"); cat("pop_cw sum !=1:", sum(abs(s$p-1)>1e-6,na.rm=T), "NA:", sum(is.na(s$p)), " area_cw !=1:", sum(abs(s$a-1)>1e-6,na.rm=T), "\n") }
for (n in c("ags_1990_to_2025_crosswalk","ags_1990_to_2023_crosswalk")) { d <- readRDS(paste0("data/crosswalks/final/",n,".rds")); print(names(d)); s <- d |> group_by(ags, year) |> summarise(p=sum(pop_cw), .groups="drop"); cat(n, "pop_cw sum !=1:", sum(abs(s$p-1)>1e-6,na.rm=T), "of", nrow(s), "\n") }
