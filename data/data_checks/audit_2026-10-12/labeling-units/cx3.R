suppressMessages(library(dplyr))
d <- readRDS("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds"); print(head(d,3))
s <- d |> group_by(ags_2021) |> summarise(p=sum(w_pop), a=sum(w_area)); cat("w_pop sum!=1:", sum(abs(s$p-1)>1e-6,na.rm=T), " NA:", sum(is.na(s$p)), " w_area!=1:", sum(abs(s$a-1)>1e-6,na.rm=T), " of", nrow(s), "\n")
for (n in c("crosswalk_ags_2021_2022_to_2023","crosswalk_ags_2023_to_2025","crosswalk_ags_2024_to_2025","crosswalk_ags_2023_24_to_2025","cty_crosswalks")) { x <- readRDS(paste0("data/crosswalks/final/",n,".rds")); k <- intersect(c("ags","county_code"), names(x)); g <- c(k, intersect("year", names(x)))
  s <- x |> group_by(across(all_of(g))) |> summarise(p=sum(pop_cw), a=sum(area_cw), .groups="drop"); cat(n, ": groups", nrow(s), " pop_cw!=1:", sum(abs(s$p-1)>1e-6, na.rm=T), " area_cw!=1:", sum(abs(s$a-1)>1e-6,na.rm=T), " NA pop_cw:", sum(is.na(s$p)), "\n") }
