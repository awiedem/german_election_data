suppressMessages(library(dplyr))
d <- readRDS("data/crosswalks/final/crosswalk_ags_2021_to_2023.rds")
s <- d |> group_by(ags_2021) |> summarise(p=sum(w_pop)) |> filter(abs(p-1)>1e-6); print(s); print(d |> filter(ags_2021 %in% s$ags_2021))
