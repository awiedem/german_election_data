
rm(list = ls())
gc()

library(foreign)
library(ggplot2)
library(data.table)
library(tidyverse)
library(readxl)
library(MASS)
library(lubridate)
library(sf)
library(viridis)
library(ggpubr)
library(gridExtra)
library(haschaR)


### -----------
### Load data
### -----------

d_muni <- as.data.table(readRDS("~/Documents/GitHub/german_election_data/output/municipal_harm.rds"))

d_state <- as.data.table(readRDS("~/Documents/GitHub/german_election_data/output/state_harm.rds"))

d_fed <- as.data.table(readRDS("~/Documents/GitHub/german_election_data/output/federal_muni_harm.rds"))


### Shapefiles
de_shp_muni <- read_sf("~/Documents/GitHub/german_election_data/data/shapefiles/2021/vg250_ebenen_0101", layer = "VG250_GEM")

de_shp_bula <- read_sf("~/Documents/GitHub/german_election_data/data/shapefiles/2021/vg250_ebenen_0101", layer = "VG250_LAN")

de_shp_bula <- de_shp_bula[ de_shp_bula$GF == 4, ]


### --------
### Municipal election (most recent)
### --------

setkey(d_muni, year, ags)

### Select most recent election by AGS
d_muni_202x <- as.data.table(d_muni %>% 
                               group_by(ags) %>%
                               slice(which.max(year)))


de_shp_muni_data <- merge(de_shp_muni, d_muni_202x, by.x="AGS", by.y="ags", all=T)


### Turnout
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_muni_turnout.pdf", width = 4, height = 6) 
(p_muni_turnout <- ggplot()
  + geom_sf(data = de_shp_muni_data, mapping=aes(fill=turnout), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Turnout", low = "gray90", high = "blue", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()




### Party vote shares
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_muni_CDU.pdf",  width = 4, height = 6) 
(p_muni_CDU <- ggplot()
  + geom_sf(data = de_shp_muni_data, mapping=aes(fill=cdu_csu), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share CDU/CSU", low = "gray80", high = "#000000", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()


pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_muni_SPD.pdf",  width = 4, height = 6) 
(p_muni_SPD <- ggplot()
  + geom_sf(data = de_shp_muni_data, mapping=aes(fill=spd), colour="NA") 
  + geom_sf(data = de_shp_bula[ de_shp_bula$GF == 4, ], fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share SPD", low = "gray80", high = "#E3000F", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()


# (p_muni_GREEN <- ggplot()
#   + geom_sf(data = de_shp_muni_data, mapping=aes(fill=gruene), colour="NA") 
#   + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
#   + coord_sf()
#   + theme_minimal()
#   + theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           title =element_text(size=9),
#           axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
#           axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#           legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
#   + scale_fill_gradient("Share Green", low = "gray80", high = "#1AA037", space = "Lab", na.value = "white", aesthetics = "fill",
#                         guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
#                                              label.position = "bottom", title.position = 'top', nrow=1))
# )
# 
# (p_muni_AfD <- ggplot()
#   + geom_sf(data = de_shp_muni_data, mapping=aes(fill=afd), colour="NA") 
#   + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
#   + coord_sf()
#   + theme_minimal()
#   + theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           title =element_text(size=9),
#           axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
#           axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#           legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
#   + scale_fill_gradient("Share AfD", low = "gray80", high = "#0489DB", space = "Lab", na.value = "white", aesthetics = "fill",
#                         guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
#                                              label.position = "bottom", title.position = 'top', nrow=1))
# )
# 
# ### Combine
# pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_municipal.pdf", width = 6, height = 7) 
# grid.arrange(p_muni_CDU, p_muni_SPD, p_muni_GREEN, p_muni_AfD, nrow=2)
# dev.off()


pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_municipal_v2.pdf", width = 8, height = 4.5) 
grid.arrange(p_muni_turnout, p_muni_CDU, p_muni_SPD, nrow=1)
dev.off()




### --------
### State election (most recent)
### --------

setkey(d_state, election_year, ags)

### Select most recent election by AGS
d_state_202x <- as.data.table(d_state %>% 
                               group_by(ags) %>%
                               slice(which.max(election_year)))

de_shp_state_data <- merge(de_shp_muni, d_state_202x, by.x="AGS", by.y="ags", all=T)


### Turnout
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_state_turnout.pdf", width = 4, height = 6) 
(p_state_turnout <- ggplot()
  + geom_sf(data = de_shp_state_data, mapping=aes(fill=turnout), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Turnout", low = "gray90", high = "blue", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()



### Party vote shares
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_state_CDU.pdf",  width = 4, height = 6) 
(p_state_CDU <- ggplot()
  + geom_sf(data = de_shp_state_data, mapping=aes(fill=cdu_csu), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share CDU/CSU", low = "gray80", high = "#000000", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()


pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_state_SPD.pdf",  width = 4, height = 6) 
(p_state_SPD <- ggplot()
  + geom_sf(data = de_shp_state_data, mapping=aes(fill=spd), colour="NA") 
  + geom_sf(data = de_shp_bula[ de_shp_bula$GF == 4, ], fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share SPD", low = "gray80", high = "#E3000F", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()


pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_state_v2.pdf", width = 8, height = 4.5) 
grid.arrange(p_state_turnout, p_state_CDU, p_state_SPD, nrow=1)
dev.off()




### --------
### Federal election 2021
### --------

d_fed_2021 <- d_fed[ election_year == "2021",.(ags,cdu_csu,spd,gruene,afd,turnout)]

de_shp_fed_data <- merge(de_shp_muni, d_fed_2021, by.x="AGS", by.y="ags", all=T)

de_shp_fed_data <- de_shp_fed_data[ de_shp_fed_data$GF == 4, ]


# Unsuccessful mergers: try by name & state -------------------------------

# get all rows in d_fed_2021 with missingness
unsuccessful <- de_shp_fed_data |>
  filter(is.na(cdu_csu) | is.na(spd) | is.na(gruene) | is.na(afd))

# get 2021 ags_names
d_fed_2021_names <- d_fed |>
  filter(election_year == "2021") |>
  left_join_check_obs(
    read_excel(path = "~/Documents/GitHub/german_election_data/data/crosswalks/31122021_Auszug_GV.xlsx", sheet = 2) |>
      dplyr::select(
        Land = `...3`,
        RB = `...4`,
        Kreis = `...5`,
        Gemeinde = `...7`,
        ags_name = `...8`
      ) |>
      mutate(
        Land = pad_zero_conditional(Land, 1),
        Kreis = pad_zero_conditional(Kreis, 1),
        Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
        Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
        ags = paste0(Land, RB, Kreis, Gemeinde)
      ) |>
      slice(6:16065) |>
      filter(!is.na(Gemeinde)) |>
      dplyr::select(ags, ags_name),
    by = "ags"
  )

# reduce the names (remove , Stadt etc.)
d_fed_2021_names_red <- d_fed_2021_names |>
  dplyr::select(ags_name, state, cdu_csu, spd, gruene, afd) |>
  # remove everything that comes after a "," in ags_name
  mutate(ags_name = gsub(",.*", "", ags_name))

# merge unsuccessful with d_fed_2021_names_red
merge_by_name <- unsuccessful |>
  dplyr::select(-c(cdu_csu:afd)) |>
  left_join_check_obs(d_fed_2021_names_red, by = c("GEN" = "ags_name", "SN_L" = "state"))

# duplicates?
merge_by_name |>
  filter(duplicated(GEN))
# some


# Build dataframe for plots
plot_df <- de_shp_fed_data |>
  filter(!is.na(cdu_csu) & !is.na(spd) & !is.na(gruene) & !is.na(afd)) |>
  bind_rows(merge_by_name) |>
  # remove observations that are duplicates
  group_by(AGS) |>
  filter(row_number() == 1) |>
  ungroup()


# for how many obs do we not have voting data?
inspect_missing <- plot_df |>
  filter(is.na(cdu_csu) & is.na(spd) & is.na(gruene) & is.na(afd) & is.na(turnout))
# 505

# turnout > 1?
inspect_turnout <- plot_df |>
  filter(turnout > 1) |>
  dplyr::select(AGS, GEN, turnout)
# none


### Turnout
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_turnout.pdf", width = 4, height = 6) 
(p_fed_turnout <- ggplot()
  + geom_sf(data = plot_df, mapping=aes(fill=turnout), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Turnout", low = "gray90", high = "blue", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()

### Party vote shares
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_CDU.pdf", width = 4, height = 6) 
(p_fed_CDU <- ggplot()
  + geom_sf(data = plot_df, mapping=aes(fill=cdu_csu), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share CDU/CSU", low = "gray80", high = "black", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()


pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_SPD.pdf", width = 4, height = 6) 
(p_fed_SPD <- ggplot()
  + geom_sf(data = plot_df, mapping=aes(fill=spd), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share SPD", low = "gray80", high = "#E3000F", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)
dev.off()


# (p_fed_GREEN <- ggplot()
#   + geom_sf(data = plot_df, mapping=aes(fill=gruene), colour="NA") 
#   + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
#   + coord_sf()
#   + theme_minimal()
#   + theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           title =element_text(size=9),
#           axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
#           axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#           legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
#   + scale_fill_gradient("Share Green", low = "gray80", high = "#1AA037", space = "Lab", na.value = "white", aesthetics = "fill",
#                         guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
#                                              label.position = "bottom", title.position = 'top', nrow=1))
# )
# 
# (p_fed_AfD <- ggplot()
#   + geom_sf(data = plot_df, mapping=aes(fill=afd), colour="NA") 
#   + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
#   + coord_sf()
#   + theme_minimal()
#   + theme(panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank(),
#           title =element_text(size=9),
#           axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
#           axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#           legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
#   + scale_fill_gradient("Share AfD", low = "gray80", high = "#0489DB", space = "Lab", na.value = "white", aesthetics = "fill",
#                         guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
#                                              label.position = "bottom", title.position = 'top', nrow=1))
# )
# 
# 
# ### Combined 
# pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_federal.pdf", width = 6, height = 7) 
# grid.arrange(p_fed_CDU, p_fed_SPD, p_fed_GREEN, p_fed_AfD, nrow=2)
# dev.off()

pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_federal_v2.pdf", width = 8, height = 4.5) 
grid.arrange(p_fed_turnout, p_fed_CDU, p_fed_SPD, nrow=1)
dev.off()


move_plots_to_overleaf("code")


