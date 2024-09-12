
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


### -----------
### Load data
### -----------

d_muni <- as.data.table(readRDS("~/Documents/GitHub/german_election_data/output/municipal_harm.rds"))

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







### Party vote shares
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

(p_muni_GREEN <- ggplot()
  + geom_sf(data = de_shp_muni_data, mapping=aes(fill=gruene), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share Green", low = "gray80", high = "#1AA037", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)

(p_muni_AfD <- ggplot()
  + geom_sf(data = de_shp_muni_data, mapping=aes(fill=afd), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share AfD", low = "gray80", high = "#0489DB", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)

### Combine
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_municipal.pdf", width = 6, height = 7) 
grid.arrange(p_muni_CDU, p_muni_SPD, p_muni_GREEN, p_muni_AfD, nrow=2)
dev.off()




### --------
### Federal election 2021
### --------

d_fed_2021 <- d_fed[ election_year == "2021",.(ags,cdu_csu,spd,gruene,afd, population)]

de_shp_fed_data <- merge(de_shp_muni, d_fed_2021, by.x="AGS", by.y="ags", all=T)

de_shp_fed_data <- de_shp_fed_data[ de_shp_fed_data$GF == 4, ]

# create county variable
de_shp_fed_data <- de_shp_fed_data |>
  # remove last three digits of AGS
  mutate(county = substr(AGS, 1, 5))


# Inspect missingness -----------------------------------------------------

# get all rows in d_fed_2021 with missingness
inspect <- de_shp_fed_data |>
  filter(is.na(cdu_csu) | is.na(spd) | is.na(gruene) | is.na(afd))

d_fed_2021_names <- d_fed |>
  filter(election_year == "2021") |>
  left_join_check_obs(
    read_excel(path = "data/crosswalks/31122021_Auszug_GV.xlsx", sheet = 2) |>
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

d_fed_2021_names_red <- d_fed_2021_names |>
  dplyr::select(ags_name, state, cdu_csu,spd,gruene,afd, population) |>
  # remove everything that comes after a "," in ags_name
  mutate(ags_name = gsub(",.*", "", ags_name))

inspect2 <- inspect |>
  filter(!is.na(AGS)) |>
  dplyr::select(-c(cdu_csu:population)) |>
  left_join_check_obs(d_fed_2021_names_red, by = c("GEN" = "ags_name", "SN_L" = "state"))

# inspect duplicates
dupl <- inspect2 |> 
  group_by(AGS) |>
  mutate(n = n()) |>
  filter(n > 1)

dupl <- inspect |> 
  group_by(AGS) |>
  mutate(n = n()) |>
  filter(n > 1)
  
                

inspect2 |> 
  filter(
    # detect rows where "," appears in column GEN
    grepl(",", GEN)
    )

d_fed_2021_names |> 
  filter(
    # detect rows where "," appears in column GEN
    grepl(",", ags_name)
  ) |>
  distinct(ags_name)


### Party vote shares

(p_fed_SPD <- ggplot()
  + geom_sf(data = de_shp_fed_data, mapping=aes(fill=spd), colour="NA") 
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

(p_fed_CDU <- ggplot()
  + geom_sf(data = de_shp_fed_data, mapping=aes(fill=cdu_csu), colour="NA") 
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

(p_fed_GREEN <- ggplot()
  + geom_sf(data = de_shp_fed_data, mapping=aes(fill=gruene), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share Green", low = "gray80", high = "#1AA037", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)

(p_fed_AfD <- ggplot()
  + geom_sf(data = de_shp_fed_data, mapping=aes(fill=afd), colour="NA") 
  + geom_sf(data = de_shp_bula, fill = NA, colour ="grey30", linewidth=0.2) 
  + coord_sf()
  + theme_minimal()
  + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          title =element_text(size=9),
          axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y=element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          legend.position = "bottom", legend.text = element_text(size=8), legend.key.size = unit(0.5, "cm"))
  + scale_fill_gradient("Share AfD", low = "gray80", high = "#0489DB", space = "Lab", na.value = "white", aesthetics = "fill",
                        guide = guide_legend(keyheight = unit(2, units = "mm"), keywidth=unit(11, units = "mm"),
                                             label.position = "bottom", title.position = 'top', nrow=1))
)


### Combined 
pdf("~/Documents/GitHub/german_election_data/output/figures/map_elec_federal.pdf", width = 6, height = 7) 
grid.arrange(p_fed_CDU, p_fed_SPD, p_fed_GREEN, p_fed_AfD, nrow=2)
dev.off()





