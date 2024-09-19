rm(list = ls())
gc()

# Load required packages using pacman
pacman::p_load(
  foreign, ggplot2, data.table, tidyverse,
  readxl, MASS, lubridate, sf, viridis, ggpubr, gridExtra, haschaR
)


### -----------
### Load data
### -----------

d_muni <- read_rds("~/Documents/GitHub/german_election_data/data/municipal_elections/final/municipal_harm.rds")
d_state <- read_rds("~/Documents/GitHub/german_election_data/data/state_elections/final/state_harm.rds")
d_fed <- read_rds("~/Documents/GitHub/german_election_data/data/federal_elections/municipality_level/final/federal_muni_harm.rds")

### Shapefiles
de_shp_muni <- read_sf("~/Documents/GitHub/german_election_data/data/shapefiles/2021/vg250_ebenen_0101", layer = "VG250_GEM")
de_shp_bula <- read_sf("~/Documents/GitHub/german_election_data/data/shapefiles/2021/vg250_ebenen_0101", layer = "VG250_LAN")
de_shp_bula <- de_shp_bula %>% dplyr::filter(GF == 4)

### Create "most recent" election for each type

# Create "most recent" election datasets for each type

# Municipal elections
d_muni_recent <- d_muni %>%
  group_by(ags) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# State elections
d_state_recent <- d_state %>%
  group_by(ags) %>%
  slice_max(election_year, n = 1) %>%
  ungroup()

# Federal elections (already most recent, 2021)
d_fed_2021 <- d_fed %>%
  dplyr::filter(election_year == 2021)


### --------
### Municipal election (most recent)
### --------

de_shp_muni_data <- de_shp_muni %>%
  left_join_check_obs(d_muni_recent, by = c("AGS" = "ags"))

### Turnout and Party vote shares
### Determine ranges for each party across all election types
get_party_range <- function(party) {
  range(c(
    d_muni_recent[[party]],
    d_state_recent[[party]],
    d_fed_2021[[party]]
  ), na.rm = TRUE)
}

cdu_range <- get_party_range("cdu_csu")
spd_range <- get_party_range("spd")
turnout_range <- get_party_range("turnout")

### Modify plotting functions to accept custom limits
plot_muni_map <- function(fill_var, legend_label, fill_palette, limits) {
  ggplot() +
    geom_sf(data = de_shp_muni_data, mapping = aes(fill = .data[[fill_var]]), colour = "NA") +
    geom_sf(data = de_shp_bula, fill = NA, colour = "grey60", linewidth = 0.2) +
    coord_sf() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      title = element_text(size = 9),
      axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "bottom", legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0)
    ) +
    scale_fill_distiller(NULL,
      palette = fill_palette,
      na.value = "white",
      direction = 1,
      limits = limits,
      guide = guide_legend(
        keyheight = unit(2, units = "mm"), keywidth = unit(11, units = "mm"),
        label.position = "bottom", title.position = "top", nrow = 1
      )
    )
}

# Turnout
p_muni_turnout <- plot_muni_map("turnout", "Turnout", "Purples", turnout_range)

# Save as PDF and PNG
# ggsave("output/figures/map_elec_muni_turnout.pdf", plot = p_muni_turnout, width = 4, height = 6)
ggsave("output/figures/map_elec_muni_turnout.png", plot = p_muni_turnout, width = 4, height = 6, dpi = 450)

# CDU/CSU
p_muni_CDU <- plot_muni_map("cdu_csu", "Share CDU/CSU", "Blues", cdu_range)

# Save as PDF and PNG
# ggsave("output/figures/map_elec_muni_CDU.pdf", plot = p_muni_CDU, width = 4, height = 6)
ggsave("output/figures/map_elec_muni_CDU.png", plot = p_muni_CDU, width = 4, height = 6, dpi = 450)

# SPD
p_muni_SPD <- plot_muni_map("spd", "Share SPD", "Reds", spd_range)

# Save as PDF and PNG
# ggsave("output/figures/map_elec_muni_SPD.pdf", plot = p_muni_SPD, width = 4, height = 6)
ggsave("output/figures/map_elec_muni_SPD.png", plot = p_muni_SPD, width = 4, height = 6, dpi = 450)


### --------
### State election (most recent)
### --------

### Select most recent election by AGS
d_state_recent <- d_state %>%
  group_by(ags) %>%
  slice_max(election_year, n = 1) %>%
  ungroup()

de_shp_state_data <- de_shp_muni %>%
  left_join_check_obs(d_state_recent, by = c("AGS" = "ags"))

# Function to plot state maps
plot_state_map <- function(variable, legend_label, fill_palette, limits) {
  ggplot() +
    geom_sf(data = de_shp_state_data, mapping = aes(fill = .data[[variable]]), colour = NA) +
    geom_sf(data = de_shp_bula, fill = NA, colour = "grey30", linewidth = 0.2) +
    coord_sf() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      title = element_text(size = 9),
      axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "bottom", legend.text = element_text(size = 12), legend.key.size = unit(0.6, "cm"),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = -25, r = 0, b = 0, l = 0)
    ) +
    scale_fill_distiller(NULL,
      palette = fill_palette,
      na.value = "white",
      direction = 1,
      limits = limits,
      guide = guide_legend(
        keyheight = unit(4, units = "mm"), keywidth = unit(18, units = "mm"),
        label.position = "bottom", title.position = "top", nrow = 1
      )
    )
}

# Turnout
p_state_turnout <- plot_state_map("turnout", "Turnout", "Purples", turnout_range)

# CDU/CSU
p_state_CDU <- plot_state_map("cdu_csu", "Share CDU/CSU", "Blues", cdu_range)

# SPD
p_state_SPD <- plot_state_map("spd", "Share SPD", "Reds", spd_range)

# Save as PDF and PNG
# ggsave("output/figures/map_elec_state_turnout.pdf", plot = p_state_turnout, width = 4, height = 6)
ggsave("output/figures/map_elec_state_turnout.png", plot = p_state_turnout, width = 4, height = 6, dpi = 450)

# ggsave("output/figures/map_elec_state_CDU.pdf", plot = p_state_CDU, width = 4, height = 6)
ggsave("output/figures/map_elec_state_CDU.png", plot = p_state_CDU, width = 4, height = 6, dpi = 450)

# ggsave("output/figures/map_elec_state_SPD.pdf", plot = p_state_SPD, width = 4, height = 6)
ggsave("output/figures/map_elec_state_SPD.png", plot = p_state_SPD, width = 4, height = 6, dpi = 450)

### --------
### Federal election 2021
### --------

# Load and prepare data
d_fed_2021 <- d_fed %>%
  dplyr::filter(election_year == "2021") %>%
  dplyr::select(ags, cdu_csu, spd, gruene, afd, turnout)

de_shp_fed_data <- de_shp_muni %>%
  left_join_check_obs(d_fed_2021, by = c("AGS" = "ags")) %>%
  dplyr::filter(GF == 4)

# Handle unsuccessful mergers
unsuccessful <- de_shp_fed_data %>%
  dplyr::filter(is.na(cdu_csu) | is.na(spd) | is.na(gruene) | is.na(afd))

# Prepare 2021 AGS names
d_fed_2021_names <- d_fed %>%
  dplyr::filter(election_year == "2021") %>%
  left_join_check_obs(
    read_excel(
      path = "data/crosswalks/raw/31122021_Auszug_GV.xlsx",
      sheet = 2
    ) %>%
      dplyr::select(
        Land = `...3`,
        RB = `...4`,
        Kreis = `...5`,
        Gemeinde = `...7`,
        ags_name = `...8`
      ) %>%
      mutate(
        Land = pad_zero_conditional(Land, 1),
        Kreis = pad_zero_conditional(Kreis, 1),
        Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
        Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
        ags = paste0(Land, RB, Kreis, Gemeinde)
      ) %>%
      slice(6:16065) %>%
      dplyr::filter(!is.na(Gemeinde)) %>%
      dplyr::select(ags, ags_name),
    by = "ags"
  )

# Reduce names and prepare for merge
d_fed_2021_names_red <- d_fed_2021_names %>%
  dplyr::select(ags_name, state, cdu_csu, spd, gruene, afd) %>%
  mutate(ags_name = str_remove(ags_name, ",.*"))

# Merge unsuccessful with reduced names
merge_by_name <- unsuccessful %>%
  dplyr::select(-c(cdu_csu:afd)) %>%
  left_join_check_obs(d_fed_2021_names_red, by = c("GEN" = "ags_name", "SN_L" = "state"))

# Build final dataframe for plots
plot_df <- de_shp_fed_data %>%
  dplyr::filter(!is.na(cdu_csu) & !is.na(spd) & !is.na(gruene) & !is.na(afd)) %>%
  bind_rows(merge_by_name) %>%
  group_by(AGS) %>%
  slice(1) %>%
  ungroup()

# Inspect missing data
missing_data <- plot_df %>%
  dplyr::filter(if_all(c(cdu_csu, spd, gruene, afd, turnout), is.na)) %>%
  nrow()

# Check for turnout > 1
turnout_over_one <- plot_df %>%
  dplyr::filter(turnout > 1) %>%
  dplyr::select(AGS, GEN, turnout)

# Print summary
cat("Number of observations without voting data:", missing_data, "\n")
cat("Number of observations with turnout > 1:", nrow(turnout_over_one), "\n")

### Function to create maps
plot_fed_map <- function(fill_var, legend_label, fill_palette, limits) {
  ggplot() +
    geom_sf(data = plot_df, mapping = aes(fill = .data[[fill_var]]), colour = "NA") +
    geom_sf(data = de_shp_bula, fill = NA, colour = "grey30", linewidth = 0.2) +
    coord_sf() +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      title = element_text(size = 9),
      axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      legend.position = "bottom", legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = -10, r = 0, b = 0, l = 0)
    ) +
    scale_fill_distiller(NULL,
      palette = fill_palette,
      na.value = "white",
      direction = 1,
      limits = limits,
      guide = guide_legend(
        keyheight = unit(2, units = "mm"), keywidth = unit(11, units = "mm"),
        label.position = "bottom", title.position = "top", nrow = 1
      )
    )
}

### Turnout
p_fed_turnout <- plot_fed_map("turnout", "Turnout", "Purples", turnout_range)

# Save as PDF and PNG
# ggsave("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_turnout.pdf", plot = p_fed_turnout, width = 4, height = 6)
ggsave("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_turnout.png", plot = p_fed_turnout, width = 4, height = 6, dpi = 450)

### CDU/CSU
p_fed_CDU <- plot_fed_map("cdu_csu", "Share CDU/CSU", "Blues", cdu_range)

# Save as PDF and PNG
# ggsave("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_CDU.pdf", plot = p_fed_CDU, width = 4, height = 6)
ggsave("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_CDU.png", plot = p_fed_CDU, width = 4, height = 6, dpi = 450)

### SPD
p_fed_SPD <- plot_fed_map("spd", "Share SPD", "Reds", spd_range)

# Save as PDF and PNG
# ggsave("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_SPD.pdf", plot = p_fed_SPD, width = 4, height = 6)
ggsave("~/Documents/GitHub/german_election_data/output/figures/map_elec_fed_SPD.png", plot = p_fed_SPD, width = 4, height = 6, dpi = 450)

### List of generated plots with descriptions

plot_list <- data.frame(
  filename = c(
    "map_elec_muni_turnout.png",
    "map_elec_muni_CDU.png",
    "map_elec_muni_SPD.png",
    "map_elec_state_turnout.png",
    "map_elec_state_CDU.png",
    "map_elec_state_SPD.png",
    "map_elec_fed_turnout.png",
    "map_elec_fed_CDU.png",
    "map_elec_fed_SPD.png"
  ),
  description = c(
    "Turnout in municipal elections",
    "CDU/CSU vote share in municipal elections",
    "SPD vote share in municipal elections",
    "Turnout in state elections",
    "CDU/CSU vote share in state elections",
    "SPD vote share in state elections",
    "Turnout in federal elections",
    "CDU/CSU vote share in federal elections",
    "SPD vote share in federal elections"
  )
)

# Full paths for plots
plot_list <- plot_list %>%
  mutate(full_path = file.path("output/figures", filename))

# Now move the plots to the Overleaf folder
if (Sys.info()["user"] == "hanno") {
  to_path <- "~/Dropbox/Apps/Overleaf/ElectionPaper/figures/"
} else if (Sys.info()["user"] == "vincentheddesheimer") {
  to_path <- "~/Dropbox (Princeton)/Apps/Overleaf/ElectionPaper/figures"
} else {
  to_path <- ""
}

# Copy plots to Overleaf folder
file.copy(
  from = plot_list$full_path,
  to = to_path,
  overwrite = TRUE,
  recursive = FALSE,
  copy.mode = TRUE
)
