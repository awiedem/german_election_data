pacman::p_load(
    tidyverse, data.table, readxl, haschaR,
    conflicted, sf
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)

rm(list = ls())

# Load 2021 data

df21 <- fread("data/federal_elections/municipality_level/raw/BTW21/btw21_wbz_ergebnisse.csv", encoding = "UTF-8") |>
    # csv file includes erst- and zweitstimme; select only variables for zweitstimme
    select(c(1:17, 65:106)) |>
    slice(-1) |>
    # Multiple entries for each municipality: variable Wbz (Wahlbezirk)
    ###
    # Create ags
    mutate(
        Land = pad_zero_conditional(Land, 1),
        Kreis = pad_zero_conditional(Kreis, 1),
        Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
        Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
        ags = paste0(Land, Regierungsbezirk, Kreis, Gemeinde),
        across(`Wahlberechtigte (A)`:Volt, as.numeric)
    ) |>
    rename(
        BWBez = `Kennziffer Briefwahlzugehörigkeit`,
        BA = Bezirksart
    )

# Summarize all variables by ags & BWBez & Bezirksart
# (code from Vincent)

df21_bezirksarten <- df21 |>
    group_by(ags, BWBez, BA) |>
    summarise_at(vars(`Wahlberechtigte (A)`:Volt), sum, na.rm = TRUE) |>
    mutate(number_voters_orig = ifelse(BA == 5, 0, `Wählende (B)`)) |>
    relocate(number_voters_orig, .before = `Wählende (B)`)

# Get ags that have their own mailin data
# Hanno: the way I understand this, "own mailin data" means that the ags has only one mailin district - correct? @Vincent
# Vincent: ags_w_mailin21 are all ags that have an assigned mailin district (maybe have to edit the comment here but this is what the code does)
ags_w_mailin21 <- df21_bezirksarten |>
    filter(BA == 5) |>
    pull(ags)

# Subset main data to ags with mailin data
df21_mailin <- df21_bezirksarten |>
    filter(ags %in% ags_w_mailin21)

# What do the fields mean?

# Anzahl der Wahlberechtigten (A)
# Anzahl der Wahlberechtigten ohne Sperrvermerk (Al) Anzahl der Wahlberechtigten mit Sperrvermerk (A2)
# Anzahl der Wahlberechtigten nach § 25 Abs. 2 (A3)
# Anzahl der Wähler (B)
# darunter: Anzahl der Wähler mit Wahlschein (B1)

# Get centroids of the munis in question
# @Vincent: this is not used right now, you can disregard it

get_centroids <- F

if (get_centroids) {
    centroids <- sf::st_read("data/shapefiles/2021/vg250_ebenen_0101/VG250_GEM.shp") %>%
        st_transform(crs = 4326) %>%
        filter(AGS %in% ags_w_mailin21) %>%
        st_centroid() %>%
        mutate(
            coords = st_coordinates(.),
            lon = coords[, 1],
            lat = coords[, 2]
        ) %>%
        dplyr::select(-coords) %>%
        dplyr::select(AGS, lon, lat) %>%
        dplyr::rename(ags = AGS) %>%
        distinct(ags, .keep_all = TRUE)

    centroids$geometry <- NULL

    # Save centroids

    saveRDS(centroids, "data/shapefiles/2021/2021_centroids.rds")
} else {
    centroids <- readRDS("data/shapefiles/2021/2021_centroids.rds")
}

# Population data ---------------------------------------------------------

# Load population data
# @Vincent: also not used for now, you can disregard it

pop21 <- read_excel("data/covars_municipality/raw/municipality_sizes/31122021_Auszug_GV.xlsx",
    sheet = 2, col_types = "numeric"
) |>
    select(
        Land = `...3`,
        RB = `...4`,
        Kreis = `...5`,
        Gemeinde = `...7`,
        area = `...9`,
        pop = `...10`
    ) |>
    slice(9:16065) |>
    # delete NAs
    filter(!is.na(Gemeinde)) |>
    # Create ags
    mutate(
        Land = pad_zero_conditional(Land, 1),
        Kreis = pad_zero_conditional(Kreis, 1),
        Gemeinde = pad_zero_conditional(Gemeinde, 1, "00"),
        Gemeinde = pad_zero_conditional(Gemeinde, 2, "0"),
        ags = paste0(Land, RB, Kreis, Gemeinde)
    ) |>
    select(ags, area, pop)

# Merge pop data to df21

df21_mailin <- df21_mailin |>
    left_join_check_obs(pop21, by = "ags")

parties <- df21_mailin %>%
    ungroup() %>%
    select(CDU:Volt) %>%
    colnames()

## Rename some variables -----------------------

df21_mailin <- df21_mailin %>%
    dplyr::rename(
        A = `Wahlberechtigte (A)`,
        A2 = `Wahlberechtigte mit Sperrvermerk (A2)`,
        A3 = `Wahlberechtigte nach § 25 Abs. 2 BWO (A3)`,
        B = `Wählende (B)`,
        B1 = `Wählende mit Wahlschein (B1)`
    )

# Get data frame containing only postal voting districts (Briefwahlbezirke)
# BA == 5 indicates postal voting districts
# BWBez == "00" filters for the main postal voting district

# Ie here we have munis that have only one mail-in district
# We select the mailin districts by doing BA == 5

df_mailin <- df21_mailin %>%
    filter(BA == 5) %>%
    filter(BWBez == "00") %>%
    dplyr::select(ags, BWBez, A2, A3, B1, all_of(parties))

# Get data frame containing only regular voting districts (not postal voting)
# BA != 5 excludes postal voting districts
# BWBez == "00" filters for the main district

# Ie here we have munis that have only one mail-in district
# However, we select the non-mailin districts by doing BA != 5

# Refine the regular voting district data to only include BA == 0 (regular districts)
# This excludes both postal voting (5) and districts without details (8)
df_not_mailin <- df21_mailin %>%
    filter(BA == 0) %>%
    filter(BWBez == "00") %>%
    dplyr::select(ags, BWBez, A2, A3, B1, all_of(parties))

# Vincent: this looks good

nrow(df_not_mailin)
nrow(df_mailin)

# CDU CSU has to be combined

df_not_mailin <- df_not_mailin %>%
    mutate(CDU_CSU = ifelse(CDU == 0, CSU, CDU)) %>%
    dplyr::select(-CDU, -CSU)

df_mailin <- df_mailin %>%
    mutate(CDU_CSU = ifelse(CDU == 0, CSU, CDU)) %>%
    dplyr::select(-CDU, -CSU)

# Redefine parties

parties <- c("CDU_CSU", parties[!parties %in% c("CDU", "CSU")]) %>%
    unique()

# Good - same number of rows

# Check if the AGS are the same in each data set

ags_mailin <- df_mailin %>% pull(ags)
ags_not_mailin <- df_not_mailin %>% pull(ags)

all(ags_mailin %in% ags_not_mailin)

# Yep - same AGS

ags_list <- df_not_mailin %>% pull(ags)
n_districts <- 100
method <- "kmeans"

# Function to get artificial mailin districts

make_district <- function(ags_list = NULL, method = "simple", n_districts = 100) {
    # Input validation
    if (is.null(ags_list)) {
        stop("ags_list must be provided")
    }

    if (method == "simple") {
        # Calculate how many AGS should be in each district
        n_ags <- length(ags_list)
        ags_per_district <- ceiling(n_ags / n_districts)

        # Create repeated district IDs
        district_ids <- rep(1:n_districts, each = ags_per_district)[1:n_ags]

        # Randomly shuffle the district IDs
        district_ids <- sample(district_ids)

        # Create districts dataframe
        districts <- data.frame(
            ags = ags_list,
            district_id = district_ids
        )

        return(districts)
    } else if (method == "kmeans") {
        # Only use ags that are in the centroids data frame

        ags_list <- ags_list[ags_list %in% centroids$ags]

        # Use k-means clustering to create districts

        # First, get the centroids for the ags in ags_list

        centroids_use <- centroids %>%
            filter(ags %in% ags_list)

        # Run k-means

        kmeans_result <- kmeans(centroids_use %>%
            dplyr::select(-ags), centers = n_districts)

        kmeans_cluster <- kmeans_result$cluster

        districts <- data.frame(
            ags = ags_list,
            district_id = kmeans_cluster
        )
        return(districts)
    } else {
        stop("Only 'simple' and 'kmeans' methods are currently implemented")
    }
}


# Example usage:
districts <- make_district(ags_list = ags_list, n_districts = 100) %>%
    arrange(desc(ags))

# Some checks:
# Distribution of A2 + A3 by district (this means: blocked voters by district)

df_not_mailin %>%
    left_join_check_obs(districts, by = "ags") %>%
    group_by(district_id) %>%
    summarise(
        blocked_voter_mean = mean(A2 + A3, na.rm = TRUE),
        blocked_voter_median = median(A2 + A3, na.rm = TRUE),
        n_munis = n()
    ) %>%
    arrange(desc(blocked_voter_mean)) %>%
    ggplot(aes(x = blocked_voter_median)) +
    geom_histogram() +
    labs(
        title = "Distribution of blocked voters by district",
        x = "Median blocked voters per district",
        y = "Number of districts"
    ) +
    theme_hanno()

# Make districts using k-means

districts_kmeans <- make_district(ags_list = ags_list, n_districts = 100, method = "kmeans") %>%
    arrange(desc(ags))

# Function to distrbute votes across districts --------------------------------

# This is the set of AGS in district 1

district_ags_set <- districts %>%
    filter(district_id == 1) %>%
    pull(ags)

distribute_votes <- function(district_ags_set = NULL) {
    # Subset of df_mailin with only the AGS in district_ags_set
    df_mailin_subset <- df_mailin %>%
        filter(ags %in% district_ags_set)

    # In this subset, sum over votes of all parties across all rows

    df_mailin_subset_agg <- df_mailin_subset %>%
        ungroup() %>%
        summarise(across(all_of(parties), \(x) sum(x, na.rm = TRUE))) %>%
        pivot_longer(cols = all_of(parties), names_to = "party", values_to = "votes_mailin_district_total")

    # This results in the total number of mail in votes for each party in the district

    # Also create a long version of the mailin votes that just has ags and all parties

    df_mailin_long_not_agg <- df_mailin_subset %>%
        ungroup() %>%
        dplyr::select(ags, all_of(parties)) %>%
        pivot_longer(cols = all_of(parties), names_to = "party", values_to = "votes_mailin_actual")

    # Now, the in-person votes:
    # Subset in-person votes to only include the AGS in district_ags_set
    # Then, create weight, which is based on A2 + A3

    df_not_mailin_subset <- df_not_mailin %>%
        ungroup() %>%
        filter(ags %in% district_ags_set) %>%
        mutate(votes_blocked = A2 + A3) %>%
        mutate(weight_blocked = votes_blocked / sum(votes_blocked))

    # Data frame that is just the weights and ags

    blocked_weights <- df_not_mailin_subset %>%
        dplyr::select(ags, weight_blocked)

    # Check if weights sum to 1

    message("Sum of weights: ", sum(blocked_weights$weight_blocked))

    # In-person votes, long format

    df_not_mailin_long <- df_not_mailin_subset %>%
        dplyr::select(ags, all_of(parties)) %>%
        pivot_longer(cols = all_of(parties), names_to = "party", values_to = "votes_in_person")

    # Mail in votes, long format

    df_mailin_long <- df_mailin_subset %>%
        ungroup() %>%
        dplyr::select(ags, all_of(parties)) %>%
        pivot_longer(cols = all_of(parties), names_to = "party", values_to = "votes_mailin")

    # Join mailin and in-person votes, also join weights

    df_long <- df_mailin_long %>%
        left_join_check_obs(df_not_mailin_long, by = c("ags", "party")) %>%
        left_join_check_obs(blocked_weights, by = "ags") %>%
        left_join_check_obs(df_mailin_subset_agg, by = "party")

    # Distribute votes across municipalities

    df_distributed <- df_long %>%
        mutate(votes_mailin_distributed = round(votes_mailin_district_total * weight_blocked, 0)) %>%
        mutate(votes_total_distributed = votes_mailin_distributed + votes_in_person) %>%
        mutate(votes_total_actual = votes_in_person + votes_mailin) %>%
        group_by(ags) %>%
        mutate(
            votes_total_actual_ags = sum(votes_total_actual),
            votes_total_distributed_ags = sum(votes_total_distributed)
        ) %>%
        ungroup() %>%
        mutate(vote_share_distributed = votes_total_distributed / votes_total_distributed_ags) %>%
        mutate(vote_share_actual = votes_total_actual / votes_total_actual_ags)

    return(df_distributed)
}

# Workflow:

# 1. Make districts using make_district
# 2. Run distribute_votes for each district, add district id to df_final

# Run for each district

districts <- make_district(ags_list = ags_list, n_districts = 100)

# For mat of distrcts: data frame w/ 2 cols: ags, district_id

district_id_list <- districts %>%
    pull(district_id) %>%
    unique()

n_districts <- 500
seed <- 123
method <- "simple"
verbose <- FALSE

# Diagnostics -------------------------------------------------------------

parties_check <- c("CDU_CSU", "SPD", "GRÜNE", "FDP", "DIE LINKE", "AfD")

# Function to run simulation -----------------------------------------------

run_allocation_simulation <- function(
    n_districts = 100,
    seed = 123, verbose = TRUE, method = "simple") {
    if (verbose) message("Starting simulation with ", n_districts, " districts")

    # Create districts
    if (verbose) message("Creating districts...")
    districts <- make_district(ags_list = ags_list, n_districts = n_districts, method = method)
    district_id_list <- districts %>%
        pull(district_id) %>%
        unique()

    # Merge population data to districts

    districts <- districts %>%
        left_join_check_obs(pop21, by = "ags")

    # Merge blocked voters data to districts

    districts <- districts %>%
        left_join_check_obs(df_not_mailin %>%
            dplyr::select(ags, A2, A3) %>%
            mutate(a2_a3 = A2 + A3), by = "ags")

    # Number of districts, mean and median district size

    n_districts <- unique(districts$district_id) %>% length()
    mean_district_size <- districts %>%
        count(district_id) %>%
        pull(n) %>%
        mean()
    median_district_size <- districts %>%
        count(district_id) %>%
        pull(n) %>%
        median()
    mean_district_pop <- districts %>%
        group_by(district_id) %>%
        summarize(sum_pop = sum(pop)) %>%
        pull(sum_pop) %>%
        mean(na.rm = TRUE)
    median_district_pop <- districts %>%
        group_by(district_id) %>%
        summarize(sum_pop = sum(pop)) %>%
        pull(sum_pop) %>%
        median(na.rm = TRUE)
    mean_district_a2_a3 <- districts %>%
        group_by(district_id) %>%
        summarise(sum_a2_a3 = sum(a2_a3, na.rm = TRUE)) %>%
        pull(sum_a2_a3) %>%
        mean(na.rm = TRUE)

    # Message

    if (verbose) message("Created ", length(district_id_list), " unique districts")

    if (verbose) message("District sizes: mean = ", mean_district_size, ", median = ", median_district_size)
    if (verbose) message("District population: mean = ", mean_district_pop, ", median = ", median_district_pop)

    # Run distribute_votes for each district
    if (verbose) message("Distributing votes across districts...")
    out_list <- lapply(district_id_list, function(did) {
        if (verbose && did %% 10 == 0) message("Processing district ", did, " of ", n_districts)
        ags_list <- districts %>%
            filter(district_id == did) %>%
            pull(ags) %>%
            unique()

        suppressMessages(
            distribute_votes(district_ags_set = ags_list) %>%
                mutate(district_id = did)
        )
    })

    # Combine results
    if (verbose) message("Combining results and calculating statistics...")
    out_df <- out_list %>% reduce(bind_rows)

    # Filter and process results
    parties_check <- c("CDU_CSU", "SPD", "GRÜNE", "FDP", "DIE LINKE", "AfD")

    out_check <- out_df %>%
        filter(party %in% parties_check) %>%
        left_join_check_obs(
            df_not_mailin %>%
                ungroup() %>%
                filter(ags %in% out_df$ags) %>%
                dplyr::select(ags, A2, A3) %>%
                mutate(a2_a3 = A2 + A3),
            by = "ags"
        ) %>%
        mutate(
            diff = vote_share_distributed - vote_share_actual,
            diff_abs = abs(diff)
        )

    # Calculate summary statistics
    out_check_sum <- out_check %>%
        group_by(party) %>%
        summarize(
            mean_diff = mean(diff, na.rm = TRUE),
            med_diff = median(diff, na.rm = TRUE),
            sd_diff = sd(diff, na.rm = TRUE),
            q25_diff = quantile(diff, 0.25, na.rm = TRUE),
            q75_diff = quantile(diff, 0.75, na.rm = TRUE),
            weighted_mean_diff = weighted.mean(diff,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            weighted_sd_diff = matrixStats::weightedSd(diff,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            weighted_median_diff = matrixStats::weightedMedian(diff,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            mean_diff_abs = mean(diff_abs, na.rm = TRUE),
            med_diff_abs = median(diff_abs, na.rm = TRUE),
            sd_diff_abs = sd(diff_abs, na.rm = TRUE),
            q25_diff_abs = quantile(diff_abs, 0.25, na.rm = TRUE),
            q75_diff_abs = quantile(diff_abs, 0.75, na.rm = TRUE),
            weighted_mean_diff_abs = weighted.mean(diff_abs,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            weighted_sd_diff_abs = matrixStats::weightedSd(diff_abs,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            weighted_median_diff_abs = matrixStats::weightedMedian(diff_abs,
                votes_total_distributed_ags,
                na.rm = TRUE
            )
        ) %>%
        ungroup() %>%
        mutate(across(-party, ~ . * 100)) %>%
        mutate(
            mean_district_size = mean_district_size,
            median_district_size = median_district_size,
            mean_district_pop = mean_district_pop,
            median_district_pop = median_district_pop,
            mean_district_a2_a3 = mean_district_a2_a3
        )

    sum_district <- out_check %>%
        group_by(district_id, party) %>%
        summarise(
            mean_diff = mean(diff, na.rm = TRUE),
            med_diff = median(diff, na.rm = TRUE),
            sd_diff = sd(diff, na.rm = TRUE),
            weighted_mean_diff = weighted.mean(diff,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            weighted_sd_diff = matrixStats::weightedSd(diff,
                votes_total_distributed_ags,
                na.rm = TRUE
            ),
            q25_diff = quantile(diff, 0.25, na.rm = TRUE),
            q75_diff = quantile(diff, 0.75, na.rm = TRUE),
        ) %>%
        ungroup() %>%
        mutate(
            mean_district_size = mean_district_size,
            median_district_size = median_district_size,
            mean_district_pop = mean_district_pop,
            median_district_pop = median_district_pop,
            mean_district_a2_a3 = mean_district_a2_a3
        )

    # Return both detailed and summary results
    if (verbose) message("Simulation complete!")
    return(list(
        detailed = out_check,
        summary_overall = out_check_sum,
        summary_district = sum_district,
        districts = districts
    ))
}

# Run simulations for different numbers of districts
district_sizes <- c(500, 1000)

set.seed(123)

run_simulations <- F

if (run_simulations) {
    simulation_results <- lapply(district_sizes, function(n) {
        results <- run_allocation_simulation(n_districts = n, seed = 1)
        results_kmeans <- run_allocation_simulation(n_districts = n, method = "kmeans", seed = 1)

        list(
            simple = list(
                summary_overall = results$summary_overall %>% mutate(n_districts = n, method = "simple"),
                summary_district = results$summary_district %>% mutate(n_districts = n, method = "simple"),
                detailed = results$detailed %>% mutate(n_districts = n, method = "simple"),
                districts = results$districts
            ),
            kmeans = list(
                summary_overall = results_kmeans$summary_overall %>% mutate(n_districts = n, method = "kmeans"),
                summary_district = results_kmeans$summary_district %>% mutate(n_districts = n, method = "kmeans"),
                detailed = results_kmeans$detailed %>% mutate(n_districts = n, method = "kmeans"),
                districts = results_kmeans$districts
            )
        )
    })

    saveRDS(simulation_results, "data/simulation_results.rds")
} else {
    simulation_results <- readRDS("data/simulation_results.rds")
}

# Combine summary_overall for both methods

simulation_summary_overall <- simulation_results %>%
    map_df(~ .x$simple$summary_overall) %>%
    bind_rows(simulation_results %>%
        map_df(~ .x$kmeans$summary_overall))


# View results grouped by number of districts and party
simulation_summary_overall <- simulation_summary_overall %>%
    arrange(n_districts, party) %>%
    mutate(method = factor(method, levels = c("simple", "kmeans"), labels = c("Simple random sample", "K-means clustering based on centroids")))

pd <- position_dodge(width = 0.15)

# Rename parties

simulation_summary_overall$party %>% unique()

simulation_summary_overall <- simulation_summary_overall %>%
    mutate(party = case_when(
        party == "CDU_CSU" ~ "CDU/CSU",
        party == "SPD" ~ "SPD",
        party == "GRÜNE" ~ "Grüne",
        party == "FDP" ~ "FDP",
        party == "DIE LINKE" ~ "Die Linke",
        party == "AfD" ~ "AfD"
    ))

# Merge weighted and unweighted for n_districts = 1000

plot_df <- simulation_summary_overall %>%
    filter(n_districts == 1000) %>%
    dplyr::select(party, method, weighted_mean_diff, weighted_sd_diff, mean_diff, sd_diff, mean_diff_abs, sd_diff_abs, weighted_mean_diff_abs, weighted_sd_diff_abs) %>%
    pivot_longer(cols = c(weighted_mean_diff, weighted_sd_diff, mean_diff, sd_diff, mean_diff_abs, sd_diff_abs, weighted_mean_diff_abs, weighted_sd_diff_abs), names_to = "statistic", values_to = "value") %>%
    mutate(what = ifelse(str_detect(statistic, "weighted"), "Weighted by total votes", "Unweighted")) %>%
    mutate(what2 = ifelse(str_detect(statistic, "mean"), "mean", "sd")) %>%
    mutate(what3 = ifelse(str_detect(statistic, "diff_abs"), "abs", "diff")) %>%
    dplyr::select(-statistic) %>%
    pivot_wider(names_from = what2, values_from = value) %>%
    mutate(what = factor(what, levels = c("Weighted by total votes", "Unweighted"))) %>%
    filter(method != "Simple random sample") %>%
    mutate(what3 = case_when(
        what3 == "diff" ~ "Simple~difference~(Delta[i]^p)",
        what3 == "abs" ~ "Absolute~difference~(abs(Delta[i]^p))"
    ))

# Plot w/ weighted mean, weighted sd

pd <- position_dodge(width = 0.4)

ggplot(plot_df, aes(
    x = party, y = mean
)) +
    geom_hline(
        yintercept = 0,
        linetype = "dotted", color = "gray"
    ) +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.0, position = pd) +
    geom_point(shape = 21, fill = "white", position = pd) +
    labs(
        title = NULL,
        subtitle = NULL,
        x = "Party",
        y = "Mean difference between distributed\nand actual vote shares across all municipalities"
    ) +
    theme_hanno() +
    theme(legend.position = "bottom") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_grid(what3 ~ what,
        labeller = labeller(
            what3 = label_parsed,
            what = label_value
        )
    )

# Save plot

ggsave("output/figures/measurement_error_allocation_weighted.pdf",
    width = 7, height = 5.5
)
