### Download and clean state election data
# Vincent Heddesheimer, Hanno Hilbig
# First: March, 06, 2024
# Last: Oct, 01, 2024

rm(list = ls())

# install package if not installed
#install_github('sumtxt/wiesbaden')
pacman::p_load(wiesbaden, devtools, reshape2, pbapply, lubridate)


# Plug in your credentials
# See wiesbaden package documentation for details
# genesis <- c(user="", password="", db="regio")
# test_login(genesis=genesis)


## Make party dict

pdict <- c('afd' = 'AFD', 'gruene' = 'B90-GRUENE', 'cdu' = 'CDU',
           'csu' = 'CSU', 'fdp' = 'FDP', 'linke_pds' = 'PDS', 'spd' = 'SPD',
           'other_party' = 'SONSTIGE')

## Landtagswahlen table list

# list <- retrieve_datalist(tableseries="143*", genesis=genesis)
# list <- retrieve_datalist(tableseries="14331*", genesis=genesis)

# tableseries start with 14331* to 14346*
# have to build list of 14331:14346 but with the * at the end
labs <- c(paste0("1433", 1:9, "*"), paste0("1434", 0:6, "*"))

i <- labs[1]

## Apply this to all tables 

out <- pblapply(1:length(labs), function(i) {
  
  cat('\nIteration is ', i)
  
  d <- retrieve_datalist(tableseries=labs[i], genesis=genesis) 
  
  ## Get Table ID
  
  id <- d %>% dplyr::filter(str_detect(description, "Gemeinden")) %>%
    dplyr::filter(str_detect(description, 'Parteien')) %>% .[, 1] %>% unlist() %>%
    as.character()
  
  ## Get ID for the turnout table
  
  # id_eligible <- d %>% dplyr::filter(str_detect(description, "Gemeinden")) %>%
  #   dplyr::filter(str_detect(description, 'Wahlberechtigte')) %>% .[, 1] %>% unlist()%>%
  #   as.character()
  
  id_turnout <- d %>% dplyr::filter(str_detect(description, "Gemeinden")) %>%
    dplyr::filter(str_detect(description, 'Wahlbeteiligung')) %>% .[, 1] %>% unlist()%>%
    as.character()
  
  ## Only continue if something was found
  
  if (!length(id) == 0) {
    
    ## 
    
    cat('\nGemeinden found')
    
    ## Get data
    
    data <- retrieve_data(tablename=id, genesis=genesis) %>%
     dplyr::select(GEMEIN, PART03, STAG, WAHL04_val)
    
# in any of the datasets in the list of datasets: WAHL04_qual 


    ## Get total votes
    
    votes_tot <- data %>%
      group_by(GEMEIN, STAG) %>%
      summarise(valid = sum(WAHL04_val, na.rm = T))
    
    ## Merge to the main data
    
    data <- left_join(data, votes_tot)
    
    ## Get % shares
    
    data <- data %>%
      mutate(voteshare = WAHL04_val / valid) %>%
      dplyr::select(GEMEIN, PART03, STAG, voteshare)
    
    ## rename parties
    
    data$PART03 <- factor(data$PART03)
    
    ## Via Loop
    
    for (j in 1:length(levels(data$PART03))) {
      print(j)
      levels(data$PART03)[j] <- names(pdict)[pdict == levels(data$PART03)[j]]
    }
    
    ## To character
    
    data <- data %>%
      mutate(PART03 = as.character(PART03)) %>%
      rename(party = PART03, ags = GEMEIN, date = STAG) %>%
      mutate(date = as.Date(date, format = '%d.%m.%Y'))
    
    ## NaN to NA
    
    data$voteshare[is.nan(data$voteshare)] <- NA
    
    ## Reshape long to wide
    
    data_wide <- data %>% spread(party, voteshare)
    
    ## Get turnout
    cat('Getting turnout')
    turn_data <- retrieve_data(tablename=id_turnout, genesis=genesis) %>%
      dplyr::select(GEMEIN, STAG, WAHLSR_val, WAHL01_val, WAHL04_val) %>%
      dplyr::mutate(ags = GEMEIN, date = as.Date(STAG, format = '%d.%m.%Y')) %>%
      mutate(turnout = WAHLSR_val / 100) %>%
      dplyr::select(ags, date, turnout, eligible_voters = WAHL01_val, valid_votes = WAHL04_val) %>%
      distinct(ags, date, .keep_all = T)
    
    ## Merge to the main results
    
    data_wide <- left_join(data_wide, turn_data)
    
    ## Return
    
    data_wide
  }
})

## To Big DF

out_df <- out %>%
  reduce(rbind) %>%
  arrange(ags, date)

glimpse(out_df)

## 

parties <- colnames(out_df)[3:9]

## Check for total missings

is_miss <- pbapply(out_df[, parties], 1, function(x) all(is.na(x)))
sum(is_miss) / nrow(out_df)

## Looks pretty good
## Check if the AGS make sense

table(nchar(out_df$ags))

out_df <- out_df %>%
  dplyr::filter(!nchar(ags) == 7)

## Add election year / election date / state

## 

out_df <- out_df %>%
  mutate(
    election_year = lubridate::year(date),
    election_date = case_when(
        election_year == "2006" & state_name == "Berlin" ~ ymd("2006-09-17"),
        election_year == "2008" & state_name == "Bavaria" ~ ymd("2008-09-28"),
        election_year == "2008" & state_name == "Niedersachsen" ~ ymd("2008-01-27"),
        election_year == "2009" & state_name == "Brandenburg" ~ ymd("2009-09-27"),
        election_year == "2009" & state_name == "Hesse" ~ ymd("2009-01-18"),
        election_year == "2009" & state_name == "Saarland" ~ ymd("2009-08-30"),
        election_year == "2009" & state_name == "Saxony" ~ ymd("2009-08-30"),
        election_year == "2009" & state_name == "Schleswig-Holstein" ~ ymd("2009-09-27"),
        election_year == "2009" & state_name == "Thuringia" ~ ymd("2009-08-30"),
        election_year == "2010" & state_name == "North Rhine-Westphalia" ~ ymd("2010-05-09"),
        election_year == "2011" & state_name == "Baden-Württemberg" ~ ymd("2011-03-27"),
        election_year == "2011" & state_name == "Berlin" ~ ymd("2011-09-18"),
        election_year == "2011" & state_name == "Mecklenburg-Vorpommern" ~ ymd("2011-09-04"),
        election_year == "2011" & state_name == "Rhineland-Palatinate" ~ ymd("2011-03-27"),
        election_year == "2011" & state_name == "Saxony-Anhalt" ~ ymd("2011-03-20"),
        election_year == "2012" & state_name == "Saarland" ~ ymd("2012-03-25"),
        election_year == "2012" & state_name == "Schleswig-Holstein" ~ ymd("2012-05-06"),
        election_year == "2012" & state_name == "North Rhine-Westphalia" ~ ymd("2012-05-13"),
        election_year == "2013" & state_name == "Bavaria" ~ ymd("2013-09-15"),
        election_year == "2013" & state_name == "Hesse" ~ ymd("2013-09-22"),
        election_year == "2013" & state_name == "Niedersachsen" ~ ymd("2013-01-20"),
        election_year == "2014" & state_name == "Brandenburg" ~ ymd("2014-09-14"),
        election_year == "2014" & state_name == "Saxony" ~ ymd("2014-08-31"),
        election_year == "2014" & state_name == "Thuringia" ~ ymd("2014-09-14"),
        election_year == "2016" & state_name == "Baden-Württemberg" ~ ymd("2016-03-13"),
        election_year == "2016" & state_name == "Berlin" ~ ymd("2016-09-18"),
        election_year == "2016" & state_name == "Mecklenburg-Vorpommern" ~ ymd("2016-09-04"),
        election_year == "2016" & state_name == "Rhineland-Palatinate" ~ ymd("2016-03-13"),
        election_year == "2016" & state_name == "Saxony-Anhalt" ~ ymd("2016-03-13"),
        election_year == "2017" & state_name == "Niedersachsen" ~ ymd("2017-10-15"),
        election_year == "2017" & state_name == "North Rhine-Westphalia" ~ ymd("2017-05-14"),
        election_year == "2017" & state_name == "Saarland" ~ ymd("2017-03-26"),
        election_year == "2018" & state_name == "Bavaria" ~ ymd("2018-10-14"),
        election_year == "2018" & state_name == "Hesse" ~ ymd("2018-10-28"),
        election_year == "2019" & state_name == "Brandenburg" ~ ymd("2019-09-01"),
        election_year == "2019" & state_name == "Saxony" ~ ymd("2019-09-01"),
        election_year == "2019" & state_name == "Thuringia" ~ ymd("2019-10-27"),
        .default = NA
    ),
    state = substr(ags, 1, 2)
  )

# check if there are missing values in the election date variable
if (out_df |> filter(is.na(election_date)) |> nrow() == 0) {
  message("No missing values for election_date")
} else {
  message("There are missing values for election_date")
}

# ## Gen left right
# 
# lparties <- c('spd', 'linke_pds', 'gruene')
# rparties <- c('cdu', 'csu', 'fdp')
# 
# ltot <- apply(out_df[, lparties], 1, sum, na.rm = T)
# rtot <- apply(out_df[, rparties], 1, sum, na.rm = T)
# 
# out_df <- out_df %>%
#   mutate(left_total = ltot,
#          right_total = rtot)

##

state_elections <- out_df

## Distribution

table(out_df$election_year, out_df$state)


glimpse(state_elections)

# some final transformations ----------------------------------------------
state_elections <- state_elections |>
  mutate(csu = ifelse(state == '09', cdu, NA),
         cdu = ifelse(state == '09', NA, cdu)) |>
  # create cdu_csu variable
  rowwise() |>
  mutate(cdu_csu = sum(cdu, csu, na.rm = TRUE)) |>
  ungroup() |>
  select(ags, election_year, state, date, eligible_voters, valid_votes, turnout, 
         cdu, csu, spd, gruene, fdp, linke_pds, afd, other = other_party, cdu_csu)
glimpse(state_elections)

# data for Schleswig Holstein in 2017 is not complete: remove state == 01 & year == 2017
state_elections <- state_elections |>
  filter(!(state == '01' & election_year == 2017))




## Save for now

fwrite(state_elections, 'data/state_elections/final/state_unharm.csv')
write_rds(state_elections, 'data/state_elections/final/state_unharm.rds')


# Fix BaWü data from raw files -------------------------------------------
# The GENESIS API reports only Urne (in-person) valid vote counts for
# non-kreisfreie BaWü municipalities, missing Briefwahl (mail-in) votes.
# This undercounts valid_votes by ~13% (2011) / ~17% (2016).
# Fix: replace BaWü 2011/2016 data with raw file data that includes totals
# (Urne + Briefwahl).
#
# This section can also be run standalone after loading state_unharm.rds:
#   state_elections <- read_rds('data/state_elections/final/state_unharm.rds')

library(readxl)

## --- BaWü 2011: from Excel, Z (total) rows ---
xlsx_path <- "data/state_elections/raw/Landtagswahlen/Baden-Württemberg/2011 & früher/LW1956_bis_2021_Gemeindeergebnisse.xlsx"
sheet_2011 <- excel_sheets(xlsx_path) |> str_subset("2011")

raw_2011 <- read_excel(xlsx_path, sheet = sheet_2011, skip = 3, col_names = FALSE)

bw_2011 <- raw_2011 |>
  filter(...3 == "G", ...5 == "Z", str_detect(...6, "^Anza")) |>
  transmute(
    ags = paste0("08", str_pad(...2, 6, pad = "0")),
    election_year = 2011,
    state = "08",
    date = as.Date("2011-03-27"),
    eligible_voters = as.numeric(...7),
    valid_votes = as.numeric(...10),
    # Party vote counts (cols 11-30 in Excel)
    cdu_n   = as.numeric(...11),
    spd_n   = as.numeric(...12),
    grn_n   = as.numeric(...13),
    fdp_n   = as.numeric(...14),
    linke_n = as.numeric(...20), # DIE LINKE
    # No AfD in 2011
    afd_n   = NA_real_
  ) |>
  mutate(
    other_n = valid_votes - rowSums(across(c(cdu_n, spd_n, grn_n, fdp_n, linke_n)), na.rm = TRUE),
    turnout = valid_votes / eligible_voters,
    cdu       = cdu_n / valid_votes,
    csu       = NA_real_,
    spd       = spd_n / valid_votes,
    gruene    = grn_n / valid_votes,
    fdp       = fdp_n / valid_votes,
    linke_pds = linke_n / valid_votes,
    afd       = NA_real_,
    other     = other_n / valid_votes,
    cdu_csu   = cdu
  ) |>
  # Drop gemeindefreie Gebiete (99x codes: shared Briefwahl areas)
  filter(!str_detect(ags, "99[0-9]$")) |>
  select(ags, election_year, state, date, eligible_voters, valid_votes, turnout,
         cdu, csu, spd, gruene, fdp, linke_pds, afd, other, cdu_csu)

cat("BaWü 2011 from raw files:", nrow(bw_2011), "municipalities,",
    "valid_votes =", sum(bw_2011$valid_votes, na.rm = TRUE), "\n")

## --- BaWü 2016: from CSV ---
csv_2016 <- "data/state_elections/raw/Landtagswahlen/Baden-Württemberg/Baden-Württemberg_2016_Landtagswahl.csv"
raw_2016 <- read_delim(csv_2016, delim = ";", show_col_types = FALSE,
                       locale = locale(encoding = "latin1"))

bw_2016 <- raw_2016 |>
  mutate(ags = str_extract(Gemeinde, "^[0-9]+")) |>
  transmute(
    ags,
    election_year = 2016,
    state = "08",
    date = as.Date("2016-03-13"),
    eligible_voters = Wahlberechtigte,
    valid_votes = `Gültige Stimmen`,
    turnout = `Wähler(innen)` / Wahlberechtigte,
    cdu       = CDU / `Gültige Stimmen`,
    csu       = NA_real_,
    spd       = SPD / `Gültige Stimmen`,
    gruene    = .data[["GRÜNE"]] / `Gültige Stimmen`,
    fdp       = FDP / `Gültige Stimmen`,
    linke_pds = .data[["DIE LINKE"]] / `Gültige Stimmen`,
    afd       = AfD / `Gültige Stimmen`,
    other     = (valid_votes - CDU - SPD - .data[["GRÜNE"]] - FDP -
                   .data[["DIE LINKE"]] - AfD) / `Gültige Stimmen`,
    cdu_csu   = cdu
  ) |>
  # Drop gemeindefreie Gebiete (99x codes)
  filter(!str_detect(ags, "99[0-9]$"))

cat("BaWü 2016 from raw files:", nrow(bw_2016), "municipalities,",
    "valid_votes =", sum(bw_2016$valid_votes, na.rm = TRUE), "\n")

## --- Replace BaWü rows in state_elections ---
state_elections <- state_elections |>
  filter(!(state == "08" & election_year %in% c(2011, 2016))) |>
  bind_rows(bw_2011, bw_2016) |>
  arrange(ags, election_year)

cat("After BaWü fix: BaWü eligible_voters =",
    sum(state_elections$eligible_voters[state_elections$state == "08" &
          state_elections$election_year == 2011], na.rm = TRUE),
    "(2011),",
    sum(state_elections$eligible_voters[state_elections$state == "08" &
          state_elections$election_year == 2016], na.rm = TRUE),
    "(2016)\n")

## Re-save
fwrite(state_elections, 'data/state_elections/final/state_unharm.csv')
write_rds(state_elections, 'data/state_elections/final/state_unharm.rds')


### END