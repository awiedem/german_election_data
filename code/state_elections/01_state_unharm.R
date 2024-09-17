### Download and clean state election data
# Vincent Heddesheimer, Hanno Hilbig
# First: March, 06, 2024
# Last: Aug, 08, 2024

rm(list = ls())

# install package if not installed
#install_github('sumtxt/wiesbaden')
pacman::p_load(wiesbaden, devtools, reshape2, pbapply, lubridate)


# Plug in your credentials
# See wiesbaden package documentation for details
genesis <- c(user="RE010680", password="BayernMuenchen1900!", db="regio")
test_login(genesis=genesis)

### Use this for final code and delete the above!!!
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

## Add year / state

## 

out_df <- out_df %>% 
  mutate(election_year = lubridate::year(date),
         state = substr(ags, 1, 2))

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

# some final transformations
state_elections <- state_elections |>
  mutate(csu = ifelse(state == '09', cdu, 0)) |>
  # create cdu_csu variable
  rowwise() |>
  mutate(cdu_csu = cdu + csu) |>
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



# Inspect -----------------------------------------------------------------

df <- read_rds('data/state_elections/final/state_unharm.rds')

# what's up with state == 01 in 2017?
insp <- df |>
  filter(state == '01', election_year == 2017) 

### END