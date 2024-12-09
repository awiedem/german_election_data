# app.R

# Load required packages
pacman::p_load(shiny, sf, tidyverse, viridis, data.table, haschaR, lubridate)

# Load the data
load_data <- function() {
  # Load the three harmonized datasets using file.path for cross-platform compatibility
  fed_data <- read_rds("~/Documents/GitHub/german_election_data/data/federal_elections/municipality_level/final/federal_muni_harm.rds") %>%
    mutate(year = election_year)
  state_data <- read_rds("~/Documents/GitHub/german_election_data/data/state_elections/final/state_harm.rds") %>%
    mutate(year = election_year)
  muni_data <- read_rds("~/Documents/GitHub/german_election_data/data/municipal_elections/final/municipal_harm.rds") %>%
    mutate(election_year = year)
  
  # Load shape files
  de_shp_muni <- st_read("~/Documents/GitHub/german_election_data/data/shapefiles/2021/vg250_ebenen_0101/VG250_GEM.shp")
  de_shp_state <- st_read("~/Documents/GitHub/german_election_data/data/shapefiles/2021/vg250_ebenen_0101/VG250_LAN.shp") %>%
    dplyr::filter(GF == 4)
  
  return(list(
    fed_data = fed_data,
    state_data = state_data,
    muni_data = muni_data,
    de_shp_muni = de_shp_muni,
    de_shp_state = de_shp_state
  ))
}

# UI
ui <- fluidPage(
  titlePanel("German Election Data"),
  
  sidebarLayout(
    sidebarPanel(
      # Dataset selection
      selectInput("dataset", "Select Dataset:",
                 choices = c("Federal Elections" = "fed",
                           "State Elections" = "state",
                           "Municipal Elections" = "muni")),
      
      # Year selection (will be updated based on dataset)
      selectInput("year", "Select Year:", choices = NULL),
      
      # Variable selection
      selectInput("variable", "Select Variable:",
                 choices = c("CDU/CSU" = "cdu_csu",
                           "SPD" = "spd", 
                           "Greens" = "gruene",
                           "FDP" = "fdp",
                           "Left" = "linke_pds", 
                           "AfD" = "afd",
                           "Turnout" = "turnout"))
    ),
    
    mainPanel(
      plotOutput("map", height = "800px"),
      uiOutput("data_info")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data
  data <- reactiveVal(load_data())
  
  # Update year choices based on selected dataset
  observe({
    dataset <- switch(input$dataset,
                     "fed" = data()$fed_data,
                     "state" = data()$state_data,
                     "muni" = data()$muni_data)
    
    years <- sort(unique(dataset$election_year))
    updateSelectInput(session, "year",
                     choices = years,
                     selected = max(years))
  })
  
  # Create the map
  output$map <- renderPlot({
    # Get selected dataset
    current_data <- switch(input$dataset,
                          "fed" = data()$fed_data,
                          "state" = data()$state_data,
                          "muni" = data()$muni_data)
    
    # Filter for selected year
    year_data <- current_data %>%
      filter(election_year == input$year)
    
    # Merge with shape data
    map_data <- data()$de_shp_muni %>%

      left_join(year_data, by = c("AGS" = "ags"))
    
    # Create plot
    ggplot() +
      geom_sf(data = map_data, 
              aes(fill = .data[[input$variable]]),
              color = NA) +
      geom_sf(data = data()$de_shp_state,
              fill = NA,
              color = "grey30",
              size = 0.2) +
      {if(input$variable == "turnout") 
        scale_fill_viridis_c(name = "Turnout",
                            labels = scales::percent_format())
       else
        scale_fill_gradient(low = "white", 
                          high = switch(input$variable,
                                      "cdu_csu" = "#000000",
                                      "spd" = "#E3000F",
                                      "gruene" = "#46962B",
                                      "fdp" = "#FFFF00",
                                      "linke_pds" = "#BE3075",
                                      "afd" = "#009EE0"),
                          name = paste("Share", 
                                     switch(input$variable,
                                           "cdu_csu" = "CDU/CSU",
                                           "spd" = "SPD",
                                           "gruene" = "Greens",
                                           "fdp" = "FDP",
                                           "linke_pds" = "Left",
                                           "afd" = "AfD")),
                          labels = scales::percent_format())
      } +
      labs(title = paste0("Mean ", 
                         switch(input$variable,
                                "cdu_csu" = "CDU/CSU",
                                "spd" = "SPD",
                                "gruene" = "Greens",
                                "fdp" = "FDP",
                                "linke_pds" = "Left",
                                "afd" = "AfD",
                                "turnout" = "Turnout"),
                         ": ",
                         round(mean(year_data[[input$variable]], na.rm = TRUE) * 100, 2),
                         "%")) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
      )
    #     +  labs(title = paste(
    #     switch(input$dataset,
    #            "fed" = "Federal",
    #            "state" = "State",
    #            "muni" = "Municipal"),
    #     "Elections", input$year
    #   ))
  })
  
  # Display data information
  output$data_info <- renderUI({
    dataset <- switch(input$dataset,
                     "fed" = data()$fed_data,
                     "state" = data()$state_data,
                     "muni" = data()$muni_data)
    
    year_data <- dataset %>%
      filter(election_year == input$year)
    
    HTML(paste0(
      "Mean ", 
      switch(input$variable,
             "cdu_csu" = "CDU/CSU",
             "spd" = "SPD",
             "gruene" = "Greens",
             "fdp" = "FDP",
             "linke_pds" = "Left",
             "afd" = "AfD",
             "turnout" = "Turnout"),
      ": ",
      round(mean(year_data[[input$variable]], na.rm = TRUE) * 100, 2), "%"
    ))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
