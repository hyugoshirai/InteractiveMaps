# Define package dependencies
packages <- c("shiny", "leaflet", "tidyverse", "reactlog", "svglite", "shinyWidgets")

if (!require(shinyWidgets)) {
  install.packages('shinyWidgets', dependencies = TRUE)
  library(shinyWidgets)
}

if (!require(tidyverse)) {
  install.packages('tidyverse', dependencies = TRUE)
  library(tidyverse)
}

if (!require(svglite)) {
  install.packages('svglite', dependencies = TRUE)
  library(svglite)
}

# Install packages if they are not already installed
try(install.packages(packages[!packages %in% installed.packages()]))

# Load packages
try(invisible(lapply(packages, library, character.only = TRUE)))

# Enable reactlog
try(reactlog_enable())

# Sample Data
df_sites <- read.csv("https://raw.githubusercontent.com/hyugoshirai/InteractiveMaps/main/data.csv", TRUE, sep = ",")
species_choices <- df_sites %>% select(species_list) %>% unlist() %>% unique()

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%} #species_list_text { padding: 20px; border: 1px solid #ededed; border-radius: 10px; margin: 10px; display: flex; flex-direction: row;flex-wrap: wrap;} .species_item{ flex-basis: 50%; } .species_list_header{font-weight:700;color: green; margin-bottom: 20px;}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(style = "max-width: 30%;background-color: rgba(255,255,255,0.7);padding: 0px 10px 0px 10px;border-radius: 10px", top = 10, right = 10,
                selectizeInput(
                  inputId = "sci_name",
                  label = "Select scientific Name(s)",
                  choices = species_choices,
                  multiple = TRUE),
                gt::gt_output("species_in_area"),
                plotOutput("histogram"),
                htmlOutput("species_list_text")),
  absolutePanel(bottom = 10, left = 10,
                style = "background-color: rgba(255,255,255,0.7);padding: 10px 30px 10px 30px;border-radius: 20px;",
                sliderInput(
                  "day_month",
                  "Select Day of year",
                  min = as.Date("2021-01-01", "%Y-%m-%d"),
                  max = as.Date("2024-12-31", "%Y-%m-%d"),
                  value = c(as.Date("2021-01-01"), as.Date("2024-12-31")),
                  timeFormat = "%Y-%m-%d"))
)


server <- function(input, output, session) {
  
  # Reactive expression to filter data based on map bounds
  df_bounds <- reactive({
    if (is.null(input$map_bounds))  # Check if map bounds are null
      return(df_sites[FALSE,])  # Return empty data frame if bounds are null
    bounds <- input$map_bounds  # Get map bounds
    latRng <- range(bounds$north, bounds$south)  # Get latitude range
    lngRng <- range(bounds$east, bounds$west)  # Get longitude range
    
    # Filter data based on latitude and longitude ranges
    subset(df_sites,
           decimalLatitude >= latRng[1] & decimalLatitude <= latRng[2] &
             decimalLongitude >= lngRng[1] & decimalLongitude <= lngRng[2])
  })
  
  # Reactive expression to filter data based on date range and selected species
  df_date_spp <- reactive({
    # Combine year, month, and day into a single date column
    species_choices <- df_sites %>% tidyr::unite("sight_date",
                                                 year, month, day,
                                                 sep = "-") %>%
      dplyr::mutate(sight_date = as.Date(sight_date)) %>%
      dplyr::filter(sight_date > input$day_month[1],  # Filter by start date
                    sight_date < input$day_month[2])  # Filter by end date
    
    # Filter further based on selected species names
    if (!is.null(input$sci_name) && length(input$sci_name) > 0) {
      return(species_choices %>%
               filter(str_detect(species_list, paste(input$sci_name, collapse = "|"))))
    } else {
      return(species_choices)  # Return species choices if no species selected
    }
  })
  
  # Render the table of most recurring species in the area
  output$species_in_area <- gt::render_gt({
    df <- df_bounds()  # Initially get the data within bounds
    df <- df_date_spp() # Get the data within time range
    
    # Perform data transformations and create visualization
    df %>%
      select(species_list) %>%
      separate_rows(species_list, sep = ",") %>%
      count(species_list, sort = TRUE, name = "Count") %>%
      slice_max(Count, n = 5) %>%
      rename("Species" = "species_list") %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = "12pt", heading.title.font.size = "14pt") %>%
      gt::tab_header(title = "Most recurring species in area") %>%
      gtExtras::gt_plt_bar(column = Count, color = "darkgreen", scale_type = "number")
  })
  
  
  # Render the histogram of individuals counts over time
  output$histogram <- renderPlot({
    # Initially get the data within bounds
    df <- df_bounds()
    
    # Filter the data further based on date and selected species
    df <- df_date_spp()
    
    # Create histogram of individuals counts over time
    ggplot(df, aes(x = sight_date, y = species_count)) +
      geom_bar(stat = "identity", fill = "blue") +  # Use geom_bar instead of geom_histogram
      labs(x = "Date", y = "Sum of Species Counts", title = "Species Counts Over Time") +
      theme_minimal()
    
  })
  
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      fitBounds(lng1 = -75, lat1 = -15, lng2 = -45, lat2 = 5)
  })
  
  # Reactive expression to update species choices based on selected species
  sci_name_choices <- reactive({
    names(species_choices) <- species_choices
    
    # Return species choices if no species selected
    if (is.null(input$sci_name) || length(input$sci_name) == 0 || input$sci_name == "") {
      return(species_choices)
    }
    species_choices[str_detect(species_choices, paste(input$sci_name, collapse = "|"))]
  })
  
  # Observe changes in selected species and update the picker input accordingly
  observe({
    updatePickerInput(session, "sci_name", choices = species_choices, selected = input$sci_name)
  })
  
  # Render the UI for displaying marker information
  output$species_list_text <- renderUI({
    if (!is.null(input$map_marker_click)) {
      marker_id <- input$map_marker_click$id
      
      # Extract data for the clicked marker
      site_data <- df_sites %>% filter(site_id == marker_id)
      
      # Extract information
      species_list <- site_data$species_list
      decimal_longitude <- round(site_data$decimalLongitude,5)  
      decimal_latitude <- round(site_data$decimalLatitude,5)
      date <- paste(site_data$year,"/",site_data$month,"/",site_data$day)
      count <- site_data$species_count
      
      # Create header and information
      header <- glue::glue("<span class='species_list_header' 
      style='color: green; 
      font-weight: bold; 
      font-size: 14px;'>
      Marker info:<br>")
      
      info <- glue::glue("<span class='species_list_info' 
      style='color: black; 
      font-weight: lighter; 
      font-size: 12px;'>
        Species: {species_list}<br>
        Longitude: {decimal_longitude}<br>
        Latitude: {decimal_latitude}<br>
        Date: {date}<br>
        Count: {count} individual (s)")
      
      # Combine header and information and return as HTML
      FullText <- paste(header, info)
      return(HTML(FullText))
    } else {
      return(HTML("<span>Click on a marker to see the species data</span>"))  # Default message
    }
  })
  
  # Create color palette for legend
  count_palet <- colorBin(palette = "Dark2", bins = 3, pretty = TRUE,
                          domain = range(df_sites$species_count))
  
  # Observe changes in filtered data and update leaflet map accordingly
  observe({
    leafletProxy("map", data = df_date_spp()) %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearMarkers() %>%
      clearControls() %>%
      addMarkers(lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 clusterOptions = markerClusterOptions(), layerId = ~site_id) %>%
      addCircles(lng = ~decimalLongitude,
                 lat = ~decimalLatitude,
                 color = ~count_palet(species_count),
                 radius = ~species_count) %>%
      addLegend("bottomright", pal = count_palet, values = ~species_count,
                title = "No. of Observations",
                opacity = 1
      )
  })
}


# Run the application
shinyApp(ui = ui, server = server)