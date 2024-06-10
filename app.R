# List of required packages
packages <- c(
  "shiny",
  "leaflet",
  "tidyverse",
  "reactlog",
  "svglite",
  "shiny",
  "shinyWidgets"
)

# Install packages if they are not already installed
install_packages <- packages[!packages %in% installed.packages()]
if (length(install_packages) > 0) {
  install.packages(install_packages)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

reactlog_enable()

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
  
  df_bounds <- reactive({
    if (is.null(input$map_bounds))
      return(df_sites[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(df_sites,
           decimalLatitude >= latRng[1] & decimalLatitude <= latRng[2] &
             decimalLongitude >= lngRng[1] & decimalLongitude <= lngRng[2])
  })
  
  output$species_in_area <- gt::render_gt({
    df <- df_bounds()
    if (!is.null(input$sci_name) && length(input$sci_name) > 0) {
      df <- df %>%
        filter(str_detect(species_list, paste(input$sci_name, collapse = "|")))
    }
    df %>%
      select(species_list) %>%
      separate_rows(species_list, sep = ",") %>%
      count(species_list, sort = TRUE, name = "Count") %>%
      slice_max(Count, n = 5) %>%
      rename("Species" = "species_list") %>%
      gt::gt() %>%
      gt::tab_options(table.font.size = "12pt", heading.title.font.size = "14pt") %>%
      gt::tab_header(title = "Most observed species in area") %>%
      gtExtras::gt_plt_bar(column = Count, color = "darkgreen", scale_type = "number")
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addTiles() %>%
      fitBounds(lng1 = -75, lat1 = -15, lng2 = -45, lat2 = 5)
  })
  
  sci_name_choices <- reactive({
    names(species_choices) <- species_choices
    
    if (is.null(input$sci_name) || length(input$sci_name) == 0 || input$sci_name == "") {
      return(species_choices)
    }
    species_choices[str_detect(species_choices, paste(input$sci_name, collapse = "|"))]
  })
  
  observe({
    updatePickerInput(session, "sci_name", choices = species_choices, selected = input$sci_name)
  })
  
  # output$species_list_text <- renderUI({
  #   if (!is.null(input$map_marker_click)) {
  #     marker_data <- str_split(input$map_marker_click, "_", simplify = TRUE)[1,]
  #     marker_month <- lubridate::month(as.Date(glue::glue("2021-{marker_data[4]}-01")), label = TRUE, abbr = FALSE)
  #     species_list <- df_sites %>%
  #       filter(site_id == input$map_marker_click$id) %>%
  #       select(species_list) %>%
  #       unlist() %>%
  #       str_trim() %>%
  #       str_split(",") %>%
  #       unlist() %>%
  #       head(10) %>%
  #       paste(collapse = "</span><span class='species_item'>")
  #     header <- glue::glue("<span class='species_list_header'> Most popular bird species found at <br> {marker_data[1]},{marker_data[1]} in the month of {marker_month}</span>")
  #     species_list <- paste(header, "<span class='species_item'>", species_list, "</span>", collapse = '')
  #     return(HTML(species_list))
  #   } else {
  #     HTML("<span> Click on a marker to see the list of birds ordered by overall popularity </span>")
  #   }
  # })
  
  df_react <- reactive({
    species_choices <- df_sites %>% tidyr::unite("sight_date",
                                                 year, month, day,
                                                 sep = "-") %>%
      dplyr::mutate(sight_date = as.Date(sight_date)) %>%
      dplyr::filter(sight_date > input$day_month[1],
                    sight_date < input$day_month[2])
    
    if (!is.null(input$sci_name) && length(input$sci_name) > 0) {
      print(input$sci_name)
      return(species_choices %>%
               filter(str_detect(species_list, paste(input$sci_name, collapse = "|"))))
    } else {
      return(species_choices)
    }
  })
  
  count_palet <- colorBin(palette = "Dark2", bins = 3, pretty = TRUE,
                          domain = range(df_sites$species_count))
  
  observe({
    leafletProxy("map", data = df_react()) %>%
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