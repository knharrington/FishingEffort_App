# This app has a time slider that allows the user to view changes in fishing effort over time
library(shiny)
library(tidyverse)
library(sp)
library(leaflet)
library(raster)

# process data
#setwd("C:/Users/FishVid/Documents/R/Shiny Apps/Effort_App")
rasterdata <- lapply(2016:2023, function(year) {
  rasterfile <- paste0("rasters/EffortDensity", year, ".tif")
  raster(rasterfile) %>% reclassify(cbind(-Inf, 0.01, NA), right=FALSE)
})

# start ui
ui <- fluidPage(
  tags$style(HTML("
    #map {
      position: absolute;
      top: 0;
      bottom: 0;
      left: 0;
      right: 0;
    }
    #time_slider {
      position: absolute;
      bottom: 20px;
      left: 20px;
      z-index: 1000; /* Ensure slider is above map */
    }
    #translucent_panel {
      position: absolute;
      bottom: 30px;
      left: 20px;
      background-color: rgba(255, 255, 255, 0.75); /* Translucent white */
      padding: 10px;
      border-radius: 5px;
      z-index: 999; /* Ensure panel is below slider */
    }
  ")),
  leafletOutput("map", height="100vh"),
  div(id="translucent_panel",
      sliderInput("year", "Retrieval Year",
                  min = 2016, max = 2023, value = 2016, sep = "", round = TRUE, step = 1,
                  animate = TRUE))
) # end ui

# start server
server <- function(input, output) {
  
  inityear <- reactive({
    match(input$year, 2016:2023)
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Base"), group = "Basemap") %>%
      addProviderTiles("Esri.OceanBasemap", options = providerTileOptions(variant = "Ocean/World_Ocean_Reference"), group = "Basemap") %>%
      setView(lng=-84, lat=27, zoom=7)
  })
  
  observe({
    initial_year_index <- inityear()
    if (is.na(initial_year_index)) {
      stop("Invalid initial year selected.")
    }
    
    raster_values <- getValues(rasterdata[[initial_year_index]])
    value_range <- range(raster_values, na.rm = TRUE)
     
    qpal <- colorQuantile("Spectral",
                          domain=value_range,
                          n=10,
                          na.color="transparent", 
                          reverse=TRUE)
    
    leafletProxy("map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(rasterdata[[initial_year_index]], 
                     colors=qpal) %>%
      leaflet::addLegend(title=HTML("Fishing Effort Density<br>(hauls per sq km)"), position="topright",
                pal=qpal, values=value_range, opacity=0.75 )
  })
  
} # end server

# Run the app
shinyApp(ui = ui, server = server)
