#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(curl)
library(leaflet)
require(jsonlite)
require(dplyr)

key <- fromJSON("key.json")$key

baseUrl <- "http://realtime.portauthority.org/bustime/api/v3/"

getRealTime <- function(endpoint, params, response) {
    if (missing(params)) {
        url <- paste0(baseUrl, endpoint, "?format=json&key=", key)
    } else if (typeof(params) == "list") {
        params_text <- paste0(names(params), "=", params, collapse = "&")
        url <- paste0(baseUrl, endpoint, "?format=json&key=", key, "&", params_text)
    }
    json <- fromJSON(url)$`bustime-response`[[response]]
}

# Routes
routes <- getRealTime("getroutes", response = "routes")

# Define UI for application that draws map
ui <- fluidPage( style = "padding: 0;",
    leafletOutput("map"),
    tags$style(type = "text/css", 
               "#map {height: calc(100vh) !important;}")
)

# Define server logic
server <- function(input, output) {
    # Refresh every 10 seconds
    autoRefresh <- reactiveTimer(10000)
    # Map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(group = "Mapnik") %>%
            addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Black & White") %>%
            addProviderTiles("OpenStreetMap.HOT", group = "Humanitarian") %>%
            addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
            setView(-79.9959, 40.4406, zoom = 12) %>% 
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="Locate Me",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
            addLayersControl(baseGroups = c("Mapnik", "Black & White", "Humanitarian", "Google", "Satellite"),
                             overlayGroups = routes$rt)
    })
    observe({
        autoRefresh()
        for (i in seq(from = 1, to = nrow(routes), by = 10)) {
            j <- i + 9
            if (i == 1) {
                vehicles <- getRealTime("getvehicles", list(rt = paste(routes$rt[i:j], collapse =",")), "vehicle")
            } else {
                vehicles <- rbind(vehicles, getRealTime("getvehicles", list(rt = paste(routes$rt[i:j], collapse =",")), "vehicle"))
            }
        }
        
        vehicles <- vehicles %>%
            mutate(lat = as.numeric(lat),
                   lon = as.numeric(lon)) %>%
            left_join(routes, by = "rt")
        
        for (route in routes$rt) {
            temp <- subset(vehicles, rt == route)
            leafletProxy("map") %>%
                clearGroup(route) %>%
                addAwesomeMarkers(data = temp, lat = ~lat, lng = ~lon, label = ~paste(rt, "-", des), group = route, icon = awesomeIcons(markerColor =  "gray", text = ~rt, iconColor = ~rtclr))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
