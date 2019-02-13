# App Name: PAAC Tracker
# Author: Geoffrey Arnold

library(shiny)
require(curl)
library(leaflet)
require(jsonlite)
require(dplyr)
require(shinyjs)

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
    if (is.null(json)) {
        return(data.frame())
    } else {
        return(json)
    }
}

# Routes
load.routes <- getRealTime("getroutes", response = "routes")

# Define UI for application that draws map
ui <- fluidPage(style = "padding: 0;", 
                tags$head(tags$title("Port Authority Bus Tracker", NULL)),
                tags$head(tags$link(rel = "shortcut icon", href="favicon-bus.ico")),
                useShinyjs(),
                absolutePanel(top = 10, right = 20, id = "expand", style = "display: none; z-index: 1000; padding: 0;",
                              actionButton("filters", "", icon = icon("map"))),
                # Column for the Filters
                column(2, style = "padding: 0;",
                       wellPanel(id = "panel", style = "z-index: 1; overflow-y: scroll; height: calc(100vh); margin-bottom: 0;",
                           selectInput("basemapSelect",
                                             label = "Basemap",
                                             choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", Google = "googleStreets", `Esri Satellite` = "Esri.WorldImagery", `Stamen Toner` = "Stamen.Toner", Esri = "Esri.WorldStreetMap", `CartoDB Dark Matter` = "CartoDB.DarkMatter"),
                                             selected = "OpenStreetMap.Mapnik"),
                                 actionButton("routeRefresh", "Deselect All", icon = icon("check-square-o")),
                                 tags$br(), tags$br(),
                                 checkboxGroupInput("routeSelect",
                                                    "Show Routes:",
                                                    choices = sort(load.routes$rt),
                                                    selected = sort(load.routes$rt))
                       )
                   ),
                # Column for the Map
                column(10, style = "padding: 0;",
                      leafletOutput("map")
                      ),
                # CSS for Map and Mobile use
               tags$style(type = "text/css",
                          "#map {height: calc(100vh) !important;}
                          @media screen and (max-width: 600px) {
                            #panel {display: none;}
                            #expand {display: initial !important;}
                          }")
)

# Define server logic
server <- function(input, output, session) {
    setBookmarkExclude(c("map_bounds", "map_center", "map_groups", "map_zoom", "map_marker_mouseout", "map_marker_mouseover", "map_marker_click"))
    # Refresh every 15 seconds
    autoRefresh <- reactiveTimer(15000)
    # Map
    output$map <- renderLeaflet({
        leaflet() %>%
            setView(-79.9959, 40.4406, zoom = 12) %>% 
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="Locate Me",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    # Filter Show - Mobile usage
    observe({
        # Has Filter button been selected?
        if ((input$filters %% 2) != 0) {
            updateActionButton(session,
                               "filters",
                               icon = icon("search"))
            hide("panel")
        } else {
            updateActionButton(session,
                               "filters",
                               icon = icon("map"))
            show("panel")
        }
    })
    # Route Selection filter
    routesInput <- reactive({
        routes <- filter(load.routes, rt %in% input$routeSelect)
    })
    # Change Basemap
    observe({
        # Basemaps
        if (input$basemapSelect == "googleStreets") {
            leafletProxy("map", session = session) %>%
                clearTiles() %>% 
                addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google")
        } else {
            leafletProxy("map", session = session) %>%
                clearTiles() %>%
                addProviderTiles(input$basemapSelect, options = providerTileOptions(noWrap = TRUE))
        }
    })
    observeEvent(input$routeRefresh, {
        # Has the button been pressed?
        if ((input$routeRefresh %% 2) != 0) {
            updateCheckboxGroupInput(session,
                                     "routeSelect",
                                     selected = "")
            updateActionButton(session,
                               "routeRefresh",
                               label = "Select All",
                               icon = icon("check-square"))
        # Update if pressed again
        } else {
            updateCheckboxGroupInput(session,
                                     "routeSelect",
                                     selected = sort(load.routes$rt))
            updateActionButton(session,
                               "routeRefresh",
                               label = "Deselect All",
                               icon = icon("check-square-o"))
        }
    })
    # Refresh Bus locations
    observe({
        autoRefresh()
        routes <- routesInput()
        
        # Check if there are any routes selected
        if (nrow(routes) > 0) {
            # Get selected route buses locations
            for (i in seq(from = 1, to = nrow(routes), by = 10)) {
                j <- i + 9
                if (j > nrow(routes)) {
                    j <- nrow(routes)
                }
                if (i == 1) {
                    vehicles <- getRealTime("getvehicles", list(rt = paste(routes$rt[i:j], collapse =",")), "vehicle")
                } else {
                    vehicles <- rbind(vehicles, getRealTime("getvehicles", list(rt = paste(routes$rt[i:j], collapse =",")), "vehicle"))
                }
            }
            
            if (nrow(vehicles) > 0) {
                
                # Merge buses to route colors
                vehicles <- vehicles %>%
                    mutate(lat = as.numeric(lat),
                           lon = as.numeric(lon)) %>%
                    left_join(routes, by = "rt")
                
                # Clear all deselected Routes
                deRoute <- subset(load.routes, !(rt %in% routes$rt))
                
                for (route in deRoute$rt) {
                    leafletProxy("map") %>%
                        clearGroup(route)
                }
                
                # Add Selected Routes
                for (route in routes$rt) {
                    temp <- subset(vehicles, rt == route)
                    leafletProxy("map") %>%
                        clearGroup(route) %>%
                        addAwesomeMarkers(data = temp, lat = ~lat, lng = ~lon, label = ~paste(rt, "-", des), group = route, icon = awesomeIcons(markerColor =  "gray", text = ~rt, iconColor = ~rtclr))
                }
            } else {
                for (route in load.routes$rt) {
                    leafletProxy("map") %>%
                        clearGroup(route)
                }
            }
        } else {
            # Clear all routes if none selected
            for (route in load.routes$rt) {
                leafletProxy("map") %>%
                    clearGroup(route)
            }
        }
    })
    observe({
        # Trigger this observer every time an input changes
        reactiveValuesToList(input)
        session$doBookmark()
    })
    # Update page URL
    onBookmarked(function(url) {
        updateQueryString(url)
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
