# App Name: PAAC Tracker
# Author: Geoffrey Arnold

library(shiny)
require(curl)
library(leaflet)
require(jsonlite)
require(dplyr)
require(shinyjs)
require(readr)
require(rgdal)

load.lines <- readOGR("paac_routes_1909/PAAC_Routes_1909.shp") %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

holiday <- read_csv("HolidayBusList.csv")$Vehicle_ID

key <- fromJSON("key.json")$key

baseUrl <- "http://realtime.portauthority.org/bustime/api/v3/"

getRealTime <- function(endpoint, params, response) {
    if (missing(params)) {
        url <- paste0(baseUrl, endpoint, "?format=json&key=", key)
    } else if (typeof(params) == "list") {
        params_text <- paste0(names(params), "=", params, collapse = "&")
        url <- URLencode(paste0(baseUrl, endpoint, "?format=json&key=", key, "&", params_text))
    }
    json <- fromJSON(url)$`bustime-response`[[response]]
    if (is.null(json)) {
        return(data.frame())
    } else {
        return(json)
    }
}

vehPal <- colorFactor(c("#d04020", "#1162a4", "#f4a460", "#5F9EA0", "#71af26"), levels = c("Red Line", "Blue Line", "Inbound", "Outbound", "Holiday"))

# Routes
load.routes <- getRealTime("getroutes", response = "routes")

# Define UI for application that draws map
ui <- fluidPage(style = "padding: 0;", 
                tags$head(tags$title("Port Authority Bus Tracker", NULL)),
                tags$head(tags$link(rel = "shortcut icon", href="favicon-bus.ico")),
                tags$head(
                    tags$style(
                        HTML(".checkbox-inline { 
                                margin-left: 0px;
                                margin-right: 10px;
                             }
                             .checkbox-inline+.checkbox-inline {
                                margin-left: 0px;
                                margin-right: 10px;
                             }"
                        )
                    ) 
                ),
                useShinyjs(),
                absolutePanel(top = 10, right = 20, id = "expand", style = "display: none; z-index: 1000; padding: 0;",
                              actionButton("filters", "", icon = icon("map"))),
                # Column for the Filters
                column(2, style = "padding: 0;",
                       wellPanel(id = "panel", style = "z-index: 1; overflow-y: auto; height: calc(100vh); margin-bottom: 0;",
                           selectInput("basemapSelect",
                                             label = "Basemap",
                                             choices = c(Google = "googleStreets", `OSM Mapnik` = "OpenStreetMap.Mapnik", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Esri Satellite` = "Esri.WorldImagery", `Stamen Toner` = "Stamen.Toner", Esri = "Esri.WorldStreetMap", `CartoDB Dark Matter` = "CartoDB.DarkMatter")),
                           radioButtons("holidayTracker",
                                        "Show Holiday Cars:",
                                        choices = c("On", "Off", "Only")),
                           radioButtons("inOut",
                                        "Transit Direction:",
                                        choices = c("Both", "Inbound", "Outbound")),
                           actionButton("routeRefresh", "Deselect All", icon = icon("check-square-o")),
                           tags$br(), tags$br(),
                           checkboxGroupInput("routeSelect",
                                              "Show Routes:",
                                              inline = TRUE,
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
    # Refresh every 5 seconds
    autoRefresh <- reactiveTimer(5000)
    # Map
    output$map <- renderLeaflet({
        map <- leaflet() %>%
            setView(-79.9959, 40.4406, zoom = 12) %>% 
            addEasyButton(easyButton(
                icon="fa-crosshairs", title="Locate Me",
                onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
            addLegend(position = "bottomright", pal = vehPal, values = c("Red Line", "Blue Line", "Inbound", "Outbound", "Holiday"))
        if (isolate(input$basemapSelect) == "googleStreets") {
             map <- addTiles(map, urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google")
        } else {
            map <- addProviderTiles(map, isoalte(input$basemapSelect), options = providerTileOptions(noWrap = TRUE))
        }
        map
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
    # Route Lines Map
    observe({
        if (!is.null(input$routeSelect) & length(input$routeSelect) == 1) {
            line <- subset(load.lines, ROUTE == input$routeSelect)
            leafletProxy("map") %>%
                clearGroup("lines") %>%
                addPolylines(data = line, group = "lines", color = "blue", weight = 2)
        } else {
            leafletProxy("map") %>%
                clearGroup("lines")
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
            # Check API return
            if (nrow(vehicles) > 0) {
                # Merge buses to route colors
                vehicles <- vehicles %>%
                    mutate(lat = as.numeric(lat),
                           lon = as.numeric(lon),
                           timestamp = as.POSIXct(tmstmp, format = "%Y%m%d %H:%M"),
                           marker = case_when(rtpidatafeed == "Light Rail" & grepl("RED", rt) ~ "red",
                                              rtpidatafeed == "Light Rail" & !grepl("RED", rt) ~ "darkblue",
                                              grepl("downtown", des, ignore.case = T) ~ "beige",
                                              TRUE ~ "cadetblue"),
                           direction = case_when(rtpidatafeed == "Light Rail" & grepl("Allegheny", des) ~ "Inbound",
                                                 grepl("downtown", des, ignore.case = T) ~ "Inbound",
                                                 rtpidatafeed == "Light Rail" & !grepl("Allegheny", des) ~ "Outbound",
                                                 TRUE ~ "Outbound"),
                           holiday = ifelse(vid %in% holiday, "Y", "N"),
                           txt = case_when(grepl("downtown", des, ignore.case = T) ~ "black",
                                           rtpidatafeed == "Light Rail" & grepl("RED", rt) ~ "black",
                                           rtpidatafeed == "Light Rail" & !grepl("RED", rt) ~ "white",
                                           TRUE ~ "white"
                           )) %>%
                    left_join(routes, by = c("rt", "rtpidatafeed"))
                # Clear all deselected Routes
                deRoute <- subset(load.routes, !(rt %in% routes$rt))
                
                for (route in deRoute$rt) {
                    leafletProxy("map") %>%
                        clearGroup(route)
                }
                # Direction Tracker
                if (input$inOut != "Both") {
                    vehicles <- subset(vehicles, direction == input$inOut)
                }
                # Add Selected Routes
                for (route in routes$rt) {
                    # Check for Holiday Tracker
                    if (input$holidayTracker == "On") {
                        temp <- subset(vehicles, rt == route & holiday == "N")
                        holiday <- subset(vehicles, rt == route & holiday == "Y")
                    } else if (input$holidayTracker == "Only") {
                        temp <- data.frame()
                        holiday <- subset(vehicles, rt == route & holiday == "Y")
                    } else {
                        temp <- subset(vehicles, rt == route)
                        holiday <- data.frame()
                    }
                    # Map Normal Buses
                    if (nrow(temp) > 0) {
                        leafletProxy("map") %>%
                            clearGroup(route) %>%
                            addAwesomeMarkers(data = temp, lat = ~lat, lng = ~lon, 
                                              label = ~paste(rt, "-", des), 
                                              popup = ~paste(rt, "-", des), 
                                              group = route,
                                              icon = awesomeIcons(markerColor =  ~marker, text = ~rt, iconColor = ~txt))
                    } else {
                        leafletProxy("map") %>%
                            clearGroup(route)
                    }
                    # Map Holiday Buses
                    if (nrow(holiday) > 0) {
                        leafletProxy("map") %>%
                            addAwesomeMarkers(data = holiday, lat = ~lat, lng = ~lon, 
                                              label = ~paste(rt, "-", des), 
                                              popup = ~paste(rt, "-", des), 
                                              group = route,
                                              icon = awesomeIcons(markerColor = "green", icon = "snowflake-o", library = "fa", iconColor = "red"))
                    }
                }
            # Clear all routes if none returned
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
