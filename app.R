# LIBRARY
#==============================================================================
library(semantic.dashboard)
library(dplyr)
library(leaflet)
library(plotly)
library(reshape2)
library(tidyverse)
library(shinycssloaders)
#==============================================================================

# LOAD DATA
#==============================================================================
ship_final <- readRDS("data/ship_final.rds")
ship_max_distance <- readRDS("data/ship_max_distance.rds")
ship_distance_map <- readRDS("data/ship_distance_map.rds")
#==============================================================================

# UI
#==============================================================================
ui <- fluidPage(
    tags$head(tags$style(
        HTML('
         #sidebar {
            background-color: rgba(52, 149, 255, 0.1);
        }
        ')
    )),
    # Application title
    titlePanel("Vessel Information"),
    
    sidebarLayout(
        position = "right",
        sidebarPanel(
            id = "sidebar",
            selectInput("ship_type", 
                        "Vessel Type:",
                        sort(unique(ship_max_distance$ship_type)),
                        selected = "Cargo"
            ),
            selectInput("ship", 
                        "Vessel:",
                        choices=NULL,
                        selected = ".PRINCE OF WAVES"
            )
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                tabPanel("Longest Distance Sailed", h4("Longest Distance Between Two Consecutive Observations"), leafletOutput("map") %>% withSpinner(color="#3495FF"), textOutput('desc')),
                tabPanel("Vessel Speed Timeseries", plotlyOutput("speed") %>% withSpinner(color="#3495FF")),
                tabPanel("Vessel Course Timeseries", plotlyOutput("course") %>% withSpinner(color="#3495FF"))
            )
        )
    )
)
#==============================================================================

# SERVER
#==============================================================================
server <- function(input, output, session) {

    observeEvent(input$ship_type,{
        updateSelectInput(session,"ship",
                          choices = sort(unique(ship_max_distance$SHIPNAME[ship_max_distance$ship_type==input$ship_type]))                          )
    } )
    
    output$map <- renderLeaflet({
        
        oceanIcons <- iconList(
            ship = makeIcon("data/ship-icon.png", "data/ship-icon.png", 25, 25)
        )
        
# FILTER SHIP
# ================================================================================
        ship_max_distance_selected <- ship_max_distance %>%
            filter(SHIPNAME == input$ship)

        ship_distance <- ship_final %>%
            filter(SHIPNAME == input$ship) %>%
            filter(DATETIME == ship_max_distance_selected$DATETIME |
                       DATETIME == ship_max_distance_selected$DATETIME2)
    
# ================================================================================

# MAP
# ================================================================================
        leaflet() %>% 
            addTiles() %>%
            addMarkers(data = ship_max_distance_selected,
                       icon = ~oceanIcons["ship"],
                       lat =  ~LAT,
                       lng = ~LON,
                       popup = paste("Vessel Name: ",ship_max_distance_selected$SHIPNAME,"<br> Datetime: ", ship_max_distance_selected$DATETIME) 
                       ) %>%
            addMarkers(data = ship_max_distance_selected,
                       icon = ~oceanIcons["ship"],
                       lat = ~LAT2,
                       lng = ~LON2,
                       popup = paste("Vessel Name: ",ship_max_distance_selected$SHIPNAME,"<br> Datetime: ", ship_max_distance_selected$DATETIME2) 
                        ) %>%
            addPolylines(data = ship_distance,
                         lng = ~LON,
                         lat = ~LAT,
                         group = ~SHIPNAME,
                         weight = 3,
                         color = "rgb(52, 149, 255)",
                         )
    })
#==============================================================================

# NOTES    
#==============================================================================
    output$desc <- renderText({
        # FILTER SHIP
        # ================================================================================
        ship_max_distance_selected <- ship_max_distance %>%
            filter(SHIPNAME == input$ship)
        # ================================================================================
        paste("Longest Distance: ", round(ship_max_distance_selected$DISTANCE, 2), " meters")
    })
#==============================================================================
    
# SPEED PLOT
#==============================================================================
    output$speed <- renderPlotly({
        
        selected_shipdata <- ship_final %>%
            filter(SHIPNAME == input$ship)
        
        plot_ly(selected_shipdata,
                x = ~DATETIME,
                y = ~SPEED,
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(size = 4,
                              color = 'rgb(52, 149, 255)'),
                line = list(size = 2)
                )
    })
#==============================================================================
    
# COURSE PLOT
#==============================================================================
    output$course <- renderPlotly({
        
        selected_shipdata <- ship_final %>%
            filter(SHIPNAME == input$ship)
        
        plot_ly(selected_shipdata,
                x = ~DATETIME,
                y = ~COURSE,
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(size = 4,
                              color = 'rgb(52, 149, 255)'),
                line = list(size = 2)
        )
    })
#==============================================================================
}
#==============================================================================

# Run the application 
shinyApp(ui = ui, server = server)