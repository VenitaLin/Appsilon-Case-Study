# LIBRARY
#==============================================================================
library(semantic.dashboard)
library(dplyr)
library(leaflet)
#==============================================================================

# shipdata <- read.csv("data/ships.csv")
# saveRDS(shipdata, "data/shipdata.rds")
# shipdata <- readRDS("data/shipdata.rds")
# ship_final <- readRDS("data/ship_final.rds")
ship_max_distance <- readRDS("data/ship_max_distance.rds")

# UI
#==============================================================================
ui <- fluidPage(
    
    # Application title
    titlePanel("Vessel Information"),
    
    sidebarLayout(
        position = "right",
        sidebarPanel(
            selectInput("ship_type", 
                        "Vessel Type:",
                        sort(unique(ship_max_distance$ship_type))
                        # c("Loading..." = "Cargo") # Placeholder
            ),
            selectInput("ship", 
                        "Vessel:",
                        choices=NULL
                        # selected=NULL
            )
        ),
        mainPanel(
            h4("Longest Distance Between Two Consecutive Observations"),
            leafletOutput("map"),
            # h4("Vessel Details"),
            textOutput('desc')
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
        
        ship_distance <- data.frame(
            "group" = c("Start Point", "End Point"),
            "lat" = c(ship_max_distance_selected$LAT, ship_max_distance_selected$LAT2),
            "lon" = c(ship_max_distance_selected$LON, ship_max_distance_selected$LON2),
            "distance" = c(ship_max_distance_selected$DISTANCE))
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
                       lat =  ~LAT2,
                       lng = ~LON2,
                       popup = paste("Vessel Name: ",ship_max_distance_selected$SHIPNAME,"<br> Datetime: ", ship_max_distance_selected$DATETIME2) 
                        ) %>%
            addPolylines(data = ship_distance, 
                         lng = ~lon, 
                         lat = ~lat, 
                         group = ~group,
                         weight = 3,
                         color = "white",
                         label = ~round(distance,2))
    })
    
    output$desc <- renderText({
        # FILTER SHIP
        # ================================================================================
        ship_max_distance_selected <- ship_max_distance %>%
            filter(SHIPNAME == input$ship)
        # ================================================================================
        paste("Longest Distance: ", round(ship_max_distance_selected$DISTANCE, 2), " meters")
    })
}
#==============================================================================

# Run the application 
shinyApp(ui = ui, server = server)