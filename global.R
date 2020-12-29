library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(dplyr)

#source("01 map theme.R")
#e## Thai
#Sys.setlocale(locale = "Thai")

# --------------- #
Dat <- readRDS("Data/Mapped_Data.RDS")
border <- readRDS("Data/khet12map_1.RDS")

### Dat
#Dat$labelss <- labelss 
bins <- c(0,0.05,0.1,0.15,0.2,0.25,0.3,5)
pal <- colorBin("YlOrRd", domain = c(0,1), bins = bins)
Dat$labelss <- sprintf("<strong>%s</strong>: %s<br/>Stunting: %g%% <br/>Wasting: %g%% <br/>Overweight: %g%%",
                       Dat[1,]$ADM1_EN , 
                       Dat$ADM2_EN , 
                       round(Dat$Stunt*100,1),
                       round(Dat$Wasting*100,1),
                       round(Dat$Overweight*100,1)
) %>% 
  lapply(htmltools::HTML)

### Define Basemap
basemap <- leaflet() %>% 
  setView(lng = 100.6046, lat = 6.859884, zoom = 8)%>% 
  addMapPane("bg", zIndex = 1) %>% 
  addMapPane("data", zIndex = 50) %>% 
  addMapPane("border", zIndex = 100) %>% 
  addProviderTiles(providers$CartoDB.Positron, 
                   providerTileOptions(detectRetina = FALSE,
                                       reuseTiles = TRUE,
                                       minZoom = 6,
                                       maxZoom = 10),
                   group = "Background",
                   options = pathOptions(pane = "bg")) %>%
  addPolylines(data = border,
               #group = "Background",
               opacity = 1,
               color = "grey", 
               fillColor = "white",
               fillOpacity = 0,
               weight = 2,
               options = pathOptions(pane = "border")) %>% 
  addLegend(pal = pal, values = 0:7/20, 
            #title = "Prev",
            labFormat = labelFormat(
              prefix = "(", suffix = ")%", between = ", ",
              transform = function(x) 100 * x
            ), 
            opacity = 0.7, title = NULL,
            position = "bottomright",
            group = "Background")#,
#
sel_col <- names(Dat)[27:32]
year <- NULL
indi <- NULL
group_name <- apply(expand.grid(sel_col[c(1,3,5)],2016:2020), 1, paste, collapse="")


for (year in 2016:2020) {
  for(indi in sel_col[c(1,3,5)]){
    #print(paste0(indi,year))
    tmp <- Dat[Dat$y == year,] 
    tmp$indo <- tmp[[indi]]
    basemap <- basemap %>% addPolygons(
      #data = Dat[Dat$y == year,] %>% ,
      data = tmp,
      fillColor = ~pal(indo),
      weight = 1,
      opacity = 0.4,
      color = "white",
      dashArray = "1",
      fillOpacity = 0.5,
      group = paste0(indi,year),
      
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = ~labelss,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      options = pathOptions(pane = "data")
    )
    
  }}

basemap  <- basemap %>% 
  hideGroup(group = group_name)

#sel_col <- names(Dat)[27:32]

## UI

ui <- fluidPage(
  
  # Application title
  titlePanel("Test of Mapview Selective Viewing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("indoo","Indicator", choices = sel_col[c(1,3,5)], selected = "Stunt"),
      sliderInput("year", "Please select the year",
                  min = 2016,max = 2020,
                  value = 2019, step = 1
                  #choices = c(2016:2020),
                  #selected = "2016"
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )
)
#leaf
#Dat %>% mutate(hrrrr = hello)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Render static basemap
  output$map <- renderLeaflet({basemap})
  
  # Reactive data filter
  # myData <- reactive({
  #   Dat$indicator <- Dat[, input$indi]
  #   Dat %>% filter(y == input$year) 
  # })
  
  # Modify basemap
  observe({
    req(input$year)
    leafletProxy("map") %>%
      hideGroup(group = group_name) %>% 
      showGroup(group = paste0(input$indoo,input$year))
    # Assigning df id to layerid
  })
  
}


