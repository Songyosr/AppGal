
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
