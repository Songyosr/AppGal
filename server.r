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

# Run the application 


