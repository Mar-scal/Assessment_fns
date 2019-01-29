#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyapp <- function(trip.log = trip.log, osa=osa, pr=pr, direct=direct, repo=repo, pect_ggplot=pect_ggplot) {
  require(shiny)

  trips <- NULL
  trip.log_f <- list()
  osa_f <- list()
  for(i in 1:length(trip.log)){
    trip<-unique(trip.log[[i]]@data$tripnum)
    trips <- c(trips,trip)
    
    trip.log_f[[i]] <- fortify(trip.log[[i]]@data)
    trip.log_f[[i]]$lon <- as.data.frame(coordinates(trip.log[[i]]))$lon
    trip.log_f[[i]]$lat <- as.data.frame(coordinates(trip.log[[i]]))$lat
    
    osa_f[[i]] <- fortify(osa[[i]]@data)
    osa_f[[i]]$lon <- as.data.frame(coordinates(osa[[i]]))$lon
    osa_f[[i]]$lat <- as.data.frame(coordinates(osa[[i]]))$lat
  }
  
  ui <- fluidPage(
    
    # Application title
    titlePanel("Spatial Log Checks"),
    
    # pick a trip
    fluidRow(selectInput(inputId = "trip",
                         label="Trip number",
                         choices = trips)
    ),
    
    # main plot
    fluidRow(
      plotOutput("maplog", 
                 click = "plot1_click",
                 brush = brushOpts(id="plot1_brush"))
    ),
    
    # subsetted data
    ## by click
    fluidRow(
      column(12,
             h4("Points near click"),
             tableOutput("click_info"))
    ),
    ## by drawing a box
    fluidRow(
      column(12,
             h4("Points in box"),
             tableOutput("brush_info"))
    )
    
  )
  
  
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    require(ggplot2)
    output$maplog <- renderPlot({

      pect_ggplot[[which(trips==input$trip)]] + 
        geom_point(data=trip.log_f[[which(trips==input$trip)]], aes(lon, lat)) +
        xlim(pr[[which(trips==input$trip)]]["min", "x"], pr[[which(trips==input$trip)]]["max", "x"]) +
        ylim(pr[[which(trips==input$trip)]]["min", "y"], pr[[which(trips==input$trip)]]["max", "y"])# +
        #ggtitle(paste0(trip.log_f[[which(trips==input$trip)]]$ves[1],"_",
                       #trip.log_f[[which(trips==input$trip)]]$vrnum[1],"_",
                       #min(trip.log_f[[which(trips==input$trip)]]$fished,na.rm=T),"-",
                       #max(trip.log_f[[which(trips==input$trip)]]$fished,na.rm=T)))
    })
    
    output$click_info <- renderTable({
      nearPoints(df=trip.log_f[[which(trips==input$trip)]], xvar = "lon", yvar="lat", coordinfo = input$plot1_click, addDist=TRUE)
    })     
    
    output$brush_info <- renderTable({
      brushedPoints(df=trip.log_f[[which(trips==input$trip)]], xvar = "lon", yvar="lat", brush = input$plot1_brush)
    })
    
  }

  app <- shinyApp(ui, server)
  
  # Run the application 
  runApp(app)
  
}
  
