#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application
shinyapp <- function(trip.log = trip.log, osa=osa, pr=pr, direct, direct_fns, repo=repo, pect_ggplot=pect_ggplot) {
  require(shiny)
  trips <- NULL
  trip.log_f <- list()
  osa_f <- list()
  
  for(i in 1:length(trip.log)){
    trip<-unique(trip.log[[i]]$tripnum)
    trips <- c(trips,trip)
    trip.log_f[[i]] <- trip.log[[i]]
    trip.log_f[[i]]$date.sail <- as.character(format(trip.log_f[[i]]$date.sail,'%Y-%m-%d'))
    trip.log_f[[i]]$date.land <- as.character(format(trip.log_f[[i]]$date.land,'%Y-%m-%d'))
    trip.log_f[[i]]$date <- as.character(format(trip.log_f[[i]]$date,'%Y-%m-%d'))
    trip.log_f[[i]]$lbs <- trip.log_f[[i]]$pro.repwt * 2.2046
    trip.log_f[[i]] <- as.data.frame(trip.log_f[[i]])
    trip.log_f[[i]] <- trip.log_f[[i]][, which(!names(trip.log_f[[i]]) == "geometry")]
    osa_f[[i]] <- as.data.frame(osa[[i]])
    osa_f[[i]] <- osa_f[[i]][, which(!names(osa_f[[i]]) == "geometry")]
  }
  
  ui <- fluidPage(
    
    # Application title
    titlePanel("Spatial Log Checks"),
    
    # a few user notes (legend)
    helpText("NAFO lines are black.  ",
             "SFA lines are blue.  ",
             "EEZ is red.  "),
    
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
        geom_point(data=trip.log_f[[which(trips==input$trip)]], aes(lon, lat))# +
       #coord_map(clip = "on") 
        
    })
    
    output$notes <- NULL

    output$click_info <- renderTable({
     nearPoints(df=as.data.frame(trip.log_f[[which(trips==input$trip)]]), xvar = "lon", yvar="lat", coordinfo = input$plot1_click, addDist=TRUE)
    })

    output$brush_info <- renderTable({
      brushedPoints(df=as.data.frame(trip.log_f[[which(trips==input$trip)]]), xvar = "lon", yvar="lat", brush = input$plot1_brush)
    })

  }

  app <- shinyApp(ui, server)
  
  # Run the application 
  runApp(app)
  
}
  
