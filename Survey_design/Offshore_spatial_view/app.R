#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Plot survey or fishery data with survey stations"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId="direct_data",
                      label="Data directory",
                      value="Y:/Offshore/Assessment/"),
            
            textInput(inputId="direct_fns",
                      label="Code directory",
                      value="C:/Users/keyserf/Documents/Github/"),
            
            selectInput(inputId = "data_type",
                        label = "Which data to load?",
                        choices = c("survey", "fishery"),
                        multiple=T),
            
            selectInput(inputId = "load_years",
                        label="Load from which year's files?",
                        choice=2008:year(Sys.Date())),
            
            actionButton('load', 'Load data'),
            
            selectInput(inputId = "overlay_data",
                        label="Which data to plot?",
                        choices = c("survey", "fishery")),
            
            selectInput(inputId = "fishery_years",
                        label="Fishery years:",
                        choices = 2008:year(Sys.Date()),
                        multiple=T),
            
            selectInput(inputId = "survey_years",
                        label="Survey years:",
                        choices = 2008:year(Sys.Date()),
                        multiple=T),
            
            selectInput(inputId = "station_years",
                        label="Station years:",
                        choices = 2008:year(Sys.Date())),
            
            selectInput(inputId = "size_class",
                        label="Which survey size class to plot?",
                        choices = c("pre", "rec", "com", "tot"),
                        multiple=T),
            
            selectInput(inputId = "bank",
                        label="Which bank?",
                        choices = c("GBa", "GBb", "BBn", "BBs", "Ger", "Sab", "Ban", "Mid"))),
        
        #Show a plot of the generated distribution
        mainPanel(
            NULL
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

        observeEvent(input$load, {
    
        showNotification("... loading data now", action = NULL, duration = NULL, closeButton = F,
                         id = "loading", type = "message",
                         session = getDefaultReactiveDomain())
    
        
            source(paste0(input$direct_fns, "Assessment_fns/Maps/plot_offshore_spatial.R"))

            
            #### THIS DOES NOT WORK
            renderText({
                if("survey" %in% input$data_type) {survey <- T}
                if("fishery" %in% input$data_type) {fishery <- T}
            })
            
        offshore_data <- load_offshore_spatial(direct_data = input$direct_data,
                                               direct_fns = input$direct_fns,
                                               survey = survey,
                                               fishery = fishery,
                                               load_years = input$load_years)

        removeNotification("loading", session = getDefaultReactiveDomain())
        
        showNotification("Data loaded successfully", action = NULL, duration = NULL, closeButton = F,
                         id = "loading", type = "message",
                         session = getDefaultReactiveDomain())
        
        })
    
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}


# Run the application 
shinyApp(ui = ui, server = server)
