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
   titlePanel(""),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        h2("How accurate will FFTrees be to Random Forests?"),
        p("Accuracy is balanced accuracy (average of sensitivity and specificity), in prediction."),
        p("0 = No accuracy, 100 = Equally accurate as Random Forests"),
         sliderInput("lowest",
                     "In the WORST dataset for FFTrees",
                     min = 0,
                     max = 100,
                     value = 50),
         
         sliderInput("best",
                     "On AVERAGE across all 10 datasets",
                     min = 0,
                     max = 100,
                     value = 50),
         
         sliderInput("highest",
                     "In the BEST dataset for FFTrees",
                     min = 0,
                     max = 100,
                     value = 50),
        
        
        textInput("initials", label = "My Initials (optional)", value = ""),
        
        actionButton("goButton", "Submit!")
        
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        br(), br(),
        h2(textOutput("finished"))
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   
  observeEvent(input$goButton, {
    
    droptoken <- readRDS("droptoken.rds")        # Reads in authentication for dropbox
    
    outputDir <- "nphillips/rwds/data"  # Directory to save data
    
    data <- data.frame("id" = input$initials, "lowest" = input$lowest, "best" = input$best, "highest" = input$highest)
    
    filename <- paste0(paste0(sample(LETTERS, size = 20, replace = TRUE), collapse = ""), ".csv")
    
    GameDatafilePath <- file.path(tempdir(), filename)
    write.csv(data, GameDatafilePath, row.names = FALSE)
    rdrop2::drop_upload(file = GameDatafilePath, 
                        path = outputDir, 
                        dtoken = droptoken)
    
    
    output$finished <- renderText({"Thank you! Your responses have been recorded :)"})
    
    
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

