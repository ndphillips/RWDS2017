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
        
        p("Give your answers here!"),
         sliderInput("lowest",
                     "In its WORST performing dataset for FFTs, I predict they they were X% as accurate as RF:",
                     min = 0,
                     max = 100,
                     value = 50),

        sliderInput("highest",
                    "In the BEST performing dataset for FFTs, I predict they were X% as accurate as RF:",
                    min = 0,
                    max = 100,
                    value = 50),
        
         sliderInput("best",
                     "On AVERAGE across all 10 datasets, I predict FFTs were X% as accurate as RF:",
                     min = 0,
                     max = 100,
                     value = 50),
        

        
        
        textInput("initials", label = "Include your initials here (optional)", value = ""),
        
        actionButton("goButton", "Submit!"),
        h2(textOutput("finished"))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h3("Predict the relative accuracy of fast-and-frugal trees to random forests!"),
        p("I conducted a prediction simulation comparing the accuracy of fast-and-frugal trees (FFTs) to random forests (RF) across 10 diverse datasets taken from the UCI machine learning respository"),
        p("In each dataset, I selected 50% of the data to train the models, and 50% to test them. I repeated this 100 times."),
        p("For each simulation, I measured accuracy as balanced accuracy (average of sensitivity and specificity), in testing."),
        p("I then calcualted the relative accuracy of fast-and-frugal trees to Random Forests, where:"),
        p("0 = No accuracy"),
        p("100 = FFTs are just as accurate as Random Forests."),
        p("Using the sliders on the left, indicate how well you think FFTs performed relative to Random Forests across these datasets!"),
        img(src='https://github.com/ndphillips/RWDS2017/blob/master/images/ten_datasets.png?raw=true', width = "800")

        
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

