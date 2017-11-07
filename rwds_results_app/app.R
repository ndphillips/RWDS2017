#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rdrop2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(""),
   
   plotOutput("distPlot"),
   
   p("On average, across 10 datasets, how accurate will FFTrees be to Random Forests?"),
   
   actionButton(inputId = "update", label = "Get Data"),
   
   checkboxInput(inputId = "showplot", value = FALSE, label = "Show Individual Results?"),
   
   checkboxInput(inputId = "showmean", value = FALSE, label = "Show mean?"),
   
   checkboxInput(inputId = "showtruth", value = FALSE, label = "Show Truth?"),
   
   verbatimTextOutput(outputId = "progress")
   
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Get tocken
  droptoken <- readRDS("droptoken.rds")        # Reads in authentication for dropbox
  
  # Authorize account 
  rdrop2::drop_auth(rdstoken = "droptoken.rds")
  # 
  
  observeEvent(input$update, {
    data_names <- rdrop2::drop_dir("nphillips/RWDS/data", dtoken = droptoken)
    
    output$progress <- renderText({paste("Downloading", nrow(data_names), "responses...")})
    
  })
  
  observeEvent(input$update, {

    # Get list of file names

    data_names <- rdrop2::drop_dir("nphillips/RWDS/data", dtoken = droptoken)

    
    # extract all data

    predictions_ls <- lapply(data_names$name, FUN = function(x) {

      data_i <- rdrop2::drop_read_csv(paste0("nphillips/RWDS/data/", x), dtoken = droptoken)

    })

    # Store as dataframe
    predictions_df <- do.call(what = rbind, args = predictions_ls)

    output$distPlot <- renderPlot({
      
      
      # predictions_df <- filter(predictions_df, grepl("test", id) == FALSE)
      
      n_pred <- nrow(predictions_df)
      
      
      plot(1, xlim = c(0, n_pred + 1), ylim = c(0, 100), type = "n", xaxt = "n", yaxt = "n", xlab = "", bty = "n", ylab = "Prediction")
      
      axis(2, at = seq(0, 100, 10), las = 1)
      
      abline(h = seq(0, 100, 10), lty = 3, lwd = c(1, .5), col = gray(.5))
      
      color.v <- rep(structure(c("#026CCBFF", "#F51E02FF", "#05B102FF", "#FB9F53FF", 
                                 "#9B9B9BFF", "#FB82BEFF", "#BA6222FF", "#EEC229FF", "#026CCBFF", 
                                 "#F51E02FF"), .Names = c("blue", "red", "green", "orange", "gray", 
                                                          "pink", "brown", "yellow", "blue", "red")), length.out = n_pred)
      
      if(input$showplot) {
        
        for(i in 1:n_pred) {
          
          segments(i + 1, predictions_df$lowest[i],i + 1, predictions_df$highest[i])
          
          points(i + 1, predictions_df$best[i], pch = 21, col = "white", bg = color.v[i], cex = 2)
          
          mtext(text = predictions_df$id[i], side = 1, at = i + 1)
          
        }
        
        #average
        
        if(input$showmean) {
          
          segments(1, mean(predictions_df$lowest), 1, mean(predictions_df$highest))
          
          points(1, mean(predictions_df$best), cex = 7, col = "black", bg = "white", pch = 21, lwd = 2)
          
          text(1,  mean(predictions_df$lowest), labels = round( mean(predictions_df$lowest), 0), pos = 1)
          text(1,  mean(predictions_df$highest), labels = round( mean(predictions_df$highest), 0), pos = 3)
          text(1,  mean(predictions_df$best), labels = round( mean(predictions_df$best), 0), adj = .5)
          
          mtext("Mean", side = 1, at = 1, font = 2)
          
        }
        
        if(input$showtruth) {
          
          par("xpd" = FALSE)
          segments(.5, 96, .5, 100)
          
          points(.5, 98, cex = 2, col = "black", bg = "white", pch = 21, lwd = 2)
          
          text(.5,  100, labels = "100", pos = 4)
          text(.5,  98, labels = "98", pos = 4)
          text(.5,  96, labels = "96", adj = .5)
          
          mtext("Simulation", side = 1, at = .5, font = 2)
          
          par("xpd" = TRUE)
          
        }
        
        
      }
      
      
    })
    
    
    
  })
  
  
 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

