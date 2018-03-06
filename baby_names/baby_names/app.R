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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Baby Name Comparison"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("given_name",
                     "Name to compare:",
                     placeholder = "Daniel")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2] 
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # baby_names <- read.csv("NationalNames.csv")
     load("baby_names.RData")
     source("useful.R")
      compare_new <- dplyr::filter(baby_names,Name == "Lua" | Name == "Ira" | Name == input$given_name) %>% 
        group_by(Year,Name) %>% 
        dplyr::summarize(Count = sum(Count))
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      my_plot <- ggplot(compare_new) + geom_line(aes(x = Year,y = Count, color = Name)) + 
        ggtitle("Babies Born in the US") +
        theme_bw() 
      
      left_right_footnote(my_plot,"Dan Spencer [2018]","https://www.kaggle.com/kaggle/us-baby-names/data")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

