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
   titlePanel("Cherwell Weekly Aging Report"),
  
   dateInput("we_date", "Enter the week ending date in the format:", value = "19-09-27"),

   # Show a plot of the generated distribution
      mainPanel(
         plotOutput("we_plots")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$text <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

