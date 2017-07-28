library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Forward Curves"),
  
  # Sidebar with a slider input for the number of bins
  
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)