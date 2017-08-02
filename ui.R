library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  

  titlePanel("Forward Curves"),
  

  
    
   navlistPanel(
     
       tabPanel("Ag Commodity Futures Forward Curves", plotOutput("CornPlot"), plotOutput("SoyPlot"),  plotOutput("WheatPlot")),
       tabPanel("Ag commodity 30 Day Returns to Storage", plotOutput("CornRetPlot"), plotOutput("WheatRetPlot"), plotOutput("SoyRetPlot")),
       tabPanel("Yields on Treasury Bills and Bonds", plotOutput("TreasPlot"), helpText("Data from https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield"))
     ) 
      
      
    )
  )
