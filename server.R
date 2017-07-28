#[copyright: Mindy L Mallory 2017]


library(shiny)
library(ggplot2)
library(quantmod)
library(xts)
library(lubridate)
library(ArgumentCheck)



# Helper Functions

month_symbol <- function(monthint){
  # Function accepts integers representing months from (1-12), and returns the futures contract code for 
  # the next month. Going to the next month lets us never need to parse whether we are before or after the 15th of the month
  
  # Check argument 
  
  Check <- ArgumentCheck::newArgCheck()
  
  if (is.integer(monthint) != TRUE){
    ArgumentCheck::addError(
      msg = "argument must be an integer, i.e., as.integer(lubridate::month(Sys.Date()))",
      argcheck = Check
    )
  }
  
  ArgumentCheck::finishArgCheck(Check)
  
  codes <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  
  code <- codes[monthint + 1]
  return(code)
  
}


get_nearby_corn <- function(MonthCode){
  codes <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  corn_codes <- c('H', 'K','N', 'U', 'Z')
  
  while(is.na(match(MonthCode,corn_codes)) == TRUE){
    MonthCode <- match(MonthCode,codes)+1
    MonthCode <- codes[MonthCode]
    }
  
  return(MonthCode)
}

corn_forward8 <- function(MonthCode, Year){
  c <- c('H', 'K','N', 'U', 'Z', 'H', 'K','N', 'U', 'Z', 'H', 'K','N', 'U', 'Z')
  y      <-  c(rep(Year, each = 5), rep(Year+1, each = 5), rep(Year +2, each = 5))
  corn_codes <- cbind(c, y)
  contract <- match(MonthCode, corn_codes)
  contract8 <- corn_codes[contract:(contract+7),]
  return(contract8)
}

get_nearby_soy <- function(MonthCode){
  codes <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')
  soy_codes <- c('F', 'H', 'K','N', 'Q', 'U', 'X')
  
  while(is.na(match(MonthCode,soy_codes)) == TRUE){
    MonthCode <- match(MonthCode,codes)+1
    MonthCode <- codes[MonthCode]
  }
  
  return(MonthCode)
}

soy_forward8 <- function(MonthCode, Year){
  s <- c('F', 'H', 'K','N', 'Q', 'U', 'X', 'F', 'H', 'K','N', 'Q', 'U', 'X', 'F', 'H', 'K','N', 'Q', 'U', 'X')
  y         <- c(rep(Year, each = 7), rep(Year+1, each = 7), rep(Year +2, each = 7))
  corn_codes <- cbind(s, y)
  contract <- match(MonthCode, corn_codes)
  contract8 <- corn_codes[contract:(contract+7),]
  return(contract8)
}

Today <- Sys.Date()
mo <- as.integer(month(Today))
Year <- as.numeric(year(Today))

MonthCode <- month_symbol(mo)
cm <- get_nearby_corn(MonthCode)
sm <- get_nearby_soy(MonthCode)

corn_contract8 <- corn_forward8(cm, Year)
corn_contract8 <- cbind(corn_contract8, "", "")
soy_contract8 <- soy_forward8(sm, Year)
soy_contract8 <- cbind(soy_contract8, "", "")

for (i in 1:8){
  
  corn_contract8[i, 3] <- paste0("C", corn_contract8[i,1], substr(corn_contract8[i, 2], 3,4), ".", "CBT")
  corn_contract8[i, 4] <- as.numeric(getQuote(corn_contract8[i,3])[2])
}



for (i in 1:8){
  
  soy_contract8[i, 3] <- paste0("S", soy_contract8[i,1], substr(soy_contract8[i, 2], 3,4), ".", "CBT")
  soy_contract8[i, 4] <- as.numeric(getQuote(soy_contract8[i,3])[2])
} 

plot(corn_contract8[,4])
plot(soy_contract8[,4])


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
})



