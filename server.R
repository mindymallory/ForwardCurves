#[copyright: Mindy L Mallory 2017]


library(shiny)
library(ggplot2)
library(quantmod)
library(xts)
library(lubridate)
library(ArgumentCheck)
library(ustyc)



# Helper Functions

month_symbol <- function(monthint){
  # Function accepts integers representing months from (1-12), and returns the futures contract code for 
  # the next month. Going to the next month lets us never need to parse whether we are before or after the 15th of the month
  
  # Check argument 
  # 
  # Check <- ArgumentCheck::newArgCheck()
  # 
  # if (is.integer(monthint) != TRUE){
  #   ArgumentCheck::addError(
  #     msg = "argument must be an integer, i.e., as.integer(lubridate::month(Sys.Date()))",
  #     argcheck = Check
  #   )
  # }
  # 
  # ArgumentCheck::finishArgCheck(Check)
  
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
corn_contract8 <- cbind(corn_contract8, "", "","","", "")
wheat_contract8 <- corn_forward8(cm, Year)
wheat_contract8 <- cbind(wheat_contract8, "", "","","", "")

soy_contract8 <- soy_forward8(sm, Year)
soy_contract8 <- cbind(soy_contract8, "", "","","", "")

codes <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z')

# Fetch Corn Quotes
for (i in 1:8){
 
  corn_contract8[i, 3] <- paste0("C", corn_contract8[i,1], substr(corn_contract8[i, 2], 3,4), ".", "CBT")
  corn_contract8[i, 4] <- as.numeric(getQuote(corn_contract8[i,3])[2])
  corn_contract8[i, 5] <- format(as.Date(paste0(sprintf("%02d", match(corn_contract8[i,1], codes)), "/", "14", "/", substr(corn_contract8[i, 2], 3,4)), "%m/%d/%y"), "%Y-%m-%d")
  corn_contract8[i, 6] <- as.Date(corn_contract8[i, 5]) - Sys.Date()
}

# Fetch Wheat Quotes
for (i in 1:8){
  
  wheat_contract8[i, 3] <- paste0("W", wheat_contract8[i,1], substr(wheat_contract8[i, 2], 3,4), ".", "CBT")
  wheat_contract8[i, 4] <- as.numeric(getQuote(wheat_contract8[i,3])[2])
  wheat_contract8[i, 5] <- format(as.Date(paste0(sprintf("%02d", match(wheat_contract8[i,1], codes)), "/", "14", "/", substr(wheat_contract8[i, 2], 3,4)), "%m/%d/%y"), "%Y-%m-%d")
  wheat_contract8[i, 6] <- as.Date(wheat_contract8[i, 5]) - Sys.Date()
}

# Fetch Soy Quotes
for (i in 1:8){
  
  soy_contract8[i, 3] <- paste0("S", soy_contract8[i,1], substr(soy_contract8[i, 2], 3,4), ".", "CBT")
  soy_contract8[i, 4] <- as.numeric(getQuote(soy_contract8[i,3])[2])
  soy_contract8[i, 5] <- format(as.Date(paste0(sprintf("%02d", match(soy_contract8[i,1], codes)), "/", "14", "/", substr(soy_contract8[i, 2], 3,4)), "%m/%d/%y"), "%Y-%m-%d")
  soy_contract8[i, 6] <- as.Date(soy_contract8[i, 5]) - Sys.Date()
  
} 

colnames(corn_contract8) <- c("Month", "Year", "Contract", "Price", "Expiration", "DTE", "Label")
colnames(wheat_contract8) <- c("Month", "Year", "Contract", "Price", "Expiration", "DTE", "Label")
colnames(soy_contract8) <- c("Month", "Year", "Contract", "Price", "Expiration", "DTE", "Label")

corn_contract8 <- as.data.frame(corn_contract8)
corn_contract8$Contract <- substr(corn_contract8$Contract, 1,4)
corn_contract8$DTE <- as.numeric(as.character(corn_contract8$DTE))
corn_contract8$Price <- as.numeric(as.character(corn_contract8$Price))
corn_contract8$Label <- paste(corn_contract8$Contract, corn_contract8$Price)
C_DRet30 <- round(diff( corn_contract8$Price)/diff(corn_contract8$DTE)*30, digits = 2)
c_label2 <- paste(corn_contract8$Contract[-8], C_DRet30)

wheat_contract8 <- as.data.frame(wheat_contract8)
wheat_contract8$Contract <- substr(wheat_contract8$Contract, 1,4)
wheat_contract8$DTE <- as.numeric(as.character(wheat_contract8$DTE))
wheat_contract8$Price <- as.numeric(as.character(wheat_contract8$Price))
wheat_contract8$Label <- paste(wheat_contract8$Contract, wheat_contract8$Price)
w_DRet30 <- round(diff( wheat_contract8$Price)/diff(wheat_contract8$DTE)*30, digits = 2)
w_label2 <- paste(wheat_contract8$Contract[-8], w_DRet30)


soy_contract8 <- as.data.frame(soy_contract8)
soy_contract8$Contract <- substr(soy_contract8$Contract, 1,4)
soy_contract8$DTE <- as.numeric(as.character(soy_contract8$DTE))
soy_contract8$Price <- as.numeric(as.character(soy_contract8$Price))
soy_contract8$Label <- paste(soy_contract8$Contract, soy_contract8$Price)
s_DRet30 <- round(diff( soy_contract8$Price)/diff(soy_contract8$DTE)*30, digits = 2)
s_label2 <- paste(soy_contract8$Contract[-8], s_DRet30)

corn_ret_store <- as.data.frame(cbind(c_label2, C_DRet30, corn_contract8$DTE[-8]))
colnames(corn_ret_store) <- c("Label", "DRet30", "DTE" )

wheat_ret_store <- as.data.frame(cbind(w_label2, w_DRet30, wheat_contract8$DTE[-8]))
colnames(wheat_ret_store) <- c("Label", "DRet30", "DTE")

soy_ret_store <- as.data.frame(cbind(s_label2, s_DRet30, soy_contract8$DTE[-8]))
colnames(soy_ret_store) <- c("Label", "DRet30", "DTE")


c <- ggplot(corn_contract8, aes(DTE, Price)) + 
      geom_point(size = 3) + geom_line(size = .75) + 
      xlim(c(0, 600)) +
      geom_text(aes(label=Label), size = 5, nudge_y = 5) +
      theme_bw(base_size = 14) +  theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Days to Expiration", y = "Price of Corn in Cents", title = "Corn Forward Curve", 
           caption = "Prices from Yahoo Finance")

w <- ggplot(wheat_contract8, aes(DTE, Price)) + 
      geom_point(size = 3) + geom_line(size = .75) + 
      xlim(c(0, 600)) +
      geom_text(aes(label=Label), size = 5, nudge_y = 5) +
      theme_bw(base_size = 14) +  theme(plot.title = element_text(hjust = 0.5)) + 
      labs(x = "Days to Expiration", y = "Price of wheat in Cents", title = "Chicago Wheat Forward Curve", 
        caption = "Prices from Yahoo Finance")



s <- ggplot(soy_contract8, aes(DTE, Price)) + 
      geom_point(size = 3) + 
      geom_line(size = .75) + 
      xlim(c(0, 600)) +
      geom_text(aes(label=Label), size = 5, nudge_y = 5) + 
      theme_bw(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Days to Expiration", y = "Price of Soybean in Cents", title = "Soybean Forward Curve", 
            caption = "Prices from Yahoo Finance")

c_ret <- ggplot(corn_ret_store, aes(DTE, DRet30, group = 1)) +
          geom_point(size = 3) + 
          geom_line(size = .75) + 
          theme_bw(base_size = 14) + 
          theme(plot.title = element_text(hjust = 0.5)) +  
          geom_text(aes(label=Label), size = 5, nudge_y = .5) + 
          labs(x = "Days to Expiration", y = "30 Day Return in Cents", title = "Corn 30 Day Return to Storage", 
                     caption = "The return plotted is the implied 30 day return to storage between the labeled contract 
                     and the next deferred. For example, the point labeled 'CHXX' plots the return for storing from 
                     March 14th to 30 days later. The return is calculated by '(CKXX - CHXX)/(Days between H and K)*30.")

w_ret <- ggplot(wheat_ret_store, aes(DTE, DRet30, group = 1)) +
          geom_point(size = 3) + 
          geom_line(size = .75) + 
          theme_bw(base_size = 14) + 
          theme(plot.title = element_text(hjust = 0.5)) +  
          geom_text(aes(label=Label), size = 5, nudge_y = .5) + 
          labs(x = "Days to Expiration", y = "30 Day Return in Cents", title = "Wheat 30 Day Return to Storage", 
               caption = "The return plotted is the implied 30 day return to storage between the labeled contract 
                and the next deferred. For example, the point labeled 'CHXX' plots the return for storing from 
                March 14th to 30 days later. The return is calculated by '(CKXX - CHXX)/(Days between H and K)*30.")


s_ret <- ggplot(soy_ret_store, aes(DTE, DRet30, group = 1)) +
          geom_point(size = 3) + 
          geom_line(size = .75) + 
          theme_bw(base_size = 14) + 
          theme(plot.title = element_text(hjust = 0.5)) +  
          geom_text(aes(label=Label), size = 5, nudge_y = .5) + 
          labs(x = "Days to Expiration", y = "30 Day Return in Cents", title = "Soy 30 Day Return to Storage", 
              caption = "The return plotted is the implied 30 day return to storage between the labeled contract 
              and the next deferred. For example, the point labeled 'CHXX' plots the return for storing from 
              March 14th to 30 days later. The return is calculated by '(CKXX - CHXX)/(Days between H and K)*30.")

# Treasury Yields

t <-getYieldCurve(year = Year)
i <- dim(t$df)[1]

t <- t$df[i,]
t <- as.data.frame(cbind(colnames(t), t(t)))
DATE <- colnames(t)[2]
colnames(t) <- c("Maturity", "Yield")
t$Maturity <- substr(t$Maturity, 4, 100)
t$Maturity <- factor(t$Maturity, levels = t$Maturity)
t$Yield <- as.numeric(as.character(t$Yield))
t <- t[-dim(t)[1],]
treas_plot <- ggplot(t, aes(Maturity, Yield, group = 1)) + 
              geom_point(size = 3) +
              ylim(c(0, 7)) +
              geom_line(size = .75) +
              annotate("text", x=2,  y = 6, label = DATE) +
              theme_bw(base_size = 14) + 
              geom_text(aes(label=Yield), size = 5, nudge_y = .5) + 
              theme(plot.title = element_text(hjust = 0.5)) +  
              labs(x = "Maturities", y = "Yields", title = "US Treasury Yields")
treas_plot


            
            
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
  
  output$CornPlot <- renderPlot({
    c
  })
  
  output$CornRetPlot <- renderPlot({
    c_ret
  })
  
  output$SoyPlot <- renderPlot({
    s
  })
  
  output$SoyRetPlot <- renderPlot({
    s_ret
  })
  
  output$WheatPlot <- renderPlot({
    w
  })
  
  output$WheatRetPlot <- renderPlot({
    w_ret
  })
  
  output$TreasPlot <- renderPlot({
    treas_plot
  })
  
})



