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
  codes <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z', 'F', 'G', 'H')
  corn_codes <- c('H', 'K','N', 'U', 'Z', 'H')
  
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
  codes <- c('F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z', 'F')
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
