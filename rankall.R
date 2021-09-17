################## Programming Assignment 3 - R Programming - Coursera - 17.09.2021 ##########################

### Step 4 - Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
  
  ## Reads the data set 'outcome-of-care-measures.csv' and store in a variable 
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that outcome input is valid
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")){stop("invalid outcome")}
  
  ## relevant data for inputed arguments
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  df <- df[, c(2, 7, outcomes[outcome])]
  colnames(df) <- c("Hospital", "State", "Deaths")
  
  ## 'Deaths' as numeric (outcome column)
  df[, 3] <- suppressWarnings(as.numeric(df[, 3]))
  
  ## remove NA
  df <- na.omit(df)
  
  # create an empty data frame
  dat <- data.frame(character(), character(), numeric())
  rankstate <- function(x){
    # return the rank within x 
    n <- NULL 
    if(!is.numeric(num)){
      if (num =='best') n <- 1
      if (num =='worst') n <- nrow(x)
    } else n <- num
    
    # sort the dataframe
    x <- x[order(x[['Deaths']], x[['Hospital']]),  ]
    
    
    if (n > nrow(x))
      dat <<- rbind (dat, c(NA, x[[1,2]], NA))
    else
      dat <<- rbind (dat, c(x[[n,1]], x[[n,2]], x[[n,3]]))
  }
  
  lapply(split(df, df$State), rankstate)
  names(dat) <- c('Hospital', 'State', 'Deaths')
  dat
}
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)
