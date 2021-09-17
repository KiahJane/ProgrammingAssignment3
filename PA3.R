################## Project 3 - R Programming - Coursera - 17.09.2021 ##########################

setwd("C:/Users/Matija/Documents/Kiah/Coursera/ProgrammingAssignment3")
getwd()

### Step 1 - Plot the 30-day mortality rates for heart attack ###
dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #-> here 'dat' instead of 'outcome' as per say in instructions --> removes later confusion
# get a feel for the data
head(dat)
ncol(dat)  #-> 46 columns
nrow(dat)  #-> 4706 rows
names(dat)#-> column names

# histogram of the 30-day death rates from heart attack
dat[, 11] <- as.numeric(dat[, 11])
hist(dat[, 11])



### Step 2 - Finding the best hospital in a state ###
best <- function(state, outcome){
  ## read csv file w/o NA
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  valid_outcome <- c("heart attack", "heart failure", "pneumonia")
  valid_state <- unique(df[, 7])
  if(! outcome %in% valid_outcome){stop("invalid outcome")}
  else if(! state %in% valid_state){stop("invalid state")}
  
  ## relevant data for inputed arguments
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  data <- df[, c(2, 7, outcomes[outcome])]  
  
  ## subset for inputed 'state' argument
  subset_data <- subset(data, State == state)
  
  ## rank order of data -> by rank then name
  min_deaths <- which(as.numeric(subset_data[, 3]) == 
                        min(as.numeric(subset_data[, 3]), na.rm = TRUE))
  hospitals <- sort(subset_data[min_deaths, 1])
  
  ## 1st hospital in alphabetical order in case of ties
  return(hospitals[1])
}
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

## Assignment Tips
# https://www.coursera.org/learn/r-programming/discussions/weeks/4/threads/znVFbLgpEeWlQwoU9G612w
# https://github.com/gafajardogr/ProgAssignment3/blob/master/best.R



### Step 3 - Ranking hospitals by outcome in a state ###
rankhospital <- function(state, outcome, num = "best") {
  ## Reads the data set 'outcome-of-care-measures.csv' and store in a variable 
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
 
   ## Check that state and outcome inputs are valid
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")){stop("invalid outcome")}
  else if(! state %in% unique(df[, 7])){stop("invalid state")}
  
  ## relevant data for inputed arguments
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  df <- df[, c(2, 7, outcomes[outcome])]
  colnames(df) <- c("Hospital", "State", "Deaths")
  
  ## 'Deaths' as numeric w/o NA
  df[, 3] <- suppressWarnings(as.numeric(df[, 3]))
  
  ## subset for input state
  df_state <- df[df$State == state, c("Hospital", "Deaths")]
  ## rank order outcome then hospital
  df_stateRank <- order(df_state$Deaths, df_state$Hospital, na.last = NA)
 
  ## Loop to return hospital name in that state with the given rank 30-day death rate
  if (num == "best") {as.character(df_state$Hospital[df_stateRank[1]])} 
  else if (num == "worst") {as.character(df_state$Hospital[df_stateRank[length(df_stateRank)]])} 
  else if (is.numeric(num)) {as.character(df_state$Hospital[df_stateRank[num]])} 
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)



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

