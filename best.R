################## Programming Assignment 3 - R Programming - Coursera - 17.09.2021 ##########################

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