################## Programming Assignment 3 - R Programming - Coursera - 17.09.2021 ##########################

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