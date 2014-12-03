rankhospital <- function(state, outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(is.null(outcomes)) {
    stop("outcome-of-care-measures.csv doesn't exist")
  }
  if(!is.element(state, unique(outcomes[,7]))) {
    stop("invalid state")
  }
  if(!is.element(outcome, c('heart attack', 'heart failure', 'pneumonia'))) {
    stop("invalid outcome")
  }
  
  #Getting name of the column
  outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", paste(sapply(strsplit(outcome, " ")[[1]], initCap), collapse="."), sep="")
 
  #Converting from character to numeric
  outcomes[, outcome] <- suppressWarnings(as.numeric(outcomes[, outcome]))
  
  #Subset of outcomes for given "State"
  state.outcomes <- outcomes[outcomes["State"] == state & !is.na(outcomes[outcome]), ]
  
  #Deciding the rank
  rank <- NULL
  states.count <- unique(state.outcomes[,2])
  if(identical(num, "best")) {
    rank <- 1
  } else if(identical(num, "worst")) {
    rank <- length(states.count)
  } else {
    rank <- num
  }
  if(rank > length(states.count)) {
    return(NA)
  }
  
  #Sort as per given outcome value
  state.outcomes[order(state.outcomes[,outcome], state.outcomes[,2], na.last=NA), 2][rank]
}

initCap <- function(string) {
  paste(toupper(substring(string, 1,1)), substring(string, 2), sep="")
}