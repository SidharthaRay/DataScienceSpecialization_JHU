best <- function(state, outcome) {
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
  
  outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", paste(sapply(strsplit(outcome, " ")[[1]], initCap), collapse="."), sep="")
  outcomes[, outcome] <- suppressWarnings(as.numeric(outcomes[, outcome]))
  state.outcomes <- outcomes[outcomes["State"] == state, ]
  minMort <- min(state.outcomes[, outcome], na.rm=T)
  print(sort(state.outcomes[state.outcomes[outcome] == minMort, 2], decreasing=T)[1])
}

initCap <- function(string) {
  paste(toupper(substring(string, 1,1)), substring(string, 2), sep="")
}