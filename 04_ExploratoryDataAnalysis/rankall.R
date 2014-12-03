rankall <- function(outcome, num = "best") {
  outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  all.states <- sort(unique(outcomes[,7]), decreasing=F)
  if(is.null(outcomes)) {
    stop("outcome-of-care-measures.csv doesn't exist")
  }
  if(!is.element(outcome, c('heart attack', 'heart failure', 'pneumonia'))) {
    stop("invalid outcome")
  }
  
  #Getting name of the column
  outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", paste(sapply(strsplit(outcome, " ")[[1]], initCap), collapse="."), sep="")
  
  #Converting from character to numeric
  outcomes[, outcome] <- suppressWarnings(as.numeric(outcomes[, outcome]))
  hospital=c()
  state=c()
  for(st in all.states) {
    #Subset of outcomes for given "State"
    state.outcomes <- outcomes[outcomes["State"] == st & !is.na(outcomes[outcome]), ]
    
    #Deciding the rank
    rank <- NULL
    if(identical(num, "best")) {
      rank <- 1
    } else if(identical(num, "worst")) {
      rank <- nrow(state.outcomes)
    } else {
      rank <- num
    }
    
    #Sort as per given outcome value
    hosp <- state.outcomes[order(state.outcomes[,outcome], state.outcomes[,2], na.last=NA), 2][rank]
    hospital <- c(hospital, hosp)
    state <- c(state, st)
  } 
  data.frame(hospital, state, row.names=state)
}

initCap <- function(string) {
  paste(toupper(substring(string, 1,1)), substring(string, 2), sep="")
}