complete <- function(directory, id = 1:332){
  if(!file.exists(directory)) {
    stop("Invalid directory!")
  }
  fileNames <- NULL
  compCases <- NULL
  for(monitorId in id) {
    file <- paste(directory, sprintf("%03d.csv", monitorId), sep="\\")
    if(!file.exists(file)) {
      stop(paste(file, "file doesn't exist!"))
    }
    monitorData <- read.csv(file, header=T)
    good <- complete.cases(monitorData)
    fileNames <- c(fileNames, monitorId)
    compCases <- c(compCases, nrow(monitorData[good,]))
  }
  compCaseObs <- data.frame(id = fileNames, nobs = compCases)
  compCaseObs
}