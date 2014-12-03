pollutantmean <- function(directory, pollutant, id = 1:332){
  if(!file.exists(directory)) {
    stop("Invalid directory!")
  }
  monitorPolData <- NULL
  for(monitorId in id) {
    file <- paste(directory, sprintf("%03d.csv", monitorId), sep="\\")
    if(!file.exists(file)) {
      stop(paste(file, "file doesn't exist!"))
    }
    monitorData <- read.csv(file, header=T)
    monitorPolData <- c(monitorPolData, monitorData[[pollutant]])
    if(is.null(monitorPolData)) {
      stop("Invalid pollutant!")
    }
  }
  meanMonitorPolData <- mean(monitorPolData, na.rm=T)
  print(round(meanMonitorPolData, digits=3))
}