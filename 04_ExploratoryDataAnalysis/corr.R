corr <- function(directory, threshold = 0) {
  if(!file.exists(directory)) {
    stop("Invalid directory!")
  }
  files <- list.files(directory)
  compCases <- NULL
  corSulpNit <- c()
  for(file in files) {
    monitorData <- read.csv(paste(directory,file, sep="\\"), header=T)
    good <- complete.cases(monitorData)
    if(nrow(monitorData[good,]) > threshold) {
      corSulpNit <- c(corSulpNit, (cor(monitorData[good,][,"sulfate"], monitorData[good,][,"nitrate"])))
    }
  }
  if(length(corSulpNit) > 0) {
    corSulpNit
  } else {
    as.numeric(c())
  }
}