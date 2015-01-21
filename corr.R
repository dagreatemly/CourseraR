corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.name=TRUE)
  rawCorr <- vector("numeric", length(files_list))
  for(i in 1:length(files_list)) {
    data <- read.csv(files_list[i])
    good <- complete.cases(data)
    data <- data[good,]
    if(length(data$sulfate) < threshold) {
      rawCorr[i] <- NA
    } else {
      rawCorr[i] <- cor(data$sulfate, data$nitrate)
    }
  }
  bad <- is.na(rawCorr)
  c <- rawCorr[!bad]
}