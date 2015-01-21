complete <- function(directory, id = 1:332) {
     id <- as.integer(id)        
     nobs <- vector("integer", length = length(id))
     j <- 1
     files_list<-list.files(directory, full.name=TRUE)
     for(i in id) {
          data <- read.csv(files_list[i])
          good <- complete.cases(data)
          data <- data[good,]
          nobs[j] <- nrow(data)
          j <- j + 1
     }
     dataset <- cbind(id, nobs)
     dataset <- data.frame(dataset)
     return(dataset)
}