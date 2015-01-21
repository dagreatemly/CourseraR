

pollutantmean <- function(directory, pollutant, id = 1:332){
        files_list<-list.files(directory, full.name=TRUE)
	data <- data.frame()
	for(i in id) {
		data <- rbind(data, read.csv(files_list[i]))
	}
        target<-data[pollutant]
        bad<-is.na(target)
        target<-target[!bad]
        mean(target)       
}
        
		
			
		 