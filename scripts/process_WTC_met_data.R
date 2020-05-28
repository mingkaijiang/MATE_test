process_WTC_met_data <- function(inFile) {
    
    ### input
    inDF <- read.csv(inFile)
    
    ### convert time
    inDF$Date <- as.Date(inDF$DateTime)
    
    
    
}