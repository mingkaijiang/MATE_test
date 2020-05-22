# function for multiple runs.
RMATEsensitivity <- function(rundfr,...){
    
    starttime <- proc.time()[3]
    results <- list()
    
    for(i in 1:nrow(rundfr)){
        results[[i]] <- do.call(RMATE2, as.list(rundfr[i,],...))
        cat("Row",i,"done.\n")
    }
    
    endtime <- proc.time()[3]
    cat("All runs finished in",round(endtime-starttime,2),"sec.,", 
        round( (endtime-starttime) / nrow(rundfr),2),"sec. per run.\n")
    results
}