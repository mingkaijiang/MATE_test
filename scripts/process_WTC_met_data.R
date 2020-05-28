process_WTC_met_data <- function(inFile, outFile) {
    
    inFile = "input/met_drawdownperiod_ch02.csv"
    
    ### input
    inDF <- read.csv(inFile)
    
    ### convert time
    inDF$Date <- as.Date(inDF$DateTime)
    
    ### convert leaf area to LAI
    inDF$LAI <- inDF$leafArea / (pi * (3.25/2)^2)
    
    ### rename some variables
    inDF$Tav <- inDF$tair
    inDF$Radtot <- inDF$par / 2
    inDF$VPDav <- inDF$vpd
    inDF$Precip <- 10
    inDF$Tmax <- max(inDF$Tav)
    inDF$Tmin <- min(inDF$Tav)
    inDF$DOY <- inDF$doy
    
    ### prepare output
    outDF <- inDF[,c("Date", "DateTime", "chamber", "Canopy", "DOY", "hod", "year",
                     "Radtot", "VPDav", "Tav", "Tmax", "Tmin", "Precip", "Ca", "LAI",
                     "FluxCO2", "NfluxCO2", "T_treatment", "Water_treatment")]
    
    write.csv(outDF, outFile, row.names=F)
    
    
}