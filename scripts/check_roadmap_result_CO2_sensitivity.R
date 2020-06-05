check_roadmap_result_CO2_sensitivity <- function() {
    
    
    inDF <- read.csv("output/MATE_output_roadmap.csv")
    subDF <- subset(inDF, Ca>=350&Ca<=650)
    
    lm1 <- lm(Asat~Ca, subDF)
    lm2 <- lm(Ac~Ca, subDF)
    lm3 <- lm(Aj~Ca, subDF)
    
    plotDF <- c()
    
    
    plotDF$A400 <- 400 * coef(lm1)[2] + coef(lm1)[1]
    plotDF$A600 <- 600 * coef(lm1)[2] + coef(lm1)[1]
    
    plotDF$Ac400 <- 400 * coef(lm2)[2] + coef(lm2)[1]
    plotDF$Ac600 <- 600 * coef(lm2)[2] + coef(lm2)[1]
    
    plotDF$Aj400 <- 400 * coef(lm3)[2] + coef(lm3)[1]
    plotDF$Aj600 <- 600 * coef(lm3)[2] + coef(lm3)[1]
    
    plotDF$A_sens <- with(plotDF, A600/A400)
    plotDF$Ac_sens <- with(plotDF, Ac600/Ac400)
    plotDF$Aj_sens <- with(plotDF, Aj600/Aj400)
    
    plotDF <- as.data.frame(plotDF)
    
    print(plotDF)
    
}


