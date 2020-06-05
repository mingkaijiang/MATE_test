check_roadmap_result_CO2_sensitivity <- function() {
    
    ### read csv
    inDF1 <- read.csv("output/MATE_output_original.csv")
    inDF2 <- read.csv("output/MATE_output_roadmap_vcmax45.csv")
    inDF3 <- read.csv("output/MATE_output_roadmap_vcmax60.csv")
    
    ### labeling
    inDF1$lab <- "original"
    inDF2$lab <- "vcmax45"
    inDF3$lab <- "vcmax60"
    
    ### merge and subset
    myDF <- rbind(inDF1, inDF2, inDF3)
    subDF <- subset(myDF, Ca>=300&Ca<=700)
    
    lm1 <- lm(Asat~Ca, subDF[subDF$lab == "original",])
    lm2 <- lm(Ac~Ca, subDF[subDF$lab == "original",])
    lm3 <- lm(Aj~Ca, subDF[subDF$lab == "original",])
    
    lm4 <- lm(Asat~Ca, subDF[subDF$lab == "vcmax45",])
    lm5 <- lm(Ac~Ca, subDF[subDF$lab == "vcmax45",])
    lm6 <- lm(Aj~Ca, subDF[subDF$lab == "vcmax45",])
    
    lm7 <- lm(Asat~Ca, subDF[subDF$lab == "vcmax60",])
    lm8 <- lm(Ac~Ca, subDF[subDF$lab == "vcmax60",])
    lm9 <- lm(Aj~Ca, subDF[subDF$lab == "vcmax60",])
    
    plotDF <- data.frame(c("original", "vcmax45", "vcmax60"),
                         NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(plotDF) <- c("lab", "A400", "A600", "A_sens",
                          "Ac400", "Ac600", "Ac_sens",
                          "Aj400", "Aj600", "Aj_sens")
    
    ### original
    plotDF$A400[plotDF$lab=="original"] <- 400 * coef(lm1)[2] + coef(lm1)[1]
    plotDF$A600[plotDF$lab=="original"] <- 600 * coef(lm1)[2] + coef(lm1)[1]
    
    plotDF$Ac400[plotDF$lab=="original"] <- 400 * coef(lm2)[2] + coef(lm2)[1]
    plotDF$Ac600[plotDF$lab=="original"] <- 600 * coef(lm2)[2] + coef(lm2)[1]
    
    plotDF$Aj400[plotDF$lab=="original"] <- 400 * coef(lm3)[2] + coef(lm3)[1]
    plotDF$Aj600[plotDF$lab=="original"] <- 600 * coef(lm3)[2] + coef(lm3)[1]
    
    ### vcmax45
    plotDF$A400[plotDF$lab=="vcmax45"] <- 400 * coef(lm4)[2] + coef(lm4)[1]
    plotDF$A600[plotDF$lab=="vcmax45"] <- 600 * coef(lm4)[2] + coef(lm4)[1]
    
    plotDF$Ac400[plotDF$lab=="vcmax45"] <- 400 * coef(lm5)[2] + coef(lm5)[1]
    plotDF$Ac600[plotDF$lab=="vcmax45"] <- 600 * coef(lm5)[2] + coef(lm5)[1]
    
    plotDF$Aj400[plotDF$lab=="vcmax45"] <- 400 * coef(lm6)[2] + coef(lm6)[1]
    plotDF$Aj600[plotDF$lab=="vcmax45"] <- 600 * coef(lm6)[2] + coef(lm6)[1]
    
    ### Vcmax60
    plotDF$A400[plotDF$lab=="vcmax60"] <- 400 * coef(lm7)[2] + coef(lm7)[1]
    plotDF$A600[plotDF$lab=="vcmax60"] <- 600 * coef(lm7)[2] + coef(lm7)[1]
    
    plotDF$Ac400[plotDF$lab=="vcmax60"] <- 400 * coef(lm8)[2] + coef(lm8)[1]
    plotDF$Ac600[plotDF$lab=="vcmax60"] <- 600 * coef(lm8)[2] + coef(lm8)[1]
    
    plotDF$Aj400[plotDF$lab=="vcmax60"] <- 400 * coef(lm9)[2] + coef(lm9)[1]
    plotDF$Aj600[plotDF$lab=="vcmax60"] <- 600 * coef(lm9)[2] + coef(lm9)[1]
    
    
    ### sensitivity
    plotDF$A_sens <- with(plotDF, A600/A400)
    plotDF$Ac_sens <- with(plotDF, Ac600/Ac400)
    plotDF$Aj_sens <- with(plotDF, Aj600/Aj400)
    
    ### subset
    subDF1 <- plotDF[,c("lab", "A_sens")]
    subDF2 <- plotDF[,c("lab", "Ac_sens")]
    subDF3 <- plotDF[,c("lab", "Aj_sens")]
    
    subDF1$lab2 <- "A"
    subDF2$lab2 <- "Ac"
    subDF3$lab2 <- "Aj"
    
    colnames(subDF1) <- colnames(subDF2) <- colnames(subDF3) <- c("lab1", "ratio", "lab2")
    
    plotDF <- rbind(subDF1, subDF2, subDF3)
    
    p1 <- ggplot(data=plotDF, 
                 aes(lab1, ratio, group=lab2)) +
        geom_bar(stat = "identity", aes(fill=lab2), 
                 position="dodge") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'vertical',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold", 
                                        hjust = 0.5))+
        ylab(expression(paste(A[600] * " / " * A[400])))+
        scale_fill_manual(name="",
                          breaks=c("A", "Ac", "Aj"),
                          labels=c("A", expression(paste(A[c])),
                                   expression(paste(A[j]))),
                          values = colorblind_pal()(3 + 1)[-1])+
        xlab("")+
        scale_x_discrete(breaks=c("original", "vcmax45", "vcmax60"),
                         labels=c(expression(paste(V[cmax91])),
                                  expression(paste(V[cmax45])),
                                  expression(paste(V[cmax60]))))+
        coord_cartesian(ylim = c(1, 1.5))
    
    plot(p1)
    
    pdf("output/relative_contribution_Ac_Aj_sensitivity_test.pdf", width=4, height=4)
    plot(p1)
    dev.off()  
    
}    



