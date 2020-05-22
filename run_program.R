#### This is the master script to run the MATE model,
#### based on codes developed by Belinda Medlyn. 
#### Contact: Mingkai Jiang (m.jiang@westernsydney.edu.au). 

#####################################################################################
################################### set up ##########################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read in necessary stuffs
source("prepare.R")

### read in parameters
source("parameter/parameters.R")

################################### end set up ######################################
#####################################################################################

#######################################################################################
################################### run MATE ##########################################
#### run
rundfr <- expand.grid(Ca=c(380,620), Wcapac=c(60,80,100,120,140,160,180,200))
rundfr$PAW0 <- rundfr$Wcapac
res1 <- RMATEsensitivity(rundfr)

# rundfr$NPP <- sapply(res1, function(x)mean(x$NPPtCha))
# rundfr$m <- sapply(res1, function(x)mean(x$m))
# rundfr$PAW <- sapply(res1, function(x)mean(x$PAWcur))

# # example
# with(rundfr, plot(PAW0, PAW, pch=19, col=nicecolors[as.factor(Ca)]))
# legend("bottomright",levels(as.factor(rundfr$Ca)),pch=19,col=nicecolors)

################################### end run MATE ######################################
#######################################################################################
