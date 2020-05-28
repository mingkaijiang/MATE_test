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
### run basic code
out <- RMATE2()



### process WTC met data so that it is readable in MATE
process_WTC_met_data(inFile = "input/met_drawdownperiod_ch01.csv",
                     outFile = "input/MATE_met_ch01.csv")

### run code modified to accommodate WTC input data and parameters
out <- RMATE2_WTC(matefile = "input/MATE_met_ch01.csv", 
                  outputfile = "output/MATE_output_ch01.csv",
                  runfrom=1, 
                  nrows=NA)




################################### end run MATE ######################################
#######################################################################################
