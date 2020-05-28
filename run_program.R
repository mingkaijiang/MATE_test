#### This is the master script to run the MATE model,
#### based on codes developed by Belinda Medlyn. 
#### Contact: Mingkai Jiang (m.jiang@westernsydney.edu.au). 

#####################################################################################
################################### set up ##########################################
#### clear wk space
rm(list=ls(all=TRUE))

#### read in necessary stuffs
source("prepare.R")

################################### end set up ######################################
#####################################################################################

#######################################################################################
################################### run MATE ##########################################
### run basic code
out <- RMATE2()



### process WTC met data so that it is readable in MATE
process_WTC_met_data(inFile = "input/met_drawdownperiod_ch01.csv",
                     outFile = "input/MATE_met_ch01.csv")

process_WTC_met_data(inFile = "input/met_drawdownperiod_ch03.csv",
                     outFile = "input/MATE_met_ch03.csv")

process_WTC_met_data(inFile = "input/met_drawdownperiod_ch11.csv",
                     outFile = "input/MATE_met_ch11.csv")

process_WTC_met_data(inFile = "input/met_drawdownperiod_ch04.csv",
                     outFile = "input/MATE_met_ch04.csv")

process_WTC_met_data(inFile = "input/met_drawdownperiod_ch08.csv",
                     outFile = "input/MATE_met_ch08.csv")


### run code modified to accommodate WTC input data and parameters
out01 <- RMATE2_WTC(matefile = "input/MATE_met_ch01.csv", 
                    outputfile = "output/MATE_output_ch01.csv",
                    sourcefile = "parameter/parameters_ch01.R",
                    runfrom=1, 
                    nrows=NA)

out03 <- RMATE2_WTC(matefile = "input/MATE_met_ch03.csv", 
                    outputfile = "output/MATE_output_ch03.csv",
                    sourcefile = "parameter/parameters_ch03.R",
                    runfrom=1, 
                    nrows=NA)

out11 <- RMATE2_WTC(matefile = "input/MATE_met_ch11.csv", 
                    outputfile = "output/MATE_output_ch11.csv",
                    sourcefile = "parameter/parameters_ch11.R",
                    runfrom=1, 
                    nrows=NA)

out04 <- RMATE2_WTC(matefile = "input/MATE_met_ch04.csv", 
                    outputfile = "output/MATE_output_ch04.csv",
                    sourcefile = "parameter/parameters_ch04.R",
                    runfrom=1, 
                    nrows=NA)

out08 <- RMATE2_WTC(matefile = "input/MATE_met_ch08.csv", 
                    outputfile = "output/MATE_output_ch08.csv",
                    sourcefile = "parameter/parameters_ch08.R",
                    runfrom=1, 
                    nrows=NA)


################################### end run MATE ######################################
#######################################################################################
