##################################################################################################
###################################################################################################
##### Visualizing everything
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
setwd("D:/test/risqueAccidentRegen_phase3/")
# setwd("~/Sync/Travail/ECCC/regenFailureRiskAssessment_phase2/2018-11-29")
####################################################################################################
scenario <- c("baseline", "RCP85")
initYear <- 2015
####################################################################################################
sourceDir <- path.expand("~")
sourceDir <- gsub("/Documents", "", sourceDir) # necessary on my Windows machine
sourceDir <- paste(sourceDir, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3", sep ="/")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
## use eval() and parse() instead of source() to deal with special character under Windows.
# eval(parse(paste(sourceDir, "outputVizFire.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputVizHarvest.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputVizVolAt120.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputVizHarvest_pastille.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputVizAge.R", sep = "/"), encoding = 'UTF-8'))

# eval(parse(paste(sourceDir, "resultsTransitionMatrix.R", sep = "/"), encoding = 'UTF-8')) 
