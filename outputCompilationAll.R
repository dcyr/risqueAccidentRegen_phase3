##################################################################################################
###################################################################################################
##### Visualizing everything
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
setwd("D:/test/risqueAccidentRegen_phase3/test_150yrs_RCP85/")
# setwd("~/Sync/Travail/ECCC/regenFailureRiskAssessment_phase2/2018-11-29")
####################################################################################################
scenario <- "RCP85"
####################################################################################################
sourceDir <- path.expand("~")
sourceDir <- gsub("\\\\", "/", sourceDir) # necessary on some Windows machine
sourceDir <- gsub("/Documents", "", sourceDir) # necessary on my Windows machine
sourceDir <- paste(sourceDir, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3", sep ="/")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
## use eval() and parse() instead of source() to deal with special character under Windows.
# eval(parse(paste(sourceDir, "outputCompilationFire.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputCompilationHarvest.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputCompilationTSD.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputCompilationVolAt120.R", sep = "/"), encoding = 'UTF-8'))
#eval(parse(paste(sourceDir, "outputCompilationVolAt120Trans.R", sep = "/"), encoding = 'UTF-8'))
