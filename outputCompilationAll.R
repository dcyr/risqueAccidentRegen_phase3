##################################################################################################
###################################################################################################
# ##### Compiling outputs
# ##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
# rm(list = ls())
# ################################################################################
# home <- path.expand("~")
# home <- gsub("\\\\", "/", home) # necessary on some Windows machine
# home <- gsub("/Documents", "", home) # necessary on my Windows machine
# # setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))
setwd("D:/test/risqueAccidentRegen_phase3")

####################################################################################################
scenario <- c("baseline_newFireImpl", "baseline_newPlantationRules")#"baseline"
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

simInfo <- strsplit(scenario, "_")
fr <- as.character(lapply(simInfo, function(x) x[[1]]))
mgmt <- as.character(lapply(simInfo, function(x) x[[2]]))
rm(simInfo)


require(doSNOW)
require(parallel)
require(foreach)
# clusterN <- 2
clusterN <-  max(1, floor(0.75*detectCores()))  ### choose number of nodes to add to cluster.

## use eval() and parse() instead of source() to deal with special character under Windows.
eval(parse(paste(sourceDir, "outputCompilationFire.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputCompilationHarvest.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputCompilationTSD.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputCompilationVolAt120.R", sep = "/"), encoding = 'UTF-8'))
#eval(parse(paste(sourceDir, "outputCompilationVolAt120Trans.R", sep = "/"), encoding = 'UTF-8'))
