##################################################################################################
###################################################################################################
##### Visualizing outputs
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
# setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))
setwd("D:/test/risqueAccidentRegen_phase3")

####################################################################################################
scenario <- c("baseline_newPlantationRules", "baseline_newFireImpl")##c("baseline", "RCP85")# 
initYear <- 2015

simInfo <- strsplit(scenario, "_")
fr <- as.character(lapply(simInfo, function(x) x[[1]]))
mgmt <- as.character(lapply(simInfo, function(x) x[[2]]))
rm(simInfo)


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


####################################################################################################
####################################################################################################
## use eval() and parse() instead of source() to deal with special character under Windows.
eval(parse(paste(sourceDir, "outputVizFire.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputVizHarvest.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputVizVolAt120.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputVizAge.R", sep = "/"), encoding = 'UTF-8'))

# eval(parse(paste(sourceDir, "resultsTransitionMatrix.R", sep = "/"), encoding = 'UTF-8')) 
