##################################################################################################
###################################################################################################
##### Visualizing outputs
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
# setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))
####################################################################################################
####################################################################################################
sourceDir <- path.expand("~")
sourceDir <- gsub("\\\\", "/", sourceDir) # necessary on some Windows machine
sourceDir <- gsub("/Documents", "", sourceDir) # necessary on my Windows machine
sourceDir <- paste(sourceDir, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3", sep ="/")
setwd(sourceDir)
####################################################################################################
require(readxl)
simInfo <- read_excel("./docs/scenTable.xlsx", sheet = 1)

simInfo <- list(simID = simInfo$id,
                simDir =  paste0("sim_", simInfo$id),
                fire = simInfo$fireScenario,
                mgmt = simInfo$mgmtScenario,
                ctDyn = ifelse(simInfo$plantPostFireSp == "PG" |
                                   simInfo$plantPostSalvSp == "PG" , T, F),
                initYear = 2015,
                clearcutting =  as.logical(simInfo$clearcut),
                varReten =  as.logical(simInfo$varReten),
                salv =  as.logical(simInfo$salv),
                plantPostFire = as.logical(simInfo$plantPostFire),
                plantPostSalv =  as.logical(simInfo$plantPostSalv),
                plantPostFireSp = simInfo$plantPostFireSp)

setwd("D:/test/risqueAccidentRegen_phase3")
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
