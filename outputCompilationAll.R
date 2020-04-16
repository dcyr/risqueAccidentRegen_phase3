##################################################################################################
###################################################################################################
# ##### Compiling outputs
# ##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
####################################################################################################
####################################################################################################
sourceDir <- path.expand("~")
sourceDir <- gsub("\\\\", "/", sourceDir) # necessary on some Windows machine
sourceDir <- gsub("/Documents", "", sourceDir) # necessary on my Windows machine
sourceDir <- paste(sourceDir, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3", sep ="/")
setwd(sourceDir)
####################################################################################################
require(readxl)
#simInfo <- read_excel("./docs/scenTable.xlsx", sheet = 1)
simInfo <- read.csv("./docs/scenTable.csv",
                     colClasses=c("id"="character"))
                    
#simInfo <- simInfo[5,]
simInfo <- list(simID = simInfo$id,
                simDir =  paste0("sim_", simInfo$id),
                fire = simInfo$fireScenario,
                mgmt = simInfo$mgmtScenario,
                ctDyn = ifelse(rowSums(cbind(as.numeric(simInfo$plantPostFire),
                                            as.numeric(simInfo$plantPostSalv)), na.rm = T) >=1 &
                    (simInfo$plantPostSalvSp %in% c("same", NA) == F |
                    simInfo$plantPostFireSp %in% c("same", NA) == F) , T, F))
#setwd("~/Data/test/risqueAccidentRegen_phase3/")
setwd("D:/risqueAccidentRegen_phase3")
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

require(doSNOW)
require(parallel)
require(foreach)
# clusterN <- 2
clusterN <-  max(1, floor(0.75*detectCores()))  ### choose number of nodes to add to cluster.
clusterN <- 4 

## use eval() and parse() instead of source() to deal with special character under Windows.
# eval(parse(paste(sourceDir, "outputCompilationFire.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputCompilationHarvest.R", sep = "/"), encoding = 'UTF-8'))
# eval(parse(paste(sourceDir, "outputCompilationTSD.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste(sourceDir, "outputCompilationVolAt120.R", sep = "/"), encoding = 'UTF-8'))
#eval(parse(paste(sourceDir, "outputCompilationVolAt120Trans.R", sep = "/"), encoding = 'UTF-8'))
