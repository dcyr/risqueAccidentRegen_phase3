##################################################################################################
###################################################################################################
##### Visualizing outputs
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
home <- path.expand("~")
home <- gsub("\\\\", "/", home) # necessary on some Windows machine
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))


rawOutputDir <- "D:/risqueAccidentRegen_phase3/" ## necessary for some scripts (ex harvest, fire)
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
####################################################################################################
require(readxl)
#simInfo <- read_excel("./docs/scenTable.xlsx", sheet = 1)
simInfo <- read.csv("../docs/scenTable.csv",
                    colClasses=c("simID"="character",
                                 "id_ms"="character"))

simInfo <- list(simID = simInfo$simID,
                id_ms = simInfo$id_ms,
                simDir =  paste0("sim_", simInfo$simID),
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
                plantPostFireSp = simInfo$plantPostFireS,
                plantLimitedAccess = simInfo$plantLimitedAccess)

id_ms <- unique(simInfo[["id_ms"]])
id_ms <- id_ms[order(id_ms)] 
id_ms <- id_ms[!is.na(as.numeric(id_ms))]
simInfo[["id_ms"]] <- factor(simInfo[["id_ms"]] , levels = id_ms)

####################################################################################################

####################################################################################################
####################################################################################################
## use eval() and parse() instead of source() to deal with special character under Windows.
eval(parse(paste("../outputVizRegenRisk_ms.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste("../outputVizVolAt120_ms.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste("../outputVizHarvest_ms.R", sep = "/"), encoding = 'UTF-8'))
eval(parse(paste("../outputTable_ms.R", sep = "/"), encoding = 'UTF-8'))

