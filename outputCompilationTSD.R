###################################################################################################
###################################################################################################
##### Compiling raw harvest outputs to a tidy data frame
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "clusterN"))])
# #######
# rm(list = ls())
# setwd("D:/test/risqueAccidentRegen_phase3/100rep_baseline/")
# ####################################################################################################
# scenario <- "baseline"
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)


#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
for(s in 1:length(simInfo$simID)) {
    
    simDir <- simInfo$simDir[s]
    fr <- simInfo$fire[s]
    mgmt <- simInfo$mgmt[s]
    simID <- simInfo$simID[s]
    
    ####################################################################
    studyArea <- raster(paste0("../", simDir, "/studyArea.tif"))
    uaf <- raster(paste0("../", simDir, "/uaf.tif"))
    uaf_RAT <- read.csv(paste0("../", simDir, "/uaf_RAT.csv"))
    subZones <- raster(paste0("../", simDir, "/subZones.tif"))
    subZones_RAT <- read.csv(paste0("../", simDir, "/subZones_RAT.csv"))
    coverTypes <- raster(paste0("../", simDir, "/coverTypes.tif"))
    ##
    convFactor <- prod(res(studyArea))/10000### to convert to hectares
    
    
    ###################################################################
    ## loading management plan (to fetch age structure targets, and productive cover types )
    managementPlan <- get(load(paste0("../", simDir, "/managementPlan.RData")))
    plan <- managementPlan$baseline
    ## eligible to harvest
    harvEligible <- uaf %in% plan$uaf &
        subZones %in% plan$subZone
    harvEligible[!harvEligible] <- NA
    
    ## spp eligible (commercial species)
    spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
    spEligible[!spEligible] <- NA
    
    ##
    # regenMaxProp <- plan$regenMaxProp
    regenMaxAge <- plan$regenMaxAge
    # oldMinProp <- plan$oldMinProp
    oldMinAge <- plan$oldMinAge
    
    # ## targets
    eligibleArea <- t(zonal(spEligible, uaf,  "sum")[,-1]) * convFactor
    eligibleArea <- data.frame(uaf = as.character(uaf_RAT[uaf_RAT$ID, "value"]), managedAreaTotal_ha = as.numeric(eligibleArea))
    
    ####################################################################
    ####################################################################
    ######
    ######      compiling simulation outputs
    ######
    outputFolder <- paste0("../", simDir, "/output")
    x <- list.files(outputFolder)
    index <- grep(".RData", x)
    index <- intersect(index, grep("TSD", x))
    x <- x[index]
    replicates <- gsub(".RData", "", x)
    replicates <- strsplit(replicates, "_")
    #scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
    replicates <- as.numeric(lapply(replicates, function(x) x[2]))
    ###########################################
    ###########################################
    
    require(doSNOW)
    require(parallel)
    require(foreach)
    # clusterN <- 2
    # clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
    #######
    cl = makeCluster(clusterN, outfile = "") ##
    registerDoSNOW(cl)
    #######
    outputCompiled <- foreach(i = seq_along(x),
                              .combine = "rbind") %dopar% {# seq_along(x)
                                  require(raster)
                                  require(reshape2)
                                  require(dplyr)
                                  
                                  # s <- scenario[i]
                                  r <- replicates[i]
                                  
                                  
                                  ## fetching outputs
                                  age <- get(load(paste(outputFolder, x[i], sep="/")))
                                  ## focusing of forest eligible to harvest
                                  age[is.na(spEligible)] <- NA
                                  
                                  ## compiling age structure
                                  oldArea <- data.frame(zonal(age>=oldMinAge, uaf,  "sum")[,-1] * convFactor)
                                  regenArea <-  data.frame(zonal(age<regenMaxAge, uaf,  "sum")[,-1] * convFactor)
                                  
                                  
                                  year <- as.numeric(gsub("[^0-9]", "", rownames(oldArea)))
                                  colnames(oldArea)[uaf_RAT$ID] <-
                                      colnames(regenArea)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
                                  
                                  
                                  ## total
                                  # oldArea[, "total"] <- apply(oldArea, 1, sum)
                                  # regenArea[, "total"] <- apply(regenArea, 1, sum)
                                  
                                  ## tidying up data frame
                                  oldArea <- data.frame(year,
                                                        oldArea)
                                  regenArea <- data.frame(year,
                                                          regenArea)
                                  
                                  
                                  oldArea <- melt(oldArea,
                                                  id.vars = c("year"),
                                                  variable.name = "uaf",
                                                  value.name = "oldArea_ha")
                                  
                                  regenArea <- melt(regenArea,
                                                    id.vars = c("year"),
                                                    variable.name = "uaf",
                                                    value.name = "regenArea_ha")
                                  
                                  oldArea$uaf <- gsub("X", "", oldArea$uaf)
                                  oldArea$uaf <- gsub("\\.", "-", oldArea$uaf)
                                  
                                  regenArea$uaf <- gsub("X", "", regenArea$uaf)
                                  regenArea$uaf <- gsub("\\.", "-", regenArea$uaf)
                                  
                                  
                                  
                                  out <- merge(regenArea, oldArea)
                                  out <- merge(out, eligibleArea) %>%
                                      mutate(simID = simID,
                                             replicate = r,
                                             fireScenario = fr,
                                             mgmtScenario = mgmt)
                                  
                                  print(paste(simID, "age", r))
                                  return(out)
                                  
                              }
    
    stopCluster(cl)
    outputCompiled <- arrange(outputCompiled, simID, uaf, replicate, year)
    
    save(outputCompiled, file = paste0("outputCompiledAge_", simID, ".RData"))
}
