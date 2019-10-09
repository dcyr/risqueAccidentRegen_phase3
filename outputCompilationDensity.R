###################################################################################################
###################################################################################################
##### Compiling relative density outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
setwd("D:/regenFailureRiskAssessmentData_phase2/2018-08-24")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
####################################################################

studyArea <- raster("../studyArea.tif")
coverTypes <- raster("../coverTypes.tif")
coverTypes_RAT <- read.csv("../coverTypes_RAT.csv")
rho100Init <- raster("../IDR100.tif")
#dens_RAT <- read.csv("../dens_RAT.csv")
uaf <- raster("../uaf.tif")
subZones <- raster("../subZones.tif")
subZones_RAT <- read.csv("../subZones_RAT.csv")
###################################################################
## loading management plan (to fetch commercial cover types )
managementPlan <- get(load("../managementPlan.RData"))
plan <- managementPlan$baseline
## eligible to harvest

## spp eligible (commercial species)
spEligibleVals <- plan$comSppId[["SEPM"]]
spEligible <- coverTypes %in% spEligibleVals
spEligible[!spEligible] <- NA


## focusing on "EN" and "PG" covertypes
ctVal <- coverTypes
ctVal[!(coverTypes %in% which(coverTypes_RAT$ID %in% spEligibleVals))] <- NA
ctVal <- values(ctVal)
##
szVal <- subZones
szVal[szVal == subZones_RAT[which(subZones_RAT$value == "Non-forest"),"ID"]] <- NA
szVal <- values(szVal)

##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
###################################################################
## loading management plan (to fetch age structure targets, and productive cover types )

# managementPlan <- get(load("../managementPlan.RData"))
# plan <- managementPlan$baseline
# ## eligible to harvest
# harvEligible <- uaf %in% plan$uaf &
#     subZones %in% plan$subZone
# harvEligible[!harvEligible] <- NA
# 
# ## spp eligible (commercial species)
# spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
# spEligible[!spEligible] <- NA
# 
# ##
# # regenMaxProp <- plan$regenMaxProp
# regenMaxAge <- plan$regenMaxAge
# # oldMinProp <- plan$oldMinProp
# oldMinAge <- plan$oldMinAge
# 
# # ## targets
# eligibleArea <- t(zonal(spEligible, uaf,  "sum")[,-1]) * convFactor
# eligibleArea <- data.frame(uaf = as.character(uaf_RAT[uaf_RAT$ID, "value"]), managedAreaTotal_ha = as.numeric(eligibleArea))
# eligibleArea <- rbind(eligibleArea,
#                       data.frame(uaf = "total",
#                                  managedAreaTotal_ha = sum(eligibleArea$managedAreaTotal_ha)))

####################################################################
####################################################################
outputFolder <- "../output"
x <- list.files(outputFolder)
index <- grep(".RData", x)
index <- intersect(index, grep("Rho100", x))
x <- x[index]
simInfo <- gsub(".RData", "", x)
simInfo <- strsplit(simInfo, "_")
#scenario <- as.character(lapply(simInfo, function(x) x[[2]]))
replicates <- as.numeric(lapply(simInfo, function(x) x[2]))
###########################################
###########################################

require(doSNOW)
require(parallel)
require(foreach)
# clusterN <- 2
clusterN <-  15#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#######
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# 
    require(raster)
    require(reshape2)
    require(dplyr)
    
    ## simInfo
    s <- "baseline"
    # s <- scenario[i]
    r <- replicates[i]
    
    
    ## fetching outputs
    rho100 <- get(load(paste(outputFolder, x[i], sep="/")))
    ## focusing on 
    rho100[is.na(spEligible)] <- NA
    ## compiling relative density structure by species and subzones
    out <- as.data.frame(values(rho100))
    vals <- colnames(out)    
    out <- cbind(ctVal, szVal, out)
    out <- melt(out, measure.vars = vals)
    out <- out[complete.cases(out),]
    out <- out %>%
        group_by(ctVal, szVal, variable, value) %>%
        summarise(densClassArea_ha = n()*convFactor)
    
    out <- data.frame(scenario = s, 
                      replicate = r,
                      coverTypes = coverTypes_RAT[match(out$ctVal, coverTypes_RAT$ID), "value"],
                      subZone = subZones_RAT[match(out$szVal, subZones_RAT$ID), "value"],
                      year = gsub("CL_DENS.", "", out$variable),
                      # dens = dens_RAT[match(out$value, dens_RAT$ID), "value"],
                      area_ha = out$densClassArea_ha)
    
    print(paste(s, r))
    return(out)
    
}

stopCluster(cl)
#outputCompiled <- arrange(outputCompiled, scenario, uaf, year, replicate)

save(outputCompiled, file = "outputCompiledDensity.RData")
