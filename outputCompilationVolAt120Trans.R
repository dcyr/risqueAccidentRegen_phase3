###################################################################################################
###################################################################################################
##### Compiling relative density outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
#######
# rm(list = ls())
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-29")
# ####################################################################################################
# ###########################################
# scenario <- "baseline"
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)


studyArea <- raster("../studyArea.tif")
coverTypes <- raster("../coverTypes.tif")
coverTypes_RAT <- read.csv("../coverTypes_RAT.csv")
iqs <- raster("../iqs.tif")
rho100Init <- raster("../IDR100.tif")
#dens_RAT <- read.csv("../dens_RAT.csv")
uaf <- raster("../uaf.tif")
subZones <- raster("../subZones.tif")
subZones_RAT <- read.csv("../subZones_RAT.csv")
source("../scripts/Pothier-Savard.R")
# ###################################################################
# ## loading management plan (to fetch commercial cover types )
# managementPlan <- get(load("../managementPlan.RData"))
# plan <- managementPlan[[scenario]]
# ## eligible to harvest

## spp eligible (commercial species)
spEligibleVals <- which(coverTypes_RAT$value %in% c("EN", "PG"))#plan$comSppId[["SEPM"]]
spEligible <- coverTypes %in% spEligibleVals
spEligible[!spEligible] <- NA


## focusing on "EN" and "PG" covertypes
ctVal <- coverTypes
ctVal[!(coverTypes %in% spEligibleVals)] <- NA
ctVal <- values(ctVal)
##
szVal <- subZones
szVal[szVal == subZones_RAT[which(subZones_RAT$value == "Non-forest"),"ID"]] <- NA
szVal <- values(szVal)
## Site index (IQS)
iqsVal <- iqs
iqsVal[!(coverTypes %in% spEligibleVals)] <- NA
iqsVal <- values(iqsVal)
## age at 1 m
t1Val <- tFnc(sp = coverTypes_RAT[match(ctVal, coverTypes_RAT$ID), "value"],
              iqs = iqsVal,
              tCoef = tCoef)

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
clusterN <-  25#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#######
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {#  seq_along(x), .combine = "rbind") %dopar% {# 
    require(raster)
    require(reshape2)
    require(dplyr)
    require(doSNOW)
    require(parallel)
    require(foreach)
    ## simInfo
    s <- scenario
    # s <- scenario[i]
    r <- replicates[i]

    ## fetching outputs
    
    rho100 <- get(load(paste(outputFolder, x[i], sep="/")))
    fire <- get(load(paste(outputFolder, gsub("Rho100", "Fire",x[i]) , sep="/")))
    
    ## focusing on commercial species
    fire[is.na(spEligible)] <- NA
    
    
    ## compiling relative density at 100 y-old species and subzones
    out <- as.data.frame(values(rho100))
    out <- cbind(values(rho100Init), out)
    ## 
    outFire <- as.data.frame(values(fire))
   
    outTransition <- foreach(y = 1:ncol(outFire), .combine = "rbind") %do% {#) {
        #head(out)
        
        index <- which(!is.na(outFire[,y]))
        
        
        df <- data.frame(coverType = ctVal[index],
                          iqs = iqsVal[index],
                          subZone = szVal[index],
                          ageAt1m = t1Val[index],
                          rho100PreFire = out[index, y],
                          rho100PostFire = out[index, y+1])
        df <- df[complete.cases(df),]
        df <- df %>%
            mutate(volAt120PreFire = VFnc(sp = coverTypes_RAT[match(coverType, coverTypes_RAT$ID), "value"],
                                          iqs = iqs,
                                          Ac = 120 - ageAt1m,
                                          rho100 = rho100PreFire,
                                          HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef,
                                          rho100Coef = rho100Coef, merchantable = T,
                                          scenesCoef = NULL, withSenescence = F),
                   volAt120PostFire = VFnc(sp = coverTypes_RAT[match(coverType, coverTypes_RAT$ID), "value"],
                                          iqs = iqs,
                                          Ac = 120 - ageAt1m,
                                          rho100 = rho100PostFire,
                                          HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef,
                                          rho100Coef = rho100Coef, merchantable = T,
                                          scenesCoef = NULL, withSenescence = F))
        
        if(nrow(df) > 0) {
            volAt120Transition <- data.frame(scenario = s,
                                                  replicate = r,
                                                  year = y,
                                                  df[,c("coverType", "subZone", "volAt120PreFire", "volAt120PostFire")])
            
        } else {
            volAt120Transition <- data.frame(scenario = s,
                                             replicate = r,
                                             year = y,
                                             coverType = NA, subZone = NA, volAt120PreFire = NA, volAt120PostFire = NA)
        }
        
        return(volAt120Transition)
        print(paste(r, y))
        
    }
    #outTransition <- outTransition[complete.cases(outTransition),]
    print(r)
    return(outTransition)
}

stopCluster(cl)
save(outputCompiled, file = paste0("outputTransitionsVolAt120Cls_", scenario, ".RData"))

# 

#     
#     head(volAt120Transition)
#     vals <- colnames(rho100)    
#     out <- cbind(ctVal, iqsVal, szVal, t1Val, out)
#     out <- melt(out, measure.vars = vals)
#     out <- out[complete.cases(out),]
#     ## compute volume at 120 y-old
#     out <- out %>%
#         mutate(volAt120 = VFnc(sp = coverTypes_RAT[match(ctVal, coverTypes_RAT$ID), "value"],
#                                iqs = iqsVal,
#                                Ac = 120 - t1Val,
#                                rho100 = value,
#                                HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef,
#                                rho100Coef = rho100Coef, merchantable = T,
#                                scenesCoef = NULL, withSenescence = F)) %>%
#         mutate(volAt120 = ifelse(is.na(volAt120), 0, volAt120)) %>%
#         mutate(volAt120Cls =  cut(volAt120, include.lowest = T, right = F, breaks=c(0,50, 80, 999))) %>%
#         group_by(ctVal, szVal, variable, volAt120Cls) %>%
#         summarise(Area_ha = n()*convFactor)
#     
#     out <- data.frame(scenario = s, 
#                       replicate = r,
#                       coverTypes = coverTypes_RAT[match(out$ctVal, coverTypes_RAT$ID), "value"],
#                       subZone = subZones_RAT[match(out$szVal, subZones_RAT$ID), "value"],
#                       year = as.numeric(gsub("layer.", "", out$variable)),
#                       volAt120Cls = out$volAt120Cls,
#                       area_ha = out$Area_ha)
#     
#     print(paste(s, r))
#     return(out)
#     
# }
# 

# 
# # assigning cluster
# dt[,"cluster"] <- as.factor(letters[clusterAssignment])
# 
# 
# # extracting psp with more than one measurement
# pspRemeas <- psp[which(psp$count>1), "pspID"]
# dtRemeas <- dt %>%
#     filter(pspID %in% pspRemeas) %>%
#     arrange(pspID, meas_dt)
# 
# # head(dtRemeas, 100)[, c(1:4, ncol(dtRemeas))]
# 
# ## creating list of individual markovian sequences
# require(markovchain)
# clusterNames <- as.character(levels(dtRemeas$cluster))
# pspRemeas <- unique(dtRemeas$pspID)
# #seqStandState <- list()
# 
# seqMatrix <- seqMatrixBlank <- matrix(0, ncol = length(clusterNames), nrow = length(clusterNames),
#                                       dimnames = list(clusterNames, clusterNames))
# for (i in seq_along(pspRemeas)) {
#     #identifying sequence
#     index <- which(dtRemeas$pspID == pspRemeas[i])
#     x <- dtRemeas[index, "cluster"]
#     years <- dtRemeas[index, "meas_dt"]
#     #creating standardized sequence
#     unifSeq <- seq(0, 10*round((max(years)-min(years))/10), by=10)
#     state <- rep(NA, length(unifSeq))
#     names(state) <- unifSeq
#     #incorporating known states
#     state[as.character(10*round((years-min(years))/10))] <- as.character(x)
#     #filling in unknown states with previous known
#     naStates <- which(is.na(state))
#     for (j in naStates) {
#         state[j] <- state[j-1]
#     }
#     
#     # adding sequence to list
#     if(length(state)>1) {
#         state <- ?createSequenceMatrix(state)
#         tmpMat <- seqMatrixBlank
#         tmpMat[rownames(state), colnames(state)] <- state
#         seqMatrix <- seqMatrix + tmpMat
#         #seqStandState <- c(seqStandState, list(state))
#     }
#     print(i)
# }
# 
# transMat <- round(prop.table(seqMatrix, margin = 1), 5)
