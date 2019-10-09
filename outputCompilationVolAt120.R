###################################################################################################
###################################################################################################
##### Compiling relative density outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
volAt120OutputDir <- "outputVolAt120"
dir.create(volAt120OutputDir)
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
####################################################################
#### Sourcing Pothier-Savard equations
psDir <- "../data/Pothier-Savard"
source(paste(psDir, "Pothier-Savard.R", sep = "/"))

studyArea <- raster("../studyArea.tif")
coverTypes <- raster("../coverTypes.tif")
coverTypes_RAT <- read.csv("../coverTypes_RAT.csv")
iqs <- raster("../iqs.tif")
rho100Init <- raster("../IDR100.tif")
#dens_RAT <- read.csv("../dens_RAT.csv")
uaf <- raster("../uaf.tif")
subZones <- raster("../subZones.tif")
subZones_RAT <- read.csv("../subZones_RAT.csv")
###################################################################
## loading management plan (to fetch commercial cover types )
managementPlan <- get(load("../managementPlan.RData"))
plan <- managementPlan[[scenario]]
## eligible to harvest

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
clusterN <-  12#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#######
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {
    require(raster)
    require(reshape2)
    require(dplyr)
    
    ## simInfo
    s <- scenario
    # s <- scenario[i]
    r <- replicates[i]
    
    ## computing initial volume at 120 y. old, only once
    if (i == 1) {
        ## fetching outputs
        rho100 <- raster("../IDR100.tif")
        rho100[rho100>1] <- 1
        
        ## focusing on commercial species
        rho100[is.na(spEligible)] <- NA
        index <- which(complete.cases(values(rho100)))
        volAt120 <- rho100
        volAt120[] <- NA
        ## compiling relative density at 100 y-old species and subzones
        out <- as.data.frame(values(rho100))
        rm(rho100)
        vals <- colnames(out)    
        out <- cbind(ctVal, iqsVal, szVal, t1Val, out)
        out <- melt(out, measure.vars = vals)
        
        index <- which(complete.cases(out))
        out <- out[index,]
        ## compute volume at 120 y-old
        out <- out %>%
            mutate(volAt120 = VFnc(sp = coverTypes_RAT[match(ctVal, coverTypes_RAT$ID), "value"],
                                   iqs = iqsVal,
                                   Ac = 120 - t1Val,
                                   rho100 = value,
                                   HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef,
                                   rho100Coef = rho100Coef, merchantable = T,
                                   scenesCoef = NULL, withSenescence = F)) %>%
            mutate(volAt120 = ifelse(is.na(volAt120), 0, volAt120)) %>%
            mutate(volAt120Cls =  cut(volAt120, include.lowest = T, right = F, breaks=c(0,50, 80, 999)))
        
        volAt120[index] <- out$volAt120
        writeRaster(volAt120, file = "volAt120_init.tif")
        
    
    }
    
    
    ## fetching outputs
    rho100 <- get(load(paste(outputFolder, x[i], sep="/")))
    ## focusing on commercial species
    rho100[is.na(spEligible)] <- NA
    index <- which(complete.cases(values(rho100)))
    volAt120 <- rho100
    volAt120[] <- NA
    ## compiling relative density at 100 y-old species and subzones
    out <- as.data.frame(values(rho100))
    rm(rho100)
    vals <- colnames(out)    
    out <- cbind(ctVal, iqsVal, szVal, t1Val, out)
    out <- melt(out, measure.vars = vals)
    vAt120 <- out[,c("variable", "value")]
    vAt120[,"value"] <- NA
    index <- which(complete.cases(out))
    out <- out[index,]
    ## compute volume at 120 y-old
    out <- out %>%
        mutate(volAt120 = VFnc(sp = coverTypes_RAT[match(ctVal, coverTypes_RAT$ID), "value"],
                               iqs = iqsVal,
                               Ac = 120 - t1Val,
                               rho100 = value,
                               HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef,
                               rho100Coef = rho100Coef, merchantable = T,
                               scenesCoef = NULL, withSenescence = F)) %>%
        mutate(volAt120 = ifelse(is.na(volAt120), 0, volAt120)) %>%
        mutate(volAt120Cls =  cut(volAt120, include.lowest = T, right = F, breaks=c(0,50, 80, 999)))
    
    
    #rVal <- unstack(out[,c("variable", "volAt120")], form = volAt120 ~ variable)
    #rVal <- do.call("cbind", rVal)
    ## storing values in raster stack
    vAt120[index,"value"] <- out$volAt120
    vAt120 <- unstack(vAt120[,c("variable", "value")], form = value ~ variable)
    vAt120 <- round(as.matrix(vAt120),1)
    values(volAt120) <- vAt120
    # save raster 'volAt120'
    save(volAt120, file = paste0(volAt120OutputDir, "/outputVolAt120_", simInfo[[i]][2], ".RData"))

    
    out <- out %>%
        group_by(ctVal, szVal, variable, volAt120Cls) %>%
        summarise(Area_ha = n()*convFactor)
    
    out <- data.frame(scenario = s, 
                      replicate = r,
                      coverTypes = coverTypes_RAT[match(out$ctVal, coverTypes_RAT$ID), "value"],
                      subZone = subZones_RAT[match(out$szVal, subZones_RAT$ID), "value"],
                      year = as.numeric(gsub("layer.", "", out$variable)),
                      volAt120Cls = out$volAt120Cls,
                      area_ha = out$Area_ha)
    
    print(paste(s, r))
    return(out)
    
}

stopCluster(cl)
#outputCompiled <- arrange(outputCompiled, scenario, uaf, year, replicate)

save(outputCompiled, file = paste0("outputCompiledVolAt120Cls_", scenario, ".RData"))


# #### testing outputs
# require(ggplot2)
# 
# df <- out 
# 
# png(filename= paste0("prodClsTest.png"),
#     width = 8, height = 4, units = "in", res = 600, pointsize=10)
# 
# options(scipen=999)
# 
# 
# ggplot(data = df, aes(x = 2015 + year, y = area_ha, colour = volAt120Cls)) +
#     geom_line() +
#     facet_grid(coverTypes ~ subZone) +
#     labs(x = "")
# 
# dev.off()