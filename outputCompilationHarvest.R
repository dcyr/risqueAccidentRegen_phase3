###################################################################################################
###################################################################################################
##### Compiling raw harvest outputs to a tidy data frame
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
#######
rm(list = ls())
setwd("D:/test/risqueAccidentRegen_phase3/100rep_baseline/")
####################################################################################################
scenario <- "baseline"
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
uaf <- raster("../uaf.tif")
#subZones <- raster
uaf_RAT <- read.csv("../uaf_RAT.csv")
##
convFactor <- prod(res(studyArea))/10000### to convert to hectares

####################################################################
####################################################################
######
######      compiling simulation outputs
######
outputFolder <- "../output"
x <- list.files(outputFolder)
index <- grep(".RData", x)
index <- intersect(index, grep("Harvest", x))
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
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# seq_along(x)
    require(raster)
    require(reshape2)
    require(dplyr)
    
    ## simInfo
    s <- scenario
    r <- replicates[i]
    
    ## fetching outputs
    harv <- get(load(paste(outputFolder, x[i], sep="/")))
    salv <- get(load(paste(outputFolder, gsub("Harvest", "Salvage", x[i]), sep="/")))
    
    ## compiling realized area burned
    
    areaHarvested <- t(zonal(harv, uaf,  "sum")[,-1]) * convFactor
    areaHarvested <- data.frame(areaHarvested, total = apply(areaHarvested, 1, "sum"))
    areaSalvaged <- t(zonal(salv, uaf,  "sum")[,-1]) * convFactor
    areaSalvaged <- data.frame(areaSalvaged, total = apply(areaSalvaged, 1, "sum"))
    
    head(areaHarvested)
    head(areaSalvaged)
    yearH <- as.numeric(gsub("[^0-9]", "", rownames(areaHarvested)))
    yearS <- as.numeric(gsub("[^0-9]", "", rownames(areaSalvaged)))
    colnames(areaHarvested)[uaf_RAT$ID] <- colnames(areaSalvaged)[uaf_RAT$ID] <- as.character(uaf_RAT[uaf_RAT$ID, "value"])
    

    ## tidying up data frame
    areaHarvested <- data.frame(year = yearH, replicate = r, harvestType = "regular", areaHarvested)
    areaSalvaged <- data.frame(year = yearS, replicate = r, harvestType = "salvage", areaSalvaged)
    areaHarvested <- rbind(areaHarvested, areaSalvaged)
    
    
    # ######################## (what's this??)
    # ## keeping only zones intercepting study areazones with non-zero area harvested
    # z <- apply(areaHarvested[], 2, sum)
    # areaHarvested <- areaHarvested[z>0]


    out <- melt(areaHarvested,
                id.vars = c("year", "replicate", "harvestType"),
                variable.name = c("uaf"),
                value.name = "areaHarvestedTotal_ha")
    
    out$uaf <- gsub("X", "", out$uaf)
    out$uaf <- gsub("\\.", "-", out$uaf)
    out <- data.frame(scenario = s, out)
    
    print(paste("harvest", s, r))
    return(out)
    
}

stopCluster(cl)
outputCompiled <- arrange(outputCompiled, scenario, uaf, year)

save(outputCompiled, file = paste0("outputCompiledHarvest_", scenario, ".RData"))
