###################################################################################################
###################################################################################################
##### Compiling raw fire outputs to a tidy data frame
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario", "clusterN"))])
# #######
# rm(list = ls())
# setwd("D:/test/risqueAccidentRegen_phase3/100rep_baseline/")
# ####################################################################################################
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

studyArea <- raster(paste0("../", scenario, "/studyArea.tif"))
fireZones <- raster(paste0("../", scenario, "/fireZones.tif"))

## focusing on fireZones in studyArea
z <- which(zonal(studyArea, fireZones, sum)[,"value"]>1)
#fireZones_RAT <- fireZones_RAT[z, ]
fireZones[is.na(studyArea)] <- NA
fireZones[!is.na(fireZones)] <- 1


##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
## in all simulation area
fireZoneArea <- as.data.frame(zonal(!is.na(fireZones), fireZones, sum))
fireZoneArea$value <- fireZoneArea$value*convFactor
colnames(fireZoneArea) <- c("ID", "areaZoneTotal_ha")


####################################################################
####################################################################
######
######      compiling simulation outputs
######
outputFolder <- paste0("../", scenario, "/output")
x <- list.files(outputFolder)

index <- grep(".RData", x)
index <- intersect(index, grep("Fire", x))
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
# clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#######
outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {# 
    require(raster)
    require(reshape2)
    require(dplyr)
    
    ## simInfo
    s <- scenario
    # s <- scenario[i]
    r <- replicates[i]
    
    ## fetching outputs
    fire <- get(load(paste(outputFolder, x[i], sep="/")))
    ## focusing on studyArea
    fire[is.na(studyArea)] <- NA
    
    ## compiling realized area burned
    
    areaBurned <- zonal(fire, fireZones,  "sum")[,-1] * convFactor
    year <- as.numeric(gsub("[^0-9]", "", names(areaBurned)))
    
    
    ## tidying up data frame
    areaBurned <- data.frame(year, replicate = r,
                             areaBurned_ha = areaBurned)
    
    # out <- melt(areaBurned,
    #             id.vars = c("year", "replicate"),
    #             value.name = "areaBurnedTotal_ha")
    
    out <- data.frame(scenario = s, areaBurned)
    
    print(paste("fire", s, r))
    return(out)
    
}

stopCluster(cl)

outputCompiled <- merge(outputCompiled, fireZoneArea)
outputCompiled <- arrange(outputCompiled, scenario, replicate, year)

save(outputCompiled, file = paste0("outputCompiledFire_", scenario, ".RData"))
     
