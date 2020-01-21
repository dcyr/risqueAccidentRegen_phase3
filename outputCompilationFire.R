###################################################################################################
###################################################################################################
##### Compiling raw fire outputs to a tidy data frame
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall

###################################################################################################
###################################################################################################
##### Visualizing fire simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
#rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
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
fireZones <- raster("../fireZones.tif")
fireZones_RAT <- read.csv("../fireZones_RAT.csv")

## focusing on fireZones in studyArea
z <- which(zonal(studyArea, fireZones, sum)[,"value"]>1)
fireZones_RAT <- fireZones_RAT[z, ]
fireZones[is.na(studyArea)] <- NA


##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
## in all simulation area
fireZoneArea <- as.data.frame(zonal(!is.na(fireZones), fireZones, sum))
fireZoneArea$value <- fireZoneArea$value*convFactor
colnames(fireZoneArea) <- c("ID", "areaZoneTotal_ha")
fireZoneArea <- merge(fireZoneArea, fireZones_RAT)
# ## within study area
# fireZoneAreaStudyArea <- as.data.frame(zonal(studyArea, fireZones, sum))
# fireZoneAreaStudyArea$value <- fireZoneAreaStudyArea$value*convFactor
# colnames(fireZoneAreaStudyArea) <- c("ID", "areaZoneStudyArea_ha")
# ##
# fireZoneArea <- merge(fireZoneArea,fireZoneAreaStudyArea)

# fireZoneArea[,"Fire_Cycle_studyArea"] <- fireZoneArea$Fire_Cycle
# fireZoneArea[fireZoneArea$areaZoneStudyArea_ha== 0,"Fire_Cycle_studyArea"] <- NA

### computing weight averaged fire cycles
x <- fireZoneArea %>%
    mutate(propTotalArea = areaZoneTotal_ha/sum(areaZoneTotal_ha),
           #propStudyArea = areaZoneStudyArea_ha/sum(areaZoneStudyArea_ha),
           pAABTotal = (1/Fire_Cycle) * propTotalArea)
#pAABStudyArea = (1/Fire_Cycle) * propStudyArea)

fireZoneArea <- rbind(fireZoneArea,
                      data.frame(ID = NA,
                                 areaZoneTotal_ha = sum(fireZoneArea$areaZoneTotal_ha),
                                 #areaZoneStudyArea_ha = sum(fireZoneArea$areaZoneStudyArea_ha),
                                 Zone_LN = "total",
                                 Fire_Cycle = round(1/sum(x$pAABTotal))#,
                                 #Fire_Cycle_studyArea = round(1/sum(x$pAABStudyArea))
                      )
)




####################################################################
####################################################################
######
######      compiling simulation outputs
######
outputFolder <- "../output"
x <- list.files(outputFolder)

# ### renaming files to put in a clean order if necessary
# require(stringr)
# simId <- gsub(".RData", "", x)
# simId <- strsplit(simId, "_")
# simId <- unique(as.character(lapply(simId, function(x) x[2])))
# simId <- simId[order(as.numeric(simId))]
# for (i in 1:1000) {
#     strPattern <- paste0("_",simId[i], ".RData")
#     oldFiles <- x[grep(strPattern, x)]
#     newFiles <- gsub(strPattern, paste0("_", str_pad(i-1, 3, pad = "0"), ".RData"), oldFiles)
#     file.rename(paste(outputFolder, oldFiles, sep = "/"),
#                 paste(outputFolder, newFiles, sep = "/"))
# }
# ## manually dispatch the remaining

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
clusterN <-  6#max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
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
    
    areaBurned <- t(zonal(fire, fireZones,  "sum")[,-1]) * convFactor
    colnames(areaBurned) <- fireZones_RAT$Zone_LN
    areaBurned <- data.frame(areaBurned, total = apply(areaBurned, 1, "sum"))
    year <- as.numeric(gsub("[^0-9]", "", rownames(areaBurned)))
    
    
    ## tidying up data frame
    areaBurned <- data.frame(year, replicate = r, areaBurned)
    
    out <- melt(areaBurned,
                id.vars = c("year", "replicate"),
                variable.name = "Zone_LN",
                value.name = "areaBurnedTotal_ha")
    
    out <- data.frame(scenario = s, out)
    
    print(paste("fire", s, r))
    return(out)
    
}

stopCluster(cl)

outputCompiled <- merge(outputCompiled, fireZoneArea)
outputCompiled <- arrange(outputCompiled, scenario, replicate, year, Zone_LN)

save(outputCompiled, file = paste0("outputCompiledFire_", scenario, ".RData"))
     
