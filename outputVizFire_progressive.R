###################################################################################################
###################################################################################################
##### Visualizing fire simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-23")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
# scenario <- "baseline"
#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
initYear <- 2015
####################################################################
####################################################################
######
require(raster)
require(dplyr)
studyArea <- raster("../studyArea.tif")
fireZones <- raster("../fireZones.tif")

fireRegime <- read.csv("../fireRegime.csv") %>%
    filter(scenario == "RCP85")
### since it's the same everywhere...
fireRegimeSummary <- distinct(fireRegime, period, fireCycle)
# extracting breaks
breaks <- strsplit(as.character(fireRegimeSummary$period), split = "-")
breaks <- unlist(lapply(breaks, function(x) c(as.numeric(x[[1]])-1, as.numeric(x[[2]]))))
breaks <- unique(breaks)

## to focus on study area
fireZones[is.na(studyArea)] <- NA 

##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
fireZoneArea <- zonal(!is.na(fireZones), fireZones, sum)
fireZoneArea <- data.frame(zone = as.character(fireRegime[match(fireZoneArea[,1], fireRegime$ID),"Zone_LN"]),
                           areaZone_ha = fireZoneArea[,2] * convFactor)
totalArea <- sum(fireZoneArea$areaZone_ha)
fireZoneArea <- rbind(fireZoneArea, data.frame(zone = "total", areaZone_ha = totalArea))
fireZoneArea[, "prop"] <- fireZoneArea$areaZone_ha/fireZoneArea[which(fireZoneArea$zone == "total"),"areaZone_ha"]
names(fireZoneArea) <- gsub("zone", "Zone_LN", names(fireZoneArea))
# 

#############################################################
#############################################################

outputCompiled <- get(load(paste0("outputCompiledFire_", scenario, ".RData")))


#################################################################################
#################################################################################
####### figure for baseline scenario (temporally constant, spatially heterogenous)
#################################################################################
require(dplyr)
## summarizing fire regimes


# outputSummary <- outputCompiled %>%
#     #filter(scenario == scenario) %>%
#     
#     arrange(Zone_LN, replicate, year)


## create data frame with annual data
df <- outputCompiled %>%
    group_by(Zone_LN, replicate, year) %>%
    summarize(#fireCycle = round((1/mean(areaBurnedTotal_ha/areaZoneTotal_ha))),
              propAAB = mean(areaBurnedTotal_ha/areaZoneTotal_ha)) %>%
    filter(Zone_LN == "total") %>%
    ungroup() %>%
    mutate(scenario = scenario) %>%
    select(scenario, replicate, year, propAAB)
write.csv(df, file = paste0("fireSummary_", scenario, ".csv"), row.names = F)


period <- cut(outputCompiled$year+initYear, breaks)
period[is.na(period)] <- last(levels(period))
period <- as.character(period)
period <- gsub("[^0-9]", "", period)
period <- paste(as.numeric(substr(period, 1,4))+1,
                as.numeric(substr(period, 5,8)), sep = "-")

outputCompiled[,"period"] <- period

## create data frame with 
outputSummary <- outputCompiled %>%
    group_by(replicate, period) %>%
    summarize(fireCycle = round((1/mean(areaBurnedTotal_ha/areaZoneTotal_ha))),
              propAAB = mean(areaBurnedTotal_ha/areaZoneTotal_ha)) %>%
    arrange(replicate)

fcSummary <- outputSummary %>%
    group_by(period) %>%
    summarize(realizedFC_median = median(fireCycle),
              realizedFC_mean = 1/mean(propAAB)) %>%
    merge(fireRegimeSummary, all.x = T ) %>%
    merge(fireZoneArea)

