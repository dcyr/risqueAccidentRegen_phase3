###################################################################################################
###################################################################################################
##### Visualizing productivity classes, mean potential volumes at 120 years old,
##### harvested volumes and economic returns
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo"))])
################
require(tidyr)

###### fetching data frames from .csv
outputCls <- read.csv("outputCompiledVolAt120Cls.csv")
outputMean <- read.csv("outputCompiledVolAt120Mean.csv")
harvSummary <- read.csv("harvSummary.csv")





###### computing areas
areaTotal <- outputCls %>%
    group_by(simID, fireScenario, mgmtScenario, replicate, year, 
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
    summarise(areaTotal = sum(area_ha))
areaTotal <- unique(areaTotal$areaTotal)



################################################################################
################################################################################
### ms table 2
outputClsinit <- outputCls %>%
    filter(year == 0) %>%
    mutate(volAt120Cls = ifelse(is.na(volAt120Cls), "[0,30)", volAt120Cls)) %>%
    group_by(simID, id_ms, replicate, year, volAt120Cls) %>%
    summarize(area_ha = sum(area_ha)) %>%
    group_by(simID, id_ms, volAt120Cls) %>%
    summarize(areaMeanInit_ha = mean(area_ha))

outputClsFinal <- outputCls %>%
    filter(year == 150) %>%
    mutate(volAt120Cls = ifelse(is.na(volAt120Cls), "[0,30)", volAt120Cls)) %>%
    group_by(simID, id_ms, replicate, year, volAt120Cls) %>%
    summarize(area_ha = sum(area_ha)) %>%
    group_by(simID, id_ms, volAt120Cls) %>%
    summarize(areaMeanFinal_ha = mean(area_ha))

outputClsSummaryInit <- merge(outputClsinit, outputClsFinal, by = c("simID", "id_ms", "volAt120Cls")) %>%
    mutate(areaMeanInit_prop = 100*areaMeanInit_ha/areaTotal) %>%
    select(id_ms, volAt120Cls, areaMeanInit_prop) %>%
    spread(volAt120Cls, areaMeanInit_prop)

outputClsSummaryFinal <- merge(outputClsinit, outputClsFinal, by = c("simID", "id_ms", "volAt120Cls")) %>%
    mutate(areaMeanFinal_prop = 100*areaMeanFinal_ha/areaTotal) %>%
    select(id_ms, volAt120Cls, areaMeanFinal_prop) %>%
    spread(volAt120Cls, areaMeanFinal_prop)

##########
outputClsSummaryFinal <- cbind(id_ms = outputClsSummaryFinal[,1],
                               round(outputClsSummaryFinal[,-1], 1))
outputClsSummaryChange <- round(100*(outputClsSummaryFinal[,-1]/outputClsSummaryInit[,-1]), 0)

table2 <- cbind(outputClsSummaryFinal, outputClsSummaryChange)
write.csv(table2, file = "table2.csv", row.names = F)
##########

################################################################################
################################################################################
### ms table 3

#### cumulative regen failure
table3 <- data.frame(id_ms = outputClsSummaryFinal[,1],
           cumulRegenFailurePercent = round(outputClsSummaryFinal[,2]-outputClsSummaryInit[,2], 1))

volAt120MeanInit_tonnesPerHa <- outputMean %>%
    filter(year == 0) %>%
    group_by(id_ms) %>%
    summarize(volAt120Mean_tonnesPerHa = round(mean(volAt120Mean_tonnesPerHa),5))
volAt120MeanInit_tonnesPerHa <- unique(volAt120MeanInit_tonnesPerHa$volAt120Mean_tonnesPerHa)

volAt120MeanFinal_tonnesPerHa <- outputMean %>%
    filter(year == 150) %>%
    group_by(id_ms) %>%
    summarize(volAt120Mean_tonnesPerHa = mean(volAt120Mean_tonnesPerHa)) %>%
    mutate(volAt120Mean_percentChange = round(100*((volAt120Mean_tonnesPerHa-volAt120MeanInit_tonnesPerHa)/volAt120MeanInit_tonnesPerHa), 1),
           volAt120Mean_tonnesPerHa = round(volAt120Mean_tonnesPerHa, 1))

financialReturn <-  harvSummary %>%
    filter(year == 150) %>%
    mutate(harvestTotalCumSum_McubMeters = round(harvestTotalCumSum_cubMeters_mean/1000000, 1),
           netReturnM_mean = round(netReturn_mean/1000000, 1)) %>%
    arrange(id_ms) %>%
    select(id_ms,
           harvestTotalCumSum_McubMeters,
           netReturnM_mean)

table3 <- merge(table3, volAt120MeanFinal_tonnesPerHa) %>%
    merge(financialReturn, all.x = T)

write.csv(table3, file = "table3.csv", row.names = T)
    