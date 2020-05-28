###################################################################################################
###################################################################################################
##### Visualizing harvest simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "rawOutputDir"))])
#################
require(raster)
require(ggplot2)
require(dplyr)
require(reshape2)

harvTarget <- output <- list()
for (s in seq_along(simInfo$simID)) {
    
    simID <- simInfo$simID[s]
    simDir <- paste0(rawOutputDir, simInfo$simDir[s])
    fr <- simInfo$fire[s]
    mgmt <- simInfo$mgmt[s]
    initYear <- simInfo$initYear
    
    ####################################################################
    ####################################################################
    ######
    studyArea <- raster(paste0(simDir, "/studyArea.tif"))
    convFactor <- prod(res(studyArea))/10000### to convert to hectares
    uaf <- raster(paste0(simDir, "/uaf.tif"))
    uaf_RAT <- read.csv(paste0(simDir, "/uaf_RAT.csv"))
    
    subZones <- raster(paste0(simDir, "/subZones.tif"))
    subZones_RAT <- read.csv(paste0(simDir, "/subZones_RAT.csv"))
    
    coverTypes <- raster(paste0(simDir, "/coverTypes.tif"))
    coverTypes_RAT <- read.csv(paste0(simDir, "/coverTypes_RAT.csv"))
    
    plan <- get(load(paste0(simDir, "/managementPlan.RData")))[["baseline"]]
    
    ## eligible to harvest
    harvEligible <- uaf %in% plan$uaf &
        subZones %in% plan$subZone
    harvEligible[!harvEligible] <- NA
    
    ## spp eligible (commercial species)
    spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
    spEligible[!spEligible] <- NA
    
    
    ## targetted harvesting level
    targetProp <- plan$targetHarvestLevels$SEPM
    target <- zonal(spEligible, uaf, sum)
    target <- data.frame(id = target[,"zone"], uaf = uaf_RAT[match(target[,"zone"], uaf_RAT$ID), "value"],
                         totalEligibleArea_ha = target[,"value"] * convFactor,
                         harvTargetArea_ha =  round(target[,"value"] * targetProp) *25)
    
    
    # target <- rbind(target, data.frame(id = NA, uaf = "total",
    #                                    totalEligibleArea_ha = sum(target$totalEligibleArea_ha),
    #                                    harvTargetArea_ha = sum(target$harvTargetArea_ha))) 
    target[,"fireScenario"] <- fr
    target[,"mgmtScenario"] <- mgmt
    
    harvTarget[[simID]] <- target
    
    ### fetching compiled results
    outputFile <- paste0("../outputCompiled/outputCompiledHarvest_", simID, ".RData")
    if(file.exists(outputFile)) {
        output[[simID]] <-  get(load(paste0(paste0("../outputCompiled/outputCompiledHarvest_", simID, ".RData"))))
        rm(outputCompiled)    
    }
    print(simID)
    
}

if(length(output)>1) {
    output <- do.call("rbind", output)
    harvTarget <- do.call("rbind", harvTarget)
} else {
    output <- output[[scenario]]
    harvTarget <- harvTarget[[simID]]
}
# nSims <- output %>% 
#     group_by(mgmtScenario, year) %>%
#     summarize(n = n()) %>%
#     distinct(fireScenario, n) %>%
#     as.data.frame()

### summarizing results, percentile & such
output <- filter(output, uaf == "total")
output <- merge(output, harvTarget, all.x = T)


output[,"clearcutting"] <- simInfo$clearcutting[match(output$simID, simInfo$simID)]
output[,"varReten"] <- simInfo$varReten[match(output$simID, simInfo$simID)]
output[,"salv"] <- simInfo$salv[match(output$simID, simInfo$simID)]
output[,"plantPostFire"] <- simInfo$plantPostFire[match(output$simID, simInfo$simID)]
output[,"plantPostSalv"] <- simInfo$plantPostSalv[match(output$simID, simInfo$simID)]
output[,"plantPostSalvSp"] <- simInfo$plantPostSalvSp[match(output$simID, simInfo$simID)]

### rounding some volumes
output[,"volSalvagedTotal_cubMeter"] <- round(output[,"volSalvagedTotal_cubMeter"], 1)

harvestCompilation <- output %>%
    mutate(harvestArea_ha = areaHarvestedTotal_ha,
           harvestVol_cubMeters = volHarvestedTotal_cubMeter,
           salvArea_ha = areaSalvagedTotal_ha,
           salvVol_cubMeters = volSalvagedTotal_cubMeter,
           targetArea_ha = harvTargetArea_ha,
           retentArea_ha = areaRetenTotal_ha,
           retenVol_cubMeter = -volRetenTotal_cubMeter ,
           plantPostSalv_ha = areaPlantPostSalv_ha,
           plantPostFire_ha = areaPlantPostFire_ha,
           managedTotal_ha = totalEligibleArea_ha,
           harvestAreaTotal_ha = harvestArea_ha + salvArea_ha ) %>%#+ retentArea_ha
    select(simID, fireScenario, replicate, year, replicate, year,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv,
           managedTotal_ha, targetArea_ha,
           harvestAreaTotal_ha, harvestVol_cubMeters, salvArea_ha, salvVol_cubMeters,
           retentArea_ha, retenVol_cubMeter,
           plantPostSalv_ha, plantPostFire_ha)
           
           
           
        # harvAreaTotal_ha = areaHarvestedTotal_ha + areaSalvagedTotal_ha,
        #    harvestVolTotal_cubMeters = volHarvestedTotal_cubMeter + volSalvagedTotal_cubMeter,
        #    propTarget = round(harvAreaTotal_ha / harvTargetArea_ha, 4),
        #    salvProp = round(areaSalvagedTotal_ha / harvAreaTotal_ha, 4))

write.csv(harvestCompilation, paste0("outputCompiledHarvest.csv"), row.names = F)


harvestSummary <- output %>%
    group_by(simID, fireScenario, mgmtScenario, uaf, year) %>%
    summarise(targetArea_ha = unique(harvTargetArea_ha),
              harvestAreaMean_ha = mean(areaHarvestedTotal_ha),
              harvestAreaP01_ha = quantile(areaHarvestedTotal_ha, .01),
              harvestAreaP05_ha = quantile(areaHarvestedTotal_ha, .05),
              harvestAreaP10_ha = quantile(areaHarvestedTotal_ha, .10),
              harvestAreaP25_ha = quantile(areaHarvestedTotal_ha, .25),
              harvestAreaP50_ha = quantile(areaHarvestedTotal_ha, .5),
              harvestAreaP75_ha = quantile(areaHarvestedTotal_ha, .75),
              harvestAreaP90_ha = quantile(areaHarvestedTotal_ha, .90),
              harvestAreaP95_ha = quantile(areaHarvestedTotal_ha, .95),
              harvestAreaP99_ha = quantile(areaHarvestedTotal_ha, .99),
              retenAreaMean_ha = mean(areaRetenTotal_ha),
              retenAreaP01_ha = quantile(areaRetenTotal_ha, .01),
              retenAreaP05_ha = quantile(areaRetenTotal_ha, .05),
              retenAreaP10_ha = quantile(areaRetenTotal_ha, .10),
              retenAreaP25_ha = quantile(areaRetenTotal_ha, .25),
              retenAreaP50_ha = quantile(areaRetenTotal_ha, .5),
              retenAreaP75_ha = quantile(areaRetenTotal_ha, .75),
              retenAreaP90_ha = quantile(areaRetenTotal_ha, .90),
              retenAreaP95_ha = quantile(areaRetenTotal_ha, .95),
              retenAreaP99_ha = quantile(areaRetenTotal_ha, .99),
              salvAreaMean_ha = mean(areaSalvagedTotal_ha, na.rm = T),
              salvAreaP01_ha = quantile(areaSalvagedTotal_ha, .01, na.rm = T),
              salvAreaP05_ha = quantile(areaSalvagedTotal_ha, .05, na.rm = T),
              salvAreaP10_ha = quantile(areaSalvagedTotal_ha, .10, na.rm = T),
              salvAreaP25_ha = quantile(areaSalvagedTotal_ha, .25, na.rm = T),
              salvAreaP50_ha = quantile(areaSalvagedTotal_ha, .5, na.rm = T),
              salvAreaP75_ha = quantile(areaSalvagedTotal_ha, .75, na.rm = T),
              salvAreaP90_ha = quantile(areaSalvagedTotal_ha, .90, na.rm = T),
              salvAreaP95_ha = quantile(areaSalvagedTotal_ha, .95, na.rm = T),
              salvAreaP99_ha = quantile(areaSalvagedTotal_ha, .99, na.rm = T),
              harvestVolMean_cubMeters = round(mean(volHarvestedTotal_cubMeter)),
              harvestVolP01_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .01)),
              harvestVolP05_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .05)),
              harvestVolP20_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .10)),
              harvestVolP25_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .25)),
              harvestVolP50_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .5)),
              harvestVolP75_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .75)),
              harvestVolP90_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .90)),
              harvestVolP95_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .95)),
              harvestVolP99_cubMeters = round(quantile(volHarvestedTotal_cubMeter, .99)),
              salvVolMean_cubMeters = round(mean(volSalvagedTotal_cubMeter)),
              salvVolP01_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .01)),
              salvVolP05_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .05)),
              salvVolP10_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .10)),
              salvVolP25_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .25)),
              salvVolP50_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .5)),
              salvVolP75_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .75)),
              salvVolP90_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .90)),
              salvVolP95_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .95)),
              salvVolP99_cubMeters = round(quantile(volSalvagedTotal_cubMeter, .99)),
              harvestAreaTotalMean_ha = harvestAreaMean_ha + salvAreaMean_ha)

write.csv(harvestSummary, paste0("harvestSummary.csv"), row.names = F)




##############################################################################
### Probability of shortfall 
#######

### summarizing results, shortfall probs
shortfallDF <- harvestCompilation %>%
    #group_by(scenario, year, replicate) %>%
    mutate(shortfall_tol75 = harvestAreaTotal_ha < .25*targetArea_ha,
           shortfall_tol50 = harvestAreaTotal_ha < .50*targetArea_ha,
           shortfall_tol25 = harvestAreaTotal_ha < .75*targetArea_ha,
           shortfall_tol10 = harvestAreaTotal_ha < .90*targetArea_ha,
           shortfall_tol05 = harvestAreaTotal_ha < .95*targetArea_ha) %>%
    group_by(simID, fireScenario, replicate) %>%
    arrange(year) %>%
    mutate(shortfall_tol75 = cumsum(shortfall_tol75)>=1,
           shortfall_tol50 = cumsum(shortfall_tol50)>=1,
           shortfall_tol25 = cumsum(shortfall_tol25)>=1,
           shortfall_tol10 = cumsum(shortfall_tol10)>=1,
           shortfall_tol05 = cumsum(shortfall_tol05)>=1) %>%
    ungroup() %>%
    group_by(simID, fireScenario, year) %>%
    summarise(shortfall_tol75 = sum(shortfall_tol75)/n(),
              shortfall_tol50 = sum(shortfall_tol50)/n(),
              shortfall_tol25 = sum(shortfall_tol25)/n(),
              shortfall_tol10 = sum(shortfall_tol10)/n(),
              shortfall_tol05 = sum(shortfall_tol05)/n())

write.csv(shortfallDF, file = paste0("harvestShortfallSummary.csv"), row.names = F)

##############################################################################
### Plotting realized harvests - all mgmt scenarios
#######

varNames <- c(varReten = "Variable retention",
              salv = "Salvage logging",
              plantPostFire = "Post-fire plantation",
              plantPostFireSp = "Planted species")

for(v in seq_along(varNames)) {
    vLab <- varNames[v]
    if(vLab == "Variable retention") {
        sims <- simInfo$simID[which(simInfo$varReten)]
        df <- harvestSummary %>%
            mutate(var = ifelse(simID %in% sims,
                                 "Variable retention", "Traditional clearcutting"))
        dfSf <- shortfallDF %>%
            mutate(var = ifelse(simID %in% sims,
                                "Variable retention", "Traditional clearcutting"))
        
        cols <- c("Variable retention" = "skyblue2",
                  "Traditional clearcutting" = "darkorange1")
    }
    if(vLab == "Salvage logging") {
        df <- harvestSummary %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$salv)],
                                "Salvage logging", "No salvage logging"))
        dfSf <- shortfallDF %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$salv)],
                                "Salvage logging", "No salvage logging"))
        
        cols <- c("Salvage logging" = "lightgoldenrod1",
                  "No salvage logging" = "brown1")
    }
    if(vLab == "Post-fire plantation") {
        df <- harvestSummary %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$plantPostFire)],
                                "Post-fire plantation", "No post-fire plantation"))
        dfSf <- shortfallDF %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$plantPostFire)],
                                "Post-fire plantation", "No post-fire plantation"))
        cols <- c("Post-fire plantation" = "darkgoldenrod1",
                  "No post-fire plantation" = "firebrick1")
    }
    if(vLab == "Planted species") {
        df <- harvestSummary %>%
            filter(simID %in% simInfo$simID[which(simInfo$plantPostFireSp %in% c("same", "PG"))]) %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$plantPostFireSp == "PG")],
                                "Planted species: Jack Pine", "Planted species: Same as before fire"))
        dfSf <- shortfallDF %>%
            filter(simID %in% simInfo$simID[which(simInfo$plantPostFireSp %in% c("same", "PG"))]) %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$plantPostFireSp == "PG")],
                                "Planted species: Jack Pine", "Planted species: Same as before fire"))
        
        cols <- c("Planted species: Jack Pine" = "orange2",
                  "Planted species: Same as before fire" = "grey")
    }

    
    
    
    ############################################################################
    ##### Proportion of target achieved
    m <- ggplot(df, aes(x = year + 2015,
                        group = mgmtScenario,
                        colour = var)) +
        facet_wrap(~ fireScenario, ncol = 1) +
        geom_line(aes(y =100 * harvestAreaTotalMean_ha /targetArea_ha),
                  size = 0.75) +
        scale_colour_manual(values = cols)
        
    
    png(filename = paste0("harvestRealized_", names(vLab), ".png"),
        width = 8, height = 8, units = "in", res = 600, pointsize=10)
    
    options(scipen=999)
    
    print(m + theme_dark() +
              
              theme(legend.position="top", legend.direction="horizontal",
                    legend.title = element_blank(),
                    legend.text = element_text(size = rel(.75)),
                    title = element_text(size = rel(0.85)),
                    plot.caption = element_text(size = rel(0.65))) +
              
              labs(title = paste0("Proportion of harvest objectives realized"),
                   x = "",
                   y = "Proportion of harvest objectives realized (%)\n"))# +
              #guides(col = guide_legend(nrow=4)))
    
    dev.off()
    
    ############################################################################
    ##### Prob shortfall
    # 
    vars <- colnames(dfSf)
    vars <- vars[grep("shortfall", vars)]
    riskTol <- paste0(as.numeric(gsub("[^0-9]","", vars)), "%")
    

    require(reshape2)
    m <- ggplot(dfSf, aes(x = year + 2015,
                          y = 100 * shortfall_tol25,
                          group = simID,
                          colour = var)) +
        facet_wrap(~ fireScenario, ncol = 1) +
        geom_line(size = 0.75) +
        scale_colour_manual(values = cols)
    
    
    png(filename= paste0("harvestShortfall_", names(vLab), ".png"),
        width = 8, height = 8, units = "in", res = 600, pointsize=10)
    options(scipen=999)
    
    print(m + theme_dark() +
              
              theme(legend.position="top", legend.direction="horizontal",
                    legend.title = element_blank(),
                    legend.text = element_text(size = rel(.75)),
                    title = element_text(size = rel(0.85)),
                    plot.caption = element_text(size = rel(0.65))) +
              
              labs(title = "Probability of timber supply shortfall",
                   x = "",
                   y = "Prob. of shorfall (%)\n"))# +
    dev.off()
}



# 
# # ggplot(df, aes(x = year + 2015,
# #                colour = mgmtScenario)) +
# #     
# #     geom_line(aes(y =100 * harvestAreaP50_ha/targetArea_ha),
# #               size = 0.75)
# 
# 
# m <- ggplot(harvestSummary, aes(x = year + 2015,
#                     colour = simID,
#                     fill = mgmt)) +
#     facet_wrap(~ fireScenario, ncol = 1) +
#     geom_line(aes(y =100 * salvAreaMean_ha/targetArea_ha),
#               size = 1) +
#     geom_ribbon(aes(y = NULL, colour = NULL,
#                     ymin = 100*salvAreaP25_ha/targetArea_ha, ymax = 100*salvAreaP75_ha/targetArea_ha),
#                 alpha = 0.25)# +
#     # geom_line(aes(y = 100*p10HarvestProp),
#     #           size = 0.5, linetype = "dotted") +
#     
#     #scale_fill_manual(values = cols) +
#     #scale_colour_manual(values = cols)
# 
# png(filename= paste0("harvestSalvagedProp.png"),
#     width = 8, height = 6, units = "in", res = 600, pointsize=10)
# 
# options(scipen=999)
# 
# print(m + theme_dark() +
#           
#           theme(#legend.position="top", legend.direction="horizontal",
#               legend.title = element_text(size = rel(0.85)),
#               title = element_text(size = rel(0.85)),
#               #plot.subtitle = element_text(size = rel(1)),
#               plot.caption = element_text(size = rel(0.65))) +
#           
#           labs(title = "Importance of salvage logging in achieving harvesting targets",
#                #subtitle = paste0(percentile, "e percentile"),
#                subtitle = paste0("Full lines and ribbons represent averages and 25th and 75th percentiles\n",
#                                  "Dotted lines represent 10th percentiles"),
#                # caption = paste0("Àge min. de récolte (sauf récup.) - Épinette noire: 90 ans\n",
#                #                  "Pin gris: 76 ans\n",
#                #                  "Vol. marchand min.: 50 m3/ha (Récup.: 70 m3/ha)\n",
#                #                  "Cycle des feux - baseline: 104 ans\n"),
#                #"Min vieilles forÃªts (>=100 ans): 14%\n",
#                #"Max régén. (< 20 ans): 35%"),
#                x = "",
#                y = "Proportion of salvage logging\n(% of total area harvested)\n"))
# 
# dev.off()



# 
# 
# for(v in c("harv", "varReten", "salv", "plantPostFire", "plantPostSalv", "plantPostSalvSp")) {
#     if(v == "harv") {
#         df <- summaryHarvest %>%
#             mutate(harv = ifelse(simID %in% simInfo$simID[which(simInfo$mgmt != "No logging")],
#                                  "Harvest", "No harvest"))
#         m <- ggplot(df, aes(x = year + 2015,
#                             colour = harv))
#         
#     }
# }
# 
