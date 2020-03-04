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

summaryHarvest <- output %>%
    mutate(harvAreaTotal_ha = areaHarvestedTotal_ha + areaSalvagedTotal_ha,
           harvVolTotal_cubMeter = volHarvestedTotal_cubMeter + volSalvagedTotal_cubMeter,
           propTarget = harvAreaTotal_ha / harvTargetArea_ha,
           salvProp = areaSalvagedTotal_ha / harvAreaTotal_ha) 
    
write.csv(summaryHarvest, paste0("outputCompiledHarvest.csv"), row.names = F)


summaryHarvest <- summaryHarvest %>%
    group_by(simID, fireScenario, mgmtScenario, uaf, year) %>%
    summarise(meanHarvestProp = mean(harvAreaTotal_ha),
              p01HarvestProp = quantile(harvAreaTotal_ha, .01),
              p05HarvestProp = quantile(harvAreaTotal_ha, .05),
              p10HarvestProp = quantile(harvAreaTotal_ha, .10),
              p25HarvestProp = quantile(harvAreaTotal_ha, .25),
              p50HarvestProp = quantile(harvAreaTotal_ha, .5),
              p75HarvestProp = quantile(harvAreaTotal_ha, .75),
              p95HarvestProp = quantile(harvAreaTotal_ha, .95),
              p90HarvestProp = quantile(harvAreaTotal_ha, .90),
              p99HarvestProp = quantile(harvAreaTotal_ha, .99),
              p01SalvProp = quantile(salvProp, .01, na.rm = T),
              p05SalvProp = quantile(salvProp, .05, na.rm = T),
              p10alvProp = quantile(salvProp, .10, na.rm = T),
              p25SalvProp = quantile(salvProp, .25, na.rm = T),
              p50SalvProp = quantile(salvProp, .5, na.rm = T),
              p75SalvProp = quantile(salvProp, .75, na.rm = T),
              p90SalvProp = quantile(salvProp, .90, na.rm = T),
              p95SalvProp = quantile(salvProp, .95, na.rm = T),
              p99SalvProp = quantile(salvProp, .99, na.rm = T),
              p01HarvestVol = quantile(harvVolTotal_cubMeter, .01),
              p05HarvestVol = quantile(harvVolTotal_cubMeter, .05),
              p10HarvestVol = quantile(harvVolTotal_cubMeter, .10),
              p25HarvestVol = quantile(harvVolTotal_cubMeter, .25),
              p50HarvestVol = quantile(harvVolTotal_cubMeter, .5),
              p75HarvestVol = quantile(harvVolTotal_cubMeter, .75),
              p90HarvestVol = quantile(harvVolTotal_cubMeter, .90),
              p95HarvestVol = quantile(harvVolTotal_cubMeter, .95),
              p99HarvestVol = quantile(harvVolTotal_cubMeter, .99),
              p01SalvVol = quantile(volSalvagedTotal_cubMeter, .01),
              p05SalvVol = quantile(volSalvagedTotal_cubMeter, .05),
              p10SalvVol = quantile(volSalvagedTotal_cubMeter, .10),
              p25SalvVol = quantile(volSalvagedTotal_cubMeter, .25),
              p50SalvVol = quantile(volSalvagedTotal_cubMeter, .5),
              p75SalvVol = quantile(volSalvagedTotal_cubMeter, .75),
              p90SalvVol = quantile(volSalvagedTotal_cubMeter, .90),
              p95SalvVol = quantile(volSalvagedTotal_cubMeter, .95),
              p99SalvVol = quantile(volSalvagedTotal_cubMeter, .99),
              target = unique(harvTargetArea_ha))

write.csv(summaryHarvest, paste0("harvestSummaryPercentiles.csv"), row.names = F)




##############################################################################
### Probability of shortfall 
#######

### summarizing results, shortfall probs
shortfallDF <- output %>%
    #group_by(scenario, year, replicate) %>%
    mutate(harvAreaTotal_ha = areaHarvestedTotal_ha + areaSalvagedTotal_ha,
           shortfall_tol75 = harvAreaTotal_ha < .25*harvTargetArea_ha,
           shortfall_tol50 = harvAreaTotal_ha < .50*harvTargetArea_ha,
           shortfall_tol25 = harvAreaTotal_ha < .75*harvTargetArea_ha,
           shortfall_tol10 = harvAreaTotal_ha < .90*harvTargetArea_ha,
           shortfall_tol05 = harvAreaTotal_ha < .95*harvTargetArea_ha) %>%
    group_by(simID, fireScenario, mgmtScenario, replicate) %>%
    arrange(year) %>%
    mutate(shortfall_tol75 = cumsum(shortfall_tol75)>=1,
           shortfall_tol50 = cumsum(shortfall_tol50)>=1,
           shortfall_tol25 = cumsum(shortfall_tol25)>=1,
           shortfall_tol10 = cumsum(shortfall_tol10)>=1,
           shortfall_tol05 = cumsum(shortfall_tol05)>=1) %>%
    ungroup() %>%
    group_by(simID, fireScenario, mgmtScenario, year) %>%
    summarise(shortfall_tol75 = sum(shortfall_tol75)/n(),
              shortfall_tol50 = sum(shortfall_tol50)/n(),
              shortfall_tol25 = sum(shortfall_tol25)/n(),
              shortfall_tol10 = sum(shortfall_tol10)/n(),
              shortfall_tol05 = sum(shortfall_tol05)/n())

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
        df <- summaryHarvest %>%
            mutate(var = ifelse(simID %in% sims,
                                 "Variable retention", "Traditional clearcutting"))
        dfSf <- shortfallDF %>%
            mutate(var = ifelse(simID %in% sims,
                                "Variable retention", "Traditional clearcutting"))
        
        cols <- c("Variable retention" = "skyblue2",
                  "Traditional clearcutting" = "darkorange1")
    }
    if(vLab == "Salvage logging") {
        df <- summaryHarvest %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$salv)],
                                "Salvage logging", "No salvage logging"))
        dfSf <- shortfallDF %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$salv)],
                                "Salvage logging", "No salvage logging"))
        
        cols <- c("Salvage logging" = "lightgoldenrod1",
                  "No salvage logging" = "brown1")
    }
    if(vLab == "Post-fire plantation") {
        df <- summaryHarvest %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$plantPostFire)],
                                "Post-fire plantation", "No post-fire plantation"))
        dfSf <- shortfallDF %>%
            mutate(var = ifelse(simID %in% simInfo$simID[which(simInfo$plantPostFire)],
                                "Post-fire plantation", "No post-fire plantation"))
        cols <- c("Post-fire plantation" = "darkgoldenrod1",
                  "No post-fire plantation" = "firebrick1")
    }
    if(vLab == "Planted species") {
        df <- summaryHarvest %>%
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
        geom_line(aes(y =100 * meanHarvestProp/target),
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
                          group = mgmtScenario,
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






ggplot(df, aes(x = year + 2015,
               colour = mgmtScenario)) +
    
    geom_line(aes(y =100 * p50HarvestProp/target),
              size = 0.75)


m <- ggplot(df, aes(x = year + 2015,
                    colour = mgmtScenario,
                    fill = mgmt)) +
    facet_wrap(~ fireScenario, ncol = 1) +
    geom_line(aes(y =100 * p50SalvProp),
              size = 1) +
    geom_ribbon(aes(y = NULL, colour = NULL,
                    ymin = 100*p25SalvProp, ymax = 100*p75SalvProp),
                alpha = 0.25) +
    # geom_line(aes(y = 100*p10HarvestProp),
    #           size = 0.5, linetype = "dotted") +
    
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = cols)

png(filename= paste0("harvestSalvagedProp.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
              legend.title = element_text(size = rel(0.85)),
              title = element_text(size = rel(0.85)),
              #plot.subtitle = element_text(size = rel(1)),
              plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Importance of salvage logging in achieving harvesting targets",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Full lines and ribbons represent averages and 25th and 75th percentiles\n",
                                 "Dotted lines represent 10th percentiles"),
               # caption = paste0("Àge min. de récolte (sauf récup.) - Épinette noire: 90 ans\n",
               #                  "Pin gris: 76 ans\n",
               #                  "Vol. marchand min.: 50 m3/ha (Récup.: 70 m3/ha)\n",
               #                  "Cycle des feux - baseline: 104 ans\n"),
               #"Min vieilles forÃªts (>=100 ans): 14%\n",
               #"Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Proportion of salvage logging\n(% of total area harvested)\n"))

dev.off()


write.csv(shortfallDF, file = paste0("harvestShortfallPercentiles.csv"), row.names = F)












for(v in c("harv", "varReten", "salv", "plantPostFire", "plantPostSalv", "plantPostSalvSp")) {
    if(v == "harv") {
        df <- summaryHarvest %>%
            mutate(harv = ifelse(simID %in% simInfo$simID[which(simInfo$mgmt != "No logging")],
                                 "Harvest", "No harvest"))
        m <- ggplot(df, aes(x = year + 2015,
                            colour = harv))
        
    }
}

