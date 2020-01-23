###################################################################################################
###################################################################################################
##### Visualizing harvest simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario", "initYear"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-11-07_coupe0.31_recup70")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
# #####
# scenario  <-  "coupe0.31_recup70"
#################
require(raster)
require(ggplot2)
require(dplyr)
require(reshape2)
initYear <- 2015

harvTarget <- output <- list()

for(s in scenario) {
    ####################################################################
    ####################################################################
    ######
    studyArea <- raster(paste0("../", s, "/studyArea.tif"))
    convFactor <- prod(res(studyArea))/10000### to convert to hectares
    uaf <- raster(paste0("../", s, "/uaf.tif"))
    uaf_RAT <- read.csv(paste0("../", s, "/uaf_RAT.csv"))
    
    subZones <- raster(paste0("../", s, "/subZones.tif"))
    subZones_RAT <- read.csv(paste0("../", s, "/subZones_RAT.csv"))
    
    coverTypes <- raster(paste0("../", s, "/coverTypes.tif"))
    coverTypes_RAT <- read.csv(paste0("../", s, "/coverTypes_RAT.csv"))
    
    plan <- get(load(paste0("../", s, "/managementPlan.RData")))[["baseline"]]
    
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
    
    target <- rbind(target, data.frame(id = NA, uaf = "total",
                                       totalEligibleArea_ha = sum(target$totalEligibleArea_ha),
                                       harvTargetArea_ha = sum(target$harvTargetArea_ha))) 
    
    target[,"scenario"] <- s
    
    harvTarget[[s]] <- target
    ####################################################################
    ####################################################################
    ######
    ### fetching compiled results
    output[[s]] <-  get(load(paste0(paste0("../outputCompiled/outputCompiledHarvest_", s, ".RData"))))
    rm(outputCompiled)
    
}

output <- do.call("rbind", output)
harvTarget <- do.call("rbind", harvTarget)
nSims <- length(unique(output$replicate))

### summarizing results, percentile & such
output <- filter(output, uaf == "total")
output <- merge(output, harvTarget, all.x = T)
targetTotal <- target[target$uaf == "total", "harvTargetArea_ha"]
###


summaryHarvest <- output %>%
    mutate(harvAreaTotal = areaHarvestedTotal_ha + areaSalvagedTotal_ha,
           propTarget = harvAreaTotal / harvTargetArea_ha,
           salvProp = areaSalvagedTotal_ha / harvAreaTotal) 
    
write.csv(summaryHarvest, paste0("harvestSummary.csv"), row.names = F)


summaryHarvest <- summaryHarvest %>%
    group_by(scenario, uaf, year) %>%
    summarise(p01HarvestProp = quantile(areaHarvestedTotal_ha, .01),
              p05HarvestProp = quantile(areaHarvestedTotal_ha, .05),
              p10HarvestProp = quantile(areaHarvestedTotal_ha, .10),
              p25HarvestProp = quantile(areaHarvestedTotal_ha, .25),
              p50HarvestProp = quantile(areaHarvestedTotal_ha, .5),
              p75HarvestProp = quantile(areaHarvestedTotal_ha, .75),
              p95HarvestProp = quantile(areaHarvestedTotal_ha, .95),
              p90HarvestProp = quantile(areaHarvestedTotal_ha, .90),
              p99HarvestProp = quantile(areaHarvestedTotal_ha, .99),
              p01SalvProp = quantile(salvProp, .01, na.rm = T),
              p05SalvProp = quantile(salvProp, .05, na.rm = T),
              p10alvProp = quantile(salvProp, .10, na.rm = T),
              p25SalvProp = quantile(salvProp, .25, na.rm = T),
              p50SalvProp = quantile(salvProp, .5, na.rm = T),
              p75SalvProp = quantile(salvProp, .75, na.rm = T),
              p90SalvProp = quantile(salvProp, .90, na.rm = T),
              p95SalvProp = quantile(salvProp, .95, na.rm = T),
              p99SalvProp = quantile(salvProp, .99, na.rm = T),
              p01HarvestVol = quantile(volHarvestedTotal_cubMeter, .01),
              p05HarvestVol = quantile(volHarvestedTotal_cubMeter, .05),
              p10HarvestVol = quantile(volHarvestedTotal_cubMeter, .10),
              p25HarvestVol = quantile(volHarvestedTotal_cubMeter, .25),
              p50HarvestVol = quantile(volHarvestedTotal_cubMeter, .5),
              p75HarvestVol = quantile(volHarvestedTotal_cubMeter, .75),
              p90HarvestVol = quantile(volHarvestedTotal_cubMeter, .90),
              p95HarvestVol = quantile(volHarvestedTotal_cubMeter, .95),
              p99HarvestVol = quantile(volHarvestedTotal_cubMeter, .99),
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
### Plotting realized harvests 
#######

df <- summaryHarvest

cols <- c(baseline = "orange",
          RCP85 = "darkred")

m <- ggplot(df, aes(x = year + 2015,
                    colour = scenario,
                    fill = scenario)) +
    #facet_grid(coverType ~ scenario) +
    geom_line(aes(y =100 * p50HarvestProp/target),
              size = 1) +
    geom_ribbon(aes(y = NULL, colour = NULL,
                    ymin = 100*p25HarvestProp/target, ymax = 100*p75HarvestProp/target),
                alpha = 0.25) +
    geom_line(aes(y = 100*p10HarvestProp/target),
              size = 0.5, linetype = "dotted") +
    
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = cols)


png(filename= paste0("harvestRealized.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = paste0("Proportion of harvest objectives realized"),
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Full lines and ribbons represent averages and 25th and 75th percentiles\n",
                                 "Dotted lines represent 10th percentiles"),
               # caption = paste0("Âge min. de récolte (sauf récup.) - Épinette noire: 90 ans\n",
               #                  "Pin gris: 76 ans\n",
               #                  "Vol. marchand min.: 50 m3/ha (Récup.: 70 m3/ha)\n",
               #                  "Cycle des feux - baseline: 104 ans\n"),
                                #"Min vieilles forÃªts (>=100 ans): 14%\n",
                                #"Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Proportion of harvest objectives realized (%)\n"))

dev.off()





##############################################################################
### Plotting salvaged proportions
#######

# vars <- colnames(summaryHarvest)
# vars <- vars[grep("SalvProp", vars)]
# 
# percentile <- as.numeric(gsub("[^0-9]","", vars))
# #varName <- paste0(p, "Harvest_ha")
# df <- summaryHarvest
# ### reformatting harvest treatments for better readability
# df <- melt(df, id.vars = c("scenario", "year"), 
#            measure.vars = vars,
#            variable.name = "prob")
# df$prob <- as.numeric(gsub("[^0-9]","", df$prob))
# df$prob <- paste0(df$prob, "%")
# df$prob <- factor(df$prob, levels = c("1%", "5%", "25%", "50%", "75%", "95%", "99%"))
# #target <- summaryHarvest
# df <- filter(df, prob %in% c("25%", "50%", "75%"))

m <- ggplot(df, aes(x = year + 2015,
                    colour = scenario,
                    fill = scenario)) +
    #facet_grid(coverType ~ scenario) +
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


##############################################################################
### Plotting probability of shortfall 
#######

### summarizing results, shortfall probs
shortfallDF <- output %>%
    #group_by(scenario, year, replicate) %>%
    mutate(shortfall_tol75 = areaHarvestedTotal_ha < .25*harvTargetArea_ha,
           shortfall_tol50 = areaHarvestedTotal_ha < .50*harvTargetArea_ha,
           shortfall_tol25 = areaHarvestedTotal_ha < .75*harvTargetArea_ha,
           shortfall_tol10 = areaHarvestedTotal_ha < .90*harvTargetArea_ha,
           shortfall_tol05 = areaHarvestedTotal_ha < .95*harvTargetArea_ha) %>%
    group_by(scenario, replicate) %>%
    arrange(year) %>%
    mutate(shortfall_tol75 = cumsum(shortfall_tol75)>=1,
           shortfall_tol50 = cumsum(shortfall_tol50)>=1,
           shortfall_tol25 = cumsum(shortfall_tol25)>=1,
           shortfall_tol10 = cumsum(shortfall_tol10)>=1,
           shortfall_tol05 = cumsum(shortfall_tol05)>=1) %>%
    ungroup() %>%
    group_by(scenario, year) %>%
    summarise(shortfall_tol75 = sum(shortfall_tol75)/n(),
              shortfall_tol50 = sum(shortfall_tol50)/n(),
              shortfall_tol25 = sum(shortfall_tol25)/n(),
              shortfall_tol10 = sum(shortfall_tol10)/n(),
              shortfall_tol05 = sum(shortfall_tol05)/n())

write.csv(shortfallDF, file = paste0("harvestShortfallPercentiles.csv"), row.names = F)

vars <- colnames(shortfallDF)
vars <- vars[grep("shortfall", vars)]
# 
riskTol <- paste0(as.numeric(gsub("[^0-9]","", vars)), "%")
df <- shortfallDF
## reformating in tall form
require(reshape2)
df <- melt(shortfallDF, id.vars = c("scenario", "year"),
            measure.vars = vars,
           variable.name = "tolerance")


df$tolerance <- as.numeric(gsub("[^0-9]", "",  df$tolerance))
df$tolerance <- paste0(df$tolerance, "%")
df$tolerance <- factor(df$tolerance, levels = c("5%", "10%", "25%", "50%", "75%"))


m <- ggplot(df, aes(x = year + 2015,
                    y = 100 * value,
                    linetype = tolerance)) +
    facet_grid(~ scenario) +
    geom_line(size = 0.5, colour = "lightblue")
    

png(filename= paste0("harvestShortfall.png"),
    width = 12, height = 6, units = "in", res = 600, pointsize=10)
options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Probability of timber supply shortfall",
               x = "",
               y = "Prob. of shorfall (%)\n"))


dev.off()







