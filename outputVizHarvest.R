###################################################################################################
###################################################################################################
##### Visualizing harvest simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
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


####################################################################
####################################################################
######
studyArea <- raster("../studyArea.tif")
convFactor <- prod(res(studyArea))/10000### to convert to hectares
uaf <- raster("../uaf.tif")
uaf_RAT <- read.csv("../uaf_RAT.csv")

subZones <- raster("../subZones.tif")
subZones_RAT <- read.csv("../subZones_RAT.csv")

coverTypes <- raster("../coverTypes.tif")
coverTypes_RAT <- read.csv("../coverTypes_RAT.csv")
    
plan <- get(load("../managementPlan.RData"))[["baseline"]]
    
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

####################################################################
####################################################################
######
### fetching compiled results
outputCompiled <- get(load(paste0("outputCompiledHarvest_", scenario, ".RData")))

nSims <- nrow(distinct(outputCompiled, scenario, replicate))

### summarizing results, percentile & such
outputCompiled <- filter(outputCompiled, uaf == "026-61")
outputCompiled <- merge(outputCompiled, target)
targetTotal <- target[target$uaf == "026-61", "harvTargetArea_ha"]
###


summaryHarvest <- outputCompiled %>%
    mutate(scenario = scenario,
           propTotal = areaHarvestedTotal_ha / harvTargetArea_ha) %>%
    group_by(scenario, year, uaf, replicate, harvestType) %>%
    summarise(areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha),
              harvTargetArea_ha = sum(harvTargetArea_ha)) %>%
    group_by(scenario, uaf, replicate, year, harvTargetArea_ha) %>%
    summarize(salvProp = areaHarvestedTotal_ha[harvestType == "salvage"] / sum(areaHarvestedTotal_ha),
              areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha)) %>%
    mutate(areaHarvestedTotal_ha = ifelse(areaHarvestedTotal_ha>harvTargetArea_ha,
                                          harvTargetArea_ha, areaHarvestedTotal_ha))

write.csv(summaryHarvest, paste0("harvestSummary_", scenario, ".csv"), row.names = F)


summaryHarvest <- summaryHarvest %>%
    group_by(scenario, uaf, year) %>%
    summarise(p01HarvestProp = quantile(areaHarvestedTotal_ha, .01),
              p05HarvestProp = quantile(areaHarvestedTotal_ha, .05),
              p25HarvestProp = quantile(areaHarvestedTotal_ha, .25),
              p50HarvestProp = quantile(areaHarvestedTotal_ha, .5),
              p75HarvestProp = quantile(areaHarvestedTotal_ha, .75),
              p95HarvestProp = quantile(areaHarvestedTotal_ha, .95),
              p99HarvestProp = quantile(areaHarvestedTotal_ha, .99),
              p01SalvProp = quantile(salvProp, .01, na.rm = T),
              p05SalvProp = quantile(salvProp, .05, na.rm = T),
              p25SalvProp = quantile(salvProp, .25, na.rm = T),
              p50SalvProp = quantile(salvProp, .5, na.rm = T),
              p75SalvProp = quantile(salvProp, .75, na.rm = T),
              p95SalvProp = quantile(salvProp, .95, na.rm = T),
              p99SalvProp = quantile(salvProp, .99, na.rm = T))

write.csv(summaryHarvest, paste0("harvestSummaryPercentiles_", scenario, ".csv"), row.names = F)

##############################################################################
### Plotting realized harvests 
#######
vars <- colnames(summaryHarvest)
vars <- vars[grep("HarvestProp", vars)]

percentile <- as.numeric(gsub("[^0-9]","", vars))
#varName <- paste0(p, "Harvest_ha")
df <- summaryHarvest
### reformatting harvest treatments for better readability
df <- melt(df, id.vars = c("scenario", "year"), 
                            measure.vars = vars,
           variable.name = "prob")
df$prob <- as.numeric(gsub("[^0-9]","", df$prob))
df$prob <- paste0(df$prob, "%")
df$prob <- factor(df$prob, levels = c("1%", "5%", "25%", "50%", "75%", "95%", "99%"))
#target <- summaryHarvest
targ <- target[which(target$uaf == "total"), "harvTargetArea_ha"]

m <- ggplot(df, aes(x = year + 2015, y = (value/targ)*100,
                    linetype = prob)) +
    #facet_grid(coverType ~ scenario) +
    geom_line(size = 0.5) +
    scale_linetype_manual("Percentiles\n",
                      values =seq_along(df$prob)) +
    guides(linetype = guide_legend(reverse=T))


png(filename= paste0("harvestRealized.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Analyse des récoltes réalisées",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Chaque courbe représente la fraction du taux de récolte ciblé (", targetTotal, " ha/année) bel et bien récoltée\n",
                                 "pour un percentile donné au sein d'un ensemble de ",nSims, " simulations."),
               caption = paste0("Âge min. de récolte (sauf récup.) - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Vol. marchand min.: 50 m3/ha (Récup.: 70 m3/ha)\n",
                                "Cycle des feux - baseline: 104 ans\n"),
                                #"Min vieilles forêts (>=100 ans): 14%\n",
                                #"Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Fraction du taux de récolte ciblé récoltée (%)\n"))

dev.off()





##############################################################################
### Plotting salvaged proportions
#######

vars <- colnames(summaryHarvest)
vars <- vars[grep("SalvProp", vars)]

percentile <- as.numeric(gsub("[^0-9]","", vars))
#varName <- paste0(p, "Harvest_ha")
df <- summaryHarvest
### reformatting harvest treatments for better readability
df <- melt(df, id.vars = c("scenario", "year"), 
           measure.vars = vars,
           variable.name = "prob")
df$prob <- as.numeric(gsub("[^0-9]","", df$prob))
df$prob <- paste0(df$prob, "%")
df$prob <- factor(df$prob, levels = c("1%", "5%", "25%", "50%", "75%", "95%", "99%"))
#target <- summaryHarvest
df <- filter(df, prob %in% c("25%", "50%", "75%"))

m <- ggplot(df, aes(x = year + 2015, y = value*100,
                    linetype = prob)) +
    #facet_grid(coverType ~ scenario) +
    geom_line(size = 0.5) +
    scale_linetype_manual("Percentiles\n",
                          values =seq_along(df$prob)) +
    guides(linetype = guide_legend(reverse=T))


png(filename= paste0("harvestSalvagedProp.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
              legend.title = element_text(size = rel(0.85)),
              title = element_text(size = rel(0.85)),
              #plot.subtitle = element_text(size = rel(1)),
              plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Analyse de la proportion des récoltes effectuée après feu (récupération)",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Chaque courbe représente la fraction du total des superficies récoltées correspondant à la coupe de récupération\n",
                                 "pour un percentile donné au sein d'un ensemble de ",nSims, " simulations."),
               caption = paste0("Âge min. de récolte (sauf récup.) - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Vol. marchand min.: 50 m3/ha (Récup.: 70 m3/ha)\n",
                                "Cycle des feux - baseline: 104 ans\n"),
               #"Min vieilles forêts (>=100 ans): 14%\n",
               #"Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Proportion des coupes de récupération (%)\n"))

dev.off()


##############################################################################
### Plotting probability of shortfall 
#######

### summarizing results, shortfall probs
shortfallDF <- outputCompiled %>%
    group_by(scenario, uaf, year, replicate) %>%
    summarise(areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha)) %>%
    #group_by(scenario, year) %>%
    mutate(shortfall_tol75 = areaHarvestedTotal_ha < .25*targetTotal,
           shortfall_tol50 = areaHarvestedTotal_ha < .50*targetTotal,
           shortfall_tol25 = areaHarvestedTotal_ha < .75*targetTotal,
           shortfall_tol10 = areaHarvestedTotal_ha < .90*targetTotal,
           shortfall_tol05 = areaHarvestedTotal_ha < .95*targetTotal) %>%
    group_by(scenario, uaf, replicate) %>%
    arrange(year) %>%
    mutate(shortfall_tol75 = cumsum(shortfall_tol75)>=1,
           shortfall_tol50 = cumsum(shortfall_tol50)>=1,
           shortfall_tol25 = cumsum(shortfall_tol25)>=1,
           shortfall_tol10 = cumsum(shortfall_tol10)>=1,
           shortfall_tol05 = cumsum(shortfall_tol05)>=1) %>%
    ungroup() %>%
    group_by(scenario, uaf, year) %>%
    summarise(shortfall_tol75 = sum(shortfall_tol75)/n(),
              shortfall_tol50 = sum(shortfall_tol50)/n(),
              shortfall_tol25 = sum(shortfall_tol25)/n(),
              shortfall_tol10 = sum(shortfall_tol10)/n(),
              shortfall_tol05 = sum(shortfall_tol05)/n())

write.csv(shortfallDF, file = paste0("harvestShortfallPercentiles_", scenario, ".csv"), row.names = F)

vars <- colnames(shortfallDF)
vars <- vars[grep("shortfall", vars)]

riskTol <- paste0(as.numeric(gsub("[^0-9]","", vars)), "%")
df <- shortfallDF
## reformating in tall form
require(reshape2)
df <- melt(shortfallDF, id.vars = c("scenario", "year"),
           measure.vars = vars)


df$variable <- as.numeric(gsub("[^0-9]", "",  df$variable))
df$variable <- paste0(df$variable, "%")
df$variable <- factor(df$variable, levels = c("5%", "10%", "25%", "50%", "75%"))


m <- ggplot(df, aes(x = year + 2015, y = value*100,
                    linetype = variable)) +
    geom_line(size = 0.5) +
    scale_linetype_manual("Écart toléré\n",
                    values = seq_along(levels(df$variable)))


png(filename= paste0("harvestShortfall.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)
options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Analyse de risque de rupture d'approvisionnement",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = paste0("Probabilité de ne pas atteindre la cible de récolte au moins une fois en fonction de différents niveaux de\n",
                                 "tolérance par rapport à cette cible - estimé sur la base d'un ensemble de ", nSims, " simulations."),
               caption = paste0("Âge min. de récolte (sauf récup.) - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Vol. marchand min.: 50 m3/ha (Récup.: 70 m3/ha)\n",
                                
                                "Cycle des feux - baseline: 104 ans\n"),
               #"Min vieilles forêts (>=100 ans): 14%\n",
               #"Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Probabilité de rupture\nd'approvisionnement (%)\n"))


dev.off()







