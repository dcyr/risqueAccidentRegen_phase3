###################################################################################################
###################################################################################################
##### Visualizing the evolution volumes at 120 y.old (indicator of landscape productivity)
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-23")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
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
subZone <- raster("../subZones.tif")
subZone_RAT <- read.csv("../subZones_RAT.csv")
##

# uaf <- raster("../uaf.tif")
# uaf_RAT <- read.csv("../uaf_RAT.csv")

####################################################################
####################################################################
######
### fetching compiled results
outputCompiled <- get(load(paste0("outputCompiledVolAt120Cls_", scenario, ".RData")))

nSims <- nrow(distinct(outputCompiled, scenario, replicate))

### summarizing results, percentile & such
# outputCompiled <- filter(outputCompiled, uaf != "total")
outputCompiled[is.na(outputCompiled$volAt120Cls), "volAt120Cls"] <- "[0,50)"


df <- outputCompiled %>%
    group_by(scenario, replicate, year, coverTypes, subZone) %>%
    mutate(areaTotal_ha = sum(area_ha),
           zone = ifelse(subZone %in% c("Conservation areas", "Productive forest - Not harvestable"),
                                "Ineligible to harvest", "Eligible to harvest")) %>%
    
    ungroup() %>%
    mutate(area_prop = area_ha/areaTotal_ha) %>%
    group_by(scenario, year, coverTypes, zone, volAt120Cls) %>%
    #summarise(areaHarvestedTotal_ha = sum(areaHarvestedTotal_ha)) %>%
    #group_by(scenario, year) %>%
    summarise(#p01HarvestProp = quantile(areaHarvestedTotal_ha, .01),
              #p05HarvestProp = quantile(areaHarvestedTotal_ha, .05),
              p25VolAt120Area_ha = quantile(area_ha, .025),
              p50VolAt120Area_ha = quantile(area_ha, .5),
              p75VolAt120Area_ha = quantile(area_ha, .975),
              p25VolAt120Area_prop = quantile(area_ha/areaTotal_ha, .025),
              p50VolAt120Area_prop = quantile(area_ha/areaTotal_ha, .5),
              p75VolAt120Area_prop = quantile(area_ha/areaTotal_ha, .075)
              #p95HarvestProp = quantile(areaHarvestedTotal_ha, .95),
              #p99HarvestProp = quantile(areaHarvestedTotal_ha, .99)
              )

#### testing outputs
require(ggplot2)


png(filename= paste0("volAt120.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = df, aes(x = 2015 + year, y = p50VolAt120Area_ha, colour = volAt120Cls)) +
    geom_line() +
   # ?geom_ribbon()
    geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
                        x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverTypes ~ zone, scales = "free_y") +
    labs(x = "",
         y = "Area (ha)") +
    scale_colour_manual("Vol. at 120 y.old\n(median value)",
                        values= c("skyblue4", "darkgreen", "indianred4")) +
    scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
           axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()




#########################################################
### variations 

diffDf <- df %>%
    group_by(scenario, coverTypes, zone, volAt120Cls) %>%
    arrange(year) %>%
    mutate(p50areaVariation_ha = c(NA, diff(p50VolAt120Area_ha)),
           p50areaVariation_prop = c(NA, diff(p50VolAt120Area_prop))) %>%
    ungroup()# %>%


png(filename= paste0("volAt120Variations.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = diffDf, aes(x = 2015 + year, y = p50areaVariation_prop * 100, colour = volAt120Cls)) +
    # geom_line() +
    geom_smooth(span = 0.1, size = 0.6) +
    # ?geom_ribbon()
    # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
    #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverTypes ~ zone, scales = "free_y") +
    labs(x = "",
         y = "Annual variation\n(% area)") +
    scale_colour_manual("Vol. at 120 y.old",values= c("skyblue4", "darkgreen", "indianred4")) +
    #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()


#########################################################
### cumul

cumulDf <- diffDf %>%
    mutate(p50areaVariation_ha = ifelse(is.na(p50areaVariation_ha), 0, p50areaVariation_ha),
           p50areaVariation_prop = ifelse(is.na(p50areaVariation_prop), 0, p50areaVariation_prop)) %>%
    group_by(scenario, coverTypes, zone, volAt120Cls) %>%
    arrange(year) %>%
    mutate(p50areaCumul_ha = cumsum(p50areaVariation_ha),
           p50areaCumul_prop = cumsum(p50areaVariation_prop)) %>%
    ungroup()# %>%


png(filename= paste0("volAt120CumulativeProp.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = cumulDf, aes(x = 2015 + year, y = p50areaCumul_prop * 100, colour = volAt120Cls)) +
    geom_line() +
    #geom_smooth(span = 0.4) +
    # ?geom_ribbon()
    # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
    #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverTypes ~ zone) +#, scales = "free_y") +
    labs(x = "",
         y = "Cumulative variation\n(% area)") +
    scale_colour_manual("Vol. at 120 y.old",values= c("skyblue4", "darkgreen", "indianred4")) +
    #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()


png(filename= paste0("volAt120CumulativeHa.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = cumulDf, aes(x = 2015 + year, y = p50areaCumul_ha, colour = volAt120Cls)) +
    #geom_line() +
    geom_smooth(span = 0.25) +
    # ?geom_ribbon()
    # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
    #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverTypes ~ zone, scales = "fixed") +#, scales = "free_y") +
    labs(x = "",
         y = "Cumulative variation\n(ha)") +
    scale_colour_manual("Vol. at 120 y.old",values= c("skyblue4", "darkgreen", "indianred4")) +
    #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()




