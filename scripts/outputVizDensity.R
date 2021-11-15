###################################################################################################
###################################################################################################
##### Visualizing density
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-08-24")
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
######
### fetching compiled results
outputCompiled <- get(load("outputCompiledDensity.RData"))

totalAreaSubZone <- outputCompiled %>%
    group_by(scenario, coverTypes, subZone, year, replicate) %>%
    summarize(areaTotal_ha = sum(area_ha)) %>%
    group_by(scenario, subZone, coverTypes) %>%
    summarise(areaTotal_ha = unique(areaTotal_ha))

totalArea <- totalAreaSubZone %>%
    group_by(scenario, coverTypes) %>%
    summarize(areaTotal_ha = sum(areaTotal_ha)) %>%
    mutate(subZone = "Total") %>%
    rbind(totalAreaSubZone)


nSims <- nrow(distinct(outputCompiled, scenario, replicate))

### summarizing results, percentile & such
outputCompiled <- filter(outputCompiled, subZone %in% c("Productive forest - harvestable",
                                                        "Conservation areas"),
                         coverTypes %in% c("EN", "PG"))


dfTotal <- outputCompiled %>%
    group_by(scenario, replicate, coverTypes, year, dens) %>%
    summarize(area_ha = sum(area_ha)) %>%
    group_by(scenario, coverTypes, year, dens) %>%
    summarize(p25 = quantile(area_ha, 0.25),
              p50 = median(area_ha),
              p75 = quantile(area_ha, 0.75)) %>%
    mutate(subZone = "Total")
dfTotal$year <- as.numeric(as.character(dfTotal$year))

df <- outputCompiled %>%
    group_by(scenario, subZone, coverTypes, year, dens) %>%
    summarize(p25 = quantile(area_ha, 0.25),
              p50 = median(area_ha),
              p75 = quantile(area_ha, 0.75))
df$year <- as.numeric(as.character(df$year))

df <- rbind(df, dfTotal)
df$subZone <- factor(df$subZone, levels = unique(df$subZone))

df <- merge(df, totalArea)

##############################################################################
### Plotting density area
require(RColorBrewer)
cols <- c(rev(brewer.pal(4, "Greens")[2:3]), "orange", "red")
#######


m <- ggplot(df, aes(x = year + 2015, y = p50,
                    colour = dens, fill = dens)) +
    facet_grid(coverTypes ~ subZone, scales = "free_y") +
    geom_line(size = 0.5) +
    # scale_linetype_manual("Percentiles\n",
    #                   values =seq_along(df$prob)) +
    geom_ribbon(aes(ymin = p25, ymax = p75), alpha=0.3,
                colour = NA) +
    guides(linetype = guide_legend(reverse=T)) +
    scale_colour_manual(name = "", values = cols) +
    scale_fill_manual(name = "", values = cols)


png(filename= paste0("densityArea.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Évolution de la densité des peuplements à l'échelle du paysage.",
               subtitle = paste0("Les courbes représentent les superficies médianes et les percentile 25% et 75% estimés sur la base d'un ensemble\n",
                                 "de ", nSims, " simulations. Seuls les peuplements initiallement productifs sont comptabilisés."),
               caption = paste0("Âge de récolte - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Cycle des feux - baseline: 101 ans\n"),
               x = "",
               y = "Superficie (ha)\n"))

dev.off()


##############################################################################
### Plotting density proportions
#######
require(RColorBrewer)

m <- ggplot(df, aes(x = year + 2015, y = 100*p50/areaTotal_ha,
                    colour = dens, fill = dens)) +
    facet_grid(coverTypes ~ subZone, scales = "free_y") +
    geom_line(size = 0.5) +
    # scale_linetype_manual("Percentiles\n",
    #                   values =seq_along(df$prob)) +
    geom_ribbon(aes(ymin = 100*p25/areaTotal_ha, ymax = 100*p75/areaTotal_ha), alpha=0.3,
                colour = NA) +
    guides(linetype = guide_legend(reverse=T)) +
    scale_colour_manual(name = "", values = cols) +
    scale_fill_manual(name = "", values = cols)


png(filename= paste0("densityProp.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(m + theme_dark() +
          
          theme(legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Évolution de la densité des peuplements à l'échelle du paysage.",
               subtitle = paste0("Les courbes représentent les proportions du territoire médianes et les percentile 25% et 75% estimés sur la base d'un ensemble\n",
                                 "de ", nSims, " simulations. Seuls les peuplements initiallement productifs sont comptabilisés."),
               caption = paste0("Âge de récolte - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Cycle des feux - baseline: 101 ans\n"),
               x = "",
               y = "Proportion du territoire (%)\n"))

dev.off()

