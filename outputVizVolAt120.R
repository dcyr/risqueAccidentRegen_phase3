###################################################################################################
###################################################################################################
##### Visualizing harvest simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo"))])
#################
require(raster)
require(ggplot2)
require(dplyr)
require(reshape2)
yearInit <- simInfo$initYear
output <- list()
for (s in seq_along(simInfo$simID)) {
    simID <- simInfo$simID[s]
    output[[s]] <-  get(load(paste0("../outputCompiled/outputCompiledVolAt120_", simID, ".RData")))
}

output <- do.call("rbind", output)

output[,"clearcutting"] <- simInfo$clearcutting[match(output$simID, simInfo$simID)]
output[,"varReten"] <- simInfo$varReten[match(output$simID, simInfo$simID)]
output[,"salv"] <- simInfo$salv[match(output$simID, simInfo$simID)]
output[,"plantPostFire"] <- simInfo$plantPostFire[match(output$simID, simInfo$simID)]
output[,"plantPostSalv"] <- simInfo$plantPostSalv[match(output$simID, simInfo$simID)]
output[,"plantPostFireSp"] <- simInfo$plantPostFireSp[match(output$simID, simInfo$simID)]


#lvls <- levels(outputCompiled$volAt120Cls)
#outputCompiled[outputCompiled$volAt120Cls == "N/A", "volAt120Cls"] <- lvls[1]

nSims <- nrow(distinct(outputCompiled, fireScenario, mgmtScenario, replicate))


df <- output %>%
    filter(!is.na(volAt120Cls)) %>%
   # mutate(volAt120Cls = paste(volAt120Cls, "cub-meters at 120 y.old") ) %>%
    group_by(simID, fireScenario, mgmtScenario, replicate, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
    summarise(area_ha = sum(area_ha)) %>%
    ungroup() %>%
    group_by(simID, fireScenario, mgmtScenario, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
    summarise(p25VolAt120Area_ha = quantile(area_ha, .25),
              p50VolAt120Area_ha = quantile(area_ha, .5),
              p75VolAt120Area_ha = quantile(area_ha, .75)) 

#### testing outputs
require(ggplot2)
colList <- list("Harvesting Regime" = c("No harvest" = "seagreen",
                                        "Clearcutting" = "orangered4",
                                        "Variable retention harvest" = "dodgerblue4"),
                "Post-Fire Intervention" = c("No post-fire interventions, no harvest" = "darkslategrey",
                                             "No post-fire interventions, with harvest" = "peru",
                                             "Salvage logging, with no interventions on non-salvaged sites" = "firebrick2",
                                             "Planting on all unsalvaged low regen. burnt sites" = "firebrick4"),
                "Planted species" = c("Return to pre-fire composition" = "darkslategrey",
                                      "Jack Pine" = "peru"))


require(patchwork)

p <- list()

for (i in seq_along(colList)) {
    options(scipen=999)
    cols <- colList[[i]]
    labels <- names(cols)
    plotName <- names(colList)[[i]]
    if(i == 1) {
        plotDf <- df %>%
            mutate(plotLvl = ifelse(!clearcutting & !varReten, "No harvest",
                                    ifelse(clearcutting, "Clearcutting",
                                           ifelse(varReten, "Variable retention harvest", NA))),
                   plotLvl = factor(plotLvl, levels = labels))
    }
    if(i == 2) {
        plotDf <- df %>%
            mutate(plotLvl = ifelse(plantPostFire, labels[4],
                                    ifelse(salv, labels[3],
                                           ifelse(varReten | clearcutting, labels[2],
                                                  labels[1]))),
                   plotLvl = factor(plotLvl, levels = labels))
                                    
              
                                           
            table(foo)
    }
    
    
    foo = ifelse(df$salv & !df$plantPostSalv & !df$plantPostFire,  "Salvage logging with no plantation",
                     ifelse(df$salv & df$plantPostSalv & !df$plantPostFire, "Salvage logging with plantation",
                            ifelse(df$plantPostFire, "Post-fire plant. on all low regen.burnt sites", NA)))
    
    p <- ggplot(data = plotDf, aes(x = yearInit + year, y = p50VolAt120Area_ha,
                               colour = plotLvl, linetype = fireScenario,
                               group = simID)) +
        geom_line(size = 0.75, alpha = 0.85) +
        facet_wrap(~ volAt120Cls, ncol = length(levels(plotDf$volAt120Cls))) +
        labs(title = "Impact of harvesting regimes",
             x = "",
             y = "Area (ha)") +
        scale_colour_manual(plotName,
                            values= cols) +
        scale_linetype_manual("Climate Change Scenario",
                              values = c("Baseline" = 1,
                                         "RCP 8.5" = 2))
    
    
    png(filename= paste0("volAt120_", gsub(" ", "", plotName), ".png"),
        width = 8, height = 4, units = "in", res = 600, pointsize=10)
    
    print(p +
              theme_light() +
              theme(strip.text.x = element_text(size = 7),
                    axis.text.x = element_text(angle = 45, hjust = 1)))
    
    
    dev.off()
}
    
    
    
    
    
}






#########################################################
### variations 

diffDf <- df %>%
    group_by(scenario, coverType,volAt120Cls) %>%
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
    
    facet_grid(coverType ~ scenario, scales = "free_y") +
    xlim(c(2015, 2065)) +
    #facet_grid( ~ coverType, scales = "free_y") +
    labs(x = "",
         y = "Annual variation\n(% area)") +
    scale_colour_manual("Vol. at 120 y.old",values= cols) +
    #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed",
                size = 0.5)

dev.off()


#########################################################
### cumul

cumulDf <- diffDf %>%
    mutate(p50areaVariation_ha = ifelse(is.na(p50areaVariation_ha), 0, p50areaVariation_ha),
           p50areaVariation_prop = ifelse(is.na(p50areaVariation_prop), 0, p50areaVariation_prop)) %>%
    group_by(scenario, coverType, volAt120Cls) %>%
    arrange(year) %>%
    mutate(p50areaCumul_ha = cumsum(p50areaVariation_ha),
           p50areaCumul_prop = cumsum(p50areaVariation_prop)) %>%
    ungroup()# %>%


png(filename= paste0("volAt120CumulativeProp.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = cumulDf, aes(x = 2015 + year, y = p50areaCumul_prop * 100, colour = volAt120Cls)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               colour = "black",
               size = 1) +
    #geom_smooth(span = 0.4) +
    # ?geom_ribbon()
    # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
    #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverType ~ scenario, scales = "fixed") +
    labs(x = "",
         y = "Cumulative variation\n(% area)") +
    scale_colour_manual("Vol. at 120 y.old",values= cols) +
    #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_line()

dev.off()


png(filename= paste0("volAt120CumulativeHa.png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)

options(scipen=999)


ggplot(data = cumulDf, aes(x = 2015 + year, y = p50areaCumul_ha, colour = volAt120Cls)) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               colour = "black",
               size = 0.5) +
    geom_smooth(span = 0.25) +
    # ?geom_ribbon()
    # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
    #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
    facet_grid(coverType ~ scenario, scales = "fixed") +
    #facet_grid(coverTypes ~ zone, scales = "fixed") +#, scales = "free_y") +
    labs(x = "",
         y = "Cumulative variation\n(ha)") +
    scale_colour_manual("Vol. at 120 y.old",values= cols) +
    #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
    theme(strip.text.x = element_text(size = 7),
          axis.text.x = element_text(angle = 45, hjust = 1)) 

dev.off()




