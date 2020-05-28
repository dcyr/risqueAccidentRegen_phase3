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
output[,"plantLimitedAccess"] <- simInfo$plantLimitedAccess[match(output$simID, simInfo$simID)]


areaTotal <- output %>%
  group_by(simID, fireScenario, mgmtScenario, replicate, year, 
                     clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(areaTotal = sum(area_ha))
areaTotal <- unique(areaTotal$areaTotal)
  

outputCls <- output %>%
  select(simID, fireScenario, mgmtScenario,
         replicate, year, coverType,  volAt120Cls, area_ha,
         clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  as.data.frame()
  
outputMean <- output %>% 
  group_by(simID, fireScenario, mgmtScenario,
           replicate, year,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(volAt120Mean_tonnesPerHa = unique(volAt120_totalLandscapeAverage)) %>%
  as.data.frame()


write.csv(outputCls, file = "outputCompiledVolAt120Cls.csv", row.names = F)
write.csv(outputMean, file = "outputCompiledVolAt120Mean.csv", row.names = F)

mean_summary <- outputMean %>%
  filter(year == 100) %>%
  group_by(simID, fireScenario, mgmtScenario,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(volAt120p25_tonnesPerHa = quantile(volAt120Mean_tonnesPerHa, 0.25),
            volAt120p75_tonnesPerHa = quantile(volAt120Mean_tonnesPerHa, 0.75),
            volAt120Mean_tonnesPerHa = mean(volAt120Mean_tonnesPerHa)) %>%
  as.data.frame()


#lvls <- levels(outputCompiled$volAt120Cls)
#outputCompiled[outputCompiled$volAt120Cls == "N/A", "volAt120Cls"] <- lvls[1]

nSims <- nrow(distinct(outputCompiled, fireScenario, mgmtScenario, replicate))


df <- output %>%
    filter(!is.na(volAt120Cls)) %>%
   # mutate(volAt120Cls = paste(volAt120Cls, "cub-meters at 120 y.old") ) %>%
    group_by(simID, fireScenario, mgmtScenario, replicate, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
    summarise(area_ha = sum(area_ha)) %>%
    ungroup() %>%
    group_by(simID, fireScenario, mgmtScenario, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
    summarise(p25VolAt120Area_ha = quantile(area_ha, .25),
              p50VolAt120Area_ha = quantile(area_ha, .5),
              p75VolAt120Area_ha = quantile(area_ha, .75)) 

dfMean <- output %>%
  filter(!is.na(volAt120Cls)) %>%
  group_by(simID, fireScenario, mgmtScenario, year, replicate,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
  summarise(volAt120_totalLandscapeAverage = unique(volAt120_totalLandscapeAverage)) %>%
  ungroup() %>%
  group_by(simID, fireScenario, mgmtScenario, year, 
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
  mutate(p25VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .25),
         p50VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .5),
         p75VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .75)) 

#### testing outputs
require(ggplot2)
library(ggpubr)

colList <- list("Harvesting Regime" = c("No harvests / No post-fire interventions" = "seagreen",
                                        "Clearcutting" = "orangered4",
                                        "Variable retention harvest" = "dodgerblue4"),
                "Post-Fire Intervention" = c("No harvests / No post-fire interventions" = "seagreen",
                                             "Harvests / No post-fire interventions" = "dodgerblue4",
                                             "No salv. logging / Post-fire planting" = "orangered2",
                                             "Salv. logging / Post-fire planting" = "orangered4"),
                "Planted species" = c("Pre-fire composition (<2 km)" = "darkslategrey",
                                      "Pre-fire composition" = "darkslategray3",
                                      "Jack Pine (<2 km)" = "tan",
                                      "Jack Pine" = "peru",
                                      "N/A (No planting)" = "grey"))

yMax <- 5*(ceiling(  100*max(df$p50VolAt120Area_ha/areaTotal)/5))

pCls <- pMean <- list()

for (i in seq_along(colList)) {
    options(scipen=999)
    cols <- colList[[i]]
    labels <- names(cols)
    plotName <- names(colList)[[i]]
    if(i == 1) {
        plotDf <- df %>%
            mutate(plotLvl = ifelse(!clearcutting & !varReten, labels[1],
                                    ifelse(clearcutting, labels[2],
                                           ifelse(varReten, labels[3], NA))),
                   plotLvl = factor(plotLvl, levels = labels))
        
        plotMean <- dfMean %>%
          mutate(plotLvl = ifelse(!clearcutting & !varReten, labels[1],
                                  ifelse(clearcutting, labels[2],
                                         ifelse(varReten, labels[3], NA))),
                 plotLvl = factor(plotLvl, levels = labels))

    }
    if(i == 2) {
        plotDf <- df %>%
            mutate(plotLvl = ifelse(salv, labels[4],
                                    ifelse(!salv & plantPostFire, labels[3],
                                           ifelse((varReten | clearcutting) & !plantPostFire, labels[2],
                                                  ifelse(!(varReten | clearcutting), labels[1], NA)))),
                   plotLvl = factor(plotLvl, levels = labels))
        plotMean <- dfMean %>%
          mutate(plotLvl = ifelse(salv, labels[4],
                                  ifelse(!salv & plantPostFire, labels[3],
                                         ifelse((varReten | clearcutting) & !plantPostFire, labels[2],
                                                ifelse(!(varReten | clearcutting), labels[1], NA)))),
                 plotLvl = factor(plotLvl, levels = labels)) 
              
                                           
        #summary(plotDf$plotLvl)
    }
    
    if(i == 3) {
        plotDf <- df %>%
          mutate(plotLvl = ifelse(is.na(plantPostFireSp), labels[5],
                                  ifelse(plantPostFireSp == "PG",
                                         ifelse(plantLimitedAccess, labels[3], labels[4]),
                                         ifelse(plantPostFireSp == "same",
                                                ifelse(plantLimitedAccess, labels[1], labels[2]),
                                                NA))),
                 plotLvl = factor(plotLvl, levels = labels)) %>%
          filter(!is.na(plotLvl)) 
        
        plotMean <- dfMean %>%
          mutate(plotLvl = ifelse(is.na(plantPostFireSp), labels[5],
                                  ifelse(plantPostFireSp == "PG",
                                         ifelse(plantLimitedAccess, labels[3], labels[4]),
                                         ifelse(plantPostFireSp == "same",
                                                ifelse(plantLimitedAccess, labels[1], labels[2]), NA))),
                 plotLvl = factor(plotLvl, levels = labels)) %>%
          filter(!is.na(plotLvl))

    }
    
    
   pCls[[i]] <- ggplot(data = plotDf, aes(x = yearInit + year,
                                          y = 100 * p50VolAt120Area_ha/areaTotal,
                               colour = plotLvl, linetype = fireScenario,
                               group = simID)) +
     facet_wrap(~ volAt120Cls, ncol = length(levels(plotDf$volAt120Cls))) +
     geom_line(size = 0.75, alpha = 0.75) +
     #facet_wrap(~ volAt120Cls, ncol = length(levels(plotDf$volAt120Cls))) +
     labs(subtitle = paste0(letters[i], ") ", plotName),
          x = "",
          y = "Proportion of productive area (%)\n") +
     scale_colour_manual(plotName,
                         values= cols,
                         guide = guide_legend(nrow = length(unique(plotMean$plotLvl)))) +
     scale_linetype_manual("Climate Change Scenario",
                           values = c("Baseline" = 1,
                                      "RCP 8.5" = 2),
                           guide = "none") +
     ylim(0, yMax) +
     theme_bw() +
     theme(legend.position = "right",
           legend.box="horizontal",
           legend.title=element_blank(),
           axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    # png(filename= paste0("volAt120_cls_", gsub(" ", "", plotName), ".png"),
    #     width = 8, height = 4, units = "in", res = 600, pointsize=10)
    # 
    # print(p +
    #           theme_light() +
    #           theme(strip.text.x = element_text(size = 7),
    #                 axis.text.x = element_text(angle = 45, hjust = 1)))
    # 
    # 
    # dev.off()
    
    
    pMean[[i]] <- ggplot(data = plotMean, aes(x = yearInit + year,
                                         y = p50VolAt120Mean_cubMeter,
                                   colour = plotLvl, linetype = fireScenario,
                                   group = simID)) +
      geom_line(size = 0.75, alpha = 0.75) +
      #facet_wrap(~ volAt120Cls, ncol = length(levels(plotDf$volAt120Cls))) +
      labs(subtitle = paste0(letters[i], ") ", plotName),
           x = "",
           y = "Average merch. volume at 120 y.old\n") +
      scale_colour_manual(plotName,
                          values= cols,
                          guide = guide_legend(nrow = length(unique(plotMean$plotLvl)))) +
      scale_linetype_manual("Climate Change Scenario",
                            values = c("Baseline" = 1,
                                       "RCP 8.5" = 2),
                            guide = "none") +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.box="vertical",
            legend.title=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
      
    
    if(i > 1) {
      pMean[[i]] <-  pMean[[i]] +
        scale_linetype_manual("Climate Change Scenario",
                              values = c("Baseline" = 1,
                                         "RCP 8.5" = 2),
                              guide = "none") +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    if(i < 3) {
      pCls[[i]] <-  pCls[[i]] + 
        scale_linetype_manual("Climate Change Scenario",
                              values = c("Baseline" = 1,
                                         "RCP 8.5" = 2),
                              guide = "none") +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank())
      
    }
    if(i != 2) {
      pCls[[i]] <-  pCls[[i]] +
        theme(axis.title.y = element_blank())
      
    }
}
  

require(patchwork)

png(filename= paste0("volAt120_cls.png"),
    width = 14, height = 12, units = "in", res = 600, pointsize=10)

print(pCls[[1]] / pCls[[2]] / pCls[[3]] +
        plot_annotation(
          title = "Evolution of stand productivity classes at the landscape level",
          subtitle = "Full lines and dotted lines represent 'baseline' and 'RCP 8.5' climate scenarios, respectively.",
          caption = paste0("All panels represents the same simulations, deciphering them based on different factors.",
                           "\nEach line represent the average of 5 replicates."),
          #y = expression(paste("mean merch. vol at 120 y.old","(m"^"3","ha"^"-1",")")),
          theme = theme(
            plot.title = element_text(size = rel(1.5)),
            plot.caption = element_text(size = rel(.8), hjust = 0))
          
        )
)
dev.off()


png(filename= paste0("volAt120_mean.png"),
    width = 10, height = 6, units = "in", res = 600, pointsize=10)

print(pMean[[1]] + pMean[[2]] + pMean[[3]] +
        plot_annotation(
          title = "Evolution of average potential productivity at the landscape level",
          subtitle = "Full lines and dotted lines represent 'baseline' and 'RCP 8.5' climate scenarios, respectively.",
          caption = paste0("All panels represents the same simulations, deciphering them based on different factors.",
                           "\nEach line represent the average of 5 replicates."),
          theme = theme(
            plot.title = element_text(size = rel(1.5)),
            plot.caption = element_text(size = rel(.8), hjust = 0))
            
          )
        )
dev.off()

# #########################################################
# ### variations 
# 
# diffDf <- df %>%
#     group_by(scenario, coverType,volAt120Cls) %>%
#     arrange(year) %>%
#     mutate(p50areaVariation_ha = c(NA, diff(p50VolAt120Area_ha)),
#            p50areaVariation_prop = c(NA, diff(p50VolAt120Area_prop))) %>%
#     ungroup()# %>%
# 
# 
# png(filename= paste0("volAt120Variations.png"),
#     width = 8, height = 4, units = "in", res = 600, pointsize=10)
# 
# options(scipen=999)
# 
# 
# ggplot(data = diffDf, aes(x = 2015 + year, y = p50areaVariation_prop * 100, colour = volAt120Cls)) +
#     # geom_line() +
#     geom_smooth(span = 0.1, size = 0.6) +
#     
#     facet_grid(coverType ~ scenario, scales = "free_y") +
#     xlim(c(2015, 2065)) +
#     #facet_grid( ~ coverType, scales = "free_y") +
#     labs(x = "",
#          y = "Annual variation\n(% area)") +
#     scale_colour_manual("Vol. at 120 y.old",values= cols) +
#     #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
#     theme(strip.text.x = element_text(size = 7),
#           axis.text.x = element_text(angle = 45, hjust = 1)) +
#     geom_hline(yintercept = 0, colour = "black", linetype = "dashed",
#                 size = 0.5)
# 
# dev.off()
# 
# 
# #########################################################
# ### cumul
# 
# cumulDf <- diffDf %>%
#     mutate(p50areaVariation_ha = ifelse(is.na(p50areaVariation_ha), 0, p50areaVariation_ha),
#            p50areaVariation_prop = ifelse(is.na(p50areaVariation_prop), 0, p50areaVariation_prop)) %>%
#     group_by(scenario, coverType, volAt120Cls) %>%
#     arrange(year) %>%
#     mutate(p50areaCumul_ha = cumsum(p50areaVariation_ha),
#            p50areaCumul_prop = cumsum(p50areaVariation_prop)) %>%
#     ungroup()# %>%
# 
# 
# png(filename= paste0("volAt120CumulativeProp.png"),
#     width = 8, height = 4, units = "in", res = 600, pointsize=10)
# 
# options(scipen=999)
# 
# 
# ggplot(data = cumulDf, aes(x = 2015 + year, y = p50areaCumul_prop * 100, colour = volAt120Cls)) +
#     geom_hline(yintercept = 0,
#                linetype = "dashed",
#                colour = "black",
#                size = 1) +
#     #geom_smooth(span = 0.4) +
#     # ?geom_ribbon()
#     # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
#     #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
#     facet_grid(coverType ~ scenario, scales = "fixed") +
#     labs(x = "",
#          y = "Cumulative variation\n(% area)") +
#     scale_colour_manual("Vol. at 120 y.old",values= cols) +
#     #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
#     theme(strip.text.x = element_text(size = 7),
#           axis.text.x = element_text(angle = 45, hjust = 1)) +
#     geom_line()
# 
# dev.off()
# 
# 
# png(filename= paste0("volAt120CumulativeHa.png"),
#     width = 8, height = 4, units = "in", res = 600, pointsize=10)
# 
# options(scipen=999)
# 
# 
# ggplot(data = cumulDf, aes(x = 2015 + year, y = p50areaCumul_ha, colour = volAt120Cls)) +
#     geom_hline(yintercept = 0,
#                linetype = "dashed",
#                colour = "black",
#                size = 0.5) +
#     geom_smooth(span = 0.25) +
#     # ?geom_ribbon()
#     # geom_ribbon(aes(ymin = p25VolAt120Area_ha, ymax=p75VolAt120Area_ha,
#     #                 x=  2015 + year, fill = df$volAt120Cls), alpha = 0.25, colour = NA) +
#     facet_grid(coverType ~ scenario, scales = "fixed") +
#     #facet_grid(coverTypes ~ zone, scales = "fixed") +#, scales = "free_y") +
#     labs(x = "",
#          y = "Cumulative variation\n(ha)") +
#     scale_colour_manual("Vol. at 120 y.old",values= cols) +
#     #scale_fill_manual("I.C.95% ",values= c("skyblue4", "darkgreen", "indianred4")) +
#     theme(strip.text.x = element_text(size = 7),
#           axis.text.x = element_text(angle = 45, hjust = 1)) 
# 
# dev.off()




