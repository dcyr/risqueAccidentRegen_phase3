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

cNames <- colnames(outputMean)


mean_summary <- outputMean %>%
  filter(year == 0) %>%
  group_by(simID, fireScenario, mgmtScenario,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(volAt120p25_tonnesPerHa = quantile(volAt120Mean_tonnesPerHa, 0.25),
            volAt120p75_tonnesPerHa = quantile(volAt120Mean_tonnesPerHa, 0.75),
            volAt120Mean_tonnesPerHa = mean(volAt120Mean_tonnesPerHa)) %>%
  as.data.frame()


deltaMeanVol <- mean_summary[,c("simID", "volAt120Mean_tonnesPerHa")] %>%
  mutate(ref = volAt120Mean_tonnesPerHa[which(simID == "01")],
         delta = volAt120Mean_tonnesPerHa - ref) %>%
  select(simID, delta)

deltaClsVol <- outputCls %>%
  filter(year == 0) %>%
  group_by(simID, volAt120Cls) %>%
  summarise(area_ha = mean(area_ha)) %>%
  ungroup() %>%
  group_by(volAt120Cls) %>%
  mutate(ref = area_ha[which(simID == "01")],
         delta = area_ha - ref) %>%
  select(simID, volAt120Cls, delta) %>%
  as.data.frame() 



outputMean <- outputMean %>%
  merge(deltaMeanVol) %>%
  mutate(volAt120Mean_tonnesPerHa = volAt120Mean_tonnesPerHa - delta)

outputMean <- outputMean[,cNames]


### adjusting for slight discrepancies in initial conditions
write.csv(outputCls, file = "outputCompiledVolAt120Cls.csv", row.names = F)
write.csv(outputMean, file = "outputCompiledVolAt120Mean.csv", row.names = F)

  
  


#lvls <- levels(outputCompiled$volAt120Cls)
#outputCompiled[outputCompiled$volAt120Cls == "N/A", "volAt120Cls"] <- lvls[1]

#nSims <- nrow(distinct(outputCompiled, fireScenario, mgmtScenario, replicate))


df <- output %>%

  merge(deltaClsVol) %>%
  filter(!is.na(volAt120Cls)) %>%
  mutate(area_ha =  area_ha - delta) %>%
    group_by(simID, fireScenario, mgmtScenario, replicate, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
    summarise(area_ha = sum(area_ha)) %>%
    ungroup() %>%
    group_by(simID, fireScenario, mgmtScenario, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
    summarise(meanVolAt120Area_ha = mean(area_ha),
              p25VolAt120Area_ha = quantile(area_ha, .25),
              p50VolAt120Area_ha = quantile(area_ha, .5),
              p75VolAt120Area_ha = quantile(area_ha, .75))

dfMean <- output %>%
  filter(!is.na(volAt120Cls)) %>%
  merge(deltaMeanVol) %>%
  mutate(volAt120_totalLandscapeAverage = volAt120_totalLandscapeAverage - delta) %>%
  group_by(simID, fireScenario, mgmtScenario, year, replicate,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
  summarise(volAt120_totalLandscapeAverage = unique(volAt120_totalLandscapeAverage)) %>%
  ungroup() %>%
  group_by(simID, fireScenario, mgmtScenario, year, 
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
  mutate(p25VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .25),
         p50VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .5),
         p75VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .75),
         meanVolAt120Mean_cubMeter = mean(volAt120_totalLandscapeAverage)) 


#### testing outputs
require(ggplot2)
library(ggpubr)
require(patchwork)





################################################################################
#### Baseline
################ 


colList <- list("Clearcutting" =
                  list(scenario = "Baseline",
                       simID = c("01", "02"),
                       labels = c("No harvests",
                                  "Clearcutting only",
                                  NA),
                       caption = c("Note: Historical fire (0.99% / yr) and harvesting levels (0.62% / yr)\n\n\n"),
                       colour =c("seagreen", "black", "black")),
                "Salvage logging" =
                  list(scenario = "Baseline",
                       simID = c("02", "15", "06"),
                       labels = c("Clearcutting only", 
                                  "+ salv. logging (+ planting, post-salv. only)",
                                  "+ salv. logging (+ planting, all post-fire regen. failure)",
                                  NA),
                       caption = c("Note: Planting in all post-fire regen. failure, salvaged or not,\nwithin a 2-km distance from current access network\n\n"),
                       colour =c("black", "orangered2", "orange2", "black")),
                "Variable retention" =
                  list(scenario = "Baseline",
                       simID = c("02", "13", "03", "14"),
                       labels = c("Clearcutting only", 
                                  "Clearcutting (+ salv. logging)",
                                  "Var. retention (no salv. logging)",
                                  "Var. retention (+ salv. logging)",
                                  NA),
                       caption = c("Note: No planting\n\n\n"),
                       colour =c("black", "firebrick2",  "mediumpurple4","mediumpurple1", "black")),
                "Planting (with access constrains)" =
                  list(scenario = "Baseline",
                       simID = c("02", "04", "05"),
                       labels = c("Clearcutting only",
                                  "+ planting same sp",
                                  "+ planting Jack Pine",
                                  NA),
                       caption = c("Note: With salvage logging and planting in all post-fire regen failure\nwithin a 2-km distance from current access network\n\n"),
                       colour =c("black", "darkseagreen3", "burlywood3", "black")),
                "Salv. logging + planting (no access constrains)" =
                  list(scenario = "Baseline",
                       simID = c("02", "13", "17", "18"),
                       labels = c("Clearcutting only",
                                  "+ salv. logging",
                                  "+ salv. logging & planting same sp",
                                  "+ salv. logging & planting Jack Pine",
                                  NA),
                       caption = c("Note: With salvage logging, planting in all post-fire\nregen failure; no access constrains\n\n"),
                       colour =c("black","firebrick2", "darkseagreen4", "burlywood4", "black")),
                "Salv. logging & var. retention + planting (with access constrains)" =
                  list(scenario = "Baseline",
                       simID = c("02", "13", "06", "07", "08", "09"),
                       labels = c("Clearcutting only",
                                  "+ salv. logging",
                                  "+ salv. logging + planting same sp",
                                  "+ salv. logging + planting Jack Pine",
                                  "Var. retention & salv. logging + planting same sp",
                                  "Var. retention & salv. logging + planting  Jack Pine)",
                                  NA),
                       caption = c("Note: With salvage logging, planting in all post-fire\nregen failure; no access constrains\n\n"),
                       colour =c("black", "firebrick2", "orange4", "orange1", "skyblue4", "skyblue2", "black")))



yMax <- 5*(ceiling(  100*max(df$p50VolAt120Area_ha/areaTotal)/5))
pMean <- list()
for (i in seq_along(colList)) {

    options(scipen=999)
    sID <- colList[[i]]$simID
    scen <- gsub(" |\\.", "", colList[[i]]$scenario)
    labels <- colList[[i]]$labels
    plotName <- names(colList)[[i]]
    cols <- colList[[i]]$colour
    
    linetype <- c("Baseline" = 1, "RCP 8.5" = 3)
    caption <- colList[[i]]$caption
    names(cols) <- labels

    
    ### producing dataframes
    plotMean <- dfMean %>%
      #filter(fireScenario == scen) %>%
      mutate(plotLvl = ifelse(simID %in% sID, labels[match(simID, sID)],
                              ""),
             plotLvl = factor(plotLvl, levels = labels),
             #cols = ifelse(!is.na(plotLvl),
             #              cols[match(simID, sID)], tail(cols, 1)),
             size = ifelse(is.na(plotLvl), 0.5,
                           ifelse(plotLvl == "Clearcutting only", 1.5, 1)),
             alpha = ifelse(is.na(plotLvl), 0.25, 1),
             linetype =  linetype[match(fireScenario, names(linetype))])

    plotDf <- df %>%
      mutate(plotLvl = ifelse(simID %in% sID, labels[match(simID, sID)],
                              ""),
             plotLvl = factor(plotLvl, levels = labels),
             size = ifelse(is.na(plotLvl), 0.5,
                           ifelse(plotLvl == "Clearcutting only", 1.5, 1)),
             alpha = ifelse(is.na(plotLvl), 0.25, 1),
             linetype =  linetype[match(fireScenario, names(linetype))])
    
    plotRibbon <- plotMean %>%
      filter(!is.na(plotLvl))
    
    
    ############################################################################
    #### Landscape averages
    pMean[[i]] <- ggplot(data = plotMean, aes(x = yearInit + year,
                                              y = meanVolAt120Mean_cubMeter,
                                              group = simID)) +
      geom_line(size = plotMean$size,
                alpha = plotMean$alpha,
                linetype = plotMean$linetype,
                aes(colour = plotLvl)) +
      geom_ribbon(data = plotRibbon, aes(ymin = p25VolAt120Mean_cubMeter,
                                  ymax = p75VolAt120Mean_cubMeter,
                                  group = simID, 
                                  fill = plotLvl),
                  show.legend = FALSE,
                  alpha = 0.25) +
      scale_colour_manual("",
                          values = cols, #c("grey", cols),
                          labels = labels,
                          na.translate = T) +
                          #guide = guide_legend(nrow = length(unique(plotMean$plotLvl)))) +
      scale_fill_manual(values = cols) +
      labs(subtitle = paste0(letters[i], ") ", plotName, "\n"),
           caption = caption,
           x = "",
           y = "Average merch. volume at 120 y.old\n") +
      theme_bw() +
      ylim(47.5, yMax) +
      theme(legend.position = "bottom",
            legend.box="vertical",
            legend.title=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.subtitle = element_text(size = rel(.9)),
            plot.caption = element_text(size = rel(.6), hjust = 1))
    ## tweaking graphs based on position in the patchwork
    if(i%%3 != 1) {
      pMean[[i]] <-  pMean[[i]] +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    
    ############################################################################
    #### Productivity classes
    pTmp <- list()
    for (k in seq_along(levels(plotDf$volAt120Cls))) {
      dfCls <- filter(plotDf, volAt120Cls == levels(volAt120Cls)[[k]])
      pTmp[[k]] <- ggplot(data = dfCls, aes(x = yearInit + year,
                                           y = 100 * p50VolAt120Area_ha/areaTotal,
                                           group = simID)) +
          geom_line(size = dfCls$size,
                    alpha = dfCls$alpha,
                    aes(colour = dfCls$plotLvl,
                        linetype = dfCls$fireScenario,
                        )) +
          labs(subtitle = levels(plotDf$volAt120Cls)[k],
               x = "",
               y = "Proportion of productive area (%)\n") +
          scale_colour_manual("",
                              values = cols, #c("grey", cols),
                              labels = labels,
                              na.translate = F,
                              guide = guide_legend(nrow = 2)) +
          
          scale_linetype_manual(values = linetype, #c("grey", cols),
                                guide = "none") +
          scale_size_continuous(range = c(0.25, 1.5),
                                guide = "none") +
          scale_alpha_continuous(guide = "none") +
          ylim(0, yMax) +
        theme(legend.position = "bottom",
              legend.box="vertical",
              legend.title=element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.subtitle = element_text(size = rel(.9)),
              plot.caption = element_text(size = rel(.6), hjust = 1),
              rect = element_rect(fill = "transparent"))
      
      ## tweaking graphs based on position in the patchwork
      if(k != 1) {
        pTmp[[k]] <-  pTmp[[k]] +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1))
      }
      if(k != 3) {
        pTmp[[k]] <-  pTmp[[k]] +
        theme(legend.position = "none")
      }

    }
    
    
    png(filename= paste0("volAt120_cls_", scen, "_", letters[i], ".png"),
        width = 12, height = 6, units = "in", res = 600, pointsize=10)
    
    print((pTmp[[1]] + pTmp[[2]] + pTmp[[3]] + pTmp[[4]]) + 
             plot_layout(nrow = 1) +
            plot_annotation(
              title = "Effects of harvesting treatments and post-fire regeneration failure adaptation/mitigation measures\non representation of stand productivity classes at the landscape level - Baseline fire scenarios",
              subtitle = paste0(letters[i], ") ", plotName),#"Full and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively, while ribbons encompass 50% of simulations for selected treatments.",
              caption = paste("All panels represent the same simulations, highlighting only specific ones.",
                             "Each line represents the average of 100 simulations."),
              theme = theme(
                plot.title = element_text(size = rel(1.5)),
                plot.caption = element_text(size = rel(.8), hjust = 0))
              
            )) 
    dev.off()
    
}

png(filename= paste0("volAt120_mean_baseline.png"),
    width = 12, height = 12, units = "in", res = 600, pointsize=10)

print((pMean[[1]] + pMean[[2]] + pMean[[3]]) /
        (pMean[[4]] + pMean[[5]] + pMean[[6]]) +
       plot_annotation(
          title = "Effects of harvesting treatments and post-fire regeneration failure adaptation/mitigation measures\non average potential productivity at the landscape level - Baseline fire scenarios",
          subtitle = "Full and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively, while ribbons encompass 50% of simulations for selected treatments.",
          caption = paste("All panels represent the same simulations, highlighting only specific ones.",
                           "Each line represents the average of 100 simulations."),
          theme = theme(
            plot.title = element_text(size = rel(1.5)),
            plot.caption = element_text(size = rel(.8), hjust = 0))

        )
)
dev.off()
    
   



################################################################################
#### RCP 8.5
################


colList <- list("Moderate adaptation/attenuation strategies in face of increased fire activity\n" =
                  list(scenario = "RCP 8.5",
                       simID = c("10", "19", "21", "22",  "20"),
                       labels = c("No harvests",
                                  "Clearcutting only",
                                  "Clearcutting (+ salv. logging)",
                                  "Clearcutting (+ salv. logging & post-salv. planting)",
                                  "Var. retention",
                                  NA),
                       caption = c("Note: Projected fire regime (from 0.99% / yr in 2015 to 4% in 2100 and onwards) and historical harvesting levels (0.62% / yr)\nWhen planting occurs, only within a 2-km distance from current access network.\n"),
                       colour =c("seagreen",  "black", "firebrick2", "orangered2", "mediumpurple4",
                                 "black")),
                "Aggressive adaptation/attenuation strategies in face of increased fire activity\n" =
                  list(scenario = "RCP 8.5",
                       simID = c("19", "11", "12", "23", "24"),
                       labels = c("Clearcutting only", 
                                  "Var. retention & salv. (+ planting same sp)",
                                  "Var. retention & salv. (+ planting Jack Pine)",
                                  "Clearcutting & salv. (+ planting same sp, no access constrains)",
                                  "Clearcutting & salv. (+ planting Jack Pine, no access constrains)",
                                  NA),
                       caption = c("Note: Planting may occur in any post-fire regen failure, salvaged or not\n\n\n"),
                       colour =c("black",  "skyblue4", "skyblue2", "orange4", "orange1", "black")))



yMax <- 5*(ceiling(  100*max(df$p50VolAt120Area_ha/areaTotal)/5))

pMean <- list()

for (i in seq_along(colList)) {
  options(scipen=999)
  sID <- colList[[i]]$simID
  scen <- gsub(" |\\.", "", colList[[i]]$scenario)
  labels <- colList[[i]]$labels
  plotName <- names(colList)[[i]]
  cols <- colList[[i]]$colour
  
  linetype <- c("Baseline" = 1, "RCP 8.5" = 3)
  caption <- colList[[i]]$caption
  names(cols) <- labels
  
  
  ### producing dataframes
  plotMean <- dfMean %>%
        mutate(plotLvl = ifelse(simID %in% sID, labels[match(simID, sID)],
                            ""),
           plotLvl = factor(plotLvl, levels = labels),
           size = ifelse(is.na(plotLvl), 0.25,
                         ifelse(plotLvl == "Clearcutting only", 1.5, 1)),
           alpha = ifelse(is.na(plotLvl), 0.25, 1),
           linetype =  linetype[match(fireScenario, names(linetype))])
  
  plotDf <- df %>%
    mutate(plotLvl = ifelse(simID %in% sID, labels[match(simID, sID)],
                            ""),
           plotLvl = factor(plotLvl, levels = labels),
           size = ifelse(is.na(plotLvl), 0.25,
                         ifelse(plotLvl == "Clearcutting only", 1.5, 1)),
           alpha = ifelse(is.na(plotLvl), 0.25, 1),
           linetype =  linetype[match(fireScenario, names(linetype))])
  
  plotRibbon <- plotMean %>%
    filter(!is.na(plotLvl))
  
  
  ############################################################################
  #### Landscape averages
  pMean[[i]] <- ggplot(data = plotMean, aes(x = yearInit + year,
                                            y = meanVolAt120Mean_cubMeter,
                                            group = simID)) +
    geom_line(size = plotMean$size,
              alpha = plotMean$alpha,
              linetype = plotMean$linetype,
              aes(colour = plotLvl)) +
    geom_ribbon(data = plotRibbon, aes(ymin = p25VolAt120Mean_cubMeter,
                                       ymax = p75VolAt120Mean_cubMeter,
                                       group = simID, 
                                       fill = plotLvl),
                show.legend = FALSE,
                alpha = 0.25) +
    scale_colour_manual("",
                        values = cols, #c("grey", cols),
                        labels = labels,
                        na.translate = F,
                        guide = guide_legend(nrow = length(unique(plotMean$plotLvl)))) +
    scale_fill_manual(values = cols) +
    labs(subtitle = paste0(letters[i], ") ", plotName, "\n"),
         caption = caption,
         x = "",
         y = "Average merch. volume at 120 y.old\n") +
    theme_bw() +
    #ylim(47.5, yMax) +
    theme(legend.position = "bottom",
          legend.box="vertical",
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.subtitle = element_text(size = rel(.9)),
          plot.caption = element_text(size = rel(.6), hjust = 1))
  
  ## tweaking graphs based on position in the patchwork
  if(i%%2 != 1) {
    pMean[[i]] <-  pMean[[i]] +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  }
  ############################################################################
  #### Productivity classes
  pTmp <- list()
  for (k in seq_along(levels(plotDf$volAt120Cls))) {
    dfCls <- filter(plotDf, volAt120Cls == levels(volAt120Cls)[[k]])
    pTmp[[k]] <- ggplot(data = dfCls, aes(x = yearInit + year,
                                          y = 100 * p50VolAt120Area_ha/areaTotal,
                                          group = simID)) +
      geom_line(size = dfCls$size,
                alpha = dfCls$alpha,
                aes(colour = dfCls$plotLvl,
                    linetype = dfCls$fireScenario,
                )) +
      labs(subtitle = levels(plotDf$volAt120Cls)[k],
           x = "",
           y = "Proportion of productive area (%)\n") +
      scale_colour_manual("",
                          values = cols, #c("grey", cols),
                          labels = labels,
                          na.translate = F,
                          guide = guide_legend(nrow = 2)) +
      
      scale_linetype_manual(values = linetype, #c("grey", cols),
                            guide = "none") +
      scale_size_continuous(range = c(0.25, 1.5),
                            guide = "none") +
      scale_alpha_continuous(guide = "none") +
      ylim(0, yMax) +
      theme(legend.position = "bottom",
            legend.box="vertical",
            legend.title=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.subtitle = element_text(size = rel(.9)),
            plot.caption = element_text(size = rel(.6), hjust = 1),
            rect = element_rect(fill = "transparent"))
    
    ## tweaking graphs based on position in the patchwork
    if(k != 1) {
      pTmp[[k]] <-  pTmp[[k]] +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    if(k != 3) {
      pTmp[[k]] <-  pTmp[[k]] +
        theme(legend.position = "none")
    }
    
  }
  
  
  png(filename= paste0("volAt120_cls_", scen, "_", letters[i], ".png"),
      width = 12, height = 6, units = "in", res = 600, pointsize=10)
  
  print((pTmp[[1]] + pTmp[[2]] + pTmp[[3]] + pTmp[[4]]) + 
          plot_layout(nrow = 1) +
          plot_annotation(
            title = "Effects of harvesting treatments and post-fire regeneration failure adaptation/mitigation measures\non representation of stand productivity classes at the landscape level - Baseline fire scenarios",
            subtitle = paste0(letters[i], ") ", plotName),#"Full and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively, while ribbons encompass 50% of simulations for selected treatments.",
            caption = paste("All panels represent the same simulations, highlighting only specific ones.",
                            "Each line represents the average of 100 simulations."),
            theme = theme(
              plot.title = element_text(size = rel(1.5)),
              plot.caption = element_text(size = rel(.8), hjust = 0))
            
          )) 
  dev.off()
  
}
emptyplot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank() + theme_void()


png(filename= paste0("volAt120_mean_RCP85.png"),
    width = 12, height = 8, units = "in", res = 600, pointsize=10)

print((pMean[[1]] + pMean[[2]]) +
        plot_annotation(
          title = "Effects of harvesting treatments and post-fire regeneration failure adaptation/mitigation measures\non average potential productivity at the landscape level - RCP 8.5 fire scenarios",
          subtitle = "Full and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively, while ribbons encompass 50% of simulations for selected treatments.",
          caption = paste("All panels represent the same simulations, highlighting only specific ones.",
                          "Each line represents the average of 100 simulations."),
          theme = theme(
            plot.title = element_text(size = rel(1.5)),
            plot.caption = element_text(size = rel(.8), hjust = 0))
          
        )
)
dev.off()


