###################################################################################################
###################################################################################################
##### Visualizing productivity classes, mean potential volumes at 120 years old,
##### harvested volumes and economic returns
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo"))])
#################
require(raster)
require(ggplot2)
require(dplyr)
require(tidyr)
require(reshape2)
yearInit <- simInfo$initYear

###################################################################################################
###################################################################################################
###### volumes at 120 y.old
output <- list()
for (s in seq_along(simInfo$simID)) {
    simID <- simInfo$simID[s]
    output[[s]] <-  get(load(paste0("../outputCompiled/outputCompiledVolAt120_", simID, ".RData")))
}
output <- do.call("rbind", output)
######
output[,"id_ms"] <- simInfo$id_ms[match(output$simID, simInfo$simID)]
output[,"clearcutting"] <- simInfo$clearcutting[match(output$simID, simInfo$simID)]

output[,"varReten"] <- simInfo$varReten[match(output$simID, simInfo$simID)]
output[,"salv"] <- simInfo$salv[match(output$simID, simInfo$simID)]
output[,"plantPostFire"] <- simInfo$plantPostFire[match(output$simID, simInfo$simID)]
output[,"plantPostSalv"] <- simInfo$plantPostSalv[match(output$simID, simInfo$simID)]
output[,"plantPostFireSp"] <- simInfo$plantPostFireSp[match(output$simID, simInfo$simID)]
output[,"plantLimitedAccess"] <- simInfo$plantLimitedAccess[match(output$simID, simInfo$simID)]



###################################################################################################
###### computing areas
areaTotal <- output %>%
  group_by(simID, fireScenario, mgmtScenario, replicate, year, 
                     clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(areaTotal = sum(area_ha))
areaTotal <- unique(areaTotal$areaTotal)
  

###################################################################################################
###### preparing data frames for plotting - Productivity metrics

##### focusing of selected simulations
output <- filter(output, !is.na(id_ms))


outputCls <- output %>%
  dplyr::select(simID, id_ms, fireScenario, mgmtScenario,
         replicate, year, coverType,  volAt120Cls, area_ha,
         clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  as.data.frame()
  
outputMean <- output %>% 
  group_by(simID, id_ms, fireScenario, mgmtScenario,
           replicate, year,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(volAt120Mean_tonnesPerHa = unique(volAt120_totalLandscapeAverage)) %>%
  as.data.frame()

cNames <- colnames(outputMean)


###### summarizing averages
mean_summary <- outputMean %>%
  filter(year == 0) %>%
  group_by(simID,  id_ms, fireScenario, mgmtScenario,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp) %>%
  summarise(volAt120p25_tonnesPerHa = quantile(volAt120Mean_tonnesPerHa, 0.25),
            volAt120p75_tonnesPerHa = quantile(volAt120Mean_tonnesPerHa, 0.75),
            volAt120Mean_tonnesPerHa = mean(volAt120Mean_tonnesPerHa)) %>%
  as.data.frame()

###### correcting for small modelling artefact
###### (slight discrepancies in initial conditions)
deltaMeanVol <- mean_summary[,c("simID", "volAt120Mean_tonnesPerHa")] %>%
  mutate(ref = volAt120Mean_tonnesPerHa[which(simID == "01")],
         delta = volAt120Mean_tonnesPerHa - ref) %>%
  dplyr::select(simID, delta)

deltaClsVol <- outputCls %>%
  filter(year == 0) %>%
  group_by(simID, id_ms, volAt120Cls) %>%
  summarise(area_ha = mean(area_ha)) %>%
  ungroup() %>%
  group_by(volAt120Cls) %>%
  mutate(ref = area_ha[which(simID == "01")],
         delta = area_ha - ref) %>%
  dplyr::select(simID, id_ms, volAt120Cls, delta) %>%
  as.data.frame() 

outputMean <- outputMean %>%
  merge(deltaMeanVol) %>%
  mutate(volAt120Mean_tonnesPerHa = volAt120Mean_tonnesPerHa - delta)

outputMean <- outputMean[,cNames]

###################################################################################################
###### storing data frames as .csv
write.csv(outputCls, file = "outputCompiledVolAt120Cls.csv", row.names = F)
write.csv(outputMean, file = "outputCompiledVolAt120Mean.csv", row.names = F)





###################################################################################################
###### finalizing data frames for plotting 


#### productivity classes
df <- output %>%
  merge(deltaClsVol) %>%
  filter(!is.na(volAt120Cls)) %>%
  mutate(#volAt120Cls = as.character(volAt120Cls),
         # volAt120Cls = ifelse(volAt120Cls %in% c("[50,80)", "[80,999]"), "[50,999)", volAt120Cls),
         #volAt120Cls = factor(volAt120Cls, levels = c("[0,30)", "[30,50)", "[50,80)", "[80,999)")),
         area_ha =  area_ha - delta) %>%
    group_by(simID, id_ms, fireScenario, mgmtScenario, replicate, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
    summarise(area_ha = sum(area_ha)) %>%
    ungroup() %>%
    group_by(simID, id_ms, fireScenario, mgmtScenario, year, volAt120Cls,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
    summarise(meanVolAt120Area_ha = mean(area_ha),
              p25VolAt120Area_ha = quantile(area_ha, .25),
              p50VolAt120Area_ha = quantile(area_ha, .5),
              p75VolAt120Area_ha = quantile(area_ha, .75))


#### average productivity
dfMean <- output %>%
  filter(!is.na(volAt120Cls)) %>%
  merge(deltaMeanVol) %>%
  mutate(volAt120_totalLandscapeAverage = volAt120_totalLandscapeAverage - delta) %>%
  group_by(simID, id_ms, fireScenario, mgmtScenario, year, replicate,
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
  summarise(volAt120_totalLandscapeAverage = unique(volAt120_totalLandscapeAverage)) %>%
  ungroup() %>%
  group_by(simID, id_ms, fireScenario, mgmtScenario, year, 
           clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess) %>%
  summarise(p25VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .25),
         p50VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .5),
         p75VolAt120Mean_cubMeter = quantile(volAt120_totalLandscapeAverage, .75),
         meanVolAt120Mean_cubMeter = mean(volAt120_totalLandscapeAverage)) 



################################################################################
################################################################################
################################################################################
#### plotting
require(ggplot2)
library(ggpubr)
require(patchwork)
################ 


colList <- list("MS" =
                  list(scenario = "Baseline",
                       id_ms = c("01", "02", "03", "04", "05", "06", "07", "08", "09"),
                       labels = c("No harvests",
                                  "Clearcutting + post-fire salv.",
                                  "Var. retention + post-fire salv.",
                                  "Clearcutting + replanting / limited access",
                                  "Clearcutting + replanting / limited access - Jack Pine",
                                  "Var. retention + replanting / limited access",
                                  "Var. retention + replanting / limited access - Jack Pine",
                                  "Clearcutting + replanting / full access",
                                  "Clearcutting + replanting / full access - Jack Pine"),
                       caption = c("Selected scenarios"),
                       colour =c("seagreen",
                                 "black",
                                 "mediumpurple4",
                                 "skyblue4",
                                 "goldenrod4",
                                 "mediumpurple2",
                                 "goldenrod2",
                                 "skyblue2",
                                 "goldenrod1")),
                "SuppMat" = 
                  list(scenario = "Baseline",
                       id_ms = unique(df$id_ms),
                       labels = "",
                       caption = "All scenarios",
                       colour = rep("black", length.out = length(unique(df$id_ms)))))




for (i in seq_along(colList)) {
    options(scipen=999)
    sID <- colList[[i]]$id_ms
    scen <- gsub(" |\\.", "", colList[[i]]$scenario)
    labels <- colList[[i]]$labels
    labels <- factor(paste(labels, paste0("(S", sID, ")")))
    labels <- factor(labels, levels = as.character(labels))
    plotName <- names(colList)[[i]]
    cols <- colList[[i]]$colour
    
    linetype <- c("Baseline" = 1, "RCP 8.5" = 3)
    caption <- colList[[i]]$caption
    names(cols) <- labels

    
    ### producing dataframes
    plotMean <- dfMean %>%
      mutate(plotLvl = labels[match(id_ms, sID)],
             size = ifelse(names(colList)[[i]] == "SuppMat", 1,
                           ifelse(is.na(plotLvl), 0.5,
                           ifelse(plotLvl == "Clearcutting + post-fire salv. (sim02)", 1.5, 1.5))),
             alpha = ifelse(is.na(plotLvl), 1, 1),
             linetype =  linetype[match(fireScenario, names(linetype))])
    plotMean[grepl("full access", plotMean$plotLvl), "linetype"] <- 3
    #### reordering levels
    #plotMean$plotLvl <- class(plotMean$labels)

    plotDf <- df %>%
      mutate(plotLvl = labels[match(id_ms, sID)],
             size = ifelse(names(colList)[[i]] == "SuppMat", 1,
                           ifelse(is.na(plotLvl), 0.5,
                           ifelse(plotLvl == "Clearcutting only", 1.5, 1.5))),
             alpha = ifelse(is.na(plotLvl), 1, 1),
             linetype =  linetype[match(fireScenario, names(linetype))])
    plotDf[grepl("full access", plotDf$plotLvl), "linetype"] <- 3
    plotRibbon <- plotMean %>%
      filter(!is.na(plotLvl))
    
    if(names(colList[i]) == "MS") {
      yLim <- c(47.5, 66)
    }
    if(names(colList[i]) == "SuppMat") {
      yLim <- c(floor(min(plotMean$meanVolAt120Mean_cubMeter)/2)*2, 
                ceiling(max(plotMean$meanVolAt120Mean_cubMeter)/2)*2)
    }
    
    plotLabels <- plotMean %>%
      filter(meanVolAt120Mean_cubMeter > yLim[1]) %>%
      group_by(id_ms) %>%
      filter(year == max(year)) %>%
      mutate(label = paste0("S", id_ms),
             hjust = ifelse(is.na(plotLvl), -0.05, -0.05),
             vjust = ifelse(is.na(plotLvl), -0.05, -0.05),
             alpha = ifelse(is.na(plotLvl), 1, 1),
             size = ifelse(is.na(plotLvl), 4, 6)) %>%
      ungroup() %>%
      mutate(meanVolAt120Mean_cubMeter = ifelse(year<max(year), yLim[1],
                                                meanVolAt120Mean_cubMeter)) %>%
      select(id_ms, year, meanVolAt120Mean_cubMeter, plotLvl,
             label, hjust, vjust, alpha, size)
    
    
    ############################################################################
    ############################################################################
    #### Landscape averages
    pMean <- ggplot(data = plotMean, aes(x = yearInit + year,
                                              y = meanVolAt120Mean_cubMeter,
                                              group = simID)) +
      geom_line(size = plotMean$size,
                alpha = plotMean$alpha,
                linetype = plotMean$linetype,
                aes(colour = plotLvl)) +
      geom_ribbon(data = plotRibbon,
                  aes(ymin = p25VolAt120Mean_cubMeter,
                      ymax = p75VolAt120Mean_cubMeter,
                      group = simID,
                      fill = plotLvl),
                  show.legend = F,
                  alpha = 0.1) +
      scale_colour_manual("",
                          values = cols, 
                          na.translate = T,
                          na.value = "black") +
      scale_fill_manual(values = cols) +
      geom_text(data = plotLabels,
                aes(label = paste0("S", id_ms), colour = plotLvl,
                    x = yearInit + year, y = meanVolAt120Mean_cubMeter),
                hjust = plotLabels$hjust,
                vjust = plotLabels$hjust,
                alpha = plotLabels$alpha,
                size = rel(plotLabels$size),
                show.legend = F) +
      labs(#subtitle = paste0(plotName, "\n"),
           x = "",
           y = 'Potential merch. vol\n(cub-m/ha@120)\n') +
      ylim(yLim[1], yLim[2]) +
      theme(legend.position = "bottom",
            legend.title=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
          plot.subtitle = element_text(size = rel(.9)),
          plot.caption = element_text(size = rel(.6), hjust = 1)) 
     

    if(names(colList)[[i]] == "MS") {
      pMean <- pMean +
        guides(colour = guide_legend(nrow = 5,
                                     override.aes = list(size = c(rep(2, length(labels)), 0.5),
                                                         alpha = c(rep(1, length(labels)), 1),
                                                         linetype = c(ifelse(grepl("full access", labels), 3, 1),3))))
    }
    
    
    if(names(colList)[[i]] == "SuppMat") {
       pMean <- pMean +
         theme(legend.position = "none")
       # guides(colour = guide_legend(nrow = 3,
       #                              override.aes = list(linetype =  c(rep(1,9), rep (3,6)))))
      
    }
    
    png(filename= paste0("volAt120_mean_", names(colList)[i], ".png"),
        width = 12, height = 10, units = "in", res = 600, pointsize=10)
    
    print(pMean +
            labs(#title = "Landscape vulnerability to post-fire regeneration failure", 
              #subtitle = paste(colList[[i]]$caption, "- Each line represents the mean of 100 simulations.\nFull and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively respectively, while ribbons encompass 50% of simulations for selected treatments.")) +
            )  +
            theme(plot.title = element_text(size = rel(1.5)),
                  axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
                  axis.text.y = element_text(size = rel(1.5)),
                  axis.title = element_text(size = rel(1.5)),
                  legend.text = element_text(size = rel(1.1)),
                  legend.key.width = unit(1.5,"cm"),
                  plot.subtitle = element_text(size = rel(.9)),
                  plot.caption = element_text(size = rel(.75), hjust = 1)
            )
    ) 
    
    dev.off() 
    #############
    ############################################################################
    ############################################################################
    
    
    
    
    
    ############################################################################
    ############################################################################
    #### Productivity classes
    yMax <- max(plotDf$meanVolAt120Area_ha)
    yMax <- ceiling((100 * yMax / areaTotal)/5)*5
    pTmp <- list()
    for (k in seq_along(levels(plotDf$volAt120Cls))) {
      dfCls <- filter(plotDf, volAt120Cls == levels(volAt120Cls)[[k]])
      
      plotLabels <- dfCls %>%
        filter(year == 150) %>%
        mutate(label = paste0("S", id_ms),
               hjust = ifelse(is.na(plotLvl), -0.05, -0.05),
               vjust = ifelse(is.na(plotLvl), -0.05, -0.05),
               alpha = ifelse(is.na(plotLvl), 1, 1),
               size = ifelse(is.na(plotLvl), 4, 6)) %>%
        select(id_ms, year, meanVolAt120Area_ha, plotLvl,
               label, hjust, vjust, alpha, size)
      
      
      
      pTmp[[k]] <- ggplot(data = dfCls, aes(x = yearInit + year,
                                            y = 100 * meanVolAt120Area_ha/areaTotal,
                                            group = simID)) +
        geom_line(size = 0.75,
                  alpha = dfCls$alpha,
                  linetype = dfCls$linetype,
                  aes(colour = dfCls$plotLvl)) +
        labs(subtitle = paste(levels(plotDf$volAt120Cls)[k], "cub-m @ 120 years old"),
             x = "",
             y = "Proportion of managed forest\n(%)\n") +
        scale_colour_manual("",
                            values = cols, 
                            na.translate = T,
                            na.value = "black") +
        
        scale_linetype_manual(values = linetype, #c("grey", cols),
                              guide = "none") +
        geom_text(data = plotLabels,
                  aes(label = paste0("S", id_ms), colour = plotLvl,
                      x = yearInit + year, y = 100 * meanVolAt120Area_ha/areaTotal),
                  hjust = plotLabels$hjust,
                  vjust = plotLabels$hjust,
                  alpha = plotLabels$alpha,
                  size = rel(plotLabels$size),
                  show.legend = F) +
        # scale_size_continuous(range = c(0.25, 1.5),
        #                       guide = "none") +
        scale_alpha_continuous(guide = "none") +
        xlim(yearInit,  yearInit + 167) +
        ylim(0, yMax) +
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.text = element_text(size = rel(1)),
              legend.key.width = unit(2,"cm"),
              plot.subtitle = element_text(size = rel(1.5)),
              plot.caption = element_text(size = rel(.6), hjust = 1),
              plot.background = element_rect(fill = "transparent", colour = NA))

      
      if(names(colList)[[i]] == "MS") {
        pTmp[[k]] <- pTmp[[k]] +
          guides(colour = guide_legend(nrow = 5,
                                       override.aes = list(size = c(rep(2, length(labels)), 0.5),
                                                           alpha = c(rep(1, length(labels)), 0.25),
                                                           linetype = c(ifelse(grepl("full access", labels), 3, 1),3))))
      }
      
      if(names(colList)[[i]] == "SuppMat") {
        pTmp[[k]] <- pTmp[[k]] +
          theme(legend.position = "none")
        # guides(colour = guide_legend(nrow = 3,
        #                              override.aes = list(linetype =  c(rep(1,9), rep (3,6)))))
        
      }
      
      ## tweaking graphs based on position in the patchwork
      if(k != 1) {
        pTmp[[k]] <-  pTmp[[k]] +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1))
      }
      # if(k != 2) {
      #   pTmp[[k]] <-  pTmp[[k]] +
      #     theme(legend.position = "none")
      # }
      # 
      ######################  regen failure figure
      if(k == 1) {
        initVal <- unique(as.data.frame(filter(dfCls, year == 0))$meanVolAt120Area_ha)
        
        if(names(colList)[i] == "MS") {
          yLim <- c(0,20)
        } else {
          yLim <- c(0,35)
        }
        
        
        plotLabels <- plotDf %>%
          filter(volAt120Cls == "[0,30)") %>%
          mutate(cumulRegenFailure = 100 * (meanVolAt120Area_ha-initVal)/areaTotal) %>%
          filter(cumulRegenFailure < yLim[2]) %>%
          group_by(id_ms) %>%
          filter(year == max(year)) %>%
          mutate(label = paste0("S", id_ms),
                 hjust = ifelse(is.na(plotLvl), -0.05, -0.05),
                 vjust = ifelse(is.na(plotLvl), -0.05, -0.05),
                 alpha = ifelse(is.na(plotLvl), 1, 1),
                 size = ifelse(is.na(plotLvl), 4, 6)) %>%
          ungroup() %>%
          mutate(cumulRegenFailure = ifelse(year<max(year), yLim[2],
                                              cumulRegenFailure)) %>%
          select(id_ms, year, cumulRegenFailure, plotLvl,
                 label, hjust, vjust, alpha, size)
        
        
                              
        rfPlot <- ggplot(data = dfCls, aes(x = yearInit + year,
                                              y = 100 * (meanVolAt120Area_ha-initVal)/areaTotal,
                                              group = simID)) +
          geom_line(size = dfCls$size,
                    alpha = dfCls$alpha,
                    linetype = dfCls$linetype,
                    aes(colour = dfCls$plotLvl)) +
          labs(#title = "Areas affected by post-fire regeneration failure",#paste(levels(plotDf$volAt120Cls)[k], "cub-m @ 120 years old"),
               x = "",
               y = "Cumulative regeneration failure\n(%)\n") +
          scale_colour_manual("",
                              values = cols, 
                              na.translate = T,
                              na.value = "black") +
          scale_linetype_manual(values = linetype, #c("grey", cols),
                                guide = "none") +
          geom_text(data = plotLabels,
                    aes(label = paste0("S", id_ms), colour = plotLvl,
                        x = yearInit + year, y = cumulRegenFailure),
                    hjust = plotLabels$hjust,
                    vjust = plotLabels$hjust,
                    alpha = plotLabels$alpha,
                    size = rel(plotLabels$size),
                    show.legend = F) +
          scale_alpha_continuous(guide = "none") +
          xlim(yearInit,  yearInit + 155) +
          ylim(yLim[1], yLim[2]) +
          theme(plot.title = element_text(size = rel(1.5)),
                plot.subtitle = element_text(size = rel(.9)),
                plot.caption = element_text(size = rel(.75), hjust = 1),
                legend.position = "bottom",
                legend.title=element_blank(),
                legend.text = element_text(size = rel(1.1)),
                legend.key.width = unit(1.5,"cm"),
                axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
                axis.text.y = element_text(size = rel(1.5)),
                axis.title = element_text(size = rel(1.5)))


        
        
                
        if(names(colList)[[i]] == "MS") {
          rfPlot <- rfPlot +
            guides(colour = guide_legend(nrow = 5,
                                         override.aes = list(size = c(rep(2, length(labels)), 0.5),
                                                             alpha = c(rep(1, length(labels)), 1),
                                                             linetype = c(ifelse(grepl("full access", labels), 3, 1),3))))
        }
        
        
        if(names(colList)[[i]] == "SuppMat") {
          rfPlot <- rfPlot +
            theme(legend.position = "none")
             # guides(colour = guide_legend(nrow = 3,
            #                              override.aes = list(linetype =  c(rep(1,9), rep (3,6)))))
          
        }
        png(filename= paste0("regenFailure_", names(colList)[i], ".png"),
            width = 12, height = 10, units = "in", res = 600, pointsize=10)
        
        print(rfPlot)
        dev.off()
      }
      

        
    } 
    


  png(filename= paste0("volAt120_cls_", names(colList)[i], ".png"),
      width = 16, height = 8, units = "in", res = 600, pointsize=10)
  
  print((pTmp[[1]] + pTmp[[2]] + pTmp[[3]] + pTmp[[4]]) + 
          plot_layout(nrow = 1,
                      guides = "collect") &
          theme(legend.position = "bottom"))
  dev.off()

  }

