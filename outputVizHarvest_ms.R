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
require(reshape2)
require(scales)
require(patchwork)
yearInit <- simInfo$initYear

###################################################################################################
###################################################################################################
###### harvest and data
harv <- read.csv("../tables/offline/outputCompiledHarvest.csv",
                 colClasses=c("simID"="character"))
######
harv[,"id_ms"] <- simInfo$id_ms[match(harv$simID, simInfo$simID)]
harv[,"plantPostFireSp"] <- simInfo$plantPostFireSp[match(harv$simID, simInfo$simID)]
harv[,"plantLimitedAccess"] <- simInfo$plantLimitedAccess[match(harv$simID, simInfo$simID)]
harv[,"fireScenario"] <- simInfo$fire[match(harv$simID, simInfo$simID)]



##### economic constants
royalt <- 13.75 # royalties $ / m3 harvested
costPlSpp <- 1315 # average cost of plantation ($ per ha, post-fire + post-salv)
costPlJp <- 1472 # cost of Jack Pine plantation ($ per ha, post-fire + post-salv)
costAccess <- 270 # additional cost pf non-accessible plantation (access cost, per ha of plantation, applied to 0.4472 proportion), capped at 75 millions
propInaccess <- .4472
costAccessCap <-  75000000

###################################################################################################
###################################################################################################
###### preparing data frames for plotting

##### focusing of selected simulations
df <- filter(harv, !is.na(id_ms))

##### computing royalties and costs
df <- df %>%
    mutate(harvestTotal_cubMeters =  harvestVol_cubMeters + salvVol_cubMeters,
           royalties = harvestTotal_cubMeters * royalt,
           costPl_perHa = ifelse(plantPostFireSp == "same", costPlSpp,
                                 ifelse(plantPostFireSp == "PG",  costPlJp, NA)),
           costPl = costPl_perHa * (plantPostSalv_ha + plantPostFire_ha),
           #costPl = costPl_perHa (plantPostFire_ha),
           costPl = ifelse(is.na(costPl), 0, costPl),
           costAccessTot = costAccess * (plantPostFire_ha * plantPostSalv_ha) * propInaccess,### 
           costTotal = costPl + costAccessTot,
           netReturn = royalties - costTotal) %>%
    group_by(simID, id_ms, fireScenario, replicate) %>%
    arrange(year) %>%
    mutate(harvestTotalCumSum_cubMeters = cumsum(harvestTotal_cubMeters),
           royaltiesCumSum = cumsum(royalties),
           costPlCumSum = cumsum(costPl),
           costAccessTotCumSum = cumsum(costAccessTot),
           costAccessTotCumSum = ifelse(costAccessTotCumSum>costAccessCap,
                                        costAccessCap, costAccessTotCumSum),
           costTotalCumSum = costPlCumSum + costAccessTotCumSum,
           netReturn = royaltiesCumSum - costTotalCumSum) %>%
    ungroup()

##### summarizing
dfSummary <- df %>%
    group_by(simID, id_ms, fireScenario, year) %>%
    summarise(harvestTotalCumSum_cubMeters_mean = mean(harvestTotalCumSum_cubMeters),
              harvestTotalCumSum_cubMeters_p50 = quantile(harvestTotalCumSum_cubMeters, 0.5),
              harvestTotalCumSum_cubMeters_p25 = quantile(harvestTotalCumSum_cubMeters, 0.25),
              harvestTotalCumSum_cubMeters_p75 = quantile(harvestTotalCumSum_cubMeters, 0.75),
              netReturn_mean = mean(netReturn),
              netReturn_p50 = quantile(netReturn, 0.5),
              netReturn_p25 = quantile(netReturn, 0.25),
              netReturn_p75 = quantile(netReturn, 0.75))

write.csv(dfSummary, file = "harvSummary.csv", row.names = F)

# ### rough plot (suppl. mat?) that shows the breaking down by type of harv (reg or salv)
# ggplot(harv, aes(x = year)) +
#     facet_wrap(~simID) +
#     geom_line(aes(y = harvestVolMean_cubMeters), col = "blue") +
#     geom_line(aes(y = salvVolMean_cubMeters), col = "red") +
#     geom_line(aes(y = salvVolMean_cubMeters + harvestVolMean_cubMeters), col = "black")


################################################################################
################################################################################
####### plotting 


colList <- list("MS" =
                    list(scenario = "Baseline",
                         id_ms = c("02", "03", "04", "05", "06", "07", "08", "09"),
                         labels = c("Clearcutting + post-fire salv.",
                                    "Var. retention + post-fire salv.",
                                    "Clearcutting + replanting / limited access",
                                    "Clearcutting + replanting / limited access - Jack Pine",
                                    "Var. retention + replanting / limited access",
                                    "Var. retention + replanting / limited access - Jack Pine",
                                    "Clearcutting + replanting / full access",
                                    "Clearcutting + replanting / full access - Jack Pine"),
                         caption = c("Selected scenarios"),
                         colour =c("black",
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


# colList <- list("MS" =
#                     list(scenario = "Baseline",
#                          id_ms = c("02", "03", "04", "05"),
#                          labels = c("Clearcutting + post-fire salv.",
#                                     "Var. retention + post-fire salv.",
#                                     "Clearcutting + replanting / limited access",
#                                     "Clearcutting + replanting / full access - Jack Pine"),
#                          subtitle = "Harvested volumes (cumulative)",
#                          caption = c("Selected scenarios"),
#                          colour =c("black", "mediumpurple4", "skyblue4", "skyblue2")),
#                 "SuppMat" = 
#                     list(scenario = "Baseline",
#                          id_ms = unique(df$id_ms),
#                          labels = "",
#                          subtitle = "Net economic return (cumulative)",
#                          caption = "All scenarios",
#                          colour = rep("black", length.out = length(unique(df$id_ms)))))




for (i in seq_along(colList)) {
    options(scipen=5)
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
    plotVol <- dfSummary %>%
        mutate(plotLvl = labels[match(id_ms, sID)],
               size = ifelse(names(colList)[[i]] == "SuppMat", 1,
                             ifelse(is.na(plotLvl), 0.5,
                                    ifelse(plotLvl == "Clearcutting + post-fire salv. (sim02)", 2, 1.5))),
               alpha = ifelse(is.na(plotLvl), 0.35, 1),
               linetype =  linetype[match(fireScenario, names(linetype))])
    

    plotRibbon <- plotVol %>%
        filter(!is.na(plotLvl))

    
    yLim <- c(-600, 500)
    
    plotLabelH <- plotVol %>%
        filter(year == 150) %>%
        group_by(id_ms, simID) %>%
        filter(year == max(year)) %>%
        mutate(yearH = year,
               label = paste0("S", id_ms),
               hjust = ifelse(is.na(plotLvl), -0.05, -0.05),
               vjust = ifelse(is.na(plotLvl), -0.05, -0.05),
               alpha = ifelse(is.na(plotLvl), 1, 1),
               size = ifelse(is.na(plotLvl), 4, 6)) %>%
        ungroup() %>%
        select(id_ms, simID, yearH,
               harvestTotalCumSum_cubMeters_mean,
               plotLvl,
               label, hjust, vjust, alpha, size)
    plotLabelR <- plotVol %>%
        filter(netReturn_mean/1000000 > yLim[1]) %>%
        group_by(id_ms, simID) %>%
        filter(year == max(year)) %>%
        mutate(yearR = year,
               label = paste0("S", id_ms),
               hjust = ifelse(is.na(plotLvl), -0.05, -0.05),
               vjust = ifelse(is.na(plotLvl), -0.05, -0.05),
               alpha = ifelse(is.na(plotLvl), 1, 1),
               size = ifelse(is.na(plotLvl), 4, 6)) %>%
        ungroup() %>%
        mutate(netReturn_mean = ifelse(year<max(year), yLim[1]*1000000,
                                       netReturn_mean)) %>%
        select(id_ms, simID, yearR,
               netReturn_mean,
               plotLvl,
               label, hjust, vjust, alpha, size)
    
    plotLabels <-  merge(plotLabelH, plotLabelR)
    
        
    pHarv <- ggplot(data = plotVol, aes(x = yearInit + year,
                                         y = harvestTotalCumSum_cubMeters_mean/1000000, 
                                          group = simID)) +
        geom_line(size = plotVol$size,
                  alpha = plotVol$alpha,
                  linetype = plotVol$linetype,
                  aes(colour = plotLvl)) +
        geom_ribbon(data = plotRibbon,
                    aes(ymin = harvestTotalCumSum_cubMeters_p25/1000000,
                        ymax = harvestTotalCumSum_cubMeters_p75/1000000,
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
                  aes(label = label, colour = plotLvl, 
                      x = yearInit + yearH,
                      y = harvestTotalCumSum_cubMeters_mean/1000000),
                  hjust = -.05,
                  vjust = 0.1,
                  alpha = plotLabels$alpha,
                  size = rel(plotLabels$size),
                  show.legend = F) +
        labs(#subtitle = paste0(colList[[i]]$subtitle, "\n"),
             x = "",
             y = "Cumulative harvested volume\n") +
        scale_y_continuous(label = unit_format(unit = "M cub-m")) +
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              legend.text = element_text(size = rel(1.1)),
              legend.key.width = unit(1.5,"cm"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
              axis.text.y = element_text(size = rel(1.5)),
              axis.title = element_text(size = rel(1.5)),
              plot.subtitle = element_text(size = rel(.9)),
              plot.caption = element_text(size = rel(.75), hjust = 1)) +
        guides(colour = guide_legend(nrow = 5,
                                     override.aes = list(size = c(rep(2, length(labels)), 0.5),
                                                         alpha = c(rep(1, length(labels)), 0.25))))

  

    pReturn <- ggplot(data = plotVol, aes(x = yearInit + year,
                                             y = netReturn_mean/1000000, 
                                             group = simID)) +
        geom_line(size = plotVol$size,
                  alpha = plotVol$alpha,
                  linetype = plotVol$linetype,
                  aes(colour = plotLvl)) +
        geom_ribbon(data = plotRibbon,
                    aes(ymin = netReturn_p25/1000000,
                        ymax = netReturn_p75/1000000,
                        group = simID,
                        fill = plotLvl),
                    show.legend = F,
                    alpha = 0.1) +
        scale_colour_manual("",
                            values = cols, 
                            na.translate = T,
                            na.value = "black") +
        scale_fill_manual(values = cols) +
        geom_hline(yintercept = 0, linetype = 1, size = 0.75) +
        geom_text(data = plotLabels,
                  aes(label = label, colour = plotLvl, 
                      x = yearInit + yearR,
                      y = netReturn_mean/1000000),
                  hjust = -.05,
                  vjust = 0.1,
                  alpha = plotLabels$alpha,
                  size = rel(plotLabels$size),
                  show.legend = F) +
        labs(#subtitle = paste0(plotName, "\n"),
             x = "",
             y = "Net financial return\n") +

        scale_y_continuous(label = unit_format(unit = "M$"),
                           limits = c(-600, 500)) +
                           #breaks = breaks/1000, minor_breaks = minorBreaks/1000) +
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              legend.text = element_text(size = rel(1.1)),
              legend.key.width = unit(1.5,"cm"),
              axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
              axis.text.y = element_text(size = rel(1.5)),
              axis.title = element_text(size = rel(1.5)),
              plot.subtitle = element_text(size = rel(.9)),
              plot.caption = element_text(size = rel(.75), hjust = 1)) +
        guides(colour = guide_legend(nrow = 5,
                                     override.aes = list(size = c(rep(2, length(labels)), 0.5),
                                                         alpha = c(rep(1, length(labels)), 0.25))))
    
    if(names(colList)[[i]] == "SuppMat") {
        pHarv <- pHarv +
            guides(color = FALSE) 
        pReturn <- pReturn +
            guides(color = FALSE) 
    }


    png(filename= paste0("harvVol&ecoReturn_", names(colList)[i], ".png"),
        width = 12, height = 14, units = "in", res = 600, pointsize=10)
    
    print((pHarv / pReturn) + 
              plot_layout(nrow = 2,
                          guides = "collect") &
              theme(legend.position = "bottom")
    )
    
    dev.off() 
    
}


