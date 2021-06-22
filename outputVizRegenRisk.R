###################################################################################################
###################################################################################################
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "rawOutputDir"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-23")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
#################
require(raster)
require(ggplot2)
require(dplyr)
require(reshape2)
require(stringr)
# initYear <- 2015
# ####################################################################
# ####################################################################
# ######
### fetching compiled results
output <- list()
for (s in seq_along(simInfo$simID)) {
    simID <- simInfo$simID[s]
    simDir <- simInfo$simDir[s]
    output[[s]] <- read.csv(paste0("../outputCompiled/outputCompiledRegenFailure_", simID, ".csv"))
    
    
}

output <- do.call("rbind", output)
output$simID <- str_pad(output$simID, 2, side = "left", pad = "0")
output[,"id_ms"] <- simInfo$id_ms[match(output$simID, simInfo$simID)]


df <- output %>%
    group_by(simID, id_ms, fireScenario, mgmtScenario, year, replicate) %>%
    summarise(regenFailureWMean = weighted.mean(x = regenFailure30,
                                               w = area_ha),
              area_ha = sum(area_ha)) %>%
    group_by(simID, id_ms, fireScenario, mgmtScenario, year) %>%
    summarise(area_ha = unique(area_ha),
              coverType = "EN + PG",
              regenFailureVulnMean = round(mean(regenFailureWMean), 4),
              regenFailureVulnP05 = round(quantile(regenFailureWMean, 0.05), 4),
              regenFailureVulnP25 = round(quantile(regenFailureWMean, 0.25), 4),
              regenFailureVulnP50 = round(quantile(regenFailureWMean, 0.50), 4),
              regenFailureVulnP75 = round(quantile(regenFailureWMean, 0.75), 4),
              regenFailureVulnP95 = round(quantile(regenFailureWMean, 0.95), 4))
              

df2 <- output %>%
    group_by(simID, id_ms, fireScenario, mgmtScenario, year, coverType) %>%
    summarise(area_ha = mean(area_ha),
              regenFailureVulnMean = round(mean(regenFailure30), 4),
              regenFailureVulnP05 = round(quantile(regenFailure30, 0.05), 4),
              regenFailureVulnP25 = round(quantile(regenFailure30, 0.25), 4),
              regenFailureVulnP50 = round(quantile(regenFailure30, 0.50), 4),
              regenFailureVulnP75 = round(quantile(regenFailure30, 0.75), 4),
              regenFailureVulnP95 = round(quantile(regenFailure30, 0.95), 4))

df <- rbind(df, df2[,colnames(df)])

##### focusing of selected simulations
df <- filter(df, !is.na(id_ms))

yearInit <- simInfo$initYear


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





yMax <- max(df$regenFailureVulnMean)
yMin <- min(df$regenFailureVulnMean)
yMax <- ceiling(yMax*10)/10
yMin <- floor(yMin*10)/10

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
    plotMean <- df %>%
        filter(coverType == "EN + PG") %>%
        mutate(plotLvl = factor(labels[match(id_ms, sID)]),
               size = ifelse(is.na(plotLvl), 0.5,
                             ifelse(plotLvl == "Clearcutting only", 2, 1.5)),
               alpha = ifelse(is.na(plotLvl), 1, 1),
               linetype =  linetype[match(fireScenario, names(linetype))])
    plotMean[grepl("full access", plotMean$plotLvl), "linetype"] <- 3
    
    plotRibbon <- plotMean %>%
        filter(!is.na(plotLvl))
    
    if(names(colList[i]) == "MS") {
      yLim <- c(0.12, 0.30)
    }
    if(names(colList[i]) == "SuppMat") {
      yLim <- c(floor(min(plotMean$regenFailureVulnMean)*10)/10, 
                ceiling(max(plotMean$regenFailureVulnMean)*10)/10)
    }
    
    
  
    plotLabels <- plotMean %>%
      filter(regenFailureVulnMean < yLim[2]) %>%
      group_by(id_ms) %>%
      filter(year == max(year)) %>%
      mutate(label = paste0("S", id_ms),
             hjust = ifelse(is.na(plotLvl), -0.05, -0.05),
             vjust = ifelse(is.na(plotLvl), -0.05, -0.05),
             alpha = ifelse(is.na(plotLvl), 1, 1),
             size = ifelse(is.na(plotLvl), 4, 6)) %>%
      ungroup() %>%
      mutate(regenFailureVulnMean = ifelse(year<max(year), yLim[2],
                                           regenFailureVulnMean)) %>%
      select(id_ms, year, regenFailureVulnMean, plotLvl,
             label, hjust, vjust, alpha, size)
             

    
    ############################################################################
    #### Landscape averages
    pMean <- ggplot(data = plotMean, aes(x = yearInit + year,
                                              y = regenFailureVulnMean,
                                              group = simID)) +
        geom_line(size = plotMean$size,
                  alpha = plotMean$alpha,
                  linetype = plotMean$linetype,
                  aes(colour = plotLvl)) +
        scale_colour_manual("",
                            values = cols, 
                            na.translate = T,
                            na.value = "black") +
        scale_fill_manual(values = cols) +
        labs(#subtitle = paste0(letters[i], ") ", plotName, "\n"),
            #caption = caption,
             x = "",
             y = "Proportion of landscape at risk\n(%)\n") + 
        ylim(yLim[1], yLim[2]) +
        geom_text(data = plotLabels,
                  aes(label = label, colour = plotLvl, 
                      x = year + yearInit, y = regenFailureVulnMean),
                  hjust = plotLabels$hjust,
                  vjust = plotLabels$vjust,
                  alpha = plotLabels$alpha,
                  size = rel(plotLabels$size),
                  show.legend = F) +
        theme(legend.position = "bottom",
              legend.title=element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              plot.subtitle = element_text(size = rel(.9)),
              plot.caption = element_text(size = rel(.6), hjust = 1)) +
        guides(colour = guide_legend(nrow = 5,
                                     override.aes = list(size = c(rep(2, length(labels)), 0.5),
                                                         alpha = c(rep(1, length(labels)), 1),
                                                         linetype = c(ifelse(grepl("full access", labels), 3, 1),3))))
               
    
    if(names(colList)[[i]] == "SuppMat") {
      pMean <- pMean +
        theme(legend.position = "none")
      # guides(colour = guide_legend(nrow = 3,
      #                              override.aes = list(linetype =  c(rep(1,9), rep (3,6)))))
        
    }
    
    png(filename= paste0("propAtRisk__", names(colList)[i], ".png"),
        width = 12, height = 10, units = "in", res = 600, pointsize=10)
    
    print(pMean +
              labs(#title = "Landscape vulnerability to post-fire regeneration failure", 
                   #subtitle = paste(colList[[i]]$caption, "- Each line represents the mean of 100 simulations.\nFull and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively respectively, while ribbons encompass 50% of simulations for selected treatments.")) +
              ) +
                theme(plot.title = element_text(size = rel(1.5)),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5)),
                    axis.text.y = element_text(size = rel(1.5)),
                    axis.title = element_text(size = rel(1.5)),
                    legend.text = element_text(size = rel(1.2)),
                    legend.key.width = unit(1.5,"cm"),
                    plot.subtitle = element_text(size = rel(.9)),
                    plot.caption = element_text(size = rel(.75), hjust = 1)
              ) 
    )
    
    dev.off() 
}

