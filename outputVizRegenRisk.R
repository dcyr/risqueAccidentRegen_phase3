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


df <- output %>%
    group_by(simID, fireScenario, mgmtScenario, year, replicate) %>%
    summarise(regenFailureWMean = weighted.mean(x = regenFailure30,
                                               w = area_ha),
              area_ha = sum(area_ha)) %>%
    group_by(simID, fireScenario, mgmtScenario, year) %>%
    summarise(area_ha = unique(area_ha),
              coverType = "EN + PG",
              regenFailureVulnMean = round(mean(regenFailureWMean), 4),
              regenFailureVulnP05 = round(quantile(regenFailureWMean, 0.05), 4),
              regenFailureVulnP25 = round(quantile(regenFailureWMean, 0.25), 4),
              regenFailureVulnP50 = round(quantile(regenFailureWMean, 0.50), 4),
              regenFailureVulnP75 = round(quantile(regenFailureWMean, 0.75), 4),
              regenFailureVulnP95 = round(quantile(regenFailureWMean, 0.95), 4))
              

df2 <- output %>%
    group_by(simID, fireScenario, mgmtScenario, year, coverType) %>%
    summarise(area_ha = mean(area_ha),
              regenFailureVulnMean = round(mean(regenFailure30), 4),
              regenFailureVulnP05 = round(quantile(regenFailure30, 0.05), 4),
              regenFailureVulnP25 = round(quantile(regenFailure30, 0.25), 4),
              regenFailureVulnP50 = round(quantile(regenFailure30, 0.50), 4),
              regenFailureVulnP75 = round(quantile(regenFailure30, 0.75), 4),
              regenFailureVulnP95 = round(quantile(regenFailure30, 0.95), 4))

df <- rbind(df, df2[,colnames(df)])

yearInit <- simInfo$initYear

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
                "Variable retention only" =
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
                                    "+ salv. logging (no planting)",
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

yMax <- max(df$regenFailureVulnMean)
yMin <- min(df$regenFailureVulnMean)
yMax <- ceiling(yMax*10)/10
yMin <- floor(yMin*10)/10


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
    plotMean <- df %>%
        filter(coverType == "EN + PG") %>%
        mutate(plotLvl = ifelse(simID %in% sID, labels[match(simID, sID)],
                                ""),
               plotLvl = factor(plotLvl, levels = labels),
               #cols = ifelse(!is.na(plotLvl),
               #              cols[match(simID, sID)], tail(cols, 1)),
               size = ifelse(is.na(plotLvl), 0.25,
                             ifelse(plotLvl == "Clearcutting only", 1.5, 1)),
               alpha = ifelse(is.na(plotLvl), 0.25, 1),
               linetype =  linetype[match(fireScenario, names(linetype))])
    
    plotRibbon <- plotMean %>%
        filter(!is.na(plotLvl))
    
    
    ############################################################################
    #### Landscape averages
    pMean[[i]] <- ggplot(data = plotMean, aes(x = yearInit + year,
                                              y = regenFailureVulnMean,
                                              group = simID)) +
        geom_line(size = plotMean$size,
                  alpha = plotMean$alpha,
                  linetype = plotMean$linetype,
                  aes(colour = plotLvl)) +
        geom_ribbon(data = plotRibbon, aes(ymin = regenFailureVulnP25,
                        ymax = regenFailureVulnP75,
                        group = simID, 
                        fill = plotLvl),
                    show.legend = FALSE,
                    alpha = 0.25) +
        scale_colour_manual("",
                            values = cols, #c("grey", cols),
                            labels = labels,
                            #na.translate = F,
                            na.value = "black",
                            guide = guide_legend(nrow = length(unique(plotMean$plotLvl)))) +
        scale_fill_manual(values = cols) +
        labs(subtitle = paste0(letters[i], ") ", plotName, "\n"),
             caption = caption,
             x = "",
             y = "Proportion at risk\n") +
        theme_bw() +
        ylim(0.1, 0.3) +
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
   
}


require(patchwork)

png(filename= paste0("regenFailureVuln_baseline.png"),
    width = 12, height = 12, units = "in", res = 600, pointsize=10)

print((pMean[[1]] + pMean[[2]] + pMean[[3]]) /
          (pMean[[4]] + pMean[[5]] + pMean[[6]]) +
          plot_annotation(
              title = "Vulnerability to regeneration failure at the landscape level - Baseline fire scenarios",
              subtitle = "Full and dotted lines represent average values for 'baseline' and 'RCP 8.5' fire scenarios, respectively, while ribbons encompass 50% of simulations for selected treatments.",
              caption = paste0("Regeneration failure is defined as post-fire tand potential productivity lower than 30 cub-m / ha at 120 yr-old, independently of pre-fire attributes.",
                              "\nAll panels represent the same simulations, highlighting only specific ones.",
                              "\nEach line represents the average of 100 simulations."),
              theme = theme(
                  plot.title = element_text(size = rel(1.5)),
                  plot.caption = element_text(size = rel(.8), hjust = 0))
              
          ) 
)
dev.off()
write.csv(df, file = "regenFailureVulnSummary.csv", row.names = F)

