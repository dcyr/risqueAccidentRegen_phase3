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
# initYear <- 2015
# ####################################################################
# ####################################################################
# ######

### fetching compiled results

### fetching compiled results
output <- list()
for (s in seq_along(simInfo$simID)) {
    simID <- simInfo$simID[s]
    simDir <- simInfo$simDir[s]
    output[[s]] <- get(load(paste0("../outputCompiled/outputCompiledAge_", simID, ".RData")))
    
    
}
output <- do.call("rbind", output)
output <- output %>%
    filter(uaf == "total")

output[,"clearcutting"] <- simInfo$clearcutting[match(output$simID, simInfo$simID)]
output[,"varReten"] <- simInfo$varReten[match(output$simID, simInfo$simID)]
output[,"salv"] <- simInfo$salv[match(output$simID, simInfo$simID)]
output[,"plantPostFire"] <- simInfo$plantPostFire[match(output$simID, simInfo$simID)]
output[,"plantPostSalv"] <- simInfo$plantPostSalv[match(output$simID, simInfo$simID)]
output[,"plantPostFireSp"] <- simInfo$plantPostFireSp[match(output$simID, simInfo$simID)]
output[,"plantLimitedAccess"] <- simInfo$plantLimitedAccess[match(output$simID, simInfo$simID)]



nSims <- nrow(distinct(output, replicate))


## management plan
managementPlan <- get(load(paste0(rawOutputDir, simDir, "/managementPlan.RData")))
plan <- managementPlan$baseline
regenMaxAge <- plan$regenMaxAge
# oldMinProp <- plan$oldMinProp
oldMinAge <- plan$oldMinAge
ageLevels <- c(paste0("Regenerating stands (<", regenMaxAge, " y.old)"),
               paste0("Old stands (>=", oldMinAge, " y.old)"))

targets <- data.frame(target = c(plan$regenMaxProp, plan$oldMinProp),
                      var = factor(ageLevels, levels = ageLevels))




df  <-  output %>%
    mutate(oldProp = oldArea_ha/managedAreaTotal_ha,
           regenProp = regenArea_ha/managedAreaTotal_ha)
#,
#           ID = as.numeric(as.factor(paste(uaf, scenario, replicate))))

vars <- colnames(df)
vars <- vars[grep("Prop", vars)]
idVars <- c("simID", "replicate", "year", "uaf", "fireScenario",  
            "clearcutting", "varReten", "salv", "plantPostFire", "plantPostSalv", "plantPostFireSp", "plantLimitedAccess")
df <- melt(df, id.vars = idVars, 
           measure.vars = vars,
           variable.name = "var",
           value.name = "prop")

write.csv(df,
          file = paste0("ageSummary.csv"), row.names = F)

## defining variables, and cleaning up names
ageLevels <- c(paste0("Regenerating stands (<", regenMaxAge, " y.old)"),
               paste0("Old stands (>=", oldMinAge, " y.old)"))

df$var <- as.character(df$var)
df[grep("regenProp", df$var), "var"] <- ageLevels[1]
df[grep("oldProp", df$var), "var"] <- ageLevels[2]
df$var <- factor(df$var, levels = ageLevels)


idVars <- c(
            )

### summarizing results, percentile & such
dfSummary <- df %>%
    group_by(simID, year, uaf, fireScenario,
             clearcutting, varReten, salv, plantPostFire, plantPostSalv, plantPostFireSp, plantLimitedAccess,
             var) %>%
    summarise(p05 = quantile(prop, .05),
              p25 = quantile(prop, .25),
              p50 = quantile(prop, .5),
              p75 = quantile(prop, .75),
              p95 = quantile(prop, .95))
   


##############################################################################
### Plotting realized age structures
#######


df <- merge(df, dfSummary)



p <- c(p05 = "5%", p25 = "25%", p50 = "médiane", p75 = "75%", p95 = "95%")

m <- ggplot(df, aes(x = year + 2015, y = 100*prop,
                    group = replicate)) +
                    #linetype = percentile, colour = variable)) +
    facet_grid(simID ~ var) +
    geom_line(colour = "black", alpha = 0.1) +#
    geom_line(aes(y = 100*p50, group = 1),
              colour = "lightblue",
              linetype = 1, size = 0.7, alpha = 1) + #fill = "grey25"
    geom_line(aes(y = 100*p05, group = 1),
              colour = "yellow",
              linetype = 3, size = 0.3, alpha = 1) +
    geom_line(aes(y = 100*p95, group = 1),
              colour = "yellow",
              linetype = 3, size = 0.3, alpha = 1) +
    geom_line(aes(y = 100*p25, group = 1),
              colour = "yellow",
              linetype = 4, size = 0.4, alpha = 1) +
    geom_line(aes(y = 100*p75, group = 1),
              colour = "yellow",
              linetype = 4, size = 0.4, alpha = 1) +
    geom_hline(data = targets, aes(yintercept = 100*target, colour = "indianred"),
               linetype = "dashed") +
    scale_linetype_discrete(guide=FALSE) +
    #scale_colour_manual(values = c("black", "red", "blue"))
    guides(colour=FALSE)
    

png(filename= paste0("ageStructureRealized.png"),
    width = 5, height = 22, units = "in", res = 600, pointsize=10)

#options(scipen=999)

print(m + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
                legend.title = element_text(size = rel(0.85)),
                title = element_text(size = rel(0.85)),
                #plot.subtitle = element_text(size = rel(1)),
                plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = "Structure d'âge",
               #subtitle = paste0(percentile, "e percentile"),
               subtitle = #paste0("En bleu sont illustrées les médianes et en jaune les percentiles ",
                   paste0("La médiane est illustrée en bleu tandis que les percentiles ",
                          p[1], ", ", p[2], ", ", p[4], " et ", p[5],
                          # ",\nsur un total de ", nRep, " réalisations."),
                          #sur un total de ", nRep, " réalisations."),
                          "",
                          " sont illustrés en jaune.\n",
                          "Les seuils d'altération acceptable pouvant contraindre les récoltes sont quant à eux indiqués en rouge.\n",
                          "(Total de ", nSims, " simulations)"),
               caption = paste0("Âge de récolte - Épinette noire: 90 ans\n",
                                "Pin gris: 76 ans\n",
                                "Cycle des feux - baseline: 104 ans\n",
                                "Min vieilles forêts (>=100 ans): ", targets[2,1]*100, "%\n",
                                "Max régén. (< 20 ans): ",targets[1,1]*100, "%"),
               x = "",
               y = "Proportion du territoire éligible à la récolte (%)\n"))

dev.off()



