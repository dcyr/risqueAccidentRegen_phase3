###################################################################################################
###################################################################################################
##### Visualizing age structure
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
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
    
    output[[s]] <- get(load(paste0("../outputCompiled/outputCompiledAge_", s, ".RData")))
    
    
}
outputCompiled <- do.call("rbind", output)
outputCompiled <- outputCompiled %>%
    filter(uaf == "total")
nSims <- nrow(distinct(outputCompiled, replicate))


## management plan
managementPlan <- get(load(paste0("../", s, "/managementPlan.RData")))
plan <- managementPlan$baseline
regenMaxAge <- plan$regenMaxAge
# oldMinProp <- plan$oldMinProp
oldMinAge <- plan$oldMinAge
ageLevels <- c(paste0("Regenerating stands (<", regenMaxAge, " y.old)"),
               paste0("Old stands (>=", oldMinAge, " y.old)"))

targets <- data.frame(target = c(plan$regenMaxProp, plan$oldMinProp),
                      var = factor(ageLevels, levels = ageLevels))




df  <-  outputCompiled %>%
    mutate(oldProp = oldArea_ha/managedAreaTotal_ha,
           regenProp = regenArea_ha/managedAreaTotal_ha,
           ID = as.numeric(as.factor(paste(uaf, scenario, replicate))))

vars <- colnames(df)
vars <- vars[grep("Prop", vars)]
df <- melt(df, id.vars = c("uaf", "ID", "scenario", "replicate", "year"), 
           measure.vars = vars,
           variable.name = "var",
           value.name = "prop")

write.csv(select(df, scenario, uaf, replicate, year, var, prop),
          file = paste0("ageSummary.csv"), row.names = F)

## defining variables, and cleaning up names
ageLevels <- c(paste0("Regenerating stands (<", regenMaxAge, " y.old)"),
               paste0("Old stands (>=", oldMinAge, " y.old)"))

df$var <- as.character(df$var)
df[grep("regenProp", df$var), "var"] <- ageLevels[1]
df[grep("oldProp", df$var), "var"] <- ageLevels[2]
df$var <- factor(df$var, levels = ageLevels)


### summarizing results, percentile & such
summaryOld <- outputCompiled %>%
    mutate(oldProp = oldArea_ha/managedAreaTotal_ha) %>%
    group_by(uaf, scenario, year) %>%
    summarise(#p01oldProp = quantile(oldProp, .01),
              p05 = quantile(oldProp, .05),
              p25 = quantile(oldProp, .25),
              p50 = quantile(oldProp, .5),
              p75 = quantile(oldProp, .75),
              p95 = quantile(oldProp, .95)) %>%
    mutate(var = ageLevels[2])
              #p99oldProp = quantile(oldProp, .99))

summaryRegen <- outputCompiled %>%
    mutate(regenProp = regenArea_ha/managedAreaTotal_ha) %>%
    group_by(uaf, scenario, year) %>%
    summarise(#p01regenProp = quantile(regenProp, .01),
              p05 = quantile(regenProp, .05),
              p25 = quantile(regenProp, .25),
              p50 = quantile(regenProp, .5),
              p75 = quantile(regenProp, .75),
              p95 = quantile(regenProp, .95)) %>%
    mutate(var = ageLevels[1])
             # p99regenProp = quantile(regenProp, .99))


##############################################################################
### Plotting realized age structures
#######
dfSummary <- rbind(summaryOld, summaryRegen)


df <- merge(df, dfSummary)



p <- c(p.050 = "5%", p.250 = "25%", p.500 = "médiane", p.750 = "75%", p.950 = "95%")

m <- ggplot(df, aes(x = year + 2015, y = 100*prop,
                    group = ID)) +
                    #linetype = percentile, colour = variable)) +
    facet_grid(scenario ~ var) +
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
    width = 8, height = 5, units = "in", res = 600, pointsize=10)

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



