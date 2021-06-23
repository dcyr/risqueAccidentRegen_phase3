###################################################################################################
###################################################################################################
##### Visualizing harvest simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "rawOutputDir"))])
#################
require(raster)
require(tidyverse)

s <- 1 ### here I assume landscape configuration don't change among scenarios


simID <- simInfo$simID[s]
simDir <- paste0(rawOutputDir, simInfo$simDir[s])
initYear <- simInfo$initYear

######
studyArea <- raster(paste0(simDir, "/studyArea.tif"))
studyArea[is.na(studyArea)] <- 0
convFactor <- prod(res(studyArea))/10000### to convert to hectares
uaf <- raster(paste0(simDir, "/uaf.tif"))
uaf[is.na(uaf)] <- 0
uaf_RAT <- read.csv(paste0(simDir, "/uaf_RAT.csv"))

subZones <- raster(paste0(simDir, "/subZones.tif"))
subZones_RAT <- read.csv(paste0(simDir, "/subZones_RAT.csv"))

coverTypes <- raster(paste0(simDir, "/coverTypes.tif"))
coverTypes_RAT <- read.csv(paste0(simDir, "/coverTypes_RAT.csv"))



### preparing management plan inputs
plan <- get(load(paste(simDir, "managementPlan.RData", sep = "/")))$baseline

studyAreaP <- get(load(paste(simDir, "studyAreaP.RData", sep = "/")))
studyAreaP <- rasterize(studyAreaP, studyArea)

## eligible to harvest
harvEligible <- uaf %in% plan$uaf &
    subZones %in% plan$subZone


## spp eligible (commercial species)
spProductive <- coverTypes %in% as.numeric(names(plan$maturity))
spProductive[!spProductive] <- 0

spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
spEligible[!spEligible] <- NA


niv <- list(niv1 = c("Total area"),
            niv2 = c("Firebreaks", "Flammable"),
            niv3 = c("Conservation",  "Exploitable"),
            niv4 = c("Forested", "Unforested (treed)"),
            niv5 = c("Black Spruce", "Jack Pine", "Broadleafs"))

df <- expand.grid(niv)
df <- df[!(df$niv2 == "Firebreaks" & 
             df$niv4 == "Forested"),]
df[df$niv2 == "Firebreaks", "niv3"] <- NA
df[df$niv4== "Unforested (treed)", "niv5"] <- NA
df <- distinct(df) %>%
    arrange(niv1, niv2, niv3, niv4, niv5)
df[df$niv2 == "Firebreaks", "niv4"] <- NA



df[,"area_ha"] <- NA
## BS
df[2,"area_ha"] <- sum(values(coverTypes == 1 & !harvEligible), na.rm = T) * convFactor
df[6,"area_ha"] <- sum(values(coverTypes == 1 & harvEligible), na.rm = T) * convFactor
## JP
df[3,"area_ha"] <- sum(values(coverTypes == 2 & !harvEligible), na.rm = T) * convFactor
df[7,"area_ha"] <- sum(values(coverTypes == 2 & harvEligible), na.rm = T) * convFactor
## Broadleaves
df[4,"area_ha"] <- sum(values(coverTypes == 3 & !harvEligible), na.rm = T) * convFactor
df[8,"area_ha"] <- sum(values(coverTypes == 3 & harvEligible), na.rm = T) * convFactor
## firebreaks
df[1,"area_ha"] <- sum(values(studyAreaP & !studyArea), na.rm = T) * convFactor
## unproductive 
df[5,"area_ha"] <- sum(values(studyArea & !harvEligible & !spProductive), na.rm = T) * convFactor
df[9,"area_ha"] <- sum(values(studyArea & harvEligible & !spProductive), na.rm = T) * convFactor

df$niv4 <- as.character(df$niv4)
df$niv5 <- as.character(df$niv5)
df$niv3 <- as.character(df$niv3)

df[df$niv2 == "Firebreaks", c("niv4","niv5")] <- "Water bodies and other fire breaks"
df[is.na(df$niv5 ), "niv5"] <- "Unforested (treed)"
df[is.na(df$niv3 ), c("niv3", "niv4")] <- "Firebreaks"
## total area
totalArea <- sum(df$area_ha) 

#conservationArea


# Build Dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)

# treemap
 
require(treemapify)

cols = c(Conservation = "darkolivegreen1", Exploitable = "darkolivegreen", Firebreaks = "dodgerblue")


require(ggplot2)

areaSummary <- c("Study area (total)" = sum(df$area_ha),
                 "Study area (inflammable)" = sum(df$area_ha) - df[1, "area_ha"],
                 "Forested area (productive forest)" = sum(df[which(df$niv4 == "Forested"), "area_ha"]),
                 "Forested area (commercial species)" = sum(df[which(df$niv5 %in% c("Black Spruce", "Jack Pine")), "area_ha"]),
                 "Exploitable area (commercial species, excl. conservation area)" = sum(df[6:7, "area_ha"]))

write.csv(data.frame(partition = names(areaSummary),
                     area_ha = areaSummary ), file = "areaSummary.csv", row.names = F)

lab <- character()
for (i in seq_along(areaSummary)) {
    x <- areaSummary[i]
    lab <- append(lab, paste0(names(x), ": ", x, " ha"))
}
lab <- paste(lab, collapse = "\n")

png(filename = "areaSummary.png",
    width = 12, height = 8, units = "in", res = 300)

ggplot(df, aes(area = area_ha,
               fill = niv3,
               subgroup = niv3,
               #colour = niv5,
               label = paste0(niv5,"\n", area_ha, " ha"))) +
    geom_treemap() +
    geom_treemap_text() +
    scale_fill_manual("", values = cols) +
    labs(title = "Summary of study area",
        caption = lab)

dev.off()


