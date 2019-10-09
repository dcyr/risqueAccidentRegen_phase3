###################################################################################################
###################################################################################################
##### Visualizing fire simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-23")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
# scenario <- "baseline"
#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
initYear <- 2015
####################################################################
####################################################################
######
require(raster)
require(dplyr)
studyArea <- raster("../studyArea.tif")
fireZones <- raster("../fireZones.tif")
fireZones_RAT <- read.csv("../fireZones_RAT.csv")

## to focus on study area
fireZones[is.na(studyArea)] <- NA 

##
convFactor <- prod(res(studyArea))/10000### to convert to hectares
fireZoneArea <- zonal(!is.na(fireZones), fireZones, sum)
fireZoneArea <- data.frame(zone = as.character(fireZones_RAT[match(fireZoneArea[,1], fireZones_RAT$ID),"Zone_LN"]),
                           areaZone_ha = fireZoneArea[,2] * convFactor)
totalArea <- sum(fireZoneArea$areaZone_ha)
fireZoneArea <- rbind(fireZoneArea, data.frame(zone = "total", areaZone_ha = totalArea))
fireZoneArea[, "prop"] <- fireZoneArea$areaZone_ha/fireZoneArea[which(fireZoneArea$zone == "total"),"areaZone_ha"]
names(fireZoneArea) <- gsub("zone", "Zone_LN", names(fireZoneArea))
# 
# ### computing global fire regime attributes (per period)
fcGlobal <- fireZones_RAT %>%
    mutate(propAAB = 1/Fire_Cycle) %>%
    merge(fireZoneArea) %>%
    mutate(weightedAAB = propAAB * prop) %>%
    group_by() %>%
    summarise(Fire_Cycle = round(1/sum(weightedAAB))) %>%
    mutate(ID = NA, Zone_LN = "total")

fireZones_RAT <- rbind(fireZones_RAT, fcGlobal)
# fireRegimeAttrib <- rbind(fireRegimeAttrib, as.data.frame(fireRegimeAttribGlobal))


#############################################################
#############################################################

outputCompiled <- get(load(paste0("outputCompiledFire_", scenario, ".RData")))


#################################################################################
#################################################################################
####### figure for baseline scenario (temporally constant, spatially heterogenous)
#################################################################################
require(dplyr)
## summarizing fire regimes


# outputSummary <- outputCompiled %>%
#     #filter(scenario == scenario) %>%
#     
#     arrange(Zone_LN, replicate, year)


## create data frame with annual data
df <- outputCompiled %>%
    group_by(Zone_LN, replicate, year) %>%
    summarize(#fireCycle = round((1/mean(areaBurnedTotal_ha/areaZoneTotal_ha))),
              propAAB = mean(areaBurnedTotal_ha/areaZoneTotal_ha)) %>%
    filter(Zone_LN == "total") %>%
    ungroup() %>%
    mutate(scenario = scenario) %>%
    select(scenario, replicate, year, propAAB)
write.csv(df, file = paste0("fireSummary_", scenario, ".csv"), row.names = F)

## create data frame with 
outputSummary <- outputCompiled %>%
    group_by(Zone_LN, replicate) %>%
    summarize(fireCycle = round((1/mean(areaBurnedTotal_ha/areaZoneTotal_ha))),
              propAAB = mean(areaBurnedTotal_ha/areaZoneTotal_ha)) %>%
    arrange(Zone_LN, replicate)

fcSummary <- outputSummary %>%
    group_by(Zone_LN) %>%
    summarize(realizedFC_median = median(fireCycle),
              realizedFC_mean = 1/mean(propAAB)) %>%
    merge(fireZones_RAT, all.x = T ) %>%
    merge(fireZoneArea)



######################
### Plotting

require(ggplot2)
options(scipen=999)
m <- ggplot(outputSummary, aes(x=fireCycle)) +
    geom_histogram() +#fill = "grey25"
    facet_wrap(~Zone_LN) +#, scales = "free_x") +
    
    scale_x_log10(breaks = c(30, 60, 125, 250, 500, 1000, 2000, 4000, 16000)) +
    geom_vline(data = fcSummary,  aes(xintercept = Fire_Cycle),
               colour="lightblue", linetype = 3, size = 0.7, alpha = 1) +
    geom_vline(data = fcSummary,  aes(xintercept = realizedFC_mean),
               colour="yellow", linetype = 3, size = 0.5, alpha = 1)

yMax <- layer_scales(m)$y$range$range[2]
xMax <- layer_scales(m)$x$range$range[2]

labelDF <-  data.frame(x = 10^xMax, y = yMax, Zone_LN = fcSummary$Zone_LN,
                       prop = paste0("Percent area: ", round(fcSummary$prop*100), "%"),
                       target = paste("Target:", fcSummary$Fire_Cycle, "years"),
                       mean = paste("Average:", round(fcSummary$realizedFC_mean), "years"))

png(filename = paste0("realizedFC_baseline.png"),
    width = 10, height = 5, units = "in", res = 600, pointsize=10)

print(m + theme_dark() +
          theme(legend.position="top", legend.direction="horizontal",
                axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text.y = element_text(size = 8))+
          labs(title ="Distribution of realized fire cycles",
               subtitle = "Baseline scenario - Dotted lines indicate targetted (blue) and realized (yellow) average fire cycles",
               caption = paste("*Total of", length(unique(outputSummary$replicate)), "realizations" ),
               x = "Fire cycle (years)",
               y = "Frequency") +
          geom_text(aes(x, y, label = prop),
                    data = labelDF, hjust = 1, size = 3, fontface = 1) +
          geom_text(aes(x, 0.85*y, label = target),
                    data = labelDF, hjust = 1, size = 3, colour = "lightblue") +
          geom_text(aes(x, 0.75*y, label = mean),
                    data = labelDF, hjust = 1, size = 3, colour = "yellow"))



dev.off()


# ####################################################################################################
# ####################################################################################################
# ######
# ###### Visualizing one realisation
# ################################
# require(rgdal)
# require(rgeos)
# require(raster)
# require(maptools)
# require(ggplot2)
# 
# fireZonesP <- get(load("../fireZonesP.RData"))
# studyAreaP <- get(load("../studyAreaP.RData"))
# 
# ### cropping 20 km to remove 'border effect'
# extentFig <- extent(fireZones)
# extentFig[c(1,3)] <- extentFig[c(1,3)]+20000
# extentFig[c(2,4)] <- extentFig[c(2,4)]-20000
# 
# fireZones <- crop(fireZones, extentFig)
# fireZonesP <- crop(fireZonesP, extentFig)
# studyAreaP <- crop(studyAreaP, extentFig)
# tsdInit <-  raster("../tsd.tif")
# 
# fireZonesF <- fortify(fireZonesP)
# studyAreaF <- fortify(studyAreaP)
# 
# 
# 
# ################################
# 
# fire <- get(load("../output/outputFire_008.RData"))
# #output <- get(load("simOutput_002.RData"))
# 
# 
# 
# require(raster)
# require(stringr)
# require(ggplot2)
# 
# fTitle <- character()
# for (l in 1:nlayers(fire)) {
#     if(l == 1) {
#         tsd <- tsdInit
#         tsd[!is.na(tsd)] <- 100
#         tsd <- crop(tsd, extentFig)
#         rNA <- is.na(tsd)
#         rNA[rNA == 0] <- NA
#         rNA <- rasterToPoints(rNA)
#         rNA[,3] <- NA
#         maxVal <- max(values(tsd), na.rm = T) + nlayers(fire)
#         
#     }
#     options(scipen=999)
# 
#     ### data to plot
#     r <- fire[[l]]
#     r <- crop(r, extentFig)
#     tsd[r==1] <- 0
#     tsd[tsd>150] <- 150
#     
#     
#     df <- rasterToPoints(tsd)
#     if(l == 1) {
#         colnames(rNA)[3] <- colnames(df)[3]
#     }
#     #
#     df <- rbind(df, data.frame(rNA))
# 
#     ### plotting parameters
#     pWidth  <- 1400
#     pHeight <- 1200
#     pointsize <- 8
#     
#     colValues <- c(0, 10, 25, 50, maxVal)
#     #cols = c("red", "orange", "gold2", "seagreen4", "darkgreen")
#     cols = c("red", "orange", "gold2", "forestgreen", "darkgreen")
# 
#     ### plotting
#     p <- ggplot(data = df, aes_string("x", "y", fill = colnames(df)[3])) +
#         theme_bw() +
#         #theme(legend.position="top", legend.direction="horizontal") +
#         geom_raster() +
#         coord_fixed() +
#         scale_fill_gradientn(name = "Time since last fire (years)", #limits = c(0,1),
#                              colours = cols,
#                              values = colValues/maxVal, limits = c(0,maxVal),
#                              na.value = "dodgerblue1") +
#         #coord_fixed(ratio = figRatio) +
#         geom_polygon(aes(x = long, y = lat, group = group), data = fireZonesF,
#                      colour = 'black', fill = NA, alpha = 0.5, size = 0.5) +
#         geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
#                      colour = 'white', fill = NA, size = 1) +
#         labs(x = "\nx (UTM 18)",
#               y = "y (UTM 18)\n") +
#         theme(legend.position="top", legend.direction="horizontal") +
#         annotate("text", x = max(df$x), y = max(df$y)+2500,
#                  label = paste("année", l),
#                  hjust = 1, vjust = 0, size = 0.3*pointsize, fontface = 2)
# 
#     f <- paste0("tsfTest_" , str_pad(l, nchar(nlayers(fire)), pad = "0"),".png")
#     fTitle <- append(fTitle, f)
# 
#     png(filename = f,
#         width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
#         bg = "white")
# 
#         print(p + theme(plot.title = element_text(size = rel(0.6)),
#                         axis.title.x = element_text(size = rel(0.5)),
#                         axis.title.y = element_text(size = rel(0.5)),
#                         axis.text.x = element_text(size = rel(0.5)),
#                         axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
#                         legend.title = element_text(size = rel(0.75)),
#                         legend.text = element_text(size = rel(0.5))))
# 
#     dev.off()
#     
#     ## aging landscape
#     tsd <- tsd + 1
#    # return(fTitle)
# }
# 
# require(animation)
# oopt = ani.options(ani.dev="png", ani.type="png",
#                    interval = 0.3, autobrowse = FALSE)
# 
# 
# im.convert(c(fTitle, rep(fTitle[length(fTitle)], 10)), output = "tsfExample.gif",
#            extra.opts = "", clean = F)
# ####################################################################################################
###################################################################################################
