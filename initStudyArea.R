####################################################################################################
####################################################################################################
###### Preparation of fire regime parameters and study area + subzones
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3", sep ="/"))
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#################
require(rgdal)
require(raster)
require(rgeos)
require(dplyr)
require(ggplot2)
#################

###################################################################################################
####################################################################################################
###### preparing  and plotting spatial information for fire zones
####################################################################################################
fireZonesP <- readOGR(dsn = "../gis", layer = "fireZones_Gauthier2015")
studyAreaP <- readOGR(dsn = "../gis", layer = "Pastille_Merge_dd")
lakesP <- readOGR(dsn = "../gis", layer = "canadlake_p")

### projecting into UTM 18 (seems appropriate for that study area)
proj <- CRS("+init=epsg:26918")
fireZonesP <- spTransform(fireZonesP, CRSobj = proj)
studyAreaP <- spTransform(studyAreaP, CRSobj = proj)
lakesP <- spTransform(lakesP, CRSobj = proj)

### defining the simulation area (study area extent + 50 km buffer for fires)
spatialExtent <- extent(studyAreaP)

# creating buffer, after rounding extent to the nearest hundred
bufferSize <- 50000 ## in meters
spatialExtent[c(1, 3)] <- round(spatialExtent[c(1, 3)], -3) - bufferSize
spatialExtent[c(2, 4)] <- round(spatialExtent[c(2, 4)], -3) + bufferSize
# creating raster from spatialExtent
spatialExtent <- raster(spatialExtent, resolution = 500, crs = projection(studyAreaP))

# cropping fireZones and lakes
fireZonesP <- crop(fireZonesP, spatialExtent)
lakesP <- crop(lakesP, spatialExtent)
save(fireZonesP, file = "fireZonesP.RData")

### 'rasterizing' fireZones, study area (+ subzones) and lakes

# fireZones
fireZones <- rasterize(fireZonesP, spatialExtent)
fireZones_RAT <- distinct(as.data.frame(fireZonesP))
fireZones_RAT <- data.frame(ID = rownames(fireZones_RAT),
                            fireZones_RAT)
# Management units
studyAreaUAF <- rasterize(studyAreaP, spatialExtent, field = "NO_UAF")
studyAreaUafRAT <- distinct(as.data.frame(studyAreaP$NO_UAF))
studyAreaUafRAT <- data.frame(ID = match(studyAreaUafRAT$`studyAreaP$NO_UAF`, levels(studyAreaP$NO_UAF)),
                              UAF = studyAreaUafRAT$`studyAreaP$NO_UAF`)
studyAreaUafRAT <- studyAreaUafRAT[complete.cases(studyAreaUafRAT),]
# Forest management subzones
studyAreaInEx <- rasterize(studyAreaP, spatialExtent, field = "INC_CO_EXC")
studyAreaInExRAT <- distinct(as.data.frame(studyAreaP$INC_CO_EXC))
studyAreaInExRAT <- data.frame(ID = match(studyAreaInExRAT$`studyAreaP$INC_CO_EXC`, levels(studyAreaP$INC_CO_EXC)),
                               InEx = studyAreaInExRAT$`studyAreaP$INC_CO_EXC`)

###
write.csv(merge(studyAreaInExRAT, read.csv("../data/subZones_RAT.csv")),
          file = "subZone_RAT.csv", row.names = F)


###
# cleaning up studyArea (small islands, inside and outside main periphery)
studyArea <- !is.na(studyAreaInEx)
studyArea[studyArea == 0] <- NA

# removing clumps (patches) from the study area
# has to be done manually at this moment
rc <- clump(studyArea)
studyArea[rc %in% c(2,3,4)] <- NA ## disconnected 'islands' (very small...)
rc <- clump(is.na(studyArea))
studyArea[rc %in% c(2,3,4,5,6,7)] <- 1
# saving as polygon for more plotting options
studyAreaP <- rasterToPolygons(studyArea, dissolve = T)
save(studyAreaP, file = "studyAreaP.RData")

# removing lakes from firezones and study area and subzones
fireZones[lakes>0] <- NA
studyArea[lakes>0] <- NA
studyAreaUAF[lakes>0] <- NA
studyAreaInEx[lakes>0] <- NA

#
lakes <- rasterize(lakesP, studyArea)
lakes[!is.na(lakes)] <- 1
#####
writeRaster(fireZones, file = "fireZones.tif", overwrite = T)
write.csv(fireZones_RAT, file = "fireZones_RAT.csv", row.names = F)
writeRaster(studyAreaUAF, file = "studyAreaUAF.tif", overwrite = T)
write.csv(studyAreaUafRAT, file = "studyAreaUafRAT.csv", row.names = F)
writeRaster(studyAreaInEx, file = "studyAreaInEx.tif", overwrite = T)
write.csv(studyAreaInExRAT, file = "studyAreaInExRAT.csv", row.names = F)
writeRaster(studyArea, file = "studyArea.tif", overwrite = T)
writeRaster(lakes, file = "lakes.tif")
#####


###############
### plotting fire zones
if(exists("fireZones"))
r <- raster("../data/fireZones.tif")
###############
rNA <- r
rNA[] <- NA
rNA[is.na(r)] <- 1
df <- rasterToPoints(r)
rNA <- rasterToPoints(rNA)
#rNA[,3] <- NA
rNA[,3] <- NA
colnames(rNA)[3] <- colnames(df)[3]
df <- rbind(df, data.frame(rNA))

fireZones_RAT <- read.csv("../data/fireZones_RAT.csv")


df <- data.frame(df, fireZones_RAT[match(df[, "fireZones"],
                                         fireZones_RAT[complete.cases(fireZones_RAT), "ID"]),
                                            c("Zone_LN", "Fire_Cycle")])


zoneLevels <- levels(df$Zone_LN)[order(as.numeric(gsub("[^0-9]", "", levels(df$Zone_LN))))]
df$Zone_LN <- factor(df$Zone_LN, levels = c(zoneLevels, "Large water bodies"))
df[is.na(df$Zone_LN), "Zone_LN"] <- "Large water bodies"
levels(df$Zone_LN) <- c(paste0(zoneLevels, " (", fireZones_RAT[match(zoneLevels, fireZones_RAT$Zone_LN), "Fire_Cycle"], " years)"),
                        "Large water bodies")


studyAreaP <- get(load("../data/studyAreaP.RData"))
studyAreaF <- fortify(studyAreaP)

###############
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8
fireCols <- c("burlywood4", "darkseagreen4", "lightgoldenrod3", "orangered4", "palegreen4",
              "orangered3", "lightsteelblue3", "darkgreen", "bisque3", "coral3", "dodgerblue1")

p <- ggplot(data = df, aes_string("x", "y", fill = "Zone_LN")) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
                 colour = 'white', fill = NA, size = 1) +
    
    scale_fill_manual(name = "",
                      #palette = "Set1",
                      values = fireCols,
                      na.value = "grey") +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "fireZones.png",
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
    bg = "white")

options(scipen=999)
print(p + theme(plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.5)),
                axis.title.y = element_text(size = rel(0.5)),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                legend.title = element_text(size = rel(0.75)),
                legend.text = element_text(size = rel(0.55))))

dev.off()

####################################################################################################
####################################################################################################
###### creating final input table for fire regimes (baseline and projected)
####################################################################################################
fireZoneTable <- data.frame(levels(fireZones))
names(fireZoneTable)[2] <- "fireCycle"


fireRegime <- list()
for (p in c("2011-2040", "2041-2070")) {
    df <- data.frame(fireZoneTable, scenario = "baseline", period = p)
    
    # fc85 <- fireCyclesProj %>%
    #     filter(scenario == "RCP85",
    #            period == p)
    # fc85 <- fc85$fireCycle
    # df85 <-  data.frame(fireZoneTable, scenario = "RCP85", period = p)
    # df85$fireCycle <- fc85
    
    fireRegime[[p]] <- df#rbind(df, df85)
    
    
}

fireRegime <- do.call("rbind", fireRegime)
rownames(fireRegime) <- 1:nrow(fireRegime)

### creating a table with fire cycles
write.csv(fireRegime, file = "fireRegime.csv", row.names = F)


