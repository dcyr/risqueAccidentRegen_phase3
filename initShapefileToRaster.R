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
# #######



### loading study area
studyArea <- raster("../data/studyArea.tif")
studyAreaP <- get(load("../data/studyAreaP.RData"))
lakes <- raster("../data/lakes.tif")
DUMMY <- raster("../data/DUMMY.tif")

initVar <- c("DUMMY","ORIGINE","AN_ORIGINE", "TYPE_COUV",
             "GR_ESS", "CL_DENS", "CL_HAUT","CL_AGE",
             "CL_DRAI", "DEP_SUR", "IQS_POT", "TYPE_ECO",
             "CO_TER", "REB_ESS1")


#######################################################################
#######################################################################
##### Creating accessibility raster from road shapefile
#######################################################################

roadShapefile <- "CheminR10_Pastille10km"
roads <- readOGR(dsn = "../gis", layer = roadShapefile)
roads <- spTransform(roads, CRSobj = crs(studyArea))

# ### 
# Le champs « CL_CHEMIN » présente les classes de chemins :
# 1, 2, 3,4 - ces classes sont carrossable quand le chemin n'est pas trop vieux ou entretenu.
# 5 - souvent utilisé pour décrire un chemin d'hiver
# Hi - Chemin d'hiver
# HN - Hors norme
# NC - Non classé
# NF - Non forestier souvent fait par une mine
# IN - Inconnu

w <- 2000
roadsPerm <- roads[roads$CL_CHEMIN %in% c("01", "02", "03", "04", "HN"),]
roadBuffer <- buffer(roadsPerm, width = 2000)
roadBuffer <- rasterize(roadBuffer, studyArea)
roadBuffer[is.na(studyArea)] <- NA
###############
writeRaster(roadBuffer, file = paste0("roadAccess_", w/1000, "k.tif"), overwrite = T)


#######################################################################
#######################################################################
##### Creating rasters from forest inventory shapefiles
#######################################################################
#########  quite long... uncomment only if needed
### loading shapefiles

originalShapeFile <- "studyAreaCyr_Ecofor4_IQS"
forestInventory <- readOGR(dsn = "../gis", layer = originalShapeFile)
#forestInventory <- forestInventory[,which(names(forestInventory) %in% initVar)]
forestInventory <- spTransform(forestInventory, CRSobj = crs(studyArea))
# creating one dummy variable to create a single contour
forestInventory$DUMMY <- 1
#
## converting factors to numeric
forestInventory$AN_ORIGINE <- as.numeric(as.character(forestInventory$AN_ORIGINE))

#######################################################################
#######################################################################
############# Computing relative density index (rho)
#######################################################################
#######################################################################
#######################################################################




require(foreach)
require(raster)
require(parallel)
require(doSNOW)
clusterN <-  5 # works with 32 Gb of memorty - max(1, floor(0.7*detectCores())) ### choose number of nodes to add to cluster.
print(paste("initiating", clusterN, "parallel process(es)"))



cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
initConditions <- foreach(i = seq_along(initVar)) %dopar% {
    require(raster)
    v <- initVar[i]
    system.time(r <- rasterize(forestInventory, studyArea, field = v))
    writeRaster(r, file = paste0(v, ".tif"), overwrite = T)
    x <- as.data.frame(forestInventory[, v])[,1]
    if(class(x)=="factor") {
        x <- data.frame(value = levels(x))
        x <- data.frame(ID = as.numeric(rownames(x)), x)
        write.csv(x, file = paste0(v, "_RAT.csv"), row.names = F)
    }
    print(paste("rasterizing", v, "completed"))
    if(v == "DUMMY") {
        rP <- rasterToPolygons(r, dissolve = T)
        save(rP, file = paste0(v, ".RData"))
    }
}
stopCluster(cl)
# ## saving source shapefile
# save(forestInventory, file = paste0(originalShapeFile, ".RData"))



###########################################################################
##### Creating RAT for forestInventory features
df <- as.data.frame(forestInventory[, initVar])
featClass <- sapply(df, class)

for (i in seq_along(initVar)) {
    if(featClass[i]=="factor") {
        v <- initVar[i]
        x <- df[,v]
        x <- data.frame(value = levels(x))
        x <- data.frame(ID = as.numeric(rownames(x)), x)
        write.csv(x, file = paste0(v, "_RAT.csv"), row.names = F)
    }
}


#######################################################################
#######################################################################
##### Creating rasters for management zones (UAFs) and subzones
#######################################################################
managementUnits <- readOGR(dsn = "../gis", layer = "Pastille_Merge_dd")
managementUnits <- spTransform(managementUnits, CRSobj = crs(studyArea))
## creating RAT
subZones_RAT <- as.data.frame(managementUnits[,"INC_CO_EXC"])
subZones_RAT <- distinct(subZones_RAT)
subZones_RAT <- data.frame(ID = 1:nrow(subZones_RAT), value = subZones_RAT[,1])
newNames <- c(EXEX = "Non-forest", EXIN = "Conservation areas",
              EXIN = "Productive forest - Not harvestable", INC = "Productive forest - harvestable")
subZones_RAT[,"value"] <- newNames[subZones_RAT[,"value"]]
## rasterizing
uaf <-  rasterize(managementUnits, studyArea, field = "NO_UAF")
uaf_RAT <- as.data.frame(managementUnits[,"NO_UAF"])
uaf_RAT <- unique(uaf_RAT[!is.na(uaf_RAT),])
uaf_RAT <- data.frame(ID = 1:length(uaf_RAT), value = uaf_RAT)
uaf_RAT[,"value"] <- paste(substr(uaf_RAT$value, 1,3), substr(uaf_RAT$value, 4,5), sep = "-")
subZones <-  rasterize(managementUnits, studyArea, field = "INC_CO_EXC")
subZones[is.na(studyArea)] <- NA
## a few cells (3) were not in any subzone, we assumed they were harvestable
index <- which(values(is.na(foo) & !is.na(studyArea)))
subZones[index] <- subZones_RAT[subZones_RAT$value =="Harvestable", "ID"]

writeRaster(subZones, file = "subZones.tif", overwrite = T)
write.csv(subZones_RAT, file = "subZones_RAT.csv", row.names = F)
writeRaster(uaf, file = "uaf.tif", overwrite = T)
write.csv(uaf_RAT, file = "uaf_RAT.csv", row.names = F)


#######################################################################
#######################################################################
##### Creating rasters from ecodistrict shapefiles
#######################################################################

## fetching latitude values (for later)
studyAreaLL <- projectRaster(studyArea, crs = CRS("+init=epsg:4326")) ## lat long WGS 84
lat <- studyAreaLL
lat[] <- coordinates(lat)[,"y"]
lat <-  projectRaster(lat, studyArea)

long <- studyAreaLL
long[] <- coordinates(long)[,"x"]
long <-  projectRaster(long, studyArea)

### fetching ecological district-level variables
ecoDist <- readOGR(dsn = "../gis/rf", layer = "distEcolVar")
#ecoDist <- readOGR(dsn = inputDir, layer = "distEcol567")
ecoDist <- spTransform(ecoDist, CRSobj = crs(studyArea))
ecoDist <- crop(ecoDist, studyArea)
## converting some variable back to numeric
ecoDist$abonEric <- as.numeric(as.character(ecoDist$abonEric))
ecoDist$degJour <- as.numeric(as.character(ecoDist$degJour))
ecoDist$saiCro <- as.numeric(as.character(ecoDist$saiCro))
ecoDist$IndSeche <- as.numeric(as.character(ecoDist$IndSeche))


####################################################################################################
#########  constants
####################################################################################################



#############
## ericaceous abundance
ericaceous <-  rasterize(ecoDist, studyArea, field = "abonEric")
ericaceous[is.na(studyArea)] <- NA

#############
## growing degree-days (above 5 degrees )
gddOver5 <- rasterize(ecoDist, studyArea, field = "degJour")
gddOver5[is.na(studyArea)] <- NA

############
## growing season (days)
growSeas_days  <- rasterize(ecoDist, studyArea, field = "saiCro")
growSeas_days[is.na(studyArea)] <- NA

############
## aridity index (de Martonne) - Supposed to be much indArid than these values, need to validate
AI_deMartonne  <- rasterize(ecoDist, studyArea, field = "indArid")
AI_deMartonne[is.na(studyArea)] <- NA

############
## proPot (what is this?)
prodPot  <- rasterize(ecoDist, studyArea, field = "prodPot")
prodPot[is.na(studyArea)] <- NA

############
## prePot (what is this?)
preTot  <- rasterize(ecoDist, studyArea, field = "preTot")
preTot[is.na(studyArea)] <- NA

# #############
# ## elevation (+ aspect and slope, may not be necessary in the end...)
# dem <- new("GDALReadOnlyDataset", paste("../gis/rf", "studyarea_dem", sep = "/"))
# dem <- asSGDF_GROD(dem)
# dem <- raster(dem)
# dem <- projectRaster(dem, studyArea) ## lat long WGS 84
# dem <- round(dem)
# 
# # asp and slo may not be necessary if the 'spatialEco' package is used to compute HLI
# asp <- new("GDALReadOnlyDataset", paste("../gis/rf", "studyarea_asp", sep = "/"))
# asp <- asSGDF_GROD(asp)
# asp <- raster(asp)
# asp <- projectRaster(asp, studyArea) ## lat long WGS 84
# 
# slo <- new("GDALReadOnlyDataset", paste("../gis/rf", "studyarea_slo", sep = "/"))
# slo <- asSGDF_GROD(slo)
# slo <- raster(slo)
# slo <- projectRaster(slo, studyArea) ## lat long WGS 84
# 
# ## computing HLI (see also other method from package 'spatialEco')
# demLL <- projectRaster(dem, crs = CRS("+init=epsg:4326")) ## lat long WGS 84
# ## Heat Load Index (McCune and Keon 2002)
# asp1 <- 180-abs(asp-180)   # aspect folded to 0-180 degrees
# hli1 <-0.339+(0.808*cos(lat/360*2*pi)*cos(slo/360*2*pi)) -
#     (0.196*sin(lat/360*2*pi)*sin(slo/360*2*pi))-(0.482*sin(asp1/360*2*pi)*sin(slo/360*2*pi))
# hli <- hli1
# hli <- round(hli, 4)
# 
# ### found some discrepancies between the two methods... to be resolved
# ### Also, some values are NA's (-1 degree asp?)
# # require(spatialEco)
# # hli2 <- hli(demLL)
# # png(filename = "hli_manuel.png",
# #         width = 1200, height = 800, units = "px", res = 300, pointsize = 6,
# #         bg = "white")
# # 
# # plot(hli1)
# # 
# # dev.off()
# 
# # png(filename = "hli_spatialEcoPackage.png",
# #     width = 1200, height = 800, units = "px", res = 300, pointsize = 6,
# #     bg = "white")
# # 
# # plot(hli2, main = "Heat Load Index - 'SpatialEco' package")
# # 
# # dev.off()
# 
# # saving rasters


#writeRaster(dem, file = "dem.tif", overwrite = T)
#writeRaster(hli, file = "hli.tif", overwrite = T)
writeRaster(lat, file = "lat.tif", overwrite = T)
writeRaster(long, file = "long.tif", overwrite = T)
writeRaster(ericaceous, file = "ericaceous.tif", overwrite = T)
writeRaster(gddOver5, file = "gddOver5.tif", overwrite = T)
writeRaster(growSeas_days, file = "growSeas_days.tif", overwrite = T)
writeRaster(AI_deMartonne, file = "AI_deMartonne.tif", overwrite = T)
#writeRaster(surfDep, file = "surfDep.tif", overwrite = T)
writeRaster(prodPot, file = "prodPot.tif", overwrite = T)
writeRaster(preTot, file = "preTot.tif", overwrite = T)

