rm(list = ls())
setwd("~/Travail/SCF/regenFailureRiskAssessment/")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(ggmap)
require(rgdal)
require(broom)

crsWorking <- CRS("+init=epsg:4326")## lat - long

studyArea <- raster("../data/studyArea.tif")
convFactor <- prod(res(studyArea))/10000
studyAreaP <- get(load("../data/studyAreaP.RData"))
fireZonesP <- readOGR(dsn = "../gis", layer = "fireZones_Gauthier2015")
limiteNordique <- readOGR(dsn = "../gis", layer = "LIM_FOR_ATT_2018")

#### fire, if necessary
NFDB_poly <- readOGR(dsn = path.expand("~/Travail/GIS/feux/LFDB/shapefile_2017/"),
                     layer = "NFDB_poly_20170928")
### subsetting
years <- c(1980, 2015)
minSize <- 200 # in ha
#
index <- which(NFDB_poly$YEAR >= min(years) &
                   NFDB_poly$YEAR <= max(years) &
                   NFDB_poly$CFS_HA >= minSize)
NFDB_subset <- NFDB_poly[index,]
NFDB_subset <- spTransform(NFDB_subset, crsWorking) ### projection
#NFDB_poly <- spTransform(NFDB_poly, crsWorking) ### projection
NFDB_subset <- tidy(NFDB_subset)
NFDB_subset[,"variable"] <- paste0("Feux récents (", years[1], "-", years[2], ")")

# Canada (provinces)
can1 <- getData('GADM', country="CAN", level=1)  ## level 1: provinces
can1 <- spTransform(can1, crsWorking) ### projection

#fireZonesP <- get(load("../data/fireZonesP.RData"))
studyAreaTotal <- rasterize(studyAreaP, studyArea)



studyAreaTotal[!is.na(studyAreaTotal)] <- 1
studyAreaTotal <- sum(values(studyAreaTotal), na.rm = T) *convFactor
flammableProp <- sum(values(studyArea), na.rm = T) *convFactor /studyAreaTotal


## converting to lat long, and fortifying for ggplot
studyAreaP <- spTransform(studyAreaP, crsWorking)
studyAreaF <- tidy(studyAreaP)
studyAreaF[, "variable"] <- "Aire d'étude"
limiteNordique <- spTransform(limiteNordique, crsWorking)
limiteNordiqueF <- tidy(limiteNordique)
limiteNordiqueF[,"variable"] <- "Limite nordique (2018)"
fireZonesP <- spTransform(fireZonesP, crsWorking)


polyDF <- rbind(studyAreaF, NFDB_subset)
alpha <- c(0.3, 1)
col <- c("darkgreen", NA)
fill <- c("darkgreen", "red1")
names(alpha) <- names(col) <- names(fill) <- unique(polyDF$variable)
polyDF[,"alpha"] <- alpha[polyDF$variable]
polyDF[,"col"] <- col[polyDF$variable]
polyDF[,"fill"] <- fill[polyDF$variable]

polyDF$variable <- factor(polyDF$variable, levels = rev(unique(polyDF$variable)))
############################################################
############################################################
#########
#########   Study area map
#########
############################################################
############################################################
#fond <- get_map(location = "Lac St-Jean", zoom = 6, source = "google")
# fond <- get_map(location = bbox(studyAreaP), zoom = 7,  source = "google", maptype = "hybrid")#, maptype = "hybrid"
# fond <- get_map(location = bbox(studyAreaP), zoom = 6,  source = "google")#, maptype = "hybrid"
fond <- get_map(location = bbox(studyAreaP), zoom = 5,  source = "google")#, maptype = "hybrid"


xlim=c(attr(fond, "bb")$ll.lon, attr(fond, "bb")$ur.lon)
ylim=c(attr(fond, "bb")$ll.lat, attr(fond, "bb")$ur.lat)

ext <-  extent(c(xlim, ylim))
ext[c(1,3)] <- ext[c(1,3)] + 0.1
ext[c(2,4)] <- ext[c(2,4)] - 0.1
fireZonesC <- crop(fireZonesP, ext)
fireZonesF <- fortify(fireZonesC)

map <- ggmap(fond, extent = "panel", maprange = F) +
    # geom_polygon(aes(x = long, y = lat, group = group), data = fireZonesF,
    #              colour = 'red1', fill = 'red1', alpha = .05, size = .2) +
    geom_polygon(aes(x = long, y = lat, group = group,
                     fill = variable, alpha = alpha, colour = variable),
                 data = polyDF, size = 0.6) + #,
    scale_fill_manual(name="", values = fill) +
    scale_colour_manual(values = col, guide = F) +
    scale_alpha(guide = 'none') +
    geom_path(aes(x = long, y = lat, group = group),
              colour = "grey25",
              data = limiteNordiqueF,
              size = .6, linetype = "dashed") +
    guides(fill = guide_legend(override.aes = list(colour = rev(col),
                                                   alpha = c(1, .3)))) +
    labs(x = "longitude",
         y = "latitude")



png(filename = "studyAreaLargeScale.png",
    width = 2048, height = 1400, units = "px", res = 300, pointsize = 10,
    bg = "white")

    print(map +
              theme_bw() +
              coord_map(projection = "mercator",
                        xlim=c(-81, -62),#c(attr(fond, "bb")$ll.lon, attr(fond, "bb")$ur.lon),
                        ylim=c(45, 55)) +#c(attr(fond, "bb")$ll.lat, attr(fond, "bb")$ur.lat)) +
              theme(plot.title = element_text(size = rel(1.2)),
                    axis.title = element_text(size = rel(0.8)),
                    axis.text = element_text(size = rel(0.5)),
                    
                    legend.text = element_text(size = rel(0.75)),
                    legend.key.width = unit(2,"lines"),
                    legend.key.height = unit(.75,"lines"),
                    legend.spacing.y = unit(1.5,"lines"),
                    panel.grid.major = element_line(size = 0.25))
                    
          )

dev.off()

