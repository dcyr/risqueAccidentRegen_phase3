###################################################################################################
###################################################################################################
##### Visualizing fire simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls())
setwd("D:/regenFailureRiskAssessmentData_phase3/2018-08-17_test")
####################################################################################################
####################################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
#################
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
initYear <- 2015

###
studyArea <- raster("../studyArea.tif")
uaf <- raster("../uaf.tif")
uaf_RAT <- read.csv("../uaf_RAT.csv")


## 
subZones <- raster("../subZones.tif")
subZones_RAT <- read.csv("../subZones_RAT.csv")



require(ggplot2)
require(RColorBrewer)
### plotting parameters
pWidth  <- 1400
pHeight <- 1200
pointsize <- 8


df <- data.frame(rasterToPoints(uaf))
df$uaf <- uaf_RAT[match(df$uaf, uaf_RAT$ID), "value"]
head(df)
#cols = c(EN = "darkolivegreen", PG = "gold2", F = "darkolivegreen1", R = "forestgreen", autres = "grey")

### plotting UAFs
p <- ggplot(data = df, aes(x, y, fill = uaf)) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    scale_fill_manual(name = "UAF",#"Cover types",
                      values = brewer.pal(n = length(unique(df$uaf)),"Set1"),
                      #labels = paste0(coverLevels, " (", round(100 * table(coverTypesDf$cover)/nrow(coverTypesDf), 1), "%)"),
                      na.value = "dodgerblue1") +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "uaf.png",
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
    bg = "white")

print(p +
          #guide_legend(nrow=2,byrow=TRUE) +
          theme(plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.5)),
                axis.title.y = element_text(size = rel(0.5)),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                legend.title = element_text(size = rel(0.6)),
                legend.text = element_text(size = rel(0.75))) +
          guides(fill=guide_legend(nrow=2,byrow=TRUE))
)
dev.off()


#cols = c(EN = "darkolivegreen", PG = "gold2", F = "darkolivegreen1", R = "forestgreen", autres = "grey")

## preparing subzones
df <- data.frame(rasterToPoints(subZones))
df$subZones <- subZones_RAT[match(df$subZones, subZones_RAT$ID), "value"]
### polygonize uaf
uafP <- rasterToPolygons(uaf, dissolve = T)
uafF <- fortify(uafP)



### plotting subZones
p <- ggplot(data = df, aes(x, y, fill = subZones)) +
    theme_bw() +
    geom_raster() +
    coord_fixed() +
    scale_fill_manual(name = "",#"Cover types",
                      values = brewer.pal(n = length(unique(df$subZones)),"Set1"),
                      #labels = paste0(coverLevels, " (", round(100 * table(coverTypesDf$cover)/nrow(coverTypesDf), 1), "%)"),
                      na.value = "dodgerblue1") +
    geom_polygon(aes(x = long, y = lat, group = group), data = uafF,
                 colour = 'black', fill = NA, size = 0.5) +
    labs(x = "\nx (UTM 18)",
         y = "y (UTM 18)\n") +
    theme(legend.position="top", legend.direction="horizontal")


png(filename = "subZones.png",
    width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
    bg = "white")

print(p +
          #guide_legend(nrow=2,byrow=TRUE) +
          theme(plot.title = element_text(size = rel(0.6)),
                axis.title.x = element_text(size = rel(0.5)),
                axis.title.y = element_text(size = rel(0.5)),
                axis.text.x = element_text(size = rel(0.5)),
                axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
                legend.title = element_text(size = rel(0.6)),
                legend.text = element_text(size = rel(0.6))) +
          guides(fill=guide_legend(nrow=2,byrow=TRUE))
)
dev.off()
