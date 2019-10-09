####################################################################################################
####################################################################################################
###### Preparation of fire regime parameters and study area + subzones
###### Dominic Cyr, in collaboration with Tadeusz Splawinski and Sylvie Gauthier
# rm(list = ls())
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # necessary on my Windows machine
# setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase2", sep ="/"))
# ###################################################################################################
# ###################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
#################
require(rgdal)
require(raster)
require(rgeos)
require(dplyr)
# #######

#### loading rasters and Raster Attribute Tables (RAT)
# study area
studyArea <- raster("../data/studyArea.tif")
studyAreaP <- get(load("../data/studyAreaP.RData"))
########

#################
#################
#################
uaf <- raster("../data/uaf.tif")
uaf_RAT <- read.csv("../data/uaf_RAT.csv") 
#################
#################
subZones <- raster("../data/subZones.tif")
subZones_RAT <- read.csv("../data/subZones_RAT.csv")
lakes <- raster("../data/lakes.tif")

# disturbance history
AN_ORIGINE <- raster("../data/Inv/AN_ORIGINE.tif")
ORIGINE <- raster("../data/Inv/ORIGINE.tif")
ORIGINE_RAT <- read.csv("../data/Inv/ORIGINE_RAT.csv")
fireZones <- raster("../data/fireZones.tif")
fireZonesP <- get(load("../data/fireZonesP.RData"))
fireZones_RAT <- read.csv("../data/fireZonesRAT.csv")
# species composition
TYPE_COUV <- raster("../data/Inv/TYPE_COUV.tif")
TYPE_COUV_RAT <- read.csv("../data/Inv/TYPE_COUV_RAT.csv")
GR_ESS <- raster("../data/Inv/GR_ESS.tif")
GR_ESS_RAT <- read.csv("../data/Inv/GR_ESS_RAT.csv")
REB_ESS1 <- raster("../data/Inv/REB_ESS1.tif")
REB_ESS1_RAT <- read.csv("../data/Inv/REB_ESS1_RAT.csv")
# stand structure and productivity
CL_DENS <- raster("../data/Inv/CL_DENS.tif")
CL_DENS_RAT <- read.csv("../data/Inv/CL_DENS_RAT.csv")
CL_HAUT <- raster("../data/Inv/CL_HAUT.tif")
CL_HAUT_RAT <- read.csv("../data/Inv/CL_HAUT_RAT.csv")
CL_AGE <- raster("../data/Inv/CL_AGE.tif")
CL_AGE_RAT <- read.csv("../data/Inv/CL_AGE_RAT.csv")
IQS_POT <- raster("../data/Inv/IQS_POT.tif")
# edaphic conditions
CL_DRAI <- raster("../data/Inv/CL_DRAI.tif")
CL_DRAI_RAT <- read.csv("../data/Inv/CL_DRAI_RAT.csv")
DEP_SUR <- raster("../data/Inv/DEP_SUR.tif")
DEP_SUR_RAT <- read.csv("../data/Inv/DEP_SUR_RAT.csv")
TYPE_ECO <- raster("../data/Inv/TYPE_ECO.tif")
TYPE_ECO_RAT <- read.csv("../data/Inv/TYPE_ECO_RAT.csv")
# general geography
lat <-  raster("../data/Geo/lat.tif")
long <-  raster("../data/Geo/long.tif")
# CO_TER <- raster("../data/Inv/CO_TER.tif")
# CO_TER_RAT <- read.csv("../data/Inv/CO_TER_RAT.csv")

#########
### fetching DEM from Amazon Web Services, source NRCAN, res ~ 90m
# require(elevatr)
# dem <- get_elev_raster(studyArea, z = 10, src = "aws") #
#########
### producing HLI
# demLL <- projectRaster(dem, crs = CRS("+init=epsg:4326")) ## lat long WGS 84
# hli <- hli(demLL)
# hli <- projectRaster(hli, studyArea)
# hli <- resample(hli, studyArea)
# writeRaster(hli, file = "hli.tif", overwrite = T)

### loading rasters for Random forest regeneration model
hli <- raster("../data/hli.tif")
dem <- raster("../data/dem.tif")
prodPot <- raster("../data/prodPot.tif")
preTot <- raster("../data/preTot.tif")
AI_deMartonne <- raster("../data/AI_deMartonne.tif")
growSeas_days <- raster("../data/growSeas_days.tif")
gddOver5 <- raster("../data/gddOver5.tif")
ericaceous <- raster("../data/ericaceous.tif")
demFullRes <- raster("../data/Geo/dem_originalRes.tif")

###############################################################################################
###############################################################################################
##### Cleaning up rasters, assimilating info, and visualizing initial conditions
###############################################################################################
###############################################################################################


###############################################################################################
###############################################################################################
## Time since last disturbance (tsd)
fireCycle <- 101
###############################################################################################

## defining age-classes
ageClassDef <- list("10" = c(1, 10),
                 "120" = c(101,999),
                 "12010" = c(101, 999),
                 "12012" = c(101, 999),
                 "12030" = c(101, 999),
                 "12050" = c(101, 999),
                 "12070" = c(101, 999),
                 "12090" = c(101, 999),
                 "30" = c(21, 40),
                 "3030" = c(21, 40),
                 "50" = c(41, 60),
                 "5010" = c(41, 60),
                 "5030" = c(41, 60),
                 "5050" = c(41, 60),
                 "70" = c(61, 80),
                 "7030" = c(61, 80),
                 "90" = c(81, 100),
                 "9010" = c(81, 100),
                 "9030" = c(81, 100),
                 "9050" = c(81, 100),
                 "9070" = c(81, 100),
                 "9090" = c(81, 100),
                 "JIN" = c(21,80), ### ????
                 "JIR" = c(21,80), ### ????
                 "VIN" = c(121, 999),
                 "VIN30" = c(121, 999),
                 "VIN50" = c(121, 999),
                 "VIR" = c(121, 999))

index <- match(as.character(CL_AGE_RAT$value), names(ageClassDef))
CL_AGE_RAT <- data.frame(CL_AGE_RAT,
                         min = sapply(ageClassDef, function(x) x[[1]])[index],
                         max = sapply(ageClassDef, function(x) x[[2]])[index])
ageMin <- sapply(ageClassDef, function(x) x[[1]])
ageMax <- sapply(ageClassDef, function(x) x[[2]])


### a RNG that draw from a neg exp distribution using the fire cycle, within a given interval
f <- function(mi, ma, fireCycle = fireCycle) {
    v <- numeric()
    if(is.na(mi)) {
        v <- NA
    } else {
        while(!length(v)) {
            i <- round(rexp(n = 1, rate = 1/fireCycle))
            if(i>=mi & i<=ma) {
                v <- i
            }
        }
    }
    return(v)
}
######
## generating age values from age classes, based on a neg exp distribution using the fire cycle as the global average
tsd <- data.frame(ID = values(CL_AGE))
tsd <- data.frame(tsd,
                  min = CL_AGE_RAT[match(tsd$ID, CL_AGE_RAT$ID), "min"],
                  max = CL_AGE_RAT[match(tsd$ID, CL_AGE_RAT$ID), "max"])

r <- CL_AGE
## here I reset the RNG for reproducibility
set.seed(12345)
r[] <- apply(tsd, 1, function(x) f(mi = x["min"], ma = x["max"], fireCycle = 101))
## replacing randomly generated values by know origin
orig <- 2015-values(AN_ORIGINE)
r[!is.na(orig)] <- orig[!is.na(orig)]
## setting unknown tsd (irrelevant for this experience) to 101 years
r[is.na(lakes) &
      is.na(r)] <- fireCycle
tsd <- r
names(tsd) <- "tsd"
##



###############################################################################################
###############################################################################################
## CoverTypes
###############################################################################################

### just a quick scan of what we're dealing with...
GR_ESS_FREQ <- freq(GR_ESS)
GR_ESS_FREQ <- data.frame(GR_ESS_FREQ,
                          GR_ESS = GR_ESS_RAT[match(GR_ESS_FREQ[,"value"], GR_ESS_RAT$ID), "value"])
GR_ESS_FREQ <- GR_ESS_FREQ[order(GR_ESS_FREQ$count, decreasing = T),]

GR_ESS_FREQ <- GR_ESS_FREQ[complete.cases(GR_ESS_FREQ),]
GR_ESS_FREQ[, "spMain"] <- substring(as.character(GR_ESS_FREQ[,"GR_ESS"]), 1, 2)
propSp <- GR_ESS_FREQ %>%
    filter(spMain %in% c("EN", "PG")) %>%
    group_by(spMain) %>%
    summarise(countTotal = sum(count)) %>%
    ungroup() %>%
    mutate(prop = countTotal/sum(countTotal))
GR_ESS_RAT[, "spMain"] <- substring(as.character(GR_ESS_RAT[,"value"]), 1, 2)

## Attributing cover type to species groups when possible
GR_ESS_RAT[, "coverType"] <- apply(GR_ESS_RAT, 1,
                                   function(x)
                                       ifelse(x["spMain"] %in% c("EN", "EP", "EE"), "EN",
                                              ifelse(x["spMain"] == "PG", "PG",
                                                     ifelse(substring(x["spMain"], 1,1) %in% c("F", "B", "P"), "F",
                                                            ifelse(x["spMain"] %in% c("ML","MR","SB", "EB", "SE"), "R",
                                                                   NA)
                                                     )
                                              )
                                       )
                                   )
### creating raster
coverTypes <- GR_ESS
coverTypes_RAT <- data.frame(ID = 1:4, value = c("EN", "PG", "F", "R"))
ct <- GR_ESS_RAT[match(values(GR_ESS), GR_ESS_RAT$ID), "coverType"]
ct <- coverTypes_RAT[match(ct, coverTypes_RAT$value), "ID"] 
coverTypes[] <- ct

### unidentified resinous covertype, randomly draw according to proportions of known covertypes
index <- which(values(GR_ESS) %in% GR_ESS_RAT[GR_ESS_RAT$spMain %in% c("RX", "RZ"), "ID"])
## resetting RNG for reproducibility
set.seed(12345)
ct <- sample(propSp$spMain, size = length(index), prob = propSp$prop, replace = T)
coverTypes[index] <- coverTypes_RAT[match(ct, as.character(coverTypes_RAT$value)), "ID"]

### plantations (no GR_ESS)
## known planted sp
index <- which(values(REB_ESS1) %in% REB_ESS1_RAT[REB_ESS1_RAT$value %in% c("EN", "PG"), "ID"])
ct <- as.character(REB_ESS1_RAT[match(values(REB_ESS1)[index], REB_ESS1_RAT$ID), "value"])
coverTypes[index] <- coverTypes_RAT[match(ct, as.character(coverTypes_RAT$value)), "ID"]
## unknown planted sp
index <- which(values(ORIGINE) %in% ORIGINE_RAT[ORIGINE_RAT$value %in% c("P", "PRR"), "ID"])
index <- intersect(index, which(is.na(values(coverTypes))))
coverTypes[index] <- coverTypes_RAT[as.character(coverTypes_RAT$value) == "EN", "ID"]  ## when unknown, assume black spruce

### remaining stands with cover type but not attributed species 
## recently burned sites
index <- which(values(ORIGINE) %in%  ORIGINE_RAT[ORIGINE_RAT$value %in% "BR", "ID"] &
                    values(is.na(coverTypes)))
ct <- sample(propSp$spMain, size = length(index), prob = propSp$prop, replace = T)
coverTypes[index] <- coverTypes_RAT[match(ct, as.character(coverTypes_RAT$value)), "ID"]
names(coverTypes) <- "coverType"
coverTypes[is.na(studyArea)] <- NA



###############################################################################################
###############################################################################################
## Surficial deposits - Reclassified by Splawinski
###############################################################################################
### Maybe should be reorganized... (hydric sands?)

depositCodes <- list(shallow = c("R", "R1A", "R4GA", "R7T",
                                 "R8E", "8E"),
                     till = c("1A", "1AA","1AAM", "1AAY",
                              "1AB", "1AD", "1AM", "1AY",
                              "1BC","1BD", "1BF", "1BG",
                              "1BI", "1BN", "1BP", "1BT"),
                     sand = c("2A", "2AE", "2AK", "2AT",
                              "2BD", "2BE", "3AE", "3AN",
                              "4GS", "4P","9S"),
                     clay = c("4GA", "4GAM", "4GAY"),
                     organic = c("7E", "7T", "7TM", "7TY"))

stored <- character()
stored <- append(stored, depositCodes)
drainageCodes <- list(Mesic = c("00", "10", "11", "16",
                                "20", "21", "23", "24",
                                "30", "31"),
                      Hydric = c("40", "41", "42", "43",
                                 "44", "50", "51", "52",
                                 "53", "54", "60", "61",
                                 "63"))


surfDep <- x <- as.character(DEP_SUR_RAT[match(values(DEP_SUR), DEP_SUR_RAT$ID),"value"])
drain <- as.character(CL_DRAI_RAT[match(values(CL_DRAI), CL_DRAI_RAT$ID),"value"])
for (dep in names(depositCodes)) {
    depIndex <- which(surfDep %in% depositCodes[[dep]])
    x[depIndex] <- dep
    # if (dep %in% c("clay", "sand")) { ## initial classification by Tadeusz
    #     for (dra in names(drainageCodes)){
    #         draIndex <-  which(drain %in% drainageCodes[[dra]])
    #         index <- intersect(depIndex, draIndex)
    #         x[index] <- paste0(dep, dra)   
    #     }
    # }
}



surfDep <- coverTypes
surfDepLevels <- c("shallow", "sand", "till", "clay", "organic")
surfDep[] <- factor(x, levels = surfDepLevels)
## creating Raster Attribute Table
surfDep_RAT <- data.frame(ID = seq_along(surfDepLevels), value = surfDepLevels)

###############################################################################################
###############################################################################################
## Stand Density
###################################################


newDens <- c(A = "AB", B = "AB", C = "C", D = "D", I = "AB", E = "E")
CL_DENS_RAT[,"newDens"] <- newDens[CL_DENS_RAT$value]
# converting CL_DENS values to new classes
densVal <- CL_DENS_RAT[match(values(CL_DENS), CL_DENS_RAT$ID), "newDens"]
newDensClasses <- unique(CL_DENS_RAT$newDens)
dens_RAT <- data.frame(ID = 1:(length(newDensClasses)+1), value = c(newDensClasses, "E"))
densVal <- dens_RAT[match(densVal, as.character(dens_RAT$value)), "ID"]
# creating raster
dens <- CL_DENS
dens[] <- densVal
##### attributing density classes to young stands
# creating frequency table 
freqTable <- table(values(coverTypes), values(dens), useNA = "ifany")
rownames(freqTable) <- c(as.character(coverTypes_RAT[order(coverTypes_RAT$ID),"value"]), "NA")
# adding class "E"

colnames(freqTable) <- as.character(dens_RAT[match(colnames(freqTable), as.character(dens_RAT$ID)), "value"])
#freqTable <- data.frame(freqTable, E = 0)
         
                       
#freqTable <- data.frame(freqTable, x <- c(as.character(dens_RAT[order(dens_RAT$ID),"value"]), "NA")
#x <- x[!grepl("E", x)]

freqTable <- as.data.frame(freqTable) %>%
    group_by(Var1, Var2) %>%
    summarize(Freq = sum(Freq)) %>%
    filter(Var1 %in% as.character(coverTypes_RAT$value))

set.seed(12345)
for (i in as.character(coverTypes_RAT$value)) {
    d <- coverTypes_RAT[coverTypes_RAT$value == i, "ID"]
    prob <- freqTable %>%
        filter(Var1 == i &
                   Var2 != "NA")
    prob <- as.data.frame(prob)
    index <- which(values(coverTypes) == d &
                       is.na(values(dens)))
    dens[index] <- sample(dens_RAT$ID, size = length(index),
                          prob = c(prob[match(prob$Var2, dens_RAT$value),"Freq"],0), ## adding a zero prob for class "E"
                          replace = T)
}

###############################################################################################
###############################################################################################
## remove unnecessary data from rf rasters if it wasn't already done
########

hli[is.na(coverTypes)] <- prodPot[is.na(coverTypes)] <- preTot[is.na(coverTypes)] <-
    AI_deMartonne[is.na(coverTypes)] <- growSeas_days[is.na(coverTypes)] <-
    gddOver5[is.na(coverTypes)] <- ericaceous[is.na(coverTypes)] <-
    IQS_POT[is.na(coverTypes)] <- surfDep[is.na(coverTypes)] <-
    dens[is.na(coverTypes)] <- NA


stored <- c("stored")
## study area
writeRaster(studyArea, file = "studyArea.tif", overwrite = T)
save(studyAreaP, file = "studyAreaP.RData")
stored <- append(stored, "studyArea")
writeRaster(lat, file = "lat.tif", overwrite = T)
writeRaster(long, file = "long.tif", overwrite = T)
stored <- append(stored, c("lat", "long"))
## zonation
writeRaster(uaf, file = "uaf.tif", overwrite = T)
write.csv(uaf_RAT, file = "uaf_RAT.csv", row.names = F)
stored <- append(stored, c("uaf", "uaf_RAT"))
writeRaster(subZones, file = "subZones.tif", overwrite = T)
write.csv(subZones_RAT, file = "subZones_RAT.csv", row.names = F)
stored <- append(stored, c("subZones", "subZones_RAT"))
## initial conditions (variables)
writeRaster(tsd, file = "tsd.tif", overwrite = T)
stored <- append(stored, "tsd")
writeRaster(coverTypes, file = "coverTypes.tif", overwrite = T)
write.csv(coverTypes_RAT, file = "coverTypes_RAT.csv", row.names = F)
stored <- append(stored, c("coverTypes", "coverTypes_RAT"))
writeRaster(dens, file = "dens.tif", overwrite = T)
write.csv(dens_RAT, file = "dens_RAT.csv", row.names = F)
stored <- append(stored, c("dens", "dens_RAT"))
## biophysical constants
writeRaster(fireZones, file = "fireZones.tif", overwrite = T)
write.csv(fireZones_RAT, file = "fireZones_RAT.csv", row.names = F)
save(fireZonesP, file = "fireZonesP.RData")
stored <- append(stored, c("fireZones", "fireZones_RAT"))
# writeRaster(lat, file = "TYPE_ECO.tif", overwrite = T)
# write.csv(fireZones_RAT, file = "TYPE_ECO_RAT.csv", row.names = F)
# stored <- append(stored, c("TYPE_ECO", "fireZones_RAT"))
writeRaster(surfDep, file = "surfDep.tif", overwrite = T)
write.csv(surfDep_RAT, file = "surfDep_RAT.csv", row.names = F)
stored <- append(stored, c("surfDep", "surfDep_RAT"))
# writeRaster(hli, file = "hli.tif", overwrite = T)
# stored <- append(stored, "hli")
# writeRaster(dem, file = "dem.tif", overwrite = T)
# stored <- append(stored, "dem")
# writeRaster(prodPot, file = "prodPot.tif", overwrite = T)
# stored <- append(stored, "prodPot")
# writeRaster(preTot, file = "preTot.tif", overwrite = T)
# stored <- append(stored, "preTot")
# writeRaster(AI_deMartonne, file = "AI_deMartonne.tif", overwrite = T)
# stored <- append(stored, "AI_deMartonne")
# writeRaster(growSeas_days, file = "growSeas_days.tif", overwrite = T)
# stored <- append(stored, "growSeas_days")
# writeRaster(gddOver5, file = "gddOver5.tif", overwrite = T)
# stored <- append(stored, "gddOver5")
# writeRaster(ericaceous, file = "ericaceous.tif", overwrite = T)
# stored <- append(stored, "ericaceous")
writeRaster(IQS_POT, file = "iqs.tif", overwrite = T)
stored <- append(stored, "IQS_POT")



## clearing everything from memory except these initial conditions 
rm(list = ls()[!ls() %in% stored])
