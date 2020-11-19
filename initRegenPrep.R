####################################################################################################
####################################################################################################
###### Preparation of regeneration modul
###### Dominic Cyr, in collaboration with Jesus Pascual Puigdevall
# rm(list = ls())
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # necessary on my Windows machine
# setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3", sep ="/"))
# ###################################################################################################
# ###################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
#################
require(dplyr)
require(rgdal)

# #######

#######
## uncomment the following if this script is run for testing, and not from 
## source("../initRasterPrep.R")

source("../scripts/regenDensityPredictFnc.R")
psDir <- "../data/Pothier-Savard"
source(paste(psDir, "Pothier-Savard.R", sep = "/"))

## loading predictive variables for initial conditions, putting them into a data frame
sDep <- rasterToPoints(surfDep)
cType <- rasterToPoints(coverTypes)
dCls <- rasterToPoints(dens)
colnames(sDep)[3] <- "surfDep"
lat <- rasterToPoints(lat)
long <-  rasterToPoints(long)

df <- merge(lat, long)
## surfDep to dummy
sD <- factor(sDep[,3])
dum <- model.matrix(~sD)[,-1]
df <- merge(df, data.frame(sDep[, 1:2], dum))

# coverType to dummy
cT <- factor(cType[,3]) 
dum <- model.matrix(~cT)[,-1]
df <- merge(df, data.frame(cType[, 1:2], dum))

# density class to dummy
cD <- factor(dCls[,3]) 
dum <- model.matrix(~cD)[,-1]
df <- merge(df, data.frame(dCls[, 1:2], dum))

## remove x, y coordinates
df <- df[,3:ncol(df)]


###############################################################################################
## loading Permanent plots table
pep <- read.csv("../data/PEP/Toutes_DB_5_6.csv")
# ###############################################################################################
# ## CoverTypes (same method as in 'initRasterPrep.R' recopied here)
# ###############################################################################################
# GR_ESS <- pep$GR_ESS
# ### just a quick scan of what we're dealing with...
# GR_ESS <- as.data.frame(table(GR_ESS))
# # GR_ESS_FREQ <- data.frame(GR_ESS_FREQ,
# #                           GR_ESS = GR_ESS_RAT[match(GR_ESS_FREQ[,"value"], GR_ESS_RAT$ID), "value"])
# GR_ESS <- GR_ESS[order(GR_ESS$Freq, decreasing = T),]
# 
# GR_ESS <- GR_ESS[!(GR_ESS$Freq == 0),]
# GR_ESS[, "spMain"] <- substring(as.character(GR_ESS[,"GR_ESS"]), 1, 2)
# 
# 
# ## Attributing cover type to species groups when possible
# GR_ESS[, "coverType"] <- apply(GR_ESS, 1,
#                                function(x)
#                                    ifelse(x["spMain"] %in% c("EN", "EP", "EE"), "EN",
#                                           ifelse(x["spMain"] == "PG", "PG",
#                                                  ifelse(substring(x["spMain"], 1,1) %in% c("F", "B", "P"), "F",
#                                                         ifelse(x["spMain"] %in% c("ML","MR","SB", "EB", "SE"), "R",
#                                                                NA)
#                                                         )
#                                                  )
#                                           )
#                                )
# 
# GR_ESS <- GR_ESS[, c("GR_ESS","coverType")]

GR_ESS_codes <- GR_ESS_RAT %>%
    mutate(GR_ESS = value) %>%
    dplyr::select(GR_ESS, coverType)

pep <- merge(pep, GR_ESS_codes)
pep$coverType <- as.factor(pep$coverType)

### filtering out some plots  
pep <- pep %>%
    filter(CL_AGE %in% c("70", "90", "120"),
           coverType %in% c("EN", "PG"),
           DOM_BIO %in% c(6))


# ###############################################################################################
# ###############################################################################################
# ############
## visualizing stem density around 100 years
# ###############################################################################################
# require(ggplot2)
# ###############################################################################################
# 
# p <- ggplot(data = pep, aes(x = coverType, y= den, color  = coverType)) +
#     geom_boxplot() +
#     #facet_wrap(~DOM_BIO) +
#     labs(title = "Distribution initiale des densités de tiges autour de 100 ans",
#          x = "",
#          y = "Densité de tiges à l'hectare\n") +
#     coord_flip() +
#     scale_color_manual("Cover type", values = c("indianred", "deepskyblue4", "darkgreen"))
# 
# 
# png(filename= paste0("densInitDistribution_100.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# 
# print(p + theme_dark())
# 
# dev.off()
# ###############################################################################################


###############################################################################################
###############################################################################################
## Surficial deposit (same method as in 'initRasterPrep.R' recopied here)
###############################################################################################

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


sDep <- x <- as.character(pep$DEP_SUR)
for (dep in names(depositCodes)) {
    depIndex <- which(sDep %in% depositCodes[[dep]])
    x[depIndex] <- dep
    # if (dep %in% c("clay", "sand")) { ## initial classification by Tadeusz
    #     for (dra in names(drainageCodes)){
    #         draIndex <-  which(drain %in% drainageCodes[[dra]])
    #         index <- intersect(depIndex, draIndex)
    #         x[index] <- paste0(dep, dra)   
    #     }
    # }
}


surfDepLevels <- c("shallow", "sand", "till", "clay", "organic")
pep[, "surfDep"] <- factor(x, levels = surfDepLevels)


##############################################################
##############################################################
### Compute relative density for permanent plots
pep[,"rho"] <- rhoFnc(sp = pep$coverType, N = pep$den, Dq = pep$DQM/10, rhoCoef = rhoCoef)

train <- data.frame(lat = pep$LATITUDE,
                    long = pep$LONGITUDE,
                    sD2 = ifelse(pep$surfDep == surfDep_RAT[2, "value"], 1, 0),
                    sD3 = ifelse(pep$surfDep == surfDep_RAT[3, "value"], 1, 0),
                    sD4 = ifelse(pep$surfDep == surfDep_RAT[4, "value"], 1, 0),
                    sD5 = ifelse(pep$surfDep == surfDep_RAT[5, "value"], 1, 0),
                    cT2 = ifelse(pep$coverType == as.character(coverTypes_RAT[2, "value"]), 1, 0),
                    cT3 = ifelse(pep$coverType == as.character(coverTypes_RAT[3, "value"]), 1, 0),
                    #cT4 = ifelse(pep$coverType == as.character(coverTypes_RAT[4, "value"]), 1, 0),
                    cD2 = ifelse(pep$CL_DENS == "C", 1, 0),
                    cD3 = ifelse(pep$CL_DENS == "D", 1, 0))


##############################################################
##############################################################
### Knn - Shouldn't probably be knn
### Something more appropriate for a continuous variable should be consider
##############################################################
### breaking relative density into a factor for knn
breaks <- seq(from = 0, to = 1.7, by = 0.1)
cl <- cut(pep[,"rho"], breaks = breaks)

## removing NAs
naIndex <- which(!complete.cases(train))
train <- train[-naIndex,] 
cl <- cl[-naIndex]

##### computing knn
require(class)
## add some pseudo-random noise as a workaround when there are too many ties
set.seed(12345)
train <- as.data.frame(apply(train, 2, jitter))
df <- as.data.frame(apply(df, 2, jitter))
knnPred <- knn(train = train[,-which(colnames(train) %in% c("lat", "long"))],
               test = df[,-which(colnames(train) %in% c("lat", "long"))],
               cl = cl, k = 5, use.all = F)
###
rhoLevels <- levels(knnPred)

rhoAttrib <- rep(NA, length(knnPred))
for (i in seq_along(rhoLevels)) {
    index <- which(knnPred == rhoLevels[i])
    x <- runif(n = length(index), min = breaks[i], max = breaks[i+1])
    rhoAttrib[index] <- x 
    
}

##################################################################################
##################################################################################
##################################################################################
## visualizing results, training set vs initial conditions
require(ggplot2)
##################################################################################

rhoDist <- data.frame(coverType = ifelse(round(train$cT2), "PG", "EN"),
                      surfDep = ifelse(round(train$sD2), "sand",
                                       ifelse(round(train$sD3), "till",
                                              ifelse(round(train$sD4), "clay",
                                                     ifelse(round(train$sD5), "organic",
                                                            "shallow")))),
                      rho = pep[-naIndex, "rho"],
                      group = "train")


rhoDist <- rbind(rhoDist,
                 data.frame(coverType = ifelse(round(df$cT2), "PG",
                                               ifelse(round(df$cT3), "F",
                                                      #ifelse(round(df$cT4), "R",
                                                             "EN")),
                            surfDep = ifelse(round(df$sD2), "sand",
                                             ifelse(round(df$sD3), "till",
                                                    ifelse(round(df$sD4), "clay",
                                                           ifelse(round(df$sD5), "organic",
                                                                  "shallow")))),
                            rho = rhoAttrib,
                            group = "test"))

rhoDist[,"ID"] <- as.numeric(as.factor(paste(rhoDist$group, rhoDist$coverType)))

x <- filter(rhoDist, coverType %in% c("EN", "PG"))
#x <- filter(rhoDist, coverType %in% c("EN", "PG"), group == "test")

p <- ggplot(x, aes(x = rho, fill = group, colour = group)) +
    geom_density(aes(y = ..scaled..), adjust = 1.25, alpha = 0.2) +
    facet_wrap(~coverType) +
    scale_colour_manual(values = c(train = "darkorange", test = "lightblue")) +
    scale_fill_manual(values = c(train = "darkorange", test = "lightblue")) +
    labs(title = "Distribution initiale des indices de densite relative a 100 ans (IDR100)",
         x = "IDR100")#,
         #y = "IDR100\n") #+
    #coord_flip()


png(filename= paste0("relDensDistribution_100.png"),
    width = 8, height = 5, units = "in", res = 600, pointsize=8)


print(p + theme_dark())

dev.off()

## summary statistics
summaryStats <- x %>%
    #filter(surfDep == "till") %>%
    filter(coverType == "EN") %>%
    #group_by(group, coverType) %>%
    group_by(group, surfDep) %>%
    summarise(IDR100_p05 = quantile(rho, 0.05),
              IDR100_p25 = quantile(rho, 0.25),
              IDR100_p50 = quantile(rho, 0.5),
              IDR100_p75 = quantile(rho, 0.75),
              IDR100_p95 = quantile(rho, 0.95),
              n = n())
# as.data.frame(summaryStats)
#######################################


#### creating raster for IDR100 (relative density at 100 y-old)
IDR100 <- coverTypes
names(IDR100) <- "IDR100"
IDR100[!is.na(coverTypes)] <- rhoAttrib
writeRaster(IDR100, file = "IDR100.tif", overwrite = T)
stored <- append(stored, "IDR100")

# # ##################################################################################
# # ##################################################################################
# # ##################################################################################
# # ## visualizing IDR100
# # require(ggplot2)
# # ##################################################################################
# df <- as.data.frame(rasterToPoints(IDR100))
# 
# ### plotting parameters
# pWidth  <- 1400
# pHeight <- 1200
# pointsize <- 8
# maxVal <- max(df[,3])
# colValues <- c(0,  0.25, 0.75, ceiling(maxVal*10)/10)
# # cols = c("red", "orange", "gold2", "seagreen4", "darkgreen")
# cols = c("darkred", "lightgoldenrod1", "forestgreen", "darkgreen")
# 
# ### plotting
# p <- ggplot(data = df, aes_string("x", "y", fill = colnames(df)[3])) +
#     theme_bw() +
#     #theme(legend.position="top", legend.direction="horizontal") +
#     geom_raster() +
#     coord_fixed() +
#     scale_fill_gradientn(name = "IDR100", #limits = c(0,1),
#                          colours = cols,
#                          values = colValues/maxVal, limits = c(0,maxVal))+
#                          #na.value = "dodgerblue1") +
#     labs(title = "Conditions initiales - Indice de densité relative à 100 ans (IRD100)",
#         x = "\nx (UTM 18)",
#           y = "y (UTM 18)\n") +
#     theme(legend.position="top", legend.direction="horizontal") #+
#     # annotate("text", x = max(df$x), y = max(df$y)+2500,
#     #          label = paste("année", l),
#     #          hjust = 1, vjust = 0, size = 0.3*pointsize, fontface = 2)
# 
# png(filename = "IDR100_init.png",
#     width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
#     bg = "white")
# 
#     print(p + theme(plot.title = element_text(size = rel(0.6)),
#                     axis.title.x = element_text(size = rel(0.5)),
#                     axis.title.y = element_text(size = rel(0.5)),
#                     axis.text.x = element_text(size = rel(0.5)),
#                     axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
#                     legend.title = element_text(size = rel(0.75)),
#                     legend.text = element_text(size = rel(0.5))))
# 
# dev.off()


#######################
### quantile mapping between regen density from initial conditions and initial IDR100

spInit <- rasterToPoints(coverTypes)
iqsInit <- rasterToPoints(IQS_POT)
rho100Init <- rasterToPoints(IDR100)
tsdInit <- rasterToPoints(tsd)
df <- merge(spInit, iqsInit)
df <- merge(df, rho100Init)
df <- merge(df, tsdInit)

## cap relative density to 1
df[which(df$IDR100 > 1), "IDR100"] <- 1


## time to 1m
df[,"t1"] <-  tFnc(sp = coverTypes_RAT[match(df$coverType, coverTypes_RAT$ID), "value"],
                   iqs = df$IQS_POT,
                   tCoef = tCoef)
df[is.infinite(df$t1), "t1"] <- NA


## Age at 1m
df[,"Ac"] <- df$tsd - df$t1
#df[is.na(df$Ac), "Ac"] 
df[which(df$Ac < 0), "Ac"] <- 0
df[which(df$Ac > 150), "Ac"] <- 150



## basal area (not merchantable, Ac > 25 years)
df[,"G"] <- GFnc(sp = coverTypes_RAT[match(df$coverType, coverTypes_RAT$ID), "value"],
                 Ac = df$Ac, iqs = df$IQS_POT, rho100 = df$IDR100,
                 HdCoef = HdCoef, GCoef = GCoef,
                 rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                 DqCoef = DqCoef, merchantable = F)

### if Ac < 25, an approximation of basal area must be computed
index <- which(df$Ac < 25)

df[index,"G"] <- #df[index, "Ac"]/25 *
    GFnc(sp = coverTypes_RAT[match(df[index, "coverType"], coverTypes_RAT$ID), "value"],
         Ac = 25, iqs = df[index, "IQS_POT"],
         rho100 = df[index, "IDR100"],
         HdCoef = HdCoef, GCoef = GCoef,
         rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
         DqCoef = DqCoef, merchantable = F)-
    (25-df[index, "Ac"])*
    (GFnc(sp = coverTypes_RAT[match(df[index, "coverType"], coverTypes_RAT$ID), "value"],
         Ac = 26, iqs = df[index, "IQS_POT"],
         rho100 = df[index, "IDR100"],
         HdCoef = HdCoef, GCoef = GCoef,
         rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
         DqCoef = DqCoef, merchantable = F) -
    GFnc(sp = coverTypes_RAT[match(df[index, "coverType"], coverTypes_RAT$ID), "value"],
         Ac = 25, iqs = df[index, "IQS_POT"],
         rho100 = df[index, "IDR100"],
         HdCoef = HdCoef, GCoef = GCoef,
         rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
         DqCoef = DqCoef, merchantable = F))

df[,"seedlings"] <- seedlingFnc(sp = coverTypes_RAT[match(df$coverType, coverTypes_RAT$ID), "value"],
                                Ac = df$Ac, G = df$G, iqs = df$IQS_POT,
                                seedCoef = seedCoef, tCoef = tCoef)


# ### visualizing empirical cumulative distribution functions
# png(filename= paste0("seedlingPredInitial_cdf.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# plot(ecdf(df$seedlings),
#      xlab = "Predicted seedling density\n(seedl. per sq-meter)",
#      ylab = "",
#      main = "Predicted post-fire seedling density - CDF - Initial conditions")
# dev.off()
# 
# png(filename= paste0("IDR100Initial_cdf.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# plot(ecdf(df$IDR100),
#      xlab = "IDR100",
#      ylab = "",
#      main = "Relative density index (IDR100) - CDF - Initial conditions")
# dev.off()


require("qmap")
seedlingQMapFit <- fitQmapQUANT(obs = df$IDR100, mod = df$seedlings,  nboot = 1,
                                qstep = 0.01, wet.day = F)#quantile(df$seedlings, 0.01, na.rm = T))
save(seedlingQMapFit, file = "seedlingQMapFit.RData")

stored <- append(stored, "seedlingQMapFit")

# ## visualizing quantile mapping function
# x <- seq(from = 0, to = max(df$seedlings, na.rm =T), by = 0.01)
# yLin <- doQmapQUANT(x = x, fobj = seedlingQMapFit, type = "linear")
# x <- data.frame(x = x, y = yLin)
# 
# png(filename= paste0("seedlingsToRho100.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = x, aes(x = x, y = y)) +
#     geom_line() +
#     labs(title = "Quantile mapping transfert function",
#          subtitle = "Predicted seedling density -> Relative density at 100 y.old",
#          x = "Seedling density (seedl. per sq-meter)",
#          y = "IDR100")
# dev.off()


# #################################################################################
# #################################################################################
# ###testing function
# Ac <- 1:150
# rho100 <- c(0.12, 0.375, 0.77)
# iqs <- c(10)
# 
# df <- data.frame(sp = c(rep("EN", length(rho100)*length(Ac)), rep("PG", length(rho100)*length(Ac))),
#                  rho100 = c(rep(rho100[1], length(Ac)), rep(rho100[2], length(Ac)),
#                             rep(rho100[3], length(Ac))),
#                             #rep(rho100[4], length(Ac))),
#                  Ac = Ac,
#                  iqs = iqs)
# 
# df[,"G"] <- GFnc(sp = df$sp, Ac = df$Ac, iqs = df$iqs, rho100 = df$rho100,
#                  HdCoef = HdCoef, GCoef = GCoef,
#                  rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#                  DqCoef = DqCoef, merchantable = F)
# 
# 
# # # if Ac < 25, an approximation of basal area must be computed
# # index <- which(df$Ac < 25)
# # df[index,"G"] <- df[index, "Ac"]/25 *
#     # GFnc(sp = df[index, "sp"],
#     #      Ac = 25, iqs = df[index, "iqs"],
#     #      rho100 = df[index, "rho100"],
#     #      HdCoef = HdCoef, GCoef = GCoef,
#     #      rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#     #      DqCoef = DqCoef, merchantable = F)
# 
# ### if Ac < 25, an approximation of basal area must be computed
# index <- which(df$Ac < 25)
# 
# df[index,"G"] <- #df[index, "Ac"]/25 *
#     GFnc(sp = df[index, "sp"],
#          Ac = 25, iqs = df[index, "iqs"],
#          rho100 = df[index, "rho100"],
#          HdCoef = HdCoef, GCoef = GCoef,
#          rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#          DqCoef = DqCoef, merchantable = F) -
#     (25-df[index, "Ac"]) *
#     (GFnc(sp = df[index, "sp"],
#           Ac = 26, iqs = df[index, "iqs"],
#           rho100 = df[index, "rho100"],
#           HdCoef = HdCoef, GCoef = GCoef,
#           rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#           DqCoef = DqCoef, merchantable = F) -
#          GFnc(sp = df[index, "sp"],
#               Ac = 25, iqs = df[index, "iqs"],
#               rho100 = df[index, "rho100"],
#               HdCoef = HdCoef, GCoef = GCoef,
#               rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#               DqCoef = DqCoef, merchantable = F))
# df[df$G < 0, "G"] <- 0
# 
# ################
# 
# 
# df[,"seedlings"] <- seedlingFnc(sp = df$sp, Ac = df$Ac, G = df$G, iqs = df$iqs,
#                                 seedCoef = seedCoef,
#                                 tCoef = tCoef)
# 
# require(dplyr)
# png(filename= paste0("Splawinski_seedlingDensVsAc.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = seedlings, colour = as.factor(rho100), linetype = sp)) +
#     geom_line() +
#     labs(title = "Post-fire seedling density (3 years) as a function of pre-fire stand attributes",
#          subtitle = paste("Site index:", iqs),
#          y = "seedlings per sq-meter",
#          x = "Age at 1 m (years)")+
#     scale_color_manual("Pre-fire IDR100", values = c("indianred", "deepskyblue4", "darkgreen")) +
#     scale_linetype_manual("Cover type", values = c(1,2))
# 
# dev.off()
# 
# 
# png(filename= paste0("Splawinski_IDR100DensVsAc.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = doQmapQUANT(x = seedlings, fobj = seedlingQMapFit, type = "linear"),
#                       colour = as.factor(rho100), linetype = sp)) +
#     geom_line() +
#     labs(title = "Post-fire relative density index (IDR100) as a function of stand attributes",
#          subtitle = paste("Site index:", iqs),
#          y = "Post-fire IDR100",
#          x = "Age at 1 m (years)")+
#     scale_color_manual("Pre-fire IDR100", values = c("indianred", "deepskyblue4", "darkgreen")) +
#     scale_linetype_manual("Cover type", values = c(1,2))
# 
# dev.off()
# 
# 
# png(filename= paste0("Splawinski_GVsRho100.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = G, colour = as.factor(rho100), linetype = sp)) +
#     geom_line() +
#     labs(title = "Basal area as a function of age (including non-merchantable)",
#          y = "Basal area (sq-m)",
#          x = "Age at 1 m (years)") +
#     scale_color_manual("Pre-fire IDR100", values = c("indianred", "deepskyblue4", "darkgreen")) +
#     scale_linetype_manual("Cover type", values = c(1,2))
# 
# dev.off()


## clearing everything from memory except what's been put into 'stored' 
rm(list = ls()[!ls() %in% stored])

