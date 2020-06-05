####################################################################
####################################################################
### Random Forest prediction test
####################################################################
####################################################################
####################################################################
rm(list=ls())
####################################################################
####################################################################
#setwd("~/Travail/SCF/regenFailureRiskAssessment_phase2/")
setwd("C:/Users/cyrdo/Travail/SCF/regenFailureRiskAssessment_phase3/")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(randomForest)
require(raster)
require(dplyr)


### loading model
rfDens <-  get(load("../test/rfDens.RData"))
# varImpPlot(rfDens)   #  Importance des variables pour pr?dire claTran

# foo <- read.csv("../rf/bon/pourDC_nouveau/fakeData.csv")
#unique(foo$codSol)

####################################################################
####################################################################
### loading initial conditions, zones, and constant
studyArea <- raster("../data/studyArea.tif")
studyAreaDf <- rasterToPoints(studyArea)
#
zones <- raster("../data/fireZones.tif")
zonesDf <-rasterToPoints(zones)
rat <- read.csv("../data/fireZoneTable.csv")
rat <- distinct(rat[,c("ID", "Zone_LN")])
x <- as.character(rat[match(zonesDf[,3], rat$ID),2])
zonesDf <- data.frame(as.data.frame(zonesDf[,c("x", "y")]), zone = x)
#
coverTypes <- raster("../data/coverTypes.tif")
coverTypes[coverTypes %in% c(2,7) == F] <- NA
coverTypesDf <- rasterToPoints(coverTypes)
rat <- read.csv("../data/coverTypesRatTmp.csv")
x <- as.character(rat[match(coverTypesDf[,3], rat$ID),2])
coverTypesDf <- data.frame(as.data.frame(coverTypesDf[,c("x", "y")]),
                           coverType = factor(x, levels = c("EPN", "FEUsab", "PIG")))

### removing unproductive covertypes

### initial TSF
tsfInit <- raster("../data/tsfInit.tif")
tsfInitDf <- rasterToPoints(tsfInit)


### loading constant features, cleaning up data
convFactor <- prod(res(studyArea))/10000### to convert to hectares

ai <- raster("../test/AI_deMartonne.tif")
aiDf <- rasterToPoints(ai)

dem <- raster("../test/dem.tif")
demDf <-  rasterToPoints(dem)

hli <- raster("../test/hli.tif")
hliDf <-  rasterToPoints(hli)

gdd <- raster("../test/gddOver5.tif")
gddDf <-  rasterToPoints(gdd)

dep <- raster("../test/surfDep.tif")
depDf <-  rasterToPoints(dep)
rat <- read.csv("../test/surfDepRatTmp.csv", allowEscapes = T)
x <- as.character(rat[match(depDf[,3], rat$ID),2])
depDf <- data.frame(as.data.frame(depDf[,c("x", "y")]), surDep = x)

grow <- raster("../test/growSeas_days.tif")
growDf <-  rasterToPoints(grow)

prodPot <- raster("../test/prodPot.tif")
prodPotDf <-  rasterToPoints(prodPot)

preTot <- raster("../test/preTot.tif")
preTotDf <-  rasterToPoints(preTot)

dens <- raster("../test/dens.tif")
densDf <-  rasterToPoints(dens)
rat <- read.csv("../test/densRat.csv")
x <- as.character(rat[match(densDf[,3], rat$ID),2])
densDf <- data.frame(as.data.frame(densDf[,c("x", "y")]), dens = x)



varPredName <- c("clasDens_Ini", "essReg_Ini", "codSol",
                 "alt", "prodPot", "IQSp", "indArid",
                 "preTot", "saiCro")

## putting everything into a data frame (only productive stands, here, EPN and PG)
df <- merge(studyAreaDf, zonesDf)
colnames(df)[ncol(df)] <- "zone"
df <- merge(df, coverTypesDf)
colnames(df)[ncol(df)] <- "essReg_Ini"
df <- merge(df, aiDf)
colnames(df)[ncol(df)] <- "indArid"
df <- merge(df, demDf)
colnames(df)[ncol(df)] <- "alt"
# df <- merge(df, hliDf)
# colnames(df)[ncol(df)] <- "hli"
# df <- merge(df, gddDf)
# colnames(df)[ncol(df)] <- "gdd"
df <- merge(df, depDf)
colnames(df)[ncol(df)] <- "codSol"
df <- merge(df, growDf)
colnames(df)[ncol(df)] <- "saiCro"
df <- merge(df, densDf)
colnames(df)[ncol(df)] <- "clasDens_Ini"
df <- merge(df, prodPotDf)
df <- merge(df, preTotDf)
df <- merge(df, tsfInitDf)
### setting following variable constant temporarily
df[,"IQSp"] <- 12.8


########################################
### petite visualisation pour l'effet coverType*densitÃ©*dÃ©pÃ´t
head(df)
summary(df)

test <- df %>%
    filter(codSol %in% c("Tills", "Organic", "Sables M\xe9siques")) %>%
    group_by(essReg_Ini, clasDens_Ini, codSol) %>%
    summarize(indArid = mean(indArid),
              alt = mean(alt),
              saiCro = mean(saiCro),
              prodPot = mean(prodPot),
              preTot = mean(preTot),
              IQSp = mean(IQSp))
x <- list()
for (i in 1:150) {
    x[[i]] <- data.frame(test, ageAvant = i)
}

test <- bind_rows(x)

x <- predict(rfDens, test,  type= "prob")

test <- data.frame(test, x)
require(reshape2)
y <- melt(test, id.vars = c("essReg_Ini", "clasDens_Ini", "codSol", "indArid", "alt",
                        "saiCro", "prodPot", "preTot", "IQSp", "ageAvant"),
      variable.name = "densitePostFeu", value.name = "prob")



require(ggplot2)

#for (s in unique(y$codSol)) {
    #x <- y
    x <- filter(y, codSol == "Sables M\xe9siques")
    
    p <- ggplot(x, aes(x = ageAvant, y = prob, group = densitePostFeu, ID = densitePostFeu, fill = densitePostFeu)) +
        stat_summary(fun.y="sum", geom="area", position="fill", col="black") +
        facet_grid(essReg_Ini ~ clasDens_Ini) +
        #xlim(45, max(x$ageAvant)) +
        scale_fill_manual(values = rev(c("red", "orange", "yellow", "green"))) +
        guides(fill = guide_legend(reverse = F)) +
        labs(title="Densité du peuplement post-feu en fonction de l'Ã¢ge et la densité avant le feu",
             y="Probabilité\n\n",
             x="Àge avant feu",
             subtitle = "Sable") +
        geom_vline(xintercept = 40, linetype = "dotted")

    png(filename=paste0("regenDens_sable.png"),
        width = 8, height = 6, units="in", res=600)
    print(p)
    dev.off()
# }



    

    
### test avec les simulations de la phase 1 (feux baseline, rÃ©colte 0.51%)

outputDir <- "../outputs"
allSim <- list.files(outputDir)
filesFire <- allSim[grep("simFire", allSim)]
filesHarvest <- allSim[grep("simHarvest", allSim)]


################################################################################
###############################################################################
## simulating density transitions
require(reshape2)
require(data.table)
require(foreach)
require(raster)
require(parallel)
require(doSNOW)
clusterN <-  6 #max(1, floor(0.7*detectCores()))  ### choose number of nodes to add to cluster.


### thresholds for young stands
head(test)

thresholds <- list(EPN = c(30, 50),
                   PIG = c(20, 30))



cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

densityResults <- foreach(i = 1:100, .combine = "rbind") %dopar% {#seq_along(filesFire)
    require(foreach)
    require(tidyr)
    require(raster)
    require(randomForest)
    t1 <- Sys.time()
    ## fetching disturbance history (fire and harvest)
    fires <- get(load(paste(outputDir, filesFire[i], sep = "/")))
    simID <- gsub("[^0-9]", "", filesFire[i])
    print(paste("processing sim", simID, "..."))
    harvests <- get(load(paste(outputDir, filesHarvest[grep(paste0(simID, ".RData"), filesHarvest)], sep = "/")))
    
    fires$tsf[is.na(studyArea)] <- NA
    
    tsf <- crop(fires$tsf, c(range(df$x),
                             range(df$y)))
    timesteps <- as.numeric(gsub("[^0-9]", "", names(tsf)))
    names(tsf) <- paste0("F", timesteps)
    
    tsh <- crop(harvests, c(range(df$x),
                            range(df$y)))
    
    
    
    ### extracting minimum values (time since last disturbance)
    tsd <- list()
    for (l in 1:nlayers(tsf)) {
        x <- stack(tsf[[l]], tsh[[l]])
        tsd[[l]] <- min(x, na.rm = T)
        
    }
    tsd <- stack(tsd)
    names(tsd) <- paste0("D", timesteps)
    
    
    
    ### formating tsf
    x <- rasterToPoints(tsf)
    simResults <- merge(df, x, all = F)
    
    ## formating tsd
    x <- rasterToPoints(tsd)
    simResults <- merge(simResults, x, all = F)
    
    
    
    
    densDf <- foreach(j = seq_along(timesteps), .combine = "cbind") %do% {
        
        ts <- timesteps[j]
        # ## creating a 'density' data frame
        # if(ts == 1) {
        #     densDf <- data.frame(densityClass_0 = df$clasDens_Ini)
        # } else {
        #     densDf[,paste("densityClass", j-1, sep = "_")] <- df$clasDens_Ini
        # }
        
        ## current tsf
        fireCol <- which(colnames(simResults) == paste0("F", ts))
        ## current tsd (actually, previous year)
        if (ts == 1) { ### actually the previous  
            distCol <- which(colnames(simResults) == "tsfInit")  
        } else {
            distCol <- which(colnames(simResults) == paste0("D", ts))   
        }
        ## tsf == 0; focussing on burned cells to simulate density transitions
        index <- which(simResults[,fireCol]==0)
        index <- index[!is.na(simResults[index,"clasDens_Ini"])]
        if (sum(!is.na(simResults[index,"clasDens_Ini"])) == 0) { ## skip if there is no fire;
            # or if no productive site were burned (density 'AB', 'C' or 'D'; no density transition
            x <- simResults$clasDens_Ini
        } else {
            ## extracting predictive variables
            x <- data.frame(simResults[index,varPredName], ageAvant = simResults[index, distCol] +1)
            ## ignoring missing values (class "E" is actually considered inreversible)
            x <- x[complete.cases(x),]
            ## simulating transition
            transProb <- predict(rfDens, x,  type= "prob")
            newDensity <- apply(transProb, 1, function(x) sample(names(x),
                                                                 size = 1,
                                                                 prob = x))
            
            # ## converting class "E" to NA
            newDensity[newDensity == "E"] <- NA
            
            ## updating index (those with no 'NAs')
            index <- as.numeric(rownames(x))
            
            ## storing yearly density
            x <- simResults$clasDens_Ini
            x[index] <- newDensity
            
            ## updating density for next time step
            simResults[index, "clasDens_Ini"] <- newDensity
        }
        return(x)
    }
    
    ### tidying things up
    colnames(densDf) <- as.numeric(gsub("result.", "", colnames(densDf)))
    
    
    densDf <- gather(as.data.frame(densDf),
                     key = "timestep",
                     value = "densityClass",
                     convert = T)
    # ## restoring "E" class
    # densDf[is.na(densDf$densityClass), "densityClass"] <- "E"
    # 
    densDf <- data.frame(simID, densDf)
    
    ## converting density classes into a factor
    tmp <- factor(levels(df$clasDens_Ini)[densDf$densityClass],
                  levels = c(levels(df$clasDens_Ini), "E"))
    ## replacing NAs by class "E"
    tmp[is.na(tmp)] <- "E"
    densDf$densityClass <- tmp
    ## computing frequency table
    densFreqTable <- table(densDf$densityClass, densDf$timestep)
    densFreqTable <- as.data.frame(densFreqTable)
    x <- data.frame(simID = simID,
                    timestep = densFreqTable[,2],
                    densityClass = densFreqTable[,1],
                    area_ha = densFreqTable[,3] * convFactor)
                    
    t2 <- Sys.time()
    print(paste(i, t2-t1))
    
    return(x)
} 
stopCluster(cl)

write.csv(densityResults, file = "outputCompiledDensity.csv", row.names = F)


    

