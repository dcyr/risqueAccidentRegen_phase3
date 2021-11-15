###################################################################################################
###################################################################################################
##### Compiling relative density outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario"))])
#######
# rm(list = ls())
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-29")
# ####################################################################################################
# ###########################################
# scenario <- "baseline"
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
#################
# require(rgdal)
# require(raster)
# require(rgeos)
require(dplyr)
require(raster)

studyArea <- raster("../studyArea.tif")
coverTypes_RAT <- read.csv("../coverTypes_RAT.csv")
#dens_RAT <- read.csv("../dens_RAT.csv")
subZones_RAT <- read.csv("../subZones_RAT.csv")

#
convFactor <- prod(res(studyArea))/10000### to convert to hectares


outputCompiled <- get(load(paste0("outputTransitionsVolAt120Cls_", scenario, ".RData")))
## removing incomplete cases
outputCompiled <- outputCompiled[complete.cases(outputCompiled),]


## reclassifying 
breaks <- c(0,50, 80, 999)
outputCompiled <- outputCompiled %>%
    mutate(volAt120PreFireCls =  cut(volAt120PreFire, include.lowest = T, right = F,
                                      breaks = breaks),
           volAt120PostFireCls =  cut(volAt120PostFire, include.lowest = T, right = F,
                                      breaks = breaks))


## creating list of individual markovian sequences
require(markovchain)
clsNames <- as.character(levels(outputCompiled$volAt120PreFireCls))

seqMatrixBlank <- matrix(0, ncol = length(unique(clsNames)), nrow = length(clsNames),
                                         dimnames = list(clsNames, clsNames))
seqMatrix <- globalMatrix <- list()
for (sp in unique(outputCompiled$coverType)) {
    seqMatrix[[sp]] <- globalMatrix[[sp]] <- list()
    for (sz in unique(outputCompiled$subZone)) {
        seqMatrix[[sp]][[sz]] <- list()
        globalMatrix[[sp]][[sz]] <- seqMatrixBlank
        
        for (y in 1:50) {
            #identifying sequence
            index <- which(outputCompiled$year == y &
                               outputCompiled$coverType == sp &
                               outputCompiled$subZone == sz)
            
            preFire <- outputCompiled[index, "volAt120PreFireCls"]
            postFire <- outputCompiled[index, "volAt120PostFireCls"]
            
            
            tmpMat <- seqMatrixBlank
            for (i in seq_along(clsNames)) {
                indexCls <- which(preFire == clsNames[i])
                tmpMat[i,] <- table(postFire[indexCls])
            }
            
            seqMatrix[[sp]][[sz]][[y]] <- tmpMat
            globalMatrix[[sp]][[sz]] <- globalMatrix[[sp]][[sz]] + tmpMat
            print(paste(sp, sz, y))
        } 
    }
}

## global transition matrix, first time steps
round(prop.table(seqMatrix[[2]][[4]][[1]]+seqMatrix[[1]][[4]][[1]], margin = 1), 2)
## global transition matrix, first time steps
round(prop.table(seqMatrix[[2]][[4]][[50]]+seqMatrix[[1]][[4]][[50]], margin = 1), 2)
## global transition matrix, all time steps
round(prop.table(globalMatrix[[2]][[4]]+globalMatrix[[1]][[4]], margin = 1), 2)




### reformatting annual transition matrices
require(reshape2)
transDF <- list()
i <- 1
for (sp in unique(outputCompiled$coverType)) {
    spName <- coverTypes_RAT[match(sp, coverTypes_RAT$ID), "value"]
    for (sz in unique(outputCompiled$subZone)) {
        szName <- subZones_RAT[match(sz, subZones_RAT$ID), "value"]
        for (y in seq_along(seqMatrix[[sp]][[sz]])) {
            x <- data.frame(seqMatrix[[sp]][[sz]][[y]])
            xProp <- round(prop.table(as.matrix(x), margin = 1), 5)
            preFireCls <- row.names(x)
            x <- data.frame(preFireCls, x)
            xProp <- data.frame(preFireCls, xProp)
            colnames(x)[2:4] <- colnames(xProp)[2:4] <- preFireCls
            x <- melt(x, id.vars = "preFireCls",
                      variable.name = "postFireCls",
                      value.name = "cellCount")
            xProp <- melt(xProp, id.vars = "preFireCls",
                      variable.name = "postFireCls",
                      value.name = "prop")
            x <- merge(x, xProp)
            x <- data.frame(coverType = spName, subZone = szName, year = y,
                            x)
            transDF[[i]] <- x
            i <- i + 1
        } 
    }
}
## storing in a single data.frame
df <- do.call("rbind", transDF) %>%
    #filter(coverType == "EN",
    #       year == 1) %>%
    mutate(zone = ifelse(subZone == "Productive forest - harvestable", "Eligible to harvest",
                         "Ineligible to harvest")) %>%
    group_by(coverType, year, preFireCls, postFireCls) %>%
    summarize(cellCount = sum(cellCount))
# 
# 
# prop.table(df$cellCount, df$preFireCls)
# require(reshape2)
# dcast(df, cellCount + preFireCls ~ postFireCls)

##  converting into wide format
df <- dcast(df, coverType + year + preFireCls ~ postFireCls)
## computing proportion based on cellCount
df[,(ncol(df)-2):ncol(df)] <- round(t(apply(df[(ncol(df)-2):ncol(df)], 1, prop.table)), 2)

## selecting transition matrix of interest
filter(df, coverType == "EN", year == 1)
filter(df, coverType == "PG", year == 1)

filter(df, coverType == "EN", year == 50)
filter(df, coverType == "PG", year == 50)

##
# require(ggplot2)
# 
# ggplot(data = transDF, aes(x = year, y = prop, colour = postFireCls, linetype = coverType)) +
#     geom_line() +
#     #stat_summary(fun.data = "mean_sdl", geom = "smooth") +
#     facet_grid(subZone ~ preFireCls) 
# 
# 
#     
# ## summarizing 
# transSummaryDF <- transDF %>% 
#     group_by(coverType, subZone, year) %>%
#     summarise(accident = sum(cellCount[which(postFireCls == "[0,50)")]) /
#                   sum(cellCount[which(postFireCls %in% c("[50,80)", "[80,999]"))]),
#               maintien = sum(cellCount[which(postFi)]))
# 
#     
# transSummaryDF <- transDF %>% 
#     group_by(coverType, subZone, preFireCls, postFireCls) %>%
#     summarise(cellCount = sum(cellCount)) %>%
#     ungroup() %>%
#     group_by(coverType, subZone, preFireCls) %>%
#     mutate(cellCountTotal = sum(cellCount)) %>%
#     ungroup() %>%
#     mutate(prop = cellCount/cellCountTotal)
# 
# transSummaryDF2 <- transSummaryDF %>%
#     group_by(coverType, subZone) %>%
#     summarise(accident = sum(cellCount[which(preFireCls != "[0,50)"&
#                                                  postFireCls == "[0,50)")])/
#                   sum(cellCount[which(preFireCls != "[0,50)")]))
# 
#     
# 
# transSummaryDF2 <- transSummaryDF %>%
#     group_by(coverType, subZone) %>%
#     
#     
#     
# 
# 
# 
# 
#         seqMatrix[[sp]][[sz]] <- list()
#         globalMatrix[[sp]][[sz]] <- seqMatrixBlank
#         
#         for (y in 1:50) {
# 
# for (y in seq_along(seqMatrix)) {
#     df <- round(prop.table(seqMatrix[[y]], margin = 1), 5)  
#     df <- data.frame(yeardf)
#     if(y == 1) {
#       transDf <- df
#     } else {
#         transDf <- rbind(transDf, df)
#     }
# }
# seqMatrix
# 
# 
#     #creating standardized sequence
#     unifSeq <- seq(0, 10*round((max(years)-min(years))/10), by=10)
#     state <- rep(NA, length(unifSeq))
#     names(state) <- unifSeq
#     #incorporating known states
#     state[as.character(10*round((years-min(years))/10))] <- as.character(x)
#     #filling in unknown states with previous known
#     naStates <- which(is.na(state))
#     for (j in naStates) {
#         state[j] <- state[j-1]
#     }
# 
#     
#     <# adding sequence to list
#     if(length(state)>1) {
#         tmpMat <- seqMatrixBlank
#         tmpMat[rownames(state), colnames(state)] <- state
#         seqMatrix <- seqMatrix + tmpMat
#         #seqStandState <- c(seqStandState, list(state))
#     }
#     print(i)
# }
# 
# transMat <- round(prop.table(seqMatrix, margin = 1), 5)