###################################################################################################
###################################################################################################
##### Compiling relative density outputs to tidy data frames
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "clusterN"))])
# #######
# rm(list = ls())
# setwd("D:/test/risqueAccidentRegen_phase3/100rep_baseline/")
# ####################################################################################################
# scenario <- "baseline"
# ####################################################################################################
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)


#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)
for(s in 1:length(simInfo$simID)) {
   
    simDir <- simInfo$simDir[s]
    fr <- simInfo$fire[s]
    mgmt <- simInfo$mgmt[s]
    simID <- simInfo$simID[s]
    ctDyn <- simInfo$ctDyn[s]
    
    ## loading management plan (to fetch commercial cover types )
    managementPlan <- get(load(paste0("../", simDir,"/managementPlan.RData")))
    plan <- managementPlan[["baseline"]] 
    x <- list.files(paste0("../", simDir,"/output"))
    index <- grep(".RData", x)
    index <- intersect(index, grep("outputVolAt120", x))
    x <- x[index]
    replicates <- gsub(".RData", "", x)
    replicates <- strsplit(replicates, "_")
    replicates <- as.character(lapply(replicates, function(x) x[2]))

   
    ########################################################################################################
    require(doSNOW)
    require(parallel)
    require(foreach)
    # clusterN <- 2
    # clusterN <- max(1, floor(0.5*detectCores()))  ### choose number of nodes to add to cluster.
    #######
    cl = makeCluster(clusterN, outfile = "") ##
    registerDoSNOW(cl)
    #######
    outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {#seq_along(x)
        
        require(raster)
        require(reshape2)
        require(dplyr)
        require(tidyr)
        
        r <- replicates[i]
        
        ## could be outside this loop at the moment, but will eventually be dynamics
        ## will need to be dynamics when covertypes will be dynamics
        coverTypes <- raster(paste0("../", simDir, "/output/coverTypesInit_", r, ".tif"))
        names(coverTypes) <- paste0("coverTypesDyn_0")
        ctStack <- paste0("../",simDir , "/output/outputCoverTypes_", r, ".RData")
        convFactor <- prod(res(coverTypes))/10000### to convert to hectares
        
        if(file.exists(ctStack)) {
            coverTypesDyn <- get(load(ctStack))
            coverTypes <- stack(coverTypes, coverTypesDyn) 
            rm(coverTypesDyn)
        }
        ######################################################################
        ######################################################################
        ######################################################################
        ######################################################################
        ######################################################################
        coverTypes_RAT <- read.csv(paste0("../", simDir, "/coverTypes_RAT.csv"))
        ctVal <- values(coverTypes)
        
        volAt120 <- get(load(paste0("../", simDir, "/output/outputVolAt120_", r, ".RData")))
        ### volAt120Init
        volAt120Init <- raster(paste0("../", simDir, "/output/volAt120Init_", r, ".tif"))
        names(volAt120Init) <- "volAt120_0"
        volAt120 <- stack(volAt120Init, volAt120)
        
        volAt120 <- values(volAt120)
        
        ### once a value has been replace with a zero, assume that any previous NA's was also a 'zero' value
        for (j in ncol(volAt120):2) {
            
            isZero <- volAt120[,j] == 0
            isNA <- is.na(volAt120[,j-1])
            
            index <- which(isZero & isNA)
            ## replace values
            volAt120[index,j-1] <- 0
            
        }
        ### computing landscape averages
        volAt120_mean <- apply(volAt120, 2, mean, na.rm = T)
        volAt120_mean <- data.frame(simID = simID,
                                    year = as.numeric(gsub("volAt120_", "", names(volAt120_mean))),
                                    volAt120_totalLandscapeAverage = round(volAt120_mean, 2))
        
        
        
        volAt120Cls <- apply(volAt120, 2, function(x) cut(x, include.lowest = T, right = F, breaks=c(0, 30, 50, 80, 999)))
       
        out <- list()
        for (sp in c("EN", "PG")) {
        
            ctID <- coverTypes_RAT[match(sp, coverTypes_RAT$value), "ID"]
            
            for (j in 1:ncol(volAt120Cls)) {
                
                cName <-  colnames(volAt120Cls)[j]
                y <- as.numeric(gsub("volAt120_", "", cName))
                
                if(ctDyn) {
                    index <- which(ctVal[,j] == ctID) 
                } else {
                    index <- which(ctVal == ctID)
                }
                v120Cls <- volAt120Cls[index, j]
                v120Cls <- data.frame(t(table(v120Cls, useNA = "always")))[,c("v120Cls", "Freq")]
                v120Cls <- data.frame(v120Cls = v120Cls$v120Cls,
                                      area_ha = v120Cls$Freq * convFactor)
                colnames(v120Cls) <- c("volAt120Cls", paste0("area_ha_", y))
                
                if (j == 1) {
                    tmp <- v120Cls    
                } else  {
                    tmp <- merge(tmp, v120Cls, by = "volAt120Cls", all = T)
                }
            }
            out[[sp]] <- gather(tmp, year, area_ha, -volAt120Cls) %>%
                mutate(year = as.numeric(gsub("area_ha_", "", year)),
                       coverType = sp)
        }
        
        out <- do.call("rbind", out) %>%
            mutate(simID = simID,
                   replicate = as.numeric(r),
                   fireScenario = fr,
                   mgmtScenario = mgmt) %>%
            select(simID, fireScenario, mgmtScenario, replicate, year, coverType,  volAt120Cls, area_ha) %>%
            arrange(simID, fireScenario, mgmtScenario, replicate, year) %>%
            merge(volAt120_mean)
        
        print(paste(simID, "volAt120", r))
        return(out)
    }
    
    stopCluster(cl)
    save(outputCompiled, file = paste0("outputCompiledVolAt120_", simID, ".RData"))
}
# 
# nrow(out_02)
# nrow(out_04)
# nrow(out_06)
# 
# 
# foo_02 <- out_02 %>%
#     filter(year == 100)
# foo_04 <- out_04 %>%
#     filter(year == 100)
# foo_06 <- out_06 %>%
#     filter(year == 100)
# 
# summary(foo_02) 
# summary(foo_04) 
# summary(foo_06) 
# 
# c(mean(foo_02$volAt120_totalLandscapeAverage), quantile(foo_02$volAt120_totalLandscapeAverage, c(0.01, .1, .25, .5, .75, 0.9, 0.95, .99)))
# c(mean(foo_04$volAt120_totalLandscapeAverage), quantile(foo_04$volAt120_totalLandscapeAverage, c(0.01, .1, .25, .5, .75, 0.9, 0.95, .99)))
# c(mean(foo_06$volAt120_totalLandscapeAverage), quantile(foo_06$volAt120_totalLandscapeAverage, c(0.01, .1, .25, .5, .75, 0.9, 0.95, .99)))
# out_02 %>%
#     group_by(year, fireScenario, mgmtScenario, coverType, volAt120Cls) %>%
#     summarise(count = n()) %>%
#     as.data.frame()
