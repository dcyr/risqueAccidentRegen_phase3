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

psDir <- paste0(sourceDir, "/data/Pothier-Savard")
source(paste0(psDir, "/Pothier-Savard.R"))
source(paste0(sourceDir, "/scripts/standAttribExtract.R"))
source(paste0(sourceDir, "/scripts/regenDensityPredictFnc.R"))
seedlingQMapFit <- get(load(paste(sourceDir, "scripts/seedlingQMapFit.RData", sep = "/")))


require(raster)
require(tidyverse)
#s <- 1
require(foreach)
foreach(s = c(1, 2, 4, 5, 6, 7, 10, 13, 15, 17, 18, 19, 21, 22, 23, 24))  %do% {#seq_along(simInfo$simID)
    require(raster)
    require(reshape2)
    require(tidyverse)
    require(tidyr)
    simDir <- simInfo$simDir[s]
    fr <- simInfo$fire[s]
    mgmt <- simInfo$mgmt[s]
    simID <- simInfo$simID[s]
    ctDyn <- simInfo$ctDyn[s]
    
    
    
    ## loading management plan (to fetch commercial cover types )
    managementPlan <- get(load(paste0("../", simDir,"/managementPlan.RData")))
    plan <- managementPlan[["baseline"]] 
    files <- list.files(paste0("../", simDir,"/output"))
    index <- grep(".RData", files)
    index <- intersect(index, grep("outputVolAt120", files))
    files <- files[index]
    replicates <- gsub(".RData", "", files)
    replicates <- strsplit(replicates, "_")
    replicates <- as.character(lapply(replicates, function(x) x[2]))

   
    ageInit <- raster(paste0("../", simDir, "/tsd.tif"))
    names(ageInit) <- "age_0"
    r100Init <- raster(paste0("../", simDir, "/IDR100.tif"))
    r100Init[r100Init>1] <- 1
    r100Init <- round(r100Init, 3)
    names(r100Init) <- "rho100_0"
    iqsInit <- raster(paste0("../", simDir, "/iqs.tif"))

    
    convFactor <- prod(res(r100Init))/10000
    ########################################################################################################
    require(doSNOW)
    require(parallel)
    
    # clusterN <- max(1, floor(0.5*detectCores()))  ### choose number of nodes to add to cluster.
    #######
    cl = makeCluster(clusterN, outfile = "") ##
    registerDoSNOW(cl)
    ### looping through replicates
    rfSummary <- foreach(i = seq_along(files), .combine = "rbind")  %dopar% {#seq_along(files)
     
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
        coverTypes_RAT <- read.csv(paste0("../", simDir, "/coverTypes_RAT.csv"))
        ctVal <- values(coverTypes)
        
        ######################################################################
        ######################################################################
        #### stand attributes volumes  

        
        volStandingInit <- raster(paste0("../", simDir, "/output/volInit_", r, ".tif"))
        
        age <- get(load(paste0("../", simDir, "/output/outputTSD_", r, ".RData")))
        age <- stack(ageInit, age)
        
        rho100 <- get(load(paste0("../", simDir, "/output/outputRho100_", r, ".RData")))
        rho100 <- stack(r100Init, rho100)    
        if(ctDyn) {
            iqs_PG <- raster(paste0("../", simDir, "/iqs_PG.tif"))
            #########################################
            #########################################
            
            x <- which(!is.na(ctVal[,1]))    
            #########################################
            #########################################
            
            for(y in 1:nlayers(rho100)) {
                if(y == 1) {
                    iqs <- iqsInit
                    
                } else {
                    tmp <- iqs[[y-1]] 
                    index <- which(ctVal[,y-1] != ctVal[,y])
                    tmp[index] <- iqs_PG[index]    
                    iqs <- stack(iqs, tmp)
                }
            }
            names(iqs) <- gsub("rho100", "iqs", names(rho100))
            
        } else {
            iqs <- iqsInit
        }
        
        
        #########################################
        if(ctDyn) {
            x <- which(!is.na(ctVal[,1]))    
        } else {
            x <- which(!is.na(ctVal))    
        }
        
        volInit <- values(volStandingInit)[x]
       
        sp <- sp_extract(r = coverTypes,
                         stands = x)
       
        iqs <- iqs_extract(r = iqs,
                           stands = x)
        a <- age_extract(r = age,
                         stands = x)
        
                 
        r100 <- IDR100_extract(r = rho100,
                               stands = x)
        
       
        ###
        Ac <- matrix(NA,
                     ncol = ncol(a),
                     nrow = nrow(a))
        if(ctDyn) {
            for(l in 1:ncol(a)) {
                Ac[,l] <-  ac_extract(a = a[,l],
                                      sp = sp[,l],
                                      iqs = iqs[,l],
                                      tCoef = tCoef,
                                      tFnc = tFnc,
                                      cap = 999)
                
            }    
        } else {
            for(l in 1:ncol(a)) {
                Ac[,l] <-  ac_extract(a = a[,l],
                                      sp = sp,
                                      iqs = iqs,
                                      tCoef = tCoef,
                                      tFnc = tFnc,
                                      cap = 999)
            }
        }
        
        ageAt1m <- a - Ac

 
        tmp <- list()
        for (j in 1:ncol(r100)) {#
            
            if(ctDyn) {
                x <-  data.frame(sp = sp[,j],
                                 iqs = iqs[,j],
                                 a = a[,j],
                                 Ac = Ac[,j],
                                 ageAt1m = ageAt1m[,j],
                                 r100 = r100[,j])  
            } else {
                x <- data.frame(sp = sp,
                               iqs = iqs,
                               a = a[,j],
                               Ac = Ac[,j],
                               ageAt1m = ageAt1m[,j],
                               r100 = r100[,j])
            }
            index <- which(x$Ac > 150)
            x[index, "Ac"] <- 150
            index <-  which(x$Ac < 0)
            x[index, "Ac"] <- 0
            tmp[[j]] <- x
        }
    
        


        #######
        df <- list()
        #df <- foreach(j = 1:5, .combine = "rbind") %do% {#ncol(r100)
        for (j in 1:ncol(r100)) {#
            
            require(tidyverse)
            y <- j-1
            source(paste0(sourceDir, "/scripts/regenDensityPredictFnc.R"))
            require("qmap")
            seedlingQMapFit <- get(load(paste(sourceDir, "scripts/seedlingQMapFit.RData", sep = "/")))
            #for (j in 1:ncol(r100)) {
            print(paste("extracting year", y, "of replicate", r, "of sim", simID)) 
            
            x <- tmp[[j]]
            index <- x$sp %in% c("EN", "PG")
            
            x <- x[index,]
            
            x[,"G"] <- g_extract(sp = x$sp,
                                Ac = x$Ac,
                                iqs = x$iqs,
                                rho100 = x$r100,
                                HdCoef,
                                GCoef,
                                rho100Coef,
                                withSenescence = F,
                                DqCoef,
                                merchantable = F)
           
           
           ## predicting seedling density
           x[,"seedlingDensity"] <- seedlingFnc(sp = x$sp, Ac = x$Ac, G = x$G, iqs = x$iqs,
                                                seedCoef = seedCoef, tCoef = tCoef,
                                                salvaged = F)
           
           
           ## converting seedling density to new relative density
           index <- which(!is.na(x$seedlingDensity))
           x[index,"r100PostFire"] <- doQmapQUANT(x = x[index, "seedlingDensity"], fobj = seedlingQMapFit, type = "linear")
           
           
           ## estimating volAt120
           x[, "v120PostFire"] <- round(VFnc(sp = x$sp, Ac = ifelse(x$ageAt1m < 25, 120 - x$ageAt1m, 0),
                                     iqs = x$iqs, rho100 = x$r100PostFire,
                                     rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef,
                                     DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                                     scenesCoef = NULL, withSenescence = F),
                                1)
           x[, "simID"] <- simID
           
            x <- x %>%
                mutate(year = y)
            
           df[[j]] <- x
           print(paste("done with year", y, "of replicate", r, "of sim", simID)) 
           
        }
        
        df <- do.call("rbind", df)
         
        df <- df %>%
            mutate(simID = simID,
                   fireScenario = fr,
                   mgmtScenario = mgmt,
                   replicate = as.numeric(r), 
                   coverType = sp) %>%
            group_by(simID, fireScenario, mgmtScenario, replicate, year, coverType) %>%
            summarize(regenFailure30 = round(sum(v120PostFire<30)/n(), 3),
                      area_ha = n()*convFactor)
        print(paste("finished with replicate", r, "of sim", simID))
        return(df)
    
    }
    stopCluster(cl)
    write.csv(rfSummary, file = paste0("regenFailureSummary_", simID, ".csv"), row.names = F)
    #return(rfSummary)
}



# 
# require(ggplot2)


# # 
# foo <- rfSummary
# 
# ggplot(foo, aes(x = year, group = paste(simID, coverType, replicate))) +
#     geom_line(aes(y = regenFailure30))
# 
# 
#     stat_summary(aes(y = regenFailure30),
#                  fun.y = mean
#                  #fun.ymin = function(x) mean(x) - sd(x),
#                  #fun.ymax = function(x) mean(x) + sd(x),
#                  #geom = "pointrange") +) +
#     ) +
#     facet_wrap(~coverType)
#                  #fun.ymin = function(x) mean(x) - sd(x),
#                  #fun.ymax = function(x) mean(x) + sd(x),
#                  #geom = "pointrange") +
#     xlim(c(0,150))
# 
# 
# 
# foo2 <- foo %>%
#     group_by(year) %>%
#     summarize(regenFailure30 = sum(v120PostFire<30)/n())



# plot(foo2$year, foo2$regenFailure30)

