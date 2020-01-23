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
#require(rgdal)
require(raster)
#require(rgeos)
require(dplyr)

for (s in scenario) {
    ## loading management plan (to fetch commercial cover types )
    managementPlan <- get(load(paste0("../", s,"/managementPlan.RData")))
    plan <- managementPlan[["baseline"]] 
    x <- list.files(paste0("../", s,"/output"))
    index <- grep(".RData", x)
    index <- intersect(index, grep("outputVolAt120", x))
    x <- x[index]
    simID <- gsub(".RData", "", x)
    simID <- strsplit(simID, "_")
    simID <- as.character(lapply(simID, function(x) x[[2]]))

    ########################################################################################################
    require(doSNOW)
    require(parallel)
    require(foreach)
    # clusterN <- 2
    clusterN <- max(1, floor(0.5*detectCores()))  ### choose number of nodes to add to cluster.
    #######
    cl = makeCluster(clusterN, outfile = "") ##
    registerDoSNOW(cl)
    #######
    outputCompiled <- foreach(i = seq_along(x), .combine = "rbind") %dopar% {
        
        require(raster)
        require(reshape2)
        require(dplyr)
        
        r <- simID[i]
        
        ## could be outside this loop at the moment, but will eventually be dynamics
        ## will need to be dynamics when covertypes will be dynamics
        coverTypes <- raster(paste0("../", s, "/coverTypes.tif"))
        coverTypes_RAT <- read.csv(paste0("../", s, "/coverTypes_RAT.csv"))
        ctVal <- values(coverTypes)
        convFactor <- prod(res(coverTypes))/10000### to convert to hectares

        volAt120 <- get(load(paste0("../", s, "/output/outputVolAt120_", simID[i], ".RData")))
        ### volAt120Init
        volAt120Init <- raster(paste0("../", s, "/output/volAt120init_", simID[i], ".tif"))
        names(volAt120Init) <- "volAt120_0"
        volAt120 <- stack(volAt120Init, volAt120)
        
        volAt120 <- values(volAt120)
        volAt120Cls <- apply(volAt120, 2, function(x) cut(x, include.lowest = T, right = F, breaks=c(0, 30, 50, 80, 999)))
       
        out <- list()
        for (sp in c("EN", "PG")) {
            ctID <- coverTypes_RAT[match(sp, coverTypes_RAT$value), "ID"]
            index <- which(ctVal == ctID)
            
            tmp <- volAt120Cls[index,]
            tmp <- apply(tmp, 2, table, useNA = "always")
            
            ### sometimes need to be converted into a data.frame if number of element differ among years
            if(class(tmp) == "list") {
                cNames <- names(tmp[[1]])
                cNames[is.na(cNames)] <- "N/A"
                df <- data.frame(matrix(NA,
                                        ncol = length(cNames),
                                       nrow = length(tmp)))
                colnames(df) <- cNames
                for (y in 1:length(tmp)) {
                    x <- tmp[[y]]
                    j <- names(x)
                    j[is.na(j)] <- "N/A"
                    df[y, j] <- x
                    df[is.na(df)] <- 0
                    tmp <- t(df)
                }
            } else {
                rownames(tmp)[is.na(rownames(tmp))] <- "N/A"
            }
            tmp <- tmp * convFactor
            tmp <- tmp %>%
                melt(value.name = "area_ha") %>%
                mutate(year = as.numeric(gsub("volAt120_", "", Var2)),
                       volAt120Cls = Var1,
                       coverType = sp,
                       scenario = s,
                       simID = r) %>%
                select(scenario, simID, coverType, year, volAt120Cls, area_ha)
            out[[sp]] <- tmp
        }
        out <- do.call("rbind", out)
        print(paste("outputVolAt120", s, r))
            
        return(out)
    }
    
    stopCluster(cl)
    outputCompiled <- arrange(outputCompiled, scenario, simID, year, coverType, volAt120Cls)
    save(outputCompiled, file = paste0("outputCompiledVolAt120_", s, ".RData"))
}
