###################################################################################################
###################################################################################################
##### Compiling raw harvest outputs to a tidy data frame
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
  
  if(!simInfo$plantation[s]) {
    next
  }
  
  simDir <- simInfo$simDir[s]
  fr <- simInfo$fire[s]
  mgmt <- simInfo$mgmt[s]
  simID <- simInfo$simID[s]
  ctDyn <- simInfo$ctDyn[s]
  ####################################################################
  studyArea <- raster(paste0("../", simDir, "/studyArea.tif"))
  uaf <- raster(paste0("../", simDir, "/uaf.tif"))
  uaf_RAT <- read.csv(paste0("../", simDir, "/uaf_RAT.csv"))
  subZones <- raster(paste0("../", simDir, "/subZones.tif"))
  subZones_RAT <- read.csv(paste0("../", simDir, "/subZones_RAT.csv"))
  coverTypes <- raster(paste0("../", simDir, "/coverTypes.tif"))
  coverType_RAT <-  read.csv(paste0("../", simDir, "/coverTypes_RAT.csv"))
  ##
  convFactor <- prod(res(studyArea))/10000### to convert to hectares
  
  
  ###################################################################
  ## loading management plan (to fetch age structure targets, and productive cover types )
  managementPlan <- get(load(paste0("../", simDir, "/managementPlan.RData")))
  plan <- managementPlan$baseline
  
  
  ## fetching age of commercial maturity 
  maturity <- coverTypes
  
  if(ctDyn) {
    maturity[] <- plan$maturity[which(names(plan$ageRef) == "PG")] 
  } else {
    maturity[] <- plan$maturity[values(maturity)]
  }
  ####################################################################
  ####################################################################
  ######
  ######      compiling simulation outputs
  ######
  outputFolder <- paste0("../", simDir, "/output")
  x <- list.files(outputFolder)
  index <- grep(".RData", x)
  
  tsdOutputs <- x[intersect(index, grep("TSD", x))]
  plantOutputs <- x[intersect(index, grep("Plantation", x))]
  fireOutputs <-  x[intersect(index, grep("Fire", x))]
  
  replicates <- gsub(".RData", "", plantOutputs)
  replicates <- strsplit(replicates, "_")
  replicates <- as.numeric(lapply(replicates, function(x) x[2]))
  ###########################################
  ###########################################
  
  require(doSNOW)
  require(parallel)
  require(foreach)
  # clusterN <- 2
  # clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
  #######
  cl = makeCluster(clusterN, outfile = "") ##
  registerDoSNOW(cl)
  #######
  outputCompiled <- foreach(i = seq_along(plantOutputs),
                            .combine = "rbind") %dopar% {
    
                              require(raster)
                              require(reshape2)
                              require(dplyr)
                              require(tidyr)          
               

      # s <- scenario[i]
      r <- replicates[i]
      
  
      ## fetching outputs
      fire <- get(load(paste(outputFolder, fireOutputs[i], sep="/")))
      tsd <- get(load(paste(outputFolder, tsdOutputs[i], sep="/")))
      plant <- get(load(paste(outputFolder, plantOutputs[i], sep="/")))
      
      
      
      ## all plantations
      plantation <- plant$postSalv
      plantation[is.na(plantation)] <- 0
      if("postFire" %in% names(plant)) {
        plantation[plant$postFire==1] <- 1
      }
      
      plantVal <- values(plantation)
      indexPlant <- which(apply(plantVal, 1, sum) >= 1)
      plantVal <- plantVal[indexPlant,]
      fireVal <- values(fire)[indexPlant,]
      tsdVal <- values(tsd)[indexPlant,]
      matVal <- values(maturity)[indexPlant]
      ctDf <- data.frame(index = indexPlant,
                         matThresh = matVal,
                         plantedSp = coverType_RAT[as.numeric(names(matVal)), "value"])
      
      
      ##############################################################################
      #######
      colPrefix <- "postSalv_"
      
      ### summarizing results, shortfall probs
      plantReburns <- as.data.frame(plantVal) %>%
          mutate(index = indexPlant) %>%
          pivot_longer(cols = starts_with(colPrefix),
                       names_to = "year",
                       values_to = "plantation") %>%
          mutate(year = as.numeric(gsub("[^0-9]", "", year)))
      
      
      fire <- as.data.frame(fireVal) %>%
          mutate(index = indexPlant) %>%
          pivot_longer(cols = which(colnames(fireVal)!="index"),
                       names_to = "year",
                       values_to = "fire") %>%
          mutate(year = as.numeric(gsub("[^0-9]", "", year)))
      
      tsd <- as.data.frame(tsdVal) %>%
          mutate(index = indexPlant) %>%
          pivot_longer(cols = which(colnames(tsdVal)!="index"),
                       names_to = "year",
                       values_to = "tsd") %>%
          mutate(year = as.numeric(gsub("[^0-9]", "", year)))
      
    
      

      ########################################################
      ########################################################
      ########################################################
      ### storing fire events, ass. with plantations or not
      plantReburns <- plantReburns %>%
        merge(fire) %>%
        merge(tsd) %>%
        arrange(index, year) %>%
        group_by(index) %>%
        mutate(plantCum = cumsum(plantation)) %>%
        ungroup() %>%
        filter(plantCum > 0,
               tsd == 0,
               fire == T) %>%
        group_by(index) %>%
        mutate(event = order(year)) %>%
               #fire = ifelse(is.na(fire), 0, 1)) %>%
        ungroup()
      
      
      

      int <- plantReburns %>%
        group_by(index) %>%
        filter(fire == T) %>%
        mutate(int = year[event+1]-year,
               int = ifelse(is.na(int), 150-year, int),
               cens = ifelse(as.numeric(fire[event+1]) == 1, 1, 0),
               cens = ifelse(is.na(cens), 0, cens)) %>%
        filter(plantation == 1) %>%
        select(index, year, int, cens) %>%
        ungroup() %>%
        mutate(simID = simID,
               fireRegime = fr,
               mgmt = mgmt,
               replicate = r) %>%
        merge(ctDf)
      print(paste(simID, i))
      return(int)
  }
  
  stopCluster(cl)
  outputCompiled <- arrange(outputCompiled, simID, replicate, index, year)
  save(outputCompiled, file = paste0("outputCompiledPlantation_", simID, ".RData"))
}

