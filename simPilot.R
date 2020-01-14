################################################################################
################################################################################
##### Main script driving the simulation
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
### setwd("C:/Users/dcyr-z840/Sync/Travail/ECCC/regenFailureRiskAssessment_phase3")
# rm(list = ls())
# ################################################################################
# home <- path.expand("~")
# home <- gsub("\\\\", "/", home) # necessary on some Windows machine
# home <- gsub("/Documents", "", home) # necessary on my Windows machine
# setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))
# setwd("D:/test/risqueAccidentRegen_phase3/")
# ################################################################################
################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
print(wwd)
################################################################################
################################################################################
require(raster)
require(dplyr)
print(paste0("Preparing INITIAL CONDITIONS: ", normalizePath("../"), "initRasterPrep.R"))
##
source("../initRasterPrep.R")
source("../initRegenPrep.R")
## Reading management scenarios
print(paste0("Preparing MANAGEMENT SCENARIOS from source file: ", normalizePath("../"), "initHarvest.R"))  

source("../initHarvest.R")
save(managementPlan, file = "managementPlan.RData")
## Sourcing fire engine & Loading fire regime(s)
source("../initFireRegime.R")
## Loading regeneration module
source("../scripts/regenDensityPredictFnc.R")
## Loading yield curves (and other forestry equations)
psDir <- "../data/Pothier-Savard"
source(paste(psDir, "Pothier-Savard.R", sep = "/"))
## Loading yield curves (and other forestry equations)
source("../scripts/standAttribExtract.R")
################################################################################
################################################################################
nRep <- 100
simDuration <- 2
simStartYear <- 2015
scen <- "RCP85"
################################################################################
tsdInit <- tsd
IDR100Init <- IDR100

################################################################################
### actual simulation
require(doSNOW)
require(parallel)
clusterN <- max(1, floor(0.9*detectCores())) ### choose number of nodes to add to cluster.
clusterN <- min(nRep, clusterN)

#######
verbose <- T
outputDir <-  paste(getwd(), "output/", sep = "/")
dir.create(outputDir)

cl = makeCluster(clusterN,
                 outfile = "") ##
registerDoSNOW(cl)

#for (i in 0:(nRep-1)) {
foreach(i = 0:(nRep-1),
  .packages= names(sessionInfo()$otherPkgs),
  .verbose = T) %dopar%  {
          
          ### workaround on Windows system to avoid multiple instances trying to access
          ### the same files at the same time
  if(i < clusterN) {
    Sys.sleep(2*(i %% clusterN))
  }
  
  
  tStart <- Sys.time()
  require(stringr)
  simID <- str_pad(i, nchar(nRep-1), pad = "0")
  print("##############################################################")
  print("##############################################################")
  print(paste("simulating replicate", i))
  print("##############################################################")
  print("##############################################################")
  
  harvestScenario <- "baseline"
  tsd <- tsdInit
  
  ###########################################################################
  IDR100 <- round(IDR100Init, 3)
  IDR100[IDR100>1] <- 1
  ###########################################################################
  ###########################################################################
  
  ### preparing management plan inputs
  plan <- managementPlan[[harvestScenario]]
  
  ## eligible to harvest
  harvEligible <- uaf %in% plan$uaf &
    subZones %in% plan$subZone
  harvEligible[!harvEligible] <- NA
  
  ## spp eligible (commercial species)
  spEligible <- coverTypes %in% plan$comSppId[["SEPM"]] & harvEligible
  spEligible[!spEligible] <- NA
  
  # ## all productive stands (may differ from commercial spp)
  # prodSpp <- coverTypes %in% plan$prodSppID & harvEligible
  # prodSpp[!prodSpp] <- NA
  
  ## commercial maturity thresholds (based on time since last disturbance)
  matThresh <- coverTypes
  matThresh[is.na(spEligible)] <- NA
  matThresh[] <- plan$maturity[values(matThresh)]
  
  ## age at 1 meter
  t1 <- coverTypes
  t1[] <- NA
  index <- which(values(!is.na(coverTypes)))
  t1[index] <- round(tFnc(sp = coverTypes_RAT[match(coverTypes[index], coverTypes_RAT$ID), "value"],
                          iqs = IQS_POT[index],
                          tCoef = tCoef))
  ## min volume at a given age to be considered commercial
  volMinAt120 <- coverTypes
  volMinAt120[is.na(coverTypes)] <- NA
  volMinAt120[] <- plan$volMin[values(volMinAt120)]
  
  ## reference age (for commercial volume)
  ageRef <- coverTypes
  ageRef[is.na(coverTypes)] <- NA
  ageRef[] <- plan$ageRef[values(ageRef)]
  
  ############################################################################
  
  
  ## initial volume at 120 years old for eligible stands
  #
  iqs <- IQS_POT[index]
  sp <- coverTypes_RAT[match(coverTypes[index], coverTypes_RAT$ID), "value"]
  a <- tsdInit[index]
  ageRefCorr <- ageRef[index] - t1[index]
  Ac <- a - t1[index] 
  r100 <- IDR100[index]
  ##
  volAt120 <- volInit <- coverTypes
  volAt120[] <- volInit[] <- NA
  volAt120[index] <- VFnc(sp = sp, Ac = ageRefCorr, iqs = iqs, rho100 = r100,
                          rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                          scenesCoef = NULL, withSenescence = F)
  volAt120 <- round(volAt120, 1)
  ## current volume 
  volInit[index] <- VFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                         rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                         scenesCoef = NULL, withSenescence = F)
  
  ## Basal area set aside in case of retention cut
  stSetAside <- volSetAside <- coverTypes  
  stSetAside[!is.na(stSetAside)] <- volSetAside[!is.na(volSetAside)] <- 0
  
  writeRaster(volAt120, file = paste0(outputDir, "volAt120Init_",simID, ".tif"), overwrite = T)
  writeRaster(volInit, file = paste0(outputDir, "volInit_",simID, ".tif"), overwrite = T)
  
  rm(iqs, sp, a, Ac, r100, volInit)
  
  ## creating raster stacks by UAFs
  uR <- uSp <- uThresh <- uProd <- list()
  for (u in plan$uaf) {
    ## eligible to harvest within uaf
    uR[[u]] <- uaf==u & coverTypes
    # ## all productive stands within uaf (for computing age structure targets)
    # uProd[[u]] <- uaf==u & prodSpp
    ## all eligible species within uaf
    uSp[[u]] <- spEligible & uR[[u]]
    ## maturity thresholds within uaf
    uThresh[[u]] <- matThresh
    uThresh[[u]][!uR[[u]] & !uSp[[u]]] <- NA
    
  }
  ## putting all this in nice stacks
  uR <- stack(uR)
  spEligible <- stack(uSp)
  matThresh <- stack(uThresh)
  #uProd <- stack(uProd)
  rm(uSp, uThresh)
  ## uniformizing names
  names(uR) <- names(spEligible) <- names(matThresh) <- plan$uaf
  
  
  ###########################
  ###########################
  if(verbose) {
    print(paste("Simulating annual processes"))  
  }
  
  ###########################
  ###########################
  
  ## variable stand attributes (stored)
  fire <- harv <- age <- salv <- plant <- rho100 <- reten <- list()
  
  volAt120 <- list(volAt120)
  
  fr <- filter(fireRegime, scenario == scen)
  for (y in 1:simDuration) {#simDuration) {### change into foreach, and return 'fire' 'harv' and 'age' as a list, then reformat
    
    
    ####################### simulating fire
    if(verbose) {
      print(paste("Simulating fire"))  
    }
    
    
    f <- simFire(tsfInit = tsd, simDur = 1, yearInit = simStartYear + y,
                 fireZones = fireZones,
                 fireRegime = fr,
                 fireSizeMean = fireSizeMean,
                 fireSizeDist = fireSizeDist,
                 # fireSizeFit = fireSizeFit,
                 # fireSizeMax = fireSizeMax,
                 id = simID)
    
    f <- f$tsf == 0
    f[!f] <- NA
    fire[[y]] <- f
    
    # # ##saving f for testing purposes
    # save(f, file = paste0(outputDir, "fire_", y, ".RData"))
    
    ####################### computing pre-fire stand attributes 
    if(verbose) {
      print(paste("computing pre-fire stand attributes"))  
    }
    # print("computing pre-fire stand attributes")
    ### focussing on burned cells 
    index <- which(values(f & studyArea & coverTypes %in% plan$comSppId[["SEPM"]]))
    ###
    iqs <- iqs_extract()
    a <- age_extract()
    sp <- sp_extract()
    r100 <- IDR100_extract()
    ## age at 1m
    
    Ac <- ac_extract(a = a,
                     sp = sp,
                     iqs = iqs,
                     tCoef = tCoef,
                     tFnc = tFnc,
                     cap = 150)
    Hd <- HdFnc(sp, Ac, iqs, HdCoef)
    if(length(index > 0)) {
      
      ## basal area (all diameters, Ac >= 25)
      g <- g_extract(sp,
                     Ac,
                     iqs,
                     rho100 = r100,
                     HdCoef,
                     GCoef,
                     rho100Coef,
                     withSenescence,
                     DqCoef,
                     merchantable)
      ## merchantable volume 
      v <- VFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                scenesCoef = NULL, withSenescence = F)
      v[g == 0] <- 0
    } else {
      g <- v <- Ac
    }
    
    ####################### Salvage logging
    if(verbose) {
      print(paste("Salvage logging"))  
    }
    vIndex <- which(v > plan$salvageEligibility$`SEPM`)
    
    salvageIndex <- data.frame(index = index[vIndex], vSalv = v[vIndex])
    ## indentifying those eligible to harvest
    eligibleToSalvage <- which(values(sum(spEligible, na.rm = T)) >= 1)
    
    salvageIndex <- filter(salvageIndex, index %in% eligibleToSalvage)
    
    ## salvaging only the allowed proportion
    ########################
    ## determining the number of cells to harvest
    nCell <- min(nrow(salvageIndex) * plan$salvageTargetStandProp$SEPM,
                 plan$targetHarvestLevels$SEPM / plan$salvageWoodPropLost$SEPM *
                   sum(values(spEligible[[u]]), na.rm = T))
    nCell <- round(nCell)
    salvageIndex <- salvageIndex[sample(1:nrow(salvageIndex),nCell),]
    
    s <- studyArea
    s[] <- NA
    s[salvageIndex$index] <- salvageIndex$vSalv
    # # ## saving salvaged cells for testing purposes
    # save(s, file = paste0(outputDir, "s_", y, ".RData"))
    salv[[y]] <- s
    
    ####################### simulating regeneration density
    if(verbose) {
      print(paste("simulating regeneration density"))  
    }
    ## applying minimum basal area based on what has been set aside in previous harvesting
    retentionIndex <- which(stSetAside[index] > 0)
    g <- apply(data.frame(g, stSetAside[index]), 1, function(x) max(x, na.rm = T))
    
    ## consider stand with retention harvesting fully mature
    AcEffective <- Ac
    AcEffective[retentionIndex] <- 100
    
    ## natural regen seedling density
    seedlingDens <- seedlingFnc(sp = sp, Ac = AcEffective, G = g, iqs = iqs,
                                seedCoef = seedCoef, tCoef = tCoef)
    
    
    ## post-salvage plantation (if applicable)
    if(verbose) {
      print(paste("simulating post-salvage plantation"))  
    }
    if(plan$salvagePlantation) {
      indexSalvPlant <- which(index %in% salvageIndex$index &
                                seedlingDens < plan$plantationThreshold)
      
      seedlingDens[indexSalvPlant] <-  seedlingDens[indexSalvPlant] + plan$plantationDensity
    }
    
    ## work with is.na(seedlingDens==F) (covertypes other than EN and PG produce NAs)
    x <- rep(NA, length(seedlingDens))
    indexSeedlings <- !is.na(seedlingDens)
    
    
    ## converting seedling density to new relative density
    x[indexSeedlings] <- doQmapQUANT(x = seedlingDens[indexSeedlings], fobj = seedlingQMapFit, type = "linear")
    x <- round(x, 3)
    
    ## store planted sites (post-salvage)
    p <- studyArea
    p[] <- NA
    p[index[indexSalvPlant]] <- 1
    
    plant[[y]] <- p
    
    
    if(verbose) {
      print(paste("Updating stand attribute in burned cells"))  
    }
    ## updating rho100 and volAt120 in each burned cell
    IDR100[index] <- x
    ## storing updated relative density
    rho100[[y]] <- IDR100
    
    ### new vol at 120
    v120 <- VFnc(sp = sp, Ac = ageRef[index]-t1[index],
                 iqs = iqs, rho100 = x,
                 rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef,
                 DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                 scenesCoef = NULL, withSenescence = F)
    v120 <- round(v120, 1)
    
    
    ### creating new raster layer
    if(y > 1) {
      volAt120[[y]] <- volAt120[[y-1]]
    }
    volAt120[[y]][index] <- v120
    
    
    
    # ##saving volAt120 for testing purposes
    # save(v120, file = paste0(outputDir, "v120_", y, ".RData"))
    # ## saving IDR100 for testing purposes
    # save(IDR100, file = paste0(outputDir, "IDR100_", y, ".RData"))
    # ## saving IDR100 for testing purposes
    # save(p, file = paste0(outputDir, "salvPlant_", y, ".RData"))
    # 
    ####################### updating tsd (do after updating density)
    tsd[f] <- 0
    #######################  resetting burned stands to 0 basal area
    stSetAside[index] <- 0
    
    
    ########################################################################
    ####################### simulating harvest
    ########################################################################
    if(verbose) {
      print(paste0("simulating harvests ; sim ", simID, " ; year ", y))  
    }
    
    
    rm(f)
    
    ##### eligible to harvest at a given timestep
    eligible <- tsd > matThresh &
      volAt120[[y]] >= volMinAt120
    ##dens %in% dens_RAT[which(dens_RAT$value %in% densProd), "ID"]  ## 
    
    x <- numeric() ## vector of cells to be harvested
    
    for (u in plan$uaf) { ### identifying stands to harvest
      ## checking for age structure conditions within uaf
      old <- tsd>=plan$oldMinAge & !is.na(coverTypes) & spEligible[[u]]
      regen <- tsd<plan$regenMaxAge & !is.na(coverTypes) & spEligible[[u]]
      
      ## computing proportions of old and regen
      propOld <- sum(values(old), na.rm = T) / sum(values(spEligible[[u]]), na.rm = T)
      propRegen <- sum(values(regen), na.rm = T) / sum(values(spEligible[[u]]), na.rm = T)
      ## computing remaining
      marginOld <- propOld - plan$oldMinProp
      marginRegen <- plan$regenMaxProp - propRegen
      ## determining the amount that was salvaged within the uaf
      sU <- which(values(spEligible[[u]]) & values(s)) 
      salvProp <- length(sU) / sum(values(spEligible[[u]]), na.rm = T)  
      salvPropVolEquiv <- salvProp*plan$salvageWoodPropLost$SEPM
      
      if(marginOld > 0  &
         marginRegen > 0 &
         salvPropVolEquiv < plan$targetHarvestLevels[["SEPM"]]) {
        
        ## determining the number of cells to harvest
        p <- min(plan$targetHarvestLevels[["SEPM"]] - salvPropVolEquiv,
                 marginOld, marginRegen)
        nCell <- round(p * sum(values(spEligible[[u]]), na.rm = T))
        ## eligible cells
        index <- which(values(eligible[[u]]))
        ## sampling cells
        if (length(index) == 1 &
            nCell >=1) {
          x <- append(x, index)
        } else {
          x <- append(x, sample(index,
                                size = min(length(index), nCell)))
        }
        
        
      }
    }
    
    reten[[y]] <- harv[[y]] <- coverTypes 
    reten[[y]][] <- harv[[y]][] <- NA
    ################ stand attributes, focussing on harvested stands
    if(verbose) {
      print("Computing stand attribute in harvested cells")
    }
    
    iqs <- iqs_extract(stands = x)
    a <- age_extract(stands = x)
    sp <- sp_extract(stands = x)
    t1 <- round(tFnc(sp, iqs, tCoef))
    r100 <- IDR100_extract(stands = x)
    ## age at 1m
    
    Ac <- ac_extract(a = a,
                     sp = sp,
                     iqs = iqs,
                     tCoef = tCoef,
                     tFnc = tFnc,
                     cap = 150)
    Hd <- HdFnc(sp, Ac, iqs, HdCoef)
    
    if(length(x) > 0) {
      
      ## basal area (all diameters, Ac >= 25)
      g <- g_extract(sp,
                     Ac,
                     iqs,
                     rho100 = r100,
                     HdCoef,
                     GCoef,
                     rho100Coef,
                     withSenescence = F,
                     DqCoef,
                     merchantable = T)
      
      ## standing merchantable volume 
      v <- VFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                scenesCoef = NULL, withSenescence = F)
      
      
      if(plan$retentionCut) {
        if(verbose) {
          print("simulating retention cut")
        }
        
        ### what would be the basal area needed to regenerate at retentionCutTarget if 
        #  that stands burned 
        
        ### scan levels of retention to assure 50 cubic-m at 120 yrs old in case of fire (brute force)
        propRetenVals <- seq(.05, 1, 0.05)
        seedlingDens <- matrix(NA,
                               nrow = length(g),
                               ncol = length(propRetenVals))
        
        ######################################################################################################
        ### predicting seedling density in case of fire
        for (j in seq_along(propRetenVals)) {
          seedlingDens[, j] <- seedlingFnc(sp = sp, Ac = Ac,
                                           G = g * propRetenVals[j],
                                           iqs = iqs,
                                           seedCoef = seedCoef, tCoef = tCoef)
        }
        
        
        rho100At120 <- matrix(apply(seedlingDens, 2, function(x) doQmapQUANT(x = x,
                                                                             fobj = seedlingQMapFit,
                                                                             type = "linear")),
                              nrow = length(g))
        
        ### vol at 120 if retention portion burned the same year
        v120 <- matrix(apply(rho100At120, 2, function(x) VFnc(sp = sp, Ac = Ac,
                                                              iqs = iqs, rho100 = x,
                                                              rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef,
                                                              DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                                                              scenesCoef = NULL, withSenescence = F)),
                       nrow = length(g))
        
        
        propRetention <- suppressWarnings(
          propRetenVals[apply(v120, 1, function(x) min(which(x >= plan$retentionCutTarget)))]
        )
        propRetention[is.na(propRetention)] <- 1
        
        ### storing values (to be written out)
        reten[[y]][x] <-  round(propRetention * v, 1)
        harv[[y]][x] <-  round(v - (propRetention * v), 1)
        
        ### storing values (temporary)
        stSetAside[x] <- round(propRetention * g, 1)
        
      } else {
        harv[[y]][x] <-  v
      }
      
      ####################### updating tsd
      tsd[x] <- 0
      
      
      v[g == 0] <- 0
    }
    # else {
    #   g <- v <- Ac
    # }
    
    
    if(verbose) {
      print("##############################################################")
      print(paste("########## sim", i, "of", nRep, "- year", y,"of", simDuration, "- completed #########"))
      print("##############################################################")
    }
    
    ########################################################################
    ### aging landscape for next year
    age[[y]] <- tsd
    tsd <- tsd + 1
    
    
  }
  
  ############################################################################
  ############################################################################
  #### stacking rasters and writing to files
  if(verbose) {
    print("stacking rasters")
  }
  
  
  ### stack consider possible missing layers
  
  
  
  ###########################  
  ### mandatory stacks... should be one layer every year
  fire <- stack(fire)
  names(fire) <- paste0("fire_", 1:nlayers(fire))
  age <- stack(age)
  names(age) <- paste0("age_", 1:nlayers(age))
  rho100 <- stack(rho100)
  names(rho100) <- paste0("rho100_", 1:nlayers(rho100))
  volAt120 <- stack(volAt120)
  names(volAt120) <- paste0("volAt120_", 1:nlayers(volAt120))
  
  
  ### optional stacks, can be missing layers
  stackFnc <- function(n) { ## where n is a string; the name of the list to stack
    x <- get(n)
    
    index <- which(!as.logical(lapply(x, is.null))) ## identify non-null layers
    assign("x", stack(x[index]))
    
    names(x) <- paste(n, index, sep = "_")
    assign("n", x)
  }
  
  
  for (n in c("harv", "salv", "plant", "reten")) {
    if(exists(n)) {x
      stackFnc(n)
    }
  }
  
  if(verbose) {
    print("writing to files")
  }
  
  save(fire, file = paste0(outputDir, "outputFire_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  save(age, file = paste0(outputDir, "outputTSD_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  save(rho100, file = paste0(outputDir, "outputRho100_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  save(volAt120, file = paste0(outputDir, "outputVolAt120_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  
  if(exists("harv")) {
    save(harv, file = paste0(outputDir, "outputHarvest_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  }
  if(exists("salv")) {
    save(salv, file = paste0(outputDir, "outputSalvage_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  }
  if(exists("plant")) {
    save(plant, file = paste0(outputDir, "outputPlantation_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  }
  if(exists("reten")) {
    save(reten, file = paste0(outputDir, "outputVolReten_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
  }
  
  
  print("##############################################################")
  print("##############################################################")
  print(paste0("Simulation #", i, " completed ", Sys.time()-tStart))
  print("##############################################################")
  
}

stopCluster(cl)
