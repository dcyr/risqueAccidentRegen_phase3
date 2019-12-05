###################################################################################################
###################################################################################################
##### Main script driving the simulation
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
### setwd("C:/Users/dcyr-z840/Sync/Travail/ECCC/regenFailureRiskAssessment_phase3")
rm(list = ls())
home <- path.expand("~")
#home <- normalizePath(home)
home <- gsub("\\\\", "/", home) # necessary on some Windows machine
home <- gsub("/Documents", "", home) # necessary on my Windows machine
print(home)
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))
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
########## RUN FIRST - RUN FIRST - RUN FIRST - RUN FIRST ##########
###Preparing initial conditions, 
########## RUN FIRST - RUN FIRST - RUN FIRST - RUN FIRST ##########
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

####################################################################################################
####################################################################################################
tsdInit <- tsd
IDR100Init <- IDR100
####################################################################################################
####################################################################################################

nRep <- 10
simDuration <- 150
simStartYear <- 2015
scen <- "RCP85"


### actual simulation
require(doSNOW)
#require(parallel)
clusterN <-  8#max(1, floor(0.98*detectCores()))  ### choose number of nodes to add to cluster.
# #######

outputDir <-  paste(getwd(), "output/", sep = "/")
dir.create(outputDir)

cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

foreach(i = 0:7,
       .packages= names(sessionInfo()$otherPkgs)) %dopar%  {
    

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
    ## current volume 
    volInit[index] <- VFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                            rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                            scenesCoef = NULL, withSenescence = F)
    
    ## Basal area set aside in case of retention cut
    stSetAside <- volSetAside <- coverTypes  
    stSetAside[!is.na(stSetAside)] <- volSetAside[!is.na(volSetAside)] <- 0
    
    writeRaster(volAt120, file = "volAt120.tif", overwrite = T)
    writeRaster(volInit, file = "volInit.tif", overwrite = T)
    
    rm(iqs, sp, a, Ac, r100)
    
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
    
    
    
    
    ## variable stand attributes (stored)
    fire <- harv <- age <- salv <- plant <- rho100 <- vReten <- vHarv <- list()
    fr <- filter(fireRegime, scenario == scen)
    for (y in 1:150) {#simDuration) {### change into foreach, and return 'fire' 'harv' and 'age' as a list, then reformat
        

        ####################### simulating fire
        
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
        
        salvageIndex <- index[which(v > plan$salvageEligibility$`SEPM`)]
        ## indentifying those eligible to harvest
        eligibleToSalvage <- which(values(sum(spEligible, na.rm = T)) >= 1)
        salvageIndex <- salvageIndex[salvageIndex %in% eligibleToSalvage]
        ## salvaging only the allowed proportion
        ########################
        ## determining the number of cells to harvest
        nCell <- min(length(salvageIndex) * plan$salvageTargetStandProp$SEPM,
                     plan$targetHarvestLevels$SEPM / plan$salvageWoodPropLost$SEPM *
                       sum(values(spEligible[[u]]), na.rm = T))
        nCell <- round(nCell)
        salvageIndex <- sample(salvageIndex,
                               size = nCell)
        
        s <- studyArea
        s[] <- NA
        s[salvageIndex] <- 1
        # # ## saving salvaged cells for testing purposes
        # save(s, file = paste0(outputDir, "s_", y, ".RData"))
        salv[[y]] <- s
        
        ####################### simulating regeneration density
        
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
        if(plan$salvagePlantation) {
            indexSalvPlant <- which(index %in% salvageIndex &
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
      
        if(y == 1) {
            volAt120 <- list(volAt120)
            volAt120[[y]][index] <- v120
        } else {
            volAt120[[y]] <- volAt120[[y-1]]
            volAt120[[y]][index] <- v120
        } 
        
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
        
        print(paste0("simulating harvests ; sim ", simID, " ; year ", y))

        h <- f
        rm(f)
        h[] <- NA
        ## 
                ##### eligible to harvest at a given timestep
        eligible <- tsd > matThresh &
            volAt120[[y]] >= volMinAt120
            ##dens %in% dens_RAT[which(dens_RAT$value %in% densProd), "ID"]  ## 
        
        x <- numeric() ## vector of cells to be harvested

        for (u in plan$uaf) {
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
                x <- append(x, sample(index, size = min(length(index), nCell)))
                
            }
        }
        
        h[x] <- 1
        
        
        ################ stand attributes focussing on harvested stands
        ### 
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
        
        if(length(x > 0)) {
          
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
                ### what would be the basal area needed to regenerate at retentionCutTarget if 
                    #  that stands burned 
                
                ### scan levels of retention to assure 50 cubic-m at 120 yrs old in case of fire (brute force)
                propRetenVals <- seq(.05, 1, 0.05)
                seedlingDensAt120 <- matrix(NA,
                                       nrow = length(g),
                                       ncol = length(propRetenVals))
                
                
                ######################################################################################################
                ### predicting seedling density in case of fire
                for (j in seq_along(propRetenVals)) {
                  seedlingDensAt120[, j] <- seedlingFnc(sp = sp, Ac = Ac,
                                                        G = g * propRetenVals[j],
                                                        iqs = iqs,
                                                        seedCoef = seedCoef, tCoef = tCoef)
                }
                
               
                rho100At120 <- apply(seedlingDensAt120, 2, function(x) doQmapQUANT(x = x,
                                                                                   fobj = seedlingQMapFit,
                                                                                   type = "linear"))
                
                ### vol at 120 if retention portion burned the same year
                v120 <- apply(rho100At120, 2, function(x) VFnc(sp = sp, Ac = Ac,
                                                               iqs = iqs, rho100 = x,
                                                               rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef,
                                                               DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                                                               scenesCoef = NULL, withSenescence = F))
                
                propRetention <- propRetenVals[apply(v120, 1, function(x) min(which(x >= plan$retentionCutTarget)))]
                propRetention[is.na(propRetention)] <- 1
                
                ### storing values (to be written out)
                vReten[[y]] <- vHarv[[y]] <- h 
                vReten[[y]][x] <-  round(propRetention * v, 1)
                vHarv[[y]][x] <-  round(v - (propRetention * v), 1)
                
                ### storing values (temporary)
                stSetAside[x] <- round(propRetention * g, 1)
                
            } else {
              vHarv[[y]] <- h 
              vHarv[[y]][x] <-  v
            }
            
          
          
          
          
          v[g == 0] <- 0
        } else {
          g <- v <- Ac
        }
        
        # ### saving yearly timesteps for testing purposes
        # save(h, file = paste0(outputDir, "h_", y, ".RData"))
        ## storing harvested stands
        harv[[y]] <- h
        ####################### updating tsd
        tsd[h] <- 0
        
        age[[y]] <- tsd
        
       
        # ### saving yearly timesteps for testing purposes
        # save(tsd, file = paste0(outputDir, "tsd_", y, ".RData"))
        ###
        print("##############################################################")
        print("##############################################################")
        
        #######################
        ### aging landscape for next year
        tsd <- tsd + 1
       
        
    }
    
    fire <- stack(fire)
    harv <- stack(harv)
    age <- stack(age)
    rho100 <- stack(rho100)
    salv <- stack(salv)
    plant <- stack(plant)
    volAt120 <- stack(volAt120)
    vHarv <- stack(vHarv)
    vReten <- stack(vReten)

    save(fire, file = paste0(outputDir, "outputFire_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(harv, file = paste0(outputDir, "outputHarvest_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(age, file = paste0(outputDir, "outputTSD_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(rho100, file = paste0(outputDir, "outputRho100_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(salv, file = paste0(outputDir, "outputSalvage_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(plant, file = paste0(outputDir, "outputPlantation_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(volAt120, file = paste0(outputDir, "outputVolAt120_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(vHarv, file = paste0(outputDir, "outputVolHarv_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(vReten, file = paste0(outputDir, "outputVolReten_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    print("##############################################################")
    print("##############################################################")
    print(paste0("Simulation #", i, " completed ", Sys.time()-tStart))
    print("##############################################################")

}

stopCluster(cl)
