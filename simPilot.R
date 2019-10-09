###################################################################################################
###################################################################################################
##### Main script driving the simulation
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
### setwd("C:/Users/dcyr-z840/Sync/Travail/ECCC/regenFailureRiskAssessment_phase3")
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
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


####################################################################################################
####################################################################################################
tsdInit <- tsd
IDR100Init <- IDR100
####################################################################################################
####################################################################################################

nRep <- 1000
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

foreach(i = 0:100,
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
    
    
    ## initial volume at 120 years old for eligible stands
    #
    iqs <- IQS_POT[index]
    sp <- coverTypes_RAT[match(coverTypes[index], coverTypes_RAT$ID), "value"]
    a <- tsdInit[index]
    Ac <- ageRef[index] - t1[index]
    r100 <- IDR100[index]
    ##
    volAt120 <- coverTypes
    volAt120[] <- NA
    volAt120[index] <- VFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                            rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                            scenesCoef = NULL, withSenescence = F)
    
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
    fire <- harv <- age <- salv <- rho100 <- list()
    fr <- filter(fireRegime, scenario == scen)
    for (y in 1:simDuration) {### change into foreach, and return 'fire' 'harv' and 'age' as a list, then reformat
        
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
       
    
        iqs <- IQS_POT[index]
        a <- tsd[index]
        sp <- coverTypes_RAT[match(coverTypes[index], coverTypes_RAT$ID), "value"]
        r100 <- IDR100[index]
        
        ## age at 1m
        Ac <- round(a - tFnc(sp = sp,
                       iqs = iqs,
                       tCoef = tCoef))
        
        Ac[iqs == 0] <- 0
        ## capping Ac at 150
        Ac[Ac>150] <- 150
      
        
        ## basal area (all diameters, Ac >= 25)
        g <- GFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                  HdCoef = HdCoef, GCoef = GCoef, rho100Coef = rho100Coef,
                  withSenescence = F, DqCoef = DqCoef, merchantable = F)
        
        ## basal area (all diameters, approximation for stands with Ac < 25)
        ageIndex <- which(Ac < 25)
        
        if(length(ageIndex) > 1) {
            # x <- Ac[ageIndex]/25 *
            #     GFnc(sp = sp[ageIndex], Ac = 25, iqs = iqs[ageIndex],
            #          rho100 = r100[ageIndex],
            #          HdCoef = HdCoef, GCoef = GCoef,
            #          rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
            #          DqCoef = DqCoef, merchantable = F)
            # 
            
            x <- #Ac[ageIndex]/25 *
                GFnc(sp = sp[ageIndex], Ac = 25, iqs = iqs[ageIndex],
                     rho100 = r100[ageIndex],
                     HdCoef = HdCoef, GCoef = GCoef,
                     rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                     DqCoef = DqCoef, merchantable = F) - 
                (25-Ac[ageIndex]) *
                (GFnc(sp = sp[ageIndex], Ac = 26, iqs = iqs[ageIndex],
                      rho100 = r100[ageIndex],
                      HdCoef = HdCoef, GCoef = GCoef,
                      rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                      DqCoef = DqCoef, merchantable = F) - 
                     GFnc(sp = sp[ageIndex], Ac = 25, iqs = iqs[ageIndex],
                          rho100 = r100[ageIndex],
                          HdCoef = HdCoef, GCoef = GCoef,
                          rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                          DqCoef = DqCoef, merchantable = F))

            x[x<0] <- 0
            g[ageIndex] <- x
            
        }

        
        ################################################################################
        
        ## merchantable volume 
        v <- VFnc(sp = sp, Ac = Ac, iqs = iqs, rho100 = r100,
                  rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                  scenesCoef = NULL, withSenescence = F)
        
        ####################### Salvage logging
        
        salvageIndex <- index[which(v > plan$salvageEligibility$`SEPM`)]
        ## indentifying those eligible to harvest
        eligibleToSalvage <- which(values(sum(spEligible, na.rm = T)) == 1)
        salvageIndex <- salvageIndex[salvageIndex %in% eligibleToSalvage]
        ## salvaging only the allowed proportion
        salvageIndex <- sample(salvageIndex,
                               size = length(salvageIndex) * plan$salvageTargetStandProp$SEPM)
        
        s <- studyArea
        s[] <- NA
        s[salvageIndex] <- 1
        # # ## saving salvaged cells for testing purposes
        # save(s, file = paste0(outputDir, "s_", y, ".RData"))
        salv[[y]] <- s
        
        ####################### simulating regeneration density
        
        ## seedling density
        seedlingDens <- seedlingFnc(sp = sp, Ac = Ac, G = g, iqs = iqs, seedCoef = seedCoef, tCoef = tCoef)
        
        ## work with is.na(seedlingDens==F)
        x <- rep(NA, length(seedlingDens))
        indexSeedlings <- !is.na(seedlingDens)
        
        ## converting seedling density to new relative density
        x[indexSeedlings] <- doQmapQUANT(x = seedlingDens[indexSeedlings], fobj = seedlingQMapFit, type = "linear")
        x <- round(x, 3)
        ## setting seedling density in cells with cells 
        
        ####################### updating relative density, only when stands were not salvaged
        ## (here we assume that salvaged stands are put back to their former density)
        
        ## updating rho100 and volAt120 in each burned cell
        IDR100[index] <- x
        
        ### new vol at 120
        v120 <- VFnc(sp = sp, Ac = ageRef[index]-t1[index], iqs = iqs, rho100 = x,
                     rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                     scenesCoef = NULL, withSenescence = F)
        
        
        volAt120[index] <- v120
        
        
        
        ## keeping best value for salvaged stands (assume no decrease in relative density)
        ## relative density
        indexMaintained <- which(index %in% salvageIndex)
        x <- cbind(r100[indexMaintained], # former value
                    x[indexMaintained]) ## new value
        x <- apply(x, 1, function(x) max(x, na.rm = T))
        IDR100[salvageIndex] <- x
        ## corresponding volume at 120
        volAt120[salvageIndex] <- VFnc(sp = sp[indexMaintained],
                                       Ac = ageRef[salvageIndex]-t1[salvageIndex],
                                       iqs = iqs[indexMaintained], rho100 = x,
                                       rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = T,
                                       scenesCoef = NULL, withSenescence = F)
        
        ## storing updated relative density
        rho100[[y]] <- IDR100
        
        # ##saving volAt120 for testing purposes
        # save(v120, file = paste0(outputDir, "v120_", y, ".RData"))
        # ## saving IDR100 for testing purposes
        # save(IDR100, file = paste0(outputDir, "IDR100_", y, ".RData"))
    
        ####################### updating tsd (do after updating density)
        tsd[f] <- 0
        
        
        ####################### simulating harvest
        #### Modifier les cibles de récolte en fonction des superficies récupérées
        #######################################        #######################################
        
        print(paste0("simulating harvests ; sim ", simID, " ; year ", y))

        h <- f
        rm(f)
        h[] <- NA
        ## 
        

        ##### eligible to harvest at a given timestep
        eligible <- tsd > matThresh &
            volAt120 >= volMinAt120
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

    save(fire, file = paste0(outputDir, "outputFire_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(harv, file = paste0(outputDir, "outputHarvest_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(age, file = paste0(outputDir, "outputTSD_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(rho100, file = paste0(outputDir, "outputRho100_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    save(salv, file = paste0(outputDir, "outputSalvage_", str_pad(i, nchar(nRep-1), pad = "0"), ".RData"))
    
    print("##############################################################")
    print("##############################################################")
    print(paste0("Simulation #", i, " completed ", Sys.time()-tStart))
    print("##############################################################")

}

stopCluster(cl)
