####################################################################################################
####################################################################################################
###### Preparation of harvesting plan
######## This is sourced at the beginning of the experiment 
uafHarvest <- uaf_RAT$ID # UAF IDs of all elligible UAFs
subZoneHarvest <- subZones_RAT[which(subZones_RAT$value == "Productive forest - harvestable"), "ID"]  # character vector of elligible subzones
comSpp <- list(SEPM = c("EN", "PG"))
comSppId <- lapply(comSpp, function(x) coverTypes_RAT[which(coverTypes_RAT$value %in% x),"ID"])
spMaturity <- c(EN = 90, PG = 76, F = NA) # commercial maturity of each species
spMaturity <- spMaturity[coverTypes_RAT[match(names(spMaturity), names(spMaturity)), "ID"]]
spvolMin <- c(EN = 50, PG = 50, F = NA) # minimum expected volume to be considered as commercial (in cubic-meters)
spAgeRef <- c(EN = 120, PG = 120, F = NA) # age at which minimum volume must be achieved
names(spMaturity) <- 1:length(spMaturity)

## zonal grouping, harvest rates and age structure targets
targetAgeGrouping <- c("uaf", "subZones") # factors
targetHarvestLevels <- list(SEPM = c(0.0062))# 0.0062
regenMaxProp <- 1#.35 # max proportion of regenerating stands
regenMaxAge <- 20 #less than x years since last disturbance
oldMinProp <- 0#.14 # min proportion of old stands
oldMinAge <- 100

## Access limitation
limitedAccess <- c(harv = F,
                   salv = T,
                   plant = T,
                   reten = F)

## salvage logging parameters
salvageLog <- T
salvageTargetStandProp <- list(SEPM = c(.70)) ## (baseline: .70) max proportion of commercial volume that is salvaged
salvageWoodPropLost <- list(SEPM = c(.75)) ## proportion burned wood that is salvageable
salvageEligibility <- list(SEPM = c(70)) ## in m3/ha, minimum merchanteable volume in order to be eligible to salvage logging

## retention cutting
retentionCut <- T
retentionCutTarget <- 30 ## targeted min volAt120 years old

## plantation
plantation <- c(postFire = F,
               postHarv = F,
               postSalv = T,
               postReten = F)
plantationSp <- c(postFire = "same",## ('"same", or any of the other covertype)
                  postHarv = "same",
                  postSalv = "same",
                  postReten = "same")
plantationThreshold <- 0.2 ## seedling density under which plantation is prescribed
plantationDensity <- 0.2 ## planted seedling density (per sq meter, is added to natural regeneration) 
## stand selection method (either 'random' or 'older first')
standSelectionMethod <- "random"


### Producing raster with minimum IDR100 based on some 'spvolMin' @ 'spAgeRef' years old (needs IQS)
###################
###################



managementPlan <- list(baseline = list(uaf = uafHarvest,
                                       subZone = subZoneHarvest,
                                       comSppId = comSppId,
                                       maturity = spMaturity,
                                       volMin = spvolMin,
                                       ageRef = spAgeRef,
                                       ## zonal grouping, harvest rates and age structure targets
                                       targetAgeGrouping = targetAgeGrouping,
                                       targetHarvestLevels = targetHarvestLevels,
                                       regenMaxProp =regenMaxProp,
                                       regenMaxAge = regenMaxAge,
                                       oldMinProp = oldMinProp,
                                       oldMinAge = oldMinAge,
                                       ## Access limitation
                                       limitedAccess = limitedAccess,
                                       ## salvage logging parameters
                                       salvageLog = salvageLog,
                                       salvageTargetStandProp= salvageTargetStandProp,
                                       salvageWoodPropLost = salvageWoodPropLost,
                                       salvageEligibility = salvageEligibility,
                                       ## retention cutting
                                       retentionCut = retentionCut,
                                       retentionCutTarget = retentionCutTarget,## plantation
                                       plantation = plantation,
                                       plantationSp = plantationSp,
                                       plantationThreshold = plantationThreshold,
                                       plantationDensity = plantationDensity,
                                       ##
                                       standSelectionMethod = standSelectionMethod)
                       )

## clearing everything from memory except what's been put into 'stored' 
stored <- append(stored, "managementPlan")
rm(list = ls()[!ls() %in% stored])



