# ####################################################################
# ####################################################################
# ### simple regenDensity prediction function
# ####################################################################
# ####################################################################


### thresholds for young stands
thresholds <- list(EN = c(30, 50),
                   PG = c(20, 30))
densProd <- c("AB", "C", "D")


regenDensityPredict <- function(dens, tsd, coverTypes,
                                dens_RAT, coverTypes_RAT) {
    

    ## regen failure
    indexFailure <- which((coverTypes == coverTypes_RAT[coverTypes_RAT$value=="EN", "ID"] & tsd < thresholds$EN[1]) |
                              (coverTypes == coverTypes_RAT[coverTypes_RAT$value=="PG", "ID"] & tsd < thresholds$PG[1]))
    
    ## loss in productivity
    indexLossProd <- which((coverTypes == coverTypes_RAT[coverTypes_RAT$value=="EN", "ID"] & tsd < thresholds$EN[2]) |
                               (coverTypes == coverTypes_RAT[coverTypes_RAT$value=="PG", "ID"] & tsd < thresholds$PG[2]))
    
    indexLossProd <- indexLossProd[indexLossProd %in% indexFailure == F]
    
    newDens <- dens
    newDens[indexLossProd] <- dens[indexLossProd]+1
    newDens[indexFailure] <- 4
    
    return(newDens)
}

    

