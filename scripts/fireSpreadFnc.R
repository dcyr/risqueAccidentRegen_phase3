################################################################################
################################################################################
####
#### A simple cellular automaton to simulate fire spread
#### Dominic Cyr
####
################################################################################
################################################################################

fireSpread <- function(eligible, w = NULL,
                       fireSize, fireZones,
                       probSpread =  0.32, maxTry = 3, id) {

    require(raster)
    scaleFactor <- prod(res(eligible) / 100) # convert pixels into hectares

    ##default propagation kernel ("queen's case")
    if(is.null(w))  {
        w <- matrix(1, nrow = 3, ncol = 3)
    }

    f <- eligible-1

    fArea <- list()
    ### looping through individual fires
    # first, shuffling fires
    fireSize <- fireSize[sample(1:nrow(fireSize)),]
    rownames(fireSize) <- 1:nrow(fireSize)
    for (i in 1:nrow(fireSize)) {
        fs <- fireSize[i,2]
        
        ## igniting fire
        ignition <- sample(which(values(eligible)>0 & values(fireZones) == fireSize[i,1]), 1)
        nIgnit <- 1
        f[ignition] <- 1
        fireFront <- f
        #
        fSize <- 1*scaleFactor
        while(fSize < fs) {
            remaining <- round((fs - fSize)/scaleFactor)
            ### looking up neighbours
            # all cells adjacent to a burned cell
            fireNeighbour <- focal(fireFront, w = w, pad = T, padValue = 0) > 0
            # remove pixels that already burned, or that are not eligible
            fireNeighbour <- fireNeighbour - (f>0 | !eligible)
            indices <- which(values(fireNeighbour) == 1)
            ### remove pixels that already burned (redondant?)
            indices <- setdiff(indices, which(values(eligible) == 0))
            ### fire spread
            # burning only a proportion of neighbours
            rVec <- runif(length(indices))
            indices <- indices[union(which.min(rVec), which(rVec < probSpread))] ## burn at least one pixel until target fire size is achieved
            #indices <- indices[which(rVec < probSpread)]
            # trim so that final fire size does not exceed target fire size
            if (length(indices) > remaining) {
                indices <- indices[1:remaining]
            }
            
            ### when fire extinguishes, check a few things... 
            if (length(indices) == 0) {
                if (remaining > 0) { ### fire got extinguished before reachong its target size
                    if (nIgnit >= maxTry) { ### if we try to reignite too often, give up...
                        print(paste("Could not reach final target size after", maxTry, "ignitions. SimID:", id, "; Jumping to next fire event"))
                        break
                    }
                    print("Fire ran out of fuel before reaching its target size, igniting additional cell")
                    ### igniting another fire, may be started in any other region
                    potentialIgnitionSite <- which(values(eligible)>0 &
                                                       values(fireZones) == fireSize[i,1] & ## uncomment this if ignition must be in a given fire
                                                       values(f)==0)
                    if (length(potentialIgnitionSite) == 0) {
                        print("Could not reach final target size. No more pixels to ignite. Jumping to next fire event")
                        break
                    }
                    ignition <- sample(potentialIgnitionSite, 1)
                    indices <- ignition

                    nIgnit <- nIgnit + 1
                } else {### fire reaches its target size
                    break  
                }
            }

            f[indices] <- 1
            # resetting fire front
            fireFront[] <- 0
            # updating fire front for next iteration
            fireFront[indices] <- 1 ## area burned in the current time step
            ## updating current fire size
            fSize <- fSize + length(indices)*scaleFactor
            # record iteration on raster
            f[f>0] <- f[f>0] + 1
        }
        
        f[f==0] <- NA
        # storing individual fire events
        fArea[[i]] <- f-1 ## spatial
        
        eligible[f>0] <- 0
        f[] <- 0
    }
    return(stack(fArea))
}

