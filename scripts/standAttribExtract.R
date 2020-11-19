################################################################################
### 
### A few  function to extract stand attributes
### 
### 

iqs_extract <- function(r = IQS_POT,
                        stands = index) {
    x <- r[stands]
    return(x)
}

age_extract <- function(r = tsd,
                        stands = index) {
    x <- r[stands]
    return(x)
}

sp_extract <- function(r = coverTypes,
                       rat = coverTypes_RAT,
                       stands = index) {
    x <- rat[match(r[stands], rat$ID), "value"]
    return(x)
}

IDR100_extract <- function(r = IDR100,
                           stands = index) {
    x <- r[stands]
    x[x>1] <- 1
    return(x)
}

## age at 1m
ac_extract <- function(a,
                       sp,
                       iqs,
                       tCoef,
                       tFnc,
                       cap = 150) {
    x <-  round(a - tFnc(sp = sp,
                      iqs = iqs,
                      tCoef = tCoef))
    
    x[iqs == 0] <- 0
    ## capping Ac at 150
    x[x>cap] <- cap
    return(round(x))
}

g_extract <- function(sp,
                      Ac,
                      iqs,
                      rho100 = r100,
                      HdCoef,
                      GCoef,
                      rho100Coef,
                      withSenescence,
                      DqCoef,
                      merchantable) {

    
    ## basal area (all diameters, Ac >= 25)
    g <- GFnc(sp, Ac, iqs, rho100,
         HdCoef, GCoef, rho100Coef,
         withSenescence = F, DqCoef, merchantable = F)
    
    # g2 <- GFnc(sp, Ac, iqs, rho100 = r100,
    #           HdCoef, GCoef, rho100Coef,
    #           withSenescence = F, DqCoef, merchantable = F)
    
    ## basal area (all diameters, approximation for stands with Ac < 25)
    ageIndex <- which(Ac < 25)
    if(length(ageIndex)>0) {
        x <- #Ac[ageIndex]/25 *
            GFnc(sp = sp[ageIndex], Ac = 25, iqs = iqs[ageIndex],
                 rho100 = rho100[ageIndex],
                 HdCoef = HdCoef, GCoef = GCoef,
                 rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                 DqCoef = DqCoef, merchantable = F) - 
            (25-Ac[ageIndex]) *
            (GFnc(sp = sp[ageIndex], Ac = 26, iqs = iqs[ageIndex],
                  rho100 = rho100[ageIndex],
                  HdCoef = HdCoef, GCoef = GCoef,
                  rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                  DqCoef = DqCoef, merchantable = F) - 
                 GFnc(sp = sp[ageIndex], Ac = 25, iqs = iqs[ageIndex],
                      rho100 = rho100[ageIndex],
                      HdCoef = HdCoef, GCoef = GCoef,
                      rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
                      DqCoef = DqCoef, merchantable = F))
        
        x[x<0] <- 0
        g[ageIndex] <- x   
    }
    
    return(g)
}
# 
# v_extract <- function(sp, Ac, iqs, rho100 = r100,
#                       rho100Coef, HdCoef, GCoef, DqCoef, VCoef, merchantable,
#                       scenesCoef, withSenescence) {
#     ## merchantable volume 
#     x <-  VFnc(sp, Ac, iqs, rho100,
#                rho100Coef, HdCoef, GCoef, DqCoef, VCoef, merchantable = T,
#                scenesCoef = NULL, withSenescence = F)
#     return(x)
# }

