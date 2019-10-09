###################################################
###################################################
### predicting the proportion of trees bearing cones as a function of age and species
###
#propConesTable <- propCones <- read.csv("../data/Splawinski/propCones.csv")

propConesFnc <- function(sp, iqs, Ac, tCoef) {
    
    .sp <- sp
    .iqs <- iqs
    .Ac <- Ac
    .tCoef <- tCoef
    .age <- .Ac + round(tFnc(sp = .sp, iqs = .iqs, tCoef = .tCoef))

    .y <- data.frame(age = .age)
    x <- rep(NA, length = length(.sp))
    
    ####### 
    # Black spruce: polynomial approximation based on Viglas et al 2013
    index <- which(.sp == "EN")
    .a <- as.data.frame(.age[index])
    #foo <- apply(.a, 1, function(x) 1.93839E-07*x[1]^3 - -0.000103703*x[1]^2 + 0.018378091*x[1] - 0.083051941)
    x[index] <- apply(.a, 1, function(x) 0.0000002*x[1]^3 - 0.0001*x[1]^2 + 0.0184*x[1] -0.0831 )
    
    ####### 
    # Jack pine:  polynomial approximation based on Briand et al 2014
    index <- which(.sp == "PG")
    .a <- as.data.frame(.age[index])
    x[index] <- apply(.a, 1, function(x) -5.36368E-09*x[1]^4 + 2.74419E-06*x[1]^3
                 - 0.000499589*x[1]^2 + 0.038258082*x[1] - 0.068439696)

    #ageIndex <- apply(.y, 1, function(x) max(which(propConesTable$X<x)))
    #spIndex <- match(.sp, colnames(propCones))
     
    
    # x <- data.frame(propConesTable[ageIndex,], spIndex)
    # x <-apply(x, 1, function(x) x[x["spIndex"]])
    return(as.numeric(x))
}


###################################################
###################################################
### establishing regeneration density (Splawinski et al) + quantile mapping (regen density -> rho100)
### Splawinski et al. (add complete reference)

seedTable <- read.csv("../data/Splawinski/Splawinski.csv")
seedCoef <- list()

for(sp in c("EN", "PG")) {
    seedCoef[[sp]] <- seedTable[,sp]
    names(seedCoef[[sp]]) <- seedTable[,"X"]
}


seedlingFnc <- function(sp, Ac, G, iqs, seedCoef, tCoef) {
    
    .sp <- as.character(sp)
    .G <- G
    .seedCoef <- seedCoef
    .Ac <- Ac
    .iqs <- iqs
    .tCoef <- tCoef
    
    
    
    a <- as.numeric(lapply(.seedCoef, function(x) x["a"]))
    b <- as.numeric(lapply(.seedCoef, function(x) x["b"]))
    c <- as.numeric(lapply(.seedCoef, function(x) x["c"]))
    
    m <- as.numeric(lapply(.seedCoef, function(x) x["m"]))
    b2 <- as.numeric(lapply(.seedCoef, function(x) x["b2"]))
    d <- as.numeric(lapply(.seedCoef, function(x) x["d"]))
    fl <- as.numeric(lapply(.seedCoef, function(x) x["fl"]))
    fh <- as.numeric(lapply(.seedCoef, function(x) x["fh"]))
    g <- as.numeric(lapply(.seedCoef, function(x) x["g"]))
    td <- as.numeric(lapply(.seedCoef, function(x) x["td"]))
    
    # seed production / m2
    
    df <- data.frame(Ac = .Ac,
                     ba = .G/10000, ## basal area in m2/m2
                     a = a[match(.sp, names(.seedCoef))],
                     b = b[match(.sp, names(.seedCoef))],
                     c = c[match(.sp, names(.seedCoef))],
                     m = m[match(.sp, names(.seedCoef))],
                     b2 = b2[match(.sp, names(.seedCoef))],
                     d = d[match(.sp, names(.seedCoef))],
                     fl = fl[match(.sp, names(.seedCoef))],
                     fh = fh[match(.sp, names(.seedCoef))],
                     g = g[match(.sp, names(.seedCoef))],
                     td = td[match(.sp, names(.seedCoef))])
    
   
    
    
    ### seed production (number of seeds per sq-meter; Greene and Johnson 1998)
    df[,"seedsPerSqM"] <- apply(df, 1, function(x) x["a"]*x["b"]*(x["ba"]^x["c"])*x["td"])

    ### survival after 3 years, depends a lot on good seedbed availability
    df[,"surv"] <- apply(df, 1, function(x)  x["g"] * (0.07 * ## seedbed available
                                                       (1-exp(-x["fl"] * x["m"]^x["b2"]))) + ## good seedbeds
                             x["g"] * (0.82 *## poor seedbed
                                           (1-exp(-x["fh"] * x["m"]^x["d"])))) ## bad seedbeds

    
    ### seedling density after 3 years
    x <- apply(df, 1, function(x) x["seedsPerSqM"] * x["surv"])
   
    ### correction for the proportion of trees bearing cones depending of stand age
    x <- x * propConesFnc(sp = .sp, iqs = .iqs, Ac = .Ac, tCoef = .tCoef)
    ### when G == 0
    x[which(df$ba == 0)] <- 0
    
    return(x)    
}


# ###testing function
# Ac <- 1:150
# rho100 <- c(0.12, 0.375, 0.77)
# iqs <- c(10)
# 
# df <- data.frame(sp = c(rep("EN", length(rho100)*length(Ac)), rep("PG", length(rho100)*length(Ac))),
#                  rho100 = c(rep(rho100[1], length(Ac)), rep(rho100[2], length(Ac)),
#                             rep(rho100[3], length(Ac))),
#                             #rep(rho100[4], length(Ac))),
#                  Ac = Ac,
#                  iqs = iqs)
# 
# df[,"G"] <- GFnc(sp = df$sp, Ac = df$Ac, iqs = df$iqs, rho100 = df$rho100,
#                  HdCoef = HdCoef, GCoef = GCoef,
#                  rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#                  DqCoef = DqCoef, merchantable = F)
# 
# 
# # if Ac < 25, an approximation of basal area must be computed
# index <- which(df$Ac < 25)
# df[index,"G"] <- df[index, "Ac"]/25 *
#     GFnc(sp = df[index, "sp"],
#          Ac = 25, iqs = df[index, "iqs"],
#          rho100 = df[index, "rho100"],
#          HdCoef = HdCoef, GCoef = GCoef,
#          rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence = F,
#          DqCoef = DqCoef, merchantable = F)
# 
# df[,"seedlings"] <- seedlingFnc(sp = df$sp, Ac = df$Ac, G = df$G, iqs = df$iqs,
#                                 seedCoef = seedCoef,
#                                 tCoef = tCoef)
# 
# require(dplyr)
# require(ggplot2)
# 
# png(filename= paste0("Splawinski_seedlingDensVsAc.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = seedlings, colour = as.factor(rho100), linetype = sp)) +
#     geom_line() +
#     labs(title = "Densité de semis 3 ans après feu en fonction de l'âge corrigé",
#          subtitle = paste("IQS:", iqs),
#          y = "semis par m2")
# 
# dev.off()
# 
# png(filename= paste0("Splawinski_GVsRho100.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = G, colour = sp, linetype = as.factor(rho100))) +
#     geom_line() +
#     labs(title = "Densité de semis 3 ans après feu en fonction de la surface terrière")
# 
# dev.off()
