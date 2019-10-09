################################################################################
################################################################################
#### Loading parameter tables
rhoTable <- read.csv(paste(psDir, "rho.csv", sep = "/"))
tTable <- read.csv(paste(psDir, "Ac.csv", sep = "/"))
rho100Table <- read.csv(paste(psDir, "rho100ToRho.csv", sep = "/"))
scenesTable <- read.csv(paste(psDir, "scenescence.csv", sep = "/"))
HdTable <- read.csv(paste(psDir, "Hd.csv", sep = "/"))
DqTable <-  read.csv(paste(psDir, "Dq.csv", sep = "/"))
GTable <- read.csv(paste(psDir, "G.csv", sep = "/"))
VTable <- read.csv(paste(psDir, "V.csv", sep = "/"))


################################################################################
################################################################################
### establishing relative density (rho) as a function of stem density and quadratic mean diameter
### Pothier-Savard 1998, p.38, Tab. 5

rhoCoef <- list()

for(i in 1:nrow(rhoTable)) {
    sp <-  as.character(rhoTable[i,"X"])
    if(sp %in% c("EN", "PG")) {
        rhoCoef[[sp]] <- as.numeric(rhoTable[i,2:3])
    }
}
rm(rhoTable)

## relative density index function (rho; unitless)
rhoFnc <- function(sp, N, Dq, rhoCoef) {
    .N <- N
    .Dq <- Dq
    .sp <- as.character(sp)
    .rhoCoef <- rhoCoef
    
    b1 <- as.numeric(lapply(rhoCoef, function(x) x[1]))
    b2 <- as.numeric(lapply(rhoCoef, function(x) x[2]))
    
    df <- data.frame(N = .N,
                     Dq = .Dq,
                     b1 = b1[match(.sp, names(.rhoCoef))],
                     b2 = b2[match(.sp, names(.rhoCoef))])
    
    x <- apply(df, 1, function(x)  x[1]/((x[2]/(10^x[3]))^(-1/x[4])))
    return(x)
}

################################################################################
################################################################################
### number of years necessary to grow to 1 m (Ac)
### Pothier-Savard 1998, p.36, Tab. 3

tCoef <- list()

for(i in 1:nrow(tTable)) {
    sp <-  as.character(tTable[i,"X"])
    if(sp %in% c("EN", "PG")) {
        tCoef[[sp]] <- as.numeric(tTable[i,2:3])
    }
}
rm(tTable)

## age at 1 m (years)
tFnc <- function(sp, iqs, tCoef) {
    .sp <- as.character(sp)
    .iqs <- iqs
    .tCoef <- tCoef

    b1 <- as.numeric(lapply(.tCoef, function(x) x[1]))
    b2 <- as.numeric(lapply(.tCoef, function(x) x[2]))

    df <- data.frame(iqs = .iqs,
                     b1 = b1[match(.sp, names(.tCoef))],
                     b2 = b2[match(.sp, names(.tCoef))])

    x <- apply(df, 1, function(x)  x["b1"]*x["iqs"]^x["b2"])
    x[is.infinite(x)] <- NA
    return(x)
}

# # testing function
# iqs <- 5:15
# 
# df <- data.frame(sp = c(rep("EN",length(iqs)),  rep("PG",length(iqs))),
#                  iqs = rep(iqs, 2))
# df["T1"] <- tFnc(sp = df$sp,
#                        iqs = df$iqs,
#                        tCoef)
# png(filename= paste0("Pothier-Savard_T1.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = iqs, y = T1, colour = sp)) +
#     geom_line() +
#     labs(title = "Temps requis pour atteindre 1 m de hauteur")
# 
# dev.off()

################################################################################
################################################################################
### relative density at 100 years-old (rho100) to relative density (rho)
### Pothier-Savard 1998, p.25, eq. 11

rho100Coef <- scenesCoef <- list()
for(i in 1:nrow(rho100Table)) {
    sp <-  as.character(rho100Table[i,"X"])
    if(sp %in% c("EN", "PG")) {
        rho100Coef[[sp]] <- as.numeric(rho100Table[i,c("b10", "b11")])
        scenesCoef[[sp]] <- list()
        x <- scenesTable[which(scenesTable$X == sp),]
        for (j in 1:nrow(x)) {
            asd <- x[j,"asd"]
            asf <- x[j,"asf"]
            scenesCoef[[sp]][[as.character(x[j,"iqs"])]] <- list()
            scenesCoef[[sp]][[as.character(x[j,"iqs"])]]["asd"] <- asd
            scenesCoef[[sp]][[as.character(x[j,"iqs"])]]["asf"] <- asf
            
        }
    }
}

rm(rho100Table)
rm(scenesTable)

## relative density (rho, unitless)
rho100ToRhoFnc <- function(rho100, sp, Ac, iqs, rho100Coef, scenesCoef = NULL, withSenescence) {
    .rho100 <- rho100
    .sp <- as.character(sp)
    .Ac <- Ac #+ 25
    .iqs <- iqs
    .rho100Coef <- rho100Coef
    .scenesCoef <- scenesCoef

    b10 <- as.numeric(lapply(.rho100Coef, function(x) x[1]))
    b11 <- as.numeric(lapply(.rho100Coef, function(x) x[2]))

    df <- data.frame(rho100 = .rho100,
                     Ac = .Ac,
                     iqs = .iqs,
                     b10 = b10[match(.sp, names(.rho100Coef))],
                     b11 = b11[match(.sp, names(.rho100Coef))],
                     stringsAsFactors = F)


    if(withSenescence) {
        .asd <- .asf <- rep(NA, length(.sp))
        for(i in 1:nrow(df)) {
            iqsCls <- df[i, "iqs"] %/% 3 *3
            iqsMin <- min(as.numeric(names(.scenesCoef[[.sp[i]]])))
            if(iqsCls < iqsMin) {
                iqsCls <- iqsMin
            }
            .asd[i] <- .scenesCoef[[.sp[i]]][[as.character(iqsCls)]]$asd
            .asf[i] <- .scenesCoef[[.sp[i]]][[as.character(iqsCls)]]$asf
        }
        
        
        df <- data.frame(df,
                         asd = .asd,
                         asf = .asf)
    }

    ## original equation
    x <- apply(df, 1, function(x)  exp((x["b10"] + (x["b11"]*x["rho100"]))/x["Ac"]))
    
    ## with additional term for scenenscence
    if(withSenescence) {
        df[,"x"] <- x
            ## with additionnal correction for senescence
        x <- apply(df, 1, function(x)  ifelse(x["Ac"] <= x["asd"], x["x"],
                                              ifelse(x["Ac"] > x["asf"], 0,
                                                     x["x"] * (1 - (x["Ac"]- x["asd"])^2/(x["asf"] - x["asd"])^2))))
    }
    
    return(x)
}


# ## testing function and visualizing results
# nYears <- 300
# rho100 <- 0.5
# iqs <- 10
# # t1 <- tFnc(sp = c(rep("EN", nYears), rep("PG", nYears)),
# #            iqs = 9,
# #            tCoef = tCoef)
# 
# df <- data.frame(rho100 = rep(rho100, nYears*2),
#                  sp = c(rep("EN", nYears), rep("PG", nYears)),
#                  iqs = iqs,
#                  Ac = rep(1:nYears, 2))
# 
# df[,"rho"] <- rho100ToRhoFnc(rho100 = df$rho100 ,
#                       sp = df$sp,
#                       Ac = df$Ac,
#                       iqs = df$iqs,
#                       rho100Coef = rho100Coef,
#                       scenesCoef  = scenesCoef,
#                       withSenescence = F)
# 
# require(ggplot2)
# 
# png(filename= paste0("Pothier-Savard_rho100ToRho.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = rho, colour = sp)) +
#     geom_line() +
#     labs(subtitle = paste("rho100 =", rho100),
#          title = "Indice de densité relative (rho) en fonction de la densité relative à 100 ans (rho100)")
# 
# dev.off()

################################################################################
################################################################################
### Dominant height (Hd)
### Pothier-Savard 1998, p.39, Tab. 6


HdCoef <- list()

for(i in 1:nrow(HdTable)) {
    sp <-  as.character(HdTable[i,"X"])
    if(sp %in% c("EN", "PG")) {
        HdCoef[[sp]] <- as.numeric(HdTable[i,2:7])
    }
}
rm(HdTable)

## Dominant height (m)
HdFnc <- function(sp, Ac, iqs, HdCoef) {
    .iqs <- iqs
    .sp <- as.character(sp)
    .Ac <- Ac
    .HdCoef <- HdCoef

    b10 <- as.numeric(lapply(.HdCoef, function(x) x[1]))
    b11 <- as.numeric(lapply(.HdCoef, function(x) x[2]))
    b12 <- as.numeric(lapply(.HdCoef, function(x) x[3]))
    b13 <- as.numeric(lapply(.HdCoef, function(x) x[4]))
    b14 <- as.numeric(lapply(.HdCoef, function(x) x[5]))
    b15 <- as.numeric(lapply(.HdCoef, function(x) x[6]))
    
    
    df <- data.frame(iqs = .iqs,
                     Ac = .Ac,
                     b10 = b10[match(.sp, names(.HdCoef))],
                     b11 = b11[match(.sp, names(.HdCoef))],
                     b12 = b12[match(.sp, names(.HdCoef))],
                     b13 = b13[match(.sp, names(.HdCoef))],
                     b14 = b14[match(.sp, names(.HdCoef))],
                     b15 = b15[match(.sp, names(.HdCoef))])

    x <- apply(df, 1, function(x)  x["b10"] + ((x["b11"]*(x["iqs"]^x["b12"])) *
                                                   (1-exp(-x["b13"]*x["Ac"]))^(x["b14"]*x["iqs"]^(-x["b15"]))))
    return(x)
}


# ### testing function and visualizing results
# nYears <- 130
# iqs <- 10
# df <- data.frame(iqs = rep(iqs, nYears*2),
#                  sp = c(rep("EN", nYears), rep("PG", nYears)),
#                  Ac = rep(1:nYears, 2))
# 
# df[,"Hd"] <- HdFnc(iqs = df$iqs,
#                   sp = df$sp,
#                   Ac = df$Ac,
#                   HdCoef = HdCoef)
# 
# require(ggplot2)
# png(filename= paste0("Pothier-Savard_Hd.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# 
# ggplot(data = df, aes(x = Ac, y = Hd, colour = sp)) +
#     geom_line() +
#     labs(subtitle = paste("IQS =", iqs))
# 
# dev.off()


################################################################################
################################################################################
### Quadratic mean diameter (Dq)
### Pothier-Savard 1998, p.40, Tab. 7


DqCoef <- list()

for(i in 1:nrow(DqTable)) {
    sp <-  as.character(DqTable[i,"X"])
    if(sp %in% c("EN", "PG")) {
        DqCoef[[sp]] <- as.numeric(DqTable[i,2:5])
    }
}
rm(DqTable)

## Quadratic mean diameter (cm)
DqFnc <- function(sp, Ac, iqs, rho100,
                  HdCoef = HdCoef, DqCoef = DqCoef,
                  rho100Coef = rho100Coef, scenesCoef = NULL,
                  merchantable = T, withSenescence = F) {
    .sp <- as.character(sp)
    .Ac <- Ac
    .iqs <- iqs
    .rho100 <- rho100
    .rho100Coef <- rho100Coef
    .scenesCoef <- scenesCoef
    .withSenescence <- withSenescence
    .rho <- rho100ToRhoFnc(rho100 = .rho100, sp = .sp, iqs = .iqs, Ac = .Ac,
                           rho100Coef = .rho100Coef, scenesCoef = .scenesCoef,
                           withSenescence = .withSenescence)
    .HdCoef <- HdCoef
    .DqCoef <- DqCoef
    
    
    b21 <- as.numeric(lapply(.DqCoef, function(x) x[1]))
    b22 <- as.numeric(lapply(.DqCoef, function(x) x[2]))
    b23 <- as.numeric(lapply(.DqCoef, function(x) x[3]))
    b24 <- as.numeric(lapply(.DqCoef, function(x) x[4]))

    .Hd <- HdFnc(sp = .sp, Ac = .Ac, iqs = .iqs, HdCoef = .HdCoef)
    
    #_psb21*_psb22^Hd*.Ac^_psb23*rho^_psb24
    
    df <- data.frame(Hd = .Hd,
                     Ac = .Ac,
                     rho = .rho,
                     b21 = b21[match(.sp, names(.DqCoef))],
                     b22 = b22[match(.sp, names(.DqCoef))],
                     b23 = b23[match(.sp, names(.DqCoef))],
                     b24 = b24[match(.sp, names(.DqCoef))])
    
    x <- apply(df, 1, function(x)  x["b21"] * (x["b22"]^x["Hd"]) * (x["Ac"]^x["b23"]) * (x["rho"]^x["b24"]))
    if(merchantable) {
        x[x<=9] <- NA
    }
    x[which(df$Ac < 25)] <- NA
    return(x)
}

# ### testing function and visualizing results
# require(dplyr)
# nYears <- 150
# iqs <- 10
# rho100 <- 1
# 
# 
# df <- data.frame(sp = c(rep("EN", nYears), rep("PG", nYears)),
#                  Ac = rep(1:nYears, 2),
#                  iqs = iqs,
#                  rho100 = rho100)
# df["Dq"] <- DqFnc(sp = df$sp,
#                   Ac = df$Ac,
#                   iqs = df$iqs,
#                   rho100 = df$rho100,
#                   HdCoef = HdCoef,
#                   DqCoef = DqCoef,
#                   rho100Coef = rho100Coef,
#                   merchantable = T, withSenescence = F)
# 
# require(ggplot2)
# png(filename= paste0("Pothier-Savard_Dq.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = Dq, colour = sp)) +
#     geom_line() +
#     xlim(0, max(df$Ac)) +
#     labs(subtitle = paste("IQS =", iqs, "\nrho100 = ", rho100))
# 
# dev.off()


################################################################################
################################################################################
### Basal area (sq-m/ha, merchantable)
### Pothier-Savard 1998, p.41, Tab. 8

GCoef <- list()

for(i in 1:nrow(GTable)) {
    sp <-  as.character(GTable[i,"X"])
    if(sp %in% c("EN", "PG")) {
        GCoef[[sp]] <- as.numeric(GTable[i,2:7])
    }
}
rm(GTable)

## basal area (sq-m/ha)
GFnc <- function(sp, Ac, iqs, rho100,
                 HdCoef, GCoef,
                 rho100Coef = rho100Coef, scenesCoef = NULL, withSenescence,
                 DqCoef = NULL, merchantable = T) {
    
    .sp <- as.character(sp)
    .Ac <- Ac
    .iqs <- iqs
    .rho100 <- rho100
    .rho100Coef <- rho100Coef
    .DqCoef <- DqCoef
    .HdCoef <- HdCoef
    .GCoef <- GCoef
    .withSenescence <- withSenescence
    
    .rho <- rho100ToRhoFnc(rho100 = .rho100, sp = .sp, iqs = .iqs, Ac = .Ac,
                           rho100Coef = .rho100Coef, withSenescence = .withSenescence)
    .scenesCoef <- scenesCoef
    .merchantable <- merchantable
    
    b31 <- as.numeric(lapply(.GCoef, function(x) x[1]))
    b32 <- as.numeric(lapply(.GCoef, function(x) x[2]))
    b33 <- as.numeric(lapply(.GCoef, function(x) x[3]))
    b34 <- as.numeric(lapply(.GCoef, function(x) x[4]))
    b35 <- as.numeric(lapply(.GCoef, function(x) x[5]))
    b36 <- as.numeric(lapply(.GCoef, function(x) x[6]))
    
    .Hd <-  HdFnc(sp = .sp, Ac = .Ac, iqs = .iqs, HdCoef = .HdCoef)
    if(merchantable) {
        .Dq <- DqFnc(sp = .sp, Ac = .Ac, iqs = .iqs, rho100 = .rho100,
                     HdCoef = .HdCoef, DqCoef = .DqCoef, scenesCoef = .scenesCoef,
                     rho100Coef = .rho100Coef, merchantable = .merchantable, withSenescence = .withSenescence)    
    }
    .Hd[which(.iqs == 0)] <- 0 ## to prevent NaN produced by division by zero
    
    
    df <- data.frame(Hd = .Hd,
                     Ac = .Ac,
                     rho = .rho,
                     b31 = b31[match(.sp, names(.GCoef))],
                     b32 = b32[match(.sp, names(.GCoef))],
                     b33 = b33[match(.sp, names(.GCoef))],
                     b34 = b34[match(.sp, names(.GCoef))],
                     b35 = b35[match(.sp, names(.GCoef))],
                     b36 = b36[match(.sp, names(.GCoef))])

    x <- apply(df, 1, function(x)  x["b31"] * x["Hd"]^x["b32"] * x["b33"]^x["Hd"] *
                   x["Ac"]^x["b34"] * x["rho"]^x["b35"] * exp(x["b36"]/x["Ac"]) )
    if(merchantable) {
        x[is.na(.Dq)] <- NA
    }
    x[which(df$Ac < 25)] <- NA
    
    
    
    return(x)
}

# ### testing function and visualizing results
# require(dplyr)
# 
# nYears <- 120
# rho100 <- c(0.12, 0.375, 0.77)
# iqs <- c(10)
# 
# 
# df <- data.frame(Ac = rep(1:nYears, length(rho100)*2),
#                  sp = c(rep("EN", nYears*length(rho100)), rep("PG", nYears*length(rho100))),
#                  rho100 = rep(c(rep(rho100[1], nYears),
#                                 rep(rho100[2], nYears),
#                                 rep(rho100[3], nYears)),2),
#                  iqs = iqs)
# df[,"G"] <- GFnc(sp =  df$sp, Ac = df$Ac,  iqs = df$iqs, rho100 = df$rho100,
#                  HdCoef = HdCoef, DqCoef = DqCoef, GCoef = GCoef, merchantable = F,
#                  rho100Coef = rho100Coef, withSenescence = F)
# 
# 
# require(ggplot2)
# png(filename= paste0("Pothier-Savard_G.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = G, colour = as.factor(rho100), linetype = sp)) +
#     geom_line() +
#     labs(title = "Surface terrière",
#         subtitle = paste("IQS =", iqs),
#         x = "Âge corrigé à 1 m",
#         y = "m2/ha")
# 
# dev.off()


################################################################################
################################################################################
### Merchandable volume
### Pothier-Savard 1998, p.41, Tab. 8

VCoef <- list()
for(i in 1:nrow(VTable)) {
    sp <-  as.character(VTable[i,"X"])
    if(sp %in% c("EN", "PG")) {
        VCoef[[sp]] <- as.numeric(VTable[i,2:5])
    }
}
rm(VTable)

## merchantable volume (cub-m/ha)
VFnc <- function(sp, Ac, iqs, rho100,
                 HdCoef, GCoef, DqCoef, VCoef,
                 rho100Coef = rho100Coef, merchantable = T,
                 scenesCoef = NULL, withSenescence) {
    
    .Ac <- Ac
    .iqs <- iqs
    .sp <- as.character(sp)
    .rho100 <- rho100
    .rho100Coef <- rho100Coef
    .scenesCoef <- scenesCoef
    .withSenescence <- withSenescence
    # .rho <- rho100ToRhoFnc(rho100 = .rho100, sp = sp, iqs = .iqs,
    #                        Ac = Ac, rho100Coef = rho100Coef,
    #                        withSenescence = .withSenescence)
    .HdCoef <- HdCoef
    .DqCoef <- DqCoef
    .VCoef <- VCoef
    .GCoef <- GCoef
    .merchantable <- merchantable
    
    b41 <- as.numeric(lapply(.VCoef, function(x) x[1]))
    b42 <- as.numeric(lapply(.VCoef, function(x) x[2]))
    b43 <- as.numeric(lapply(.VCoef, function(x) x[3]))
    b44 <- as.numeric(lapply(.VCoef, function(x) x[4]))
    
    ##
    df <- data.frame(iqs = .iqs,
                     Ac = .Ac,
                     rho100 = .rho100,
                     b41 = b41[match(.sp, names(.VCoef))],
                     b42 = b42[match(.sp, names(.VCoef))],
                     b43 = b43[match(.sp, names(.VCoef))],
                     b44 = b44[match(.sp, names(.VCoef))])
    
    df[,"Hd"] <- HdFnc(sp = .sp, Ac = .Ac, iqs = .iqs, HdCoef = .HdCoef)
    df[,"Dq"] <- DqFnc(sp = .sp, Ac = .Ac, iqs = .iqs, rho100 = .rho100,
                       HdCoef = .HdCoef, DqCoef = .DqCoef,
                       merchantable = .merchantable,
                       rho100Coef = .rho100Coef,
                       withSenescence = .withSenescence, scenesCoef = .scenesCoef)
    df[,"G"] <- GFnc(sp =  .sp, Ac = .Ac,  iqs = .iqs, rho100 = .rho100,
                     HdCoef = .HdCoef, DqCoef = .DqCoef, GCoef = .GCoef, merchantable = .merchantable,
                     rho100Coef = .rho100Coef, withSenescence = .withSenescence)
    

    x <- apply(df, 1, function(x)  x["b41"] * x["Hd"]^x["b42"] * x["G"]^x["b43"] * x["Dq"]^x["b44"])
    x[which(df$iqs == 0)] <- 0
    if(.merchantable) {
        x[which(df$Dq < 9)] <- 0 
    }
    
    return(x)
}
# 
# ### testing function and visualizing results
# require(dplyr)
# nYears <- 120
# rho100 <- c(0.043, 0.375, 0.77)
# iqs <- c(10)
# 
# 
# df <- data.frame(Ac = rep(1:nYears, 2*length(rho100)),
#                  sp = c(rep("EN", nYears*length(rho100)), rep("PG", nYears*length(rho100))),
#                  iqs = iqs,
#                  rho100 = rep(c(rep(rho100[1], nYears),
#                                 rep(rho100[2], nYears),
#                                 rep(rho100[3], nYears)),2))
# 
# df[,"V"] <- VFnc(sp = df$sp, Ac = df$Ac, iqs = df$iqs, rho100 = df$rho100,
#                  rho100Coef = rho100Coef, HdCoef = HdCoef, GCoef = GCoef, DqCoef = DqCoef, VCoef = VCoef, merchantable = F,
#                  scenesCoef = NULL, withSenescence = F)
# 
# 
# require(ggplot2)
# png(filename= paste0("Pothier-Savard_V.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=8)
# 
# ggplot(data = df, aes(x = Ac, y = V, colour = as.factor(rho100), linetype = sp)) +
#     geom_line() +
#     labs(title = "Volumes marchands",
#         subtitle = paste("IQS =", iqs),
#         x = "Âge corrigé à 1 m",
#         y = "m3/ha") +
#     geom_hline(yintercept = 30, linetype = "dotted", colour = "darkgreen")
# 
# dev.off()
