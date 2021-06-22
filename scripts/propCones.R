##################################################################################################
###################################################################################################
##### Visualizing outputs
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
home <- path.expand("~")
home <- gsub("\\\\", "/", home) # necessary on some Windows machine
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/regenFailureRiskAssessment_phase3/", sep ="/"))



wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)

getwd()
psDir <- "../data/Pothier-Savard/"
source(paste(psDir, "Pothier-Savard.R", sep = "/"))

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
    return(data.frame(sp =.sp,
                      iqs = .iqs,
                      Ac = .Ac,
                      age = .age,
                      propCone = as.numeric(x)))
}


spp <- c("EN", "PG")
iqs <- c(10)
Ac <- 1:150


df <- expand.grid(sp = spp,
                  iqs = iqs,
                  Ac = Ac)

goo <- propConesFnc(sp = df$sp,
             iqs = df$iqs,
             Ac = df$Ac,
             tCoef = tCoef) %>%
    group_by(sp, iqs) %>%
    mutate(propCone = ifelse(sp == "PG" & age>76, 0.9800415, propCone),
           propCone = ifelse(propCone>1, 1, propCone))


#foo2 <- filter(goo, sp == "PG")



as.data.frame(foo2)

require(ggplot2)
require(tidyverse)
library(dplyr)
foo <- read.csv("../data/Splawinski/propCones.csv")

foo <- gather(foo, "sp", "propCone", -X)




p <- ggplot(goo, aes(x = age, y = propCone,
               colour = sp)) +
    geom_line() +
    geom_hline(yintercept = c(0, 1), linetype = "dashed", alpha = 0.5) +
    scale_color_manual("Cover type", values = c("darkgreen", "darkorange")) +
    labs(title = "Proportion of cone-bearing trees",
         x = "Age",
         y = "Proportion of trees")




    # geom_point(data = foo, aes(x = X, 
    #                           y = propCone,
    #                           colour = sp))


