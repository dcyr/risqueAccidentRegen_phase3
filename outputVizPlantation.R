###################################################################################################
###################################################################################################
##### Visualizing harvest simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "simInfo", "rawOutputDir"))])
#################
require(raster)
require(ggplot2)
require(dplyr)
require(reshape2)
# 
# require(doSNOW)
# require(parallel)
# require(foreach)
# 
# clusterN <-  24  ### choose number of nodes to add to cluster.
# #######
# cl = makeCluster(clusterN, outfile = "") ##
# registerDoSNOW(cl)
# #######
# plantSurv <- foreach(s = seq_along(simInfo$simID),
#                          .combine = "rbind") %dopar% {


out <- list()
for(s in seq_along(simInfo$simID)) {
    simID <- simInfo$simID[[s]]
    f <- paste0("../outputCompiled/outputCompiledPlantation_", simID, ".RData")
    if(!file.exists(f)) {
        next
    }
    
    fr <- simInfo$fire[[s]]
    mgmt <- simInfo$mgmt[[s]]
    ctDyn <- simInfo$ctDyn[[s]]
    
    if(is.na(ctDyn)) {
        ctDyn <- F
    }
    
    
    outputCompiled <- get(load(f))
    
    
    #breaks <- seq(0, 150, by = 10)
    
    df <- outputCompiled %>%
        mutate(surv = int > matThresh | cens == 0)
    
    require(survival)
    
    time <- df$int
    cens <- df$cens
    
    int <- Surv(time[time>0], cens[time>0], type = "right")
    
    
    matThresh <- data.frame(sp = c("EN", "PG"),
                            matThresh = c(90, 76))
    if(ctDyn) {
        matThresh <- matThresh[matThresh$sp == "PG",]
    }
    
    # cycle (exp neg)
    fitExp <- survreg(int ~ 1, dist="exponential")
    bExp <- exp(fitExp$coefficients)
    survProbExp <- pexp(q = matThresh$matThresh,
                        rate = 1/bExp, lower.tail = F)
    
    # cycle (Weibull)
    fitWeib <- survreg(int ~ 1, dist="weibull")
    bW <- exp(fitWeib$coefficients)
    cW <- fitWeib$scale
    fcW <- bW*gamma((1/cW)+1)
    survProbWeib <- pweibull(q = matThresh$matThresh,
                             shape = cW, scale = bW, lower.tail = F)
    
    # actual survival
    matThresh <- outputCompiled %>%
        filter(year <= 150 - matThresh) %>%
        mutate(surv2Maturity = int >= matThresh) %>%
        group_by(plantedSp ) %>%
        summarise(survProbSim = mean(surv2Maturity)) %>%
        merge(matThresh, by.x = "plantedSp", by.y = "sp")
    
    
    
    out[[simID]] <- cbind(matThresh,
                 simID = simID,
                 fire = fr,
                 mgmt = mgmt,
                 cycleExp = bExp,
                 cycleWeib = fcW,
                 survProbExp = survProbExp,
                 survProbWeib = survProbWeib) %>%
        mutate(survProbExp = ifelse(fire == "RCP 8.5", NA, survProbExp),
               survProbWeib = ifelse(fire == "RCP 8.5", NA, survProbWeib)) %>%
        select(simID, fire, mgmt, cycleExp, cycleWeib,
               plantedSp, matThresh,
               survProbExp, survProbWeib, survProbSim)
    
    print(s)
}
   
out <- do.call("rbind", out)
rownames(out) <- 1:nrow(out)
# est. survival prob surv to maturity

out[, c("cycleExp", "cycleWeib")] <-  round(out[, c("cycleExp", "cycleWeib")], 1)
out[, c("survProbExp", "survProbWeib", "survProbSim")] <-  round(out[, c("survProbExp", "survProbWeib", "survProbSim")], 3)
out[out$fire == "Baseline", "survProbBaseline"] <-  round(pexp(q = out[out$fire ==  "Baseline", "matThresh"],
                                                               rate = 1/101, lower.tail = F), 3)

write.csv(out, file = "outputPlantationSurv.csv", row.names = F)
# pexp(q = 90, rate = 1/101, lower.tail = F)





# cox_reg <- coxph(int ~ 1)
# base.cox <- basehaz(cox_reg)
# maxHaz <- max(base.cox[,1])
# index <- which(base.cox[,1] == maxHaz)
# fc <- mean(base.cox[index,"time"])/maxHaz  



# mean(foo$surv2Maturity)
# 
# require(ggplot2)
# 
# ggplot(foo, aes(x = year, y = surv2Maturity)) +
#   geom_smooth(#,group = periode
#               method = "loess",
#               level = 0.9,
#               span = 0.5,
#               size = 0.25)
# 
# 
# mean(foo$surv3Maturity)
#   group_by(index)
#   
# 
# 

# 

# 
# b <- exp(fitExp$coefficients)
# 
# b <- exp(fitWeib$coefficients)
# c <- 1/(fitWeib$scale)
# fc <- b*gamma((1/c)+1)
# 
# 

# 
# 
# 
# summary(int)
# basehaz(fit)
# predict(coxM, type = "expected")
