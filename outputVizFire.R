###################################################################################################
###################################################################################################
##### Visualizing fire simulations
##### Dominic Cyr, in collaboration with Tadeusz Splawinski, Sylvie Gauthier, and Jesus Pascual Puigdevall
rm(list = ls()[-which(ls() %in% c("sourceDir", "scenario", "clusterN", "fr", "mgmt", "initYear"))])
# setwd("D:/regenFailureRiskAssessmentData_phase2/2018-10-23")
# wwd <- paste(getwd(), Sys.Date(), sep = "/")
# dir.create(wwd)
# setwd(wwd)
# scenario <- "baseline"
#################
#require(rgdal)
# require(raster)
#require(rgeos)
####################################################################
####################################################################
######
require(raster)
require(dplyr)
require(ggplot2)


#############################################################
#############################################################
output <- list()
fireRegime <- list()

for (s in seq_along(scenario)) {
    
    ### fetching outputs
    output[[scenario[s]]] <- get(load(paste0(paste0("../", "outputCompiled/outputCompiledFire_", scenario[s], ".RData"))))
    
    ## fetching fire regimes
    x <- read.csv(paste0("../", scenario[s], "/fireRegime.csv"))
    x <- x %>%
        distinct(scenario, period, fireCycle) %>%
        filter(scenario == fr[s])
    midPoint <- strsplit(as.character(x$period), "-")
    x[, "year"] <- round(as.numeric(lapply(midPoint, function(x) mean(as.numeric(x)))) - initYear)
    x <- x %>% 
        dplyr::select(scenario, fireCycle, year)
    ### add plateau if necessary
    x <- rbind(x,
               data.frame(year = seq(from = max(x$year),
                                     to = max(output[[s]]$year),
                                     by = 5),
                          fireCycle = x[which.max(x$year), "fireCycle"],
                          scenario = fr[s]))
    
    x <- distinct(x)
    fireRegime[[scenario[s]]] <- x
    
}
output <- do.call("rbind", output)
fireRegime <- do.call("rbind", fireRegime)
nSims <- length(unique(output$replicate))

#################################################################################
#################################################################################
####### figure for baseline scenario (temporally constant, spatially heterogenous)
##########################################################################
require(dplyr)
## summarizing fire regimes


# outputSummary <- outputCompiled %>%
#     #filter(scenario == scenario) %>%
#     
#     arrange(Zone_LN, replicate, year)


## create data frame with annual data
df <- output %>%
    mutate(propAAB = areaBurned_ha/areaZoneTotal_ha) %>%
    dplyr::select(scenario, replicate, year, areaBurned_ha, areaZoneTotal_ha, propAAB)
write.csv(df, file = paste0("fireSummary.csv"), row.names = F)

## global summary
globalSummary <- df %>%
    group_by(scenario, replicate) %>%
    summarize(fireCycle = round((1/mean(propAAB))),
              propAAB = mean(propAAB)) %>%
    arrange(scenario, replicate)

## global summary
span = 0.5
f <- function(x, y, span) {
    mod <- loess(y ~ x, span = span)
    return(predict(mod, x))
}



annualSummary <- df %>%
    group_by(scenario, year) %>%
    summarize(propAAB_mean = mean(propAAB),
              propAAB_p10 = quantile(propAAB, 0.05),
              propAAB_p25 = quantile(propAAB, 0.25),
              propAAB_p50 = quantile(propAAB, 0.50),
              propAAB_p75 = quantile(propAAB, 0.75),
              propAAB_p90 = quantile(propAAB, 0.95)) %>%
    mutate(meanS = f(x = year, y = propAAB_mean, span = span),
           p10S = f(x = year, y = propAAB_p10, span = span),
           p25S = f(x = year, y = propAAB_p25, span = span),
           p50S = f(x = year, y = propAAB_p50, span = span),
           p75S = f(x = year, y = propAAB_p75, span = span),
           p90S = f(x = year, y = propAAB_p90, span = span))
#### smoooth
fireRegime <- fireRegime %>%
    group_by(scenario) %>%
    mutate(target = f(x = year,
                      y = fireCycle,
                      span = span))

fcSummary <- globalSummary %>%
    group_by(scenario) %>%
    summarize(FC_median = median(fireCycle),
              FC_mean = 1/mean(propAAB),
              FP_p10 = quantile(fireCycle, 0.1),
              FP_p25 = quantile(fireCycle, 0.25),
              FP_p50 = quantile(fireCycle, 0.50),
              FP_p75 = quantile(fireCycle, 0.75),
              FP_p90 = quantile(fireCycle, 0.9))



################################################################################

cols <- c(baseline = "orange",
          RCP85 = "darkred")

p <- ggplot(annualSummary, aes(x = year + 2015, group = scenario,
                               fill = scenario,
                               colour = scenario)) +
    geom_line(aes(y = 100*meanS),
              size = 1) +
    geom_ribbon(aes(y = NULL, colour = NULL,
                    ymin = 100*p25S, ymax = 100*p75S),
                alpha = 0.25) +
    geom_line(aes(y = 100*p90S),
              size = 0.5, linetype = "dotted") +
    geom_line(data = fireRegime, aes(y = 100*(1/fireCycle)),
              size = 1, linetype = "dashed") +
    scale_fill_manual(values = cols) +
    scale_colour_manual(values = cols)

png(filename= paste0("fireRegimeRealized.png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)

options(scipen=999)

print(p + theme_dark() +
          
          theme(#legend.position="top", legend.direction="horizontal",
              legend.title = element_text(size = rel(0.85)),
              title = element_text(size = rel(0.85)),
              #plot.subtitle = element_text(size = rel(1)),
              plot.caption = element_text(size = rel(0.65))) +
          
          labs(title = paste0("Simulated fire regimes - ", nSims, " simulations per scenario"),
               subtitle = paste0("Full lines and ribbons represent averages and 25th and 75th percentiles\n",
                                 "Dotted lines represent 90th percentiles (extreme fire years)\n",
                                 "Dashed lines represent targetted fire regimes"),
               #"Min vieilles forÃªts (>=100 ans): 14%\n",
               #"Max régén. (< 20 ans): 35%"),
               x = "",
               y = "Percent area burned (%)\n"))

dev.off()

################################################################################













# ######################
# ### Plotting
# 
# require(ggplot2)
# options(scipen=999)
# m <- ggplot(outputSummary, aes(x=fireCycle)) +
#     geom_histogram() +#fill = "grey25"
#     facet_wrap(~Zone_LN) +#, scales = "free_x") +
#     
#     scale_x_log10(breaks = c(30, 60, 125, 250, 500, 1000, 2000, 4000, 16000)) +
#     geom_vline(data = fcSummary,  aes(xintercept = Fire_Cycle),
#                colour="lightblue", linetype = 3, size = 0.7, alpha = 1) +
#     geom_vline(data = fcSummary,  aes(xintercept = realizedFC_mean),
#                colour="yellow", linetype = 3, size = 0.5, alpha = 1)
# 
# yMax <- layer_scales(m)$y$range$range[2]
# xMax <- layer_scales(m)$x$range$range[2]
# 
# labelDF <-  data.frame(x = 10^xMax, y = yMax, Zone_LN = fcSummary$Zone_LN,
#                        prop = paste0("Percent area: ", round(fcSummary$prop*100), "%"),
#                        target = paste("Target:", fcSummary$Fire_Cycle, "years"),
#                        mean = paste("Average:", round(fcSummary$realizedFC_mean), "years"))
# 
# png(filename = paste0("realizedFC_baseline.png"),
#     width = 10, height = 5, units = "in", res = 600, pointsize=10)
# 
# print(m + theme_dark() +
#           theme(legend.position="top", legend.direction="horizontal",
#                 axis.text.x = element_text(angle = 45, hjust = 1),
#                 strip.text.y = element_text(size = 8))+
#           labs(title ="Distribution of realized fire cycles",
#                subtitle = "Baseline scenario - Dotted lines indicate targetted (blue) and realized (yellow) average fire cycles",
#                caption = paste("*Total of", length(unique(outputSummary$replicate)), "realizations" ),
#                x = "Fire cycle (years)",
#                y = "Frequency") +
#           geom_text(aes(x, y, label = prop),
#                     data = labelDF, hjust = 1, size = 3, fontface = 1) +
#           geom_text(aes(x, 0.85*y, label = target),
#                     data = labelDF, hjust = 1, size = 3, colour = "lightblue") +
#           geom_text(aes(x, 0.75*y, label = mean),
#                     data = labelDF, hjust = 1, size = 3, colour = "yellow"))
# 
# 
# 
# dev.off()


# ####################################################################################################
# ####################################################################################################
# ######
# ###### Visualizing one realisation
# ################################
# require(rgdal)
# require(rgeos)
# require(raster)
# require(maptools)
# require(ggplot2)
# 
# fireZonesP <- get(load("../fireZonesP.RData"))
# studyAreaP <- get(load("../studyAreaP.RData"))
# 
# ### cropping 20 km to remove 'border effect'
# extentFig <- extent(fireZones)
# extentFig[c(1,3)] <- extentFig[c(1,3)]+20000
# extentFig[c(2,4)] <- extentFig[c(2,4)]-20000
# 
# fireZones <- crop(fireZones, extentFig)
# fireZonesP <- crop(fireZonesP, extentFig)
# studyAreaP <- crop(studyAreaP, extentFig)
# tsdInit <-  raster("../tsd.tif")
# 
# fireZonesF <- fortify(fireZonesP)
# studyAreaF <- fortify(studyAreaP)
# 
# 
# 
# ################################
# 
# fire <- get(load("../output/outputFire_008.RData"))
# #output <- get(load("simOutput_002.RData"))
# 
# 
# 
# require(raster)
# require(stringr)
# require(ggplot2)
# 
# fTitle <- character()
# for (l in 1:nlayers(fire)) {
#     if(l == 1) {
#         tsd <- tsdInit
#         tsd[!is.na(tsd)] <- 100
#         tsd <- crop(tsd, extentFig)
#         rNA <- is.na(tsd)
#         rNA[rNA == 0] <- NA
#         rNA <- rasterToPoints(rNA)
#         rNA[,3] <- NA
#         maxVal <- max(values(tsd), na.rm = T) + nlayers(fire)
#         
#     }
#     options(scipen=999)
# 
#     ### data to plot
#     r <- fire[[l]]
#     r <- crop(r, extentFig)
#     tsd[r==1] <- 0
#     tsd[tsd>150] <- 150
#     
#     
#     df <- rasterToPoints(tsd)
#     if(l == 1) {
#         colnames(rNA)[3] <- colnames(df)[3]
#     }
#     #
#     df <- rbind(df, data.frame(rNA))
# 
#     ### plotting parameters
#     pWidth  <- 1400
#     pHeight <- 1200
#     pointsize <- 8
#     
#     colValues <- c(0, 10, 25, 50, maxVal)
#     #cols = c("red", "orange", "gold2", "seagreen4", "darkgreen")
#     cols = c("red", "orange", "gold2", "forestgreen", "darkgreen")
# 
#     ### plotting
#     p <- ggplot(data = df, aes_string("x", "y", fill = colnames(df)[3])) +
#         theme_bw() +
#         #theme(legend.position="top", legend.direction="horizontal") +
#         geom_raster() +
#         coord_fixed() +
#         scale_fill_gradientn(name = "Time since last fire (years)", #limits = c(0,1),
#                              colours = cols,
#                              values = colValues/maxVal, limits = c(0,maxVal),
#                              na.value = "dodgerblue1") +
#         #coord_fixed(ratio = figRatio) +
#         geom_polygon(aes(x = long, y = lat, group = group), data = fireZonesF,
#                      colour = 'black', fill = NA, alpha = 0.5, size = 0.5) +
#         geom_polygon(aes(x = long, y = lat, group = group), data = studyAreaF,
#                      colour = 'white', fill = NA, size = 1) +
#         labs(x = "\nx (UTM 18)",
#               y = "y (UTM 18)\n") +
#         theme(legend.position="top", legend.direction="horizontal") +
#         annotate("text", x = max(df$x), y = max(df$y)+2500,
#                  label = paste("annÃ©e", l),
#                  hjust = 1, vjust = 0, size = 0.3*pointsize, fontface = 2)
# 
#     f <- paste0("tsfTest_" , str_pad(l, nchar(nlayers(fire)), pad = "0"),".png")
#     fTitle <- append(fTitle, f)
# 
#     png(filename = f,
#         width = pWidth, height = pHeight, units = "px", res = 300, pointsize = pointsize,
#         bg = "white")
# 
#         print(p + theme(plot.title = element_text(size = rel(0.6)),
#                         axis.title.x = element_text(size = rel(0.5)),
#                         axis.title.y = element_text(size = rel(0.5)),
#                         axis.text.x = element_text(size = rel(0.5)),
#                         axis.text.y =  element_text(size = rel(0.5), angle = 90, hjust = 0.5),
#                         legend.title = element_text(size = rel(0.75)),
#                         legend.text = element_text(size = rel(0.5))))
# 
#     dev.off()
#     
#     ## aging landscape
#     tsd <- tsd + 1
#    # return(fTitle)
# }
# 
# require(animation)
# oopt = ani.options(ani.dev="png", ani.type="png",
#                    interval = 0.3, autobrowse = FALSE)
# 
# 
# im.convert(c(fTitle, rep(fTitle[length(fTitle)], 10)), output = "tsfExample.gif",
#            extra.opts = "", clean = F)
# ####################################################################################################
###################################################################################################
