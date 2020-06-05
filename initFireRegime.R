####################################################################################################
####################################################################################################
###### Preparation of fire regime

### logNormal fit for fire size distribution

fireObs <- read.csv("../data/fireObs.csv", header=TRUE)
thresh <- c(0, 200000) ##prod(res(studyArea))/10000
df <- fireObs %>%
    filter(areaTotal_ha>thresh[1],
           areaTotal_ha<thresh[2])

fireSizeDist <- round(df$areaTotal_ha)
fireSizeMean <- round(mean(fireSizeDist))

fireNCorrFactor <- 0.9386

###parametric fire distribution
# require(MASS)
# maxFireSize <- 1.5*max(fireObs$areaTotal_ha) ## 1.5 * max obs => average fc of 104 years in the study area 
# 
# fireSizeFit <- fireSizeMax <- list()
# for (i in unique(as.character(fireObs$zone))) {
#     x <- filter(fireObs, zone == i)$areaTotal_ha
#     fireSizeFit[[i]] <- fitdistr(x, "lognormal")
#     fireSizeMax[[i]] <- maxFireSize
# }
# 
## clearing everything from memory except what's been put into 'stored'

#rm(list = ls())
print("loading baseline fire regime...")
fireRegime <- read.csv("../data/fireRegime_baseline.csv")
print("loading projected fire regime...")
fireProj <- get(load("../data/FireProjections_Boulanger2016.RData"))
###
baselineFC <- 101
proj1st <- 2056
breaks <- seq(2010, 2100, 5)
scenProg <- "RCP85"
##########
fireRegime <- distinct(fireRegime, ID, Zone_LN, scenario) %>%
    mutate(fireCycle = baselineFC,
           period = "2011-2020")

####  computing projected fire regimes
breakNames <- paste((breaks+1)[-length(breaks)], breaks[-1], sep = "-")

fireProj <- fireProj %>%
    filter(ZONES == 7,
           scenario == scenProg,
           Year >= proj1st)

fireProj <- fireProj[,c("Year", "AdjustedBurnRate")]

yrsTmp <- (breaks[1]+1):proj1st
brInterp <- seq(100/baselineFC, fireProj[which(fireProj$Year == proj1st), "AdjustedBurnRate"], length.out = length(yrsTmp))

fireProj <- rbind(fireProj,
                  data.frame(Year = yrsTmp,
                             AdjustedBurnRate = brInterp)) %>%
    distinct() %>%
    arrange(Year)

fireProj[,"period"] <- cut(fireProj$Year, breaks)
fireProj <- fireProj %>%
    group_by(period) %>%
    summarise(PAAB = mean(AdjustedBurnRate)/100,
              fireCycle= round(1/PAAB)) %>%
    as.data.frame() %>%
    mutate(period = breakNames[as.numeric(period)],
           scenario = scenProg)

fireProj <- fireProj[,c("period", "fireCycle")]



fireRegime <- expand.grid(ID = fireRegime$ID,
                          period = fireProj$period,
                          scenario = scenProg) %>%
    merge(fireRegime[,c("ID", "Zone_LN")]) %>%
    merge(fireProj) %>%
    rbind(fireRegime) %>%
    arrange(scenario)
fireRegime<- fireRegime[, c("ID", "Zone_LN", "scenario", "period", "fireCycle")]


write.csv(fireRegime, file = "fireRegime.csv", row.names = F) 
##############
stored <- append(stored, c("fireSizeFit", "fireSizeMax", "fireSizeDist", "fireSizeMean", "fireRegime", "fireNCorrFactor"))
rm(list = ls()[!ls() %in% stored])
source("../scripts/simFireFnc.R")



################################################################################
################################################################################
### figure
# require(ggplot2)
# breaks <- c(250, 500, 1000, 2500, 5000, 10000, 25000, 100000, 500000)
# minorBreaks <- breaks-(diff(c(0,breaks))/2)
# p <- ggplot(df, aes(x = areaTotal_ha)) +
#     geom_histogram() +
#     scale_x_log10(breaks = breaks,
#                    minor_breaks = minorBreaks) +
#     geom_histogram(color="black",
#                    fill="white") +
#     geom_vline(xintercept = mean(df$areaTotal_ha),
#            linetype = 3, color = "indianred", size = 1) +
#     geom_text(aes(label = paste("Mean fire size =", round(meanFireSize), "ha")
#                   , y = 115, x = meanFireSize),
#               colour = "indianred",
#               hjust = 1 , vjust = -0.5, angle = 90, size = 2) +
#     geom_text(aes(label = paste("N =", nrow(df)),
#                   y = 115, x = max(df$areaTotal_ha)),
#               colour = "black",
#               hjust = 1 , vjust = 0, size = 3) +
#     theme_bw() +
#     labs(title = "Fire size distribution",
#          subtitle = paste0("Observations > ", thresh, " ha (", paste(range(df$year), collapse = "-"), ")"),
#          y = "Freq.",
#          x = "Area (ha)", size = 0.5)
# 
# png(filename = paste0("fireSizeDistr.png"), width = 8, height = 6, units = "in", res = 300, pointsize = 8)
#     options(scipen=999)
#     print(p + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
#     options(scipen=0)
# dev.off()



