require(ggplot2)
require(tidyverse)
require(stringr)
setwd("D:/risqueAccidentRegen_phase3/2020-11-20/")
x <- list.files()
output <- list()
for (i in seq_along(x)) {
    output[[i]] <- foo <-  read.csv(x[i])
}



output <- do.call("rbind", output)

output$simID <- str_pad(output$simID, 2, side = "left", pad = "0")


df <- output %>%
    filter(simID %in% c("04", "05")) %>% ##c("01", "02",  "04", "05")
           #coverType == "EN") %>%
    group_by(simID, fireScenario, mgmtScenario, year, coverType) %>%
    summarise(regenFailureMean = mean(regenFailure30, na.rm = T),
              p25 = quantile(regenFailure30, 0.25, na.rm = T),
              p50 = quantile(regenFailure30, 0.50, na.rm = T),
              p75 = quantile(regenFailure30, 0.75, na.rm = T))




ggplot(df, aes(x = year, group = paste(simID, coverType),
               colour = simID,
               fill = coverType)) +
    
    geom_ribbon(aes(ymin = p25, ymax = p75),
                colour = NA,
                alpha = 0.5) +
    geom_line(aes(y = regenFailureMean)) +
    facet_wrap(~coverType, scales = "free_y")
    #ylim(c(0, 0.35)) 

simInfo




df <- output %>%
    #filter(simID %in% c("04", "05")) %>% ##c("01", "02",  "04", "05")
    #coverType == "EN") %>%
    group_by(simID, fireScenario, mgmtScenario, year)#, coverType) %>%
summarise(regenFailureMean = mean(regenFailure30, na.rm = T),
          p25 = quantile(regenFailure30, 0.25, na.rm = T),
          p50 = quantile(regenFailure30, 0.50, na.rm = T),
          p75 = quantile(regenFailure30, 0.75, na.rm = T))
