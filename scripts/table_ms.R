
read


outputCls %>%
    mutate(volAt120Cls = factor(ifelse(is.na(volAt120Cls), "[0,30)", as.character(volAt120Cls)), levels = levels(outputCls$volAt120Cls))) %>%
    
    
    ### ms table 2
    outputClsinit <- outputCls %>%
        filter(year == 0) %>%
        mutate(volAt120Cls = factor(ifelse(is.na(volAt120Cls), "[0,30)", as.character(volAt120Cls)), levels = levels(outputCls$volAt120Cls))) %>%
        group_by(simID, id_ms, replicate, year, volAt120Cls) %>%
        summarize(area_ha = sum(area_ha)) %>%
        group_by(simID, id_ms, volAt120Cls) %>%
        summarize(areaMeanInit_ha = mean(area_ha))
    
    outputClsFinal <- outputCls %>%
        filter(year == 150) %>%
        mutate(volAt120Cls = factor(ifelse(is.na(volAt120Cls), "[0,30)", as.character(volAt120Cls)), levels = levels(outputCls$volAt120Cls))) %>%
        group_by(simID, id_ms, replicate, year, volAt120Cls) %>%
        summarize(area_ha = sum(area_ha)) %>%
        group_by(simID, id_ms, volAt120Cls) %>%
        summarize(areaMeanFinal_ha = mean(area_ha))
    
    outputClsSummaryInit <- merge(outputClsinit, outputClsFinal, by = c("simID", "id_ms", "volAt120Cls")) %>%
        mutate(areaMeanInit_prop = 100*areaMeanInit_ha/areaTotal) %>%
        select(id_ms, volAt120Cls, areaMeanInit_prop) %>%
        spread(volAt120Cls, areaMeanInit_prop)
    
    outputClsSummaryFinal <- merge(outputClsinit, outputClsFinal, by = c("simID", "id_ms", "volAt120Cls")) %>%
        mutate(areaMeanFinal_prop = 100*areaMeanFinal_ha/areaTotal) %>%
        select(id_ms, volAt120Cls, areaMeanFinal_prop) %>%
        spread(volAt120Cls, areaMeanFinal_prop)
    
    
    outputClsSummaryFinal <- cbind(id_ms = outputClsSummaryFinal[,1],
                                   round(outputClsSummaryFinal[,-1], 1))
    outputClsSummaryChange <- round(100*(outputClsSummaryFinal[,-1]/outputClsSummaryInit[,-1]), 0)
    
    write.csv(cbind(outputClsSummaryFinal, outputClsSummaryChange), file = "table2.csv", row.names = F)
    
    