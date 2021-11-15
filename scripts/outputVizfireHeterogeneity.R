#fire[is.na(studyArea)] <- NA

fireZone <- raster(paste0("../", simDir, "/fireZones.tif"))
fireZone[!is.na(fireZone)] <- 1
fireBreaks <- fireZone
fireBreaks <- is.na(fireBreaks)
fireBreaks[fireBreaks==0] <- NA

studyAreaP <- get(load(paste0("../", simDir, "/studyAreaP.RData")))


#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

output <- foreach(i = seq_along(plantOutputs)) %dopar% { 
    require(raster)
                              
    fire <- get(load(paste(outputFolder, fireOutputs[i], sep="/")))
    fireMean <- fire
    fireMean[is.na(fire)] <- 0
    fireMean <- mean(fireMean)
    
    plant <- get(load(paste(outputFolder, plantOutputs[i], sep="/")))
    plantPs <- sum(plant$postSalv, na.rm = T)
    plantPf <- sum(plant$postFire, na.rm = T)
    plant <- plantPs+plantPf
    
    return(list(fireMean, plant))
}
stopCluster(cl)


fMean <-  stack(lapply(output, function(x) x[[1]]))
pMean <- stack(lapply(output, function(x) x[[2]]))


f <- mean(fMean)
p <- sum(pMean)/nlayers(pMean)


plot(p)
plot(f)


pVals <-values(p)

x <- values(f)[pVals>0]
y <- pVals[pVals>0]


1/quantile(x, 0.05)

cor(x, y)

plot(x, y, pch = 16, cex = 0.25)
abline(lm(y ~ x), col = "red")
abline(h = mean(y), col = "blue", lt = 3)
abline(v = mean(x), col = "blue", lt = 3)
plot(x, y)

xPred <- seq(range(x), length.out = 100)
plot(x = range(x), y = predict(foo, range(x)) 
abline(b = foo$coefficients)



fMean[is.na(fireZone)] <- NA





get(load())



plot(f>(1/75), main = "Cycle < 75 ans")
plot(fireBreaks,  col = "blue", add = T)
plot(studyAreaP, add = T, border = "red")

plot(f<(1/150), main = "Cycle > 150 ans")
plot(fireBreaks,  col = "blue", add = T)
plot(studyAreaP, add = T, border = "red")


plot(f>(1/120) & f<(1/80), main = "80 > Cycle > 120")
plot(fireBreaks,  col = "blue", add = T)
plot(studyAreaP, add = T, border = "red")

pexp(90, 1/101, lower.tail = F)

fireZone <- raster(paste0("../", simDir, "/fireZones.tif"))
fireZone[!is.na(fireZone)] <- 1
fireBreaks <- fireZone
fireBreaks <- is.na(fireBreaks)
fireBreaks[fireBreaks==0] <- NA
plot(fireBreaks)

size <- sum(values(studyArea), na.rm = T)

firemean <- fire

firemean[is.na(fire)] <- 0
#firemean[is.na(studyArea)] <- NA

plot(firemean[[6]])
foo <- mean(firemean)

plot(foo)
plot(fireBreaks,  col = "blue", add = T)

plot(foo, na.col = NA)
nFire <- apply(fireVals, 2, sum, na.rm = T)


1/mean(nFire/size)
