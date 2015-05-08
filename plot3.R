DoPlot3 <- function(){
    srcData <- GetCachedData()
    png(file="plot3.png")
    par(mfrow = c(1,1))
    CreateMeteredDataPlot(srcData)
    dev.off()    
}