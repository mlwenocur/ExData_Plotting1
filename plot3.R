# The heavy lifting for doing this plot is done when calling 
# GetCachedData and CreateMeteredDataPlot which are GetDataSet.R functions.
# GetCachedData computes the tidy data on the fly or returns a cached 
# value, and CreateMeteredDataPlot creates comparison plot of three
# metered electrical demand graphs as a function of time.


DoPlot3 <- function(){
    srcData <- GetCachedData()
    png(file="plot3.png")
    CreateMeteredDataPlot(srcData)
    dev.off()    
}