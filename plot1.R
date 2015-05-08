DoPlot1 <- function(){
    srcData <- GetCachedData()
    globActPow <- srcData$Global_active_power
    buckets <- 0:15 * 0.5
    title <- "Global Active Power"
    xLabel <- paste(title, "(kilowatts)")
    png(file="plot1.png")
    hist(globActPow, buckets, freq=TRUE, font = 2, font.lab = 2, 
         xlab = xLabel, col = 'red', main = title)
    dev.off()
}