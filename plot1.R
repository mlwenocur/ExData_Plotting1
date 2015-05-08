# The heavy lifting for doing this plot is done when calling 
# auxiliary function GetCachedData, a GetDataSet.R function, which either
# computes the tidy data on the fly or returns a cached value.
# 
# The DoPlot1 function extracts the Global_active_power component
# from the tidy data table, creates png graphics device and outputs
# the histogram to it. 
#
# Determining how to bold the font took a while.

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