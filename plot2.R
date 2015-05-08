# The heavy lifting for doing this plot is done when calling 
# auxiliary function GetCachedData, a GetDataSet.R function, which either
# computes the tidy data on the fly or returns a cached value.
# 
# The DoPlot2 function extracts the DateTime and Global_active_power 
# components from the tidy data table, creates png graphics device 
# and outputs the plot(DateTime, Global_active_power) to it. 

# Determining how to bold the font took a while.

DoPlot2 <-function(){
    srcData <- GetCachedData()
    png(file="plot2.png")
    yLabel <-"Global Active Power (kilowatts)"
    with(srcData, plot(DateTime, Global_active_power, type="l", 
                       font= 2, font.lab = 2, ylab = yLabel))
    dev.off()
    
}
