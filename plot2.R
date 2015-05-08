DoPlot2 <-function(){
    srcData <- GetCachedData()
    png(file="plot2.png")
    yLabel <-"Global Active Power (kilowatts)"
    with(srcData, plot(DateTime, Global_active_power, type="l", 
                       font= 2, font.lab = 2, ylab = yLabel))
    dev.off()
    
}
