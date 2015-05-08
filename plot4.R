
DoPlot4 <-function(){
    srcData <- GetCachedData()
    png(file="plot4.png")
    par(mfrow = c(2,2))
    par(cex = 0.6)
    with(srcData, {
        plot(DateTime, Global_active_power, type="l", font= 2, font.lab = 2,
             ylab = "Global Active Power")
        plot(DateTime, Voltage, type="l", font= 2, font.lab = 2, 
             xlab = "datetime")
        CreateMeteredDataPlot(srcData, boxType =  "n")
        plot(DateTime, Global_reactive_power, type="l", font= 2,
             font.lab = 2, xlab = "datetime")
    })
    dev.off()
}