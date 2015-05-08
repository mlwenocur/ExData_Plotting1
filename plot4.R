# The heavy lifting for doing this plot is done when calling 
# GetCachedData and CreateMeteredDataPlot function defined in 
# GetDataSet.R functions.

# GetCachedData computes the tidy data on the fly or returns a cached 
# value, and CreateMeteredDataPlot creates the comparison subplot of 
# three metered electrical demand graphs as a function of time.

# The first subplot is a mere recapitulation of that defined in
# plot1.R, whereas the second and fourth subplots are closely analogous
# to the first.

# Determining how to bold and decrease the font was a bit of a challenge.

DoPlot4 <-function(){
    srcData <- GetCachedData()
    png(file="plot4.png")
    par(mfrow = c(2,2), font = 2, font.lab = 2, cex = 0.8)
    with(srcData, {
        plot(DateTime, Global_active_power, type="l", 
             ylab = "Global Active Power")
        plot(DateTime, Voltage, type="l", 
             xlab = "datetime")
        CreateMeteredDataPlot(srcData, boxType =  "n")
        plot(DateTime, Global_reactive_power, type="l",
             xlab = "datetime")
    })
    dev.off()
}