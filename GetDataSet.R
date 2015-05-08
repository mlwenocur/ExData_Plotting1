require(dplyr)
require(reshape)
require(data.table)

cmpDate <- function(date){
    convertedDate = as.Date(date, format = "%d/%m/%Y") 
    as.character(convertedDate) %in% c("2007-02-01", "2007-02-02")
}

getData <- function() {
    srcFile <- "household_power_consumption.txt"
    allTheData <- read.table(srcFile, header=TRUE, sep=";",stringsAsFactors = FALSE)
    desiredData <- filter(allTheData, cmpDate(allTheData$Date)) %>%
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
        mutate(Voltage = as.numeric(Voltage)) %>%
        mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
        mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
        mutate(Sub_metering_3 = as.numeric(Sub_metering_3))
    rm(allTheData)
    DateTime <- with(desiredData, strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
    desiredData$DateTime <- DateTime
    return(desiredData)
}
MakeTableContainerFunction <- function(){
    dataSet <- NULL
    function()
    {
        if (is.null(dataSet)){
            dataSet <<- getData()
            return(dataSet)
        }
        else {
            return(dataSet)
        }
    }
}
GetCachedData <- MakeTableContainerFunction()

MeltMeterData <- function(srcData){
    meterNamesAndTime <- c("Sub_metering_1", "Sub_metering_2", 
                           "Sub_metering_3", "DateTime")
    meterDateNames <- c('meter1', 'meter2', 'meter3', 'dt')
    renamedData <- data.frame(srcData)
    setnames(renamedData, meterNamesAndTime, meterDateNames)
    meterAndDateTime <- renamedData[meterDateNames]
    meltedData = melt(meterAndDateTime, id = "dt")
}

# With thanks to http://stackoverflow.com/questions/19053440/r-legend-with-points-and-lines-being-different-colors-for-the-same-legend-item
CreateMeteredDataPlot <- function(srcData, boxType = "l") {
    meltedMeterWithDateTime <- MeltMeterData(srcData)
    meterNames = c("Sub_metering_1", "Sub_metering_2", 
                   "Sub_metering_3")
    with (meltedMeterWithDateTime, {
    plot(dt, value, type='n', font = 2, font.lab = 2, ylab = 'Energy sub metering', xlab = '')
    points(dt[variable == 'meter1'], value[variable == 'meter1'], type = 'l' )
    points(dt[variable == 'meter2'], value[variable == 'meter2'], col = 'red', type = 'l' )
    points(dt[variable == 'meter3'], value[variable == 'meter3'], col = 'blue', type = 'l' )
    par(font=2)
    legend('topright', bty = boxType, pch =NA, lwd=1, lty=c(1), col = c('black', 'red', 'blue'), legend = meterNames)
    
    })
    
}





