# The heavy lifting for doing this plot is done when calling 
# GetCachedData and CreateMeteredDataPlot which are appended below.
# GetCachedData computes the tidy data on the fly or returns a cached 
# value, and CreateMeteredDataPlot creates comparison plot of three
# metered electrical demand graphs as a function of time.


require(dplyr)
require(reshape)
require(data.table)

DoPlot3 <- function(){
    srcData <- GetCachedData()
    png(file="plot3.png")
    CreateMeteredDataPlot(srcData)
    dev.off()    
}

# Overview of Technical Aspects
# R's function closure capability is used to avoid creating the tidied
# subset. Also dplyr has been used where possible to improve the code
# clarity. In some instances it wasn't possible to use dplyr's methods
# because dplyr doesn't support POSIXlt values.

# One technical deficiency is that the entire database of household power
# data is read in which is slow, generating a high temporary memory
# demand. One alternative, using R's read a line at a time capability, 
# would be to create a temporary csv file consisting of the header row 
# and only those readings transcribed on January 1 and 2 of 2007.


# Boolean function used to select data collected on the required dates
CmpDate <- function(date){
    convertedDate = as.Date(date, format = "%d/%m/%Y") 
    as.character(convertedDate) %in% c("2007-02-01", "2007-02-02")
}

# Creates the tidied subset by first creating a table of all the
# household power data and then filtering it down to the tidied subset.
# Observe:
#   1. Numerical data are converted to numerical values
#   2. A DateTime column is created from the original Date and Time columns.
#   3. Date and Time have been dropped.

GetData <- function() {
    srcFile <- "household_power_consumption.txt"
    allTheData <- read.table(srcFile, header=TRUE, sep=";",stringsAsFactors = FALSE)
    desiredData <- filter(allTheData, CmpDate(allTheData$Date)) %>%
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>%
        mutate(Voltage = as.numeric(Voltage)) %>%
        mutate(Sub_metering_1 = as.numeric(Sub_metering_1)) %>%
        mutate(Sub_metering_2 = as.numeric(Sub_metering_2)) %>%
        mutate(Sub_metering_3 = as.numeric(Sub_metering_3))
    rm(allTheData)
    DateTime <- with(desiredData, strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
    desiredData$DateTime <- DateTime
    desiredData$Date <- NULL
    desiredData$Time <- NULL
    return(desiredData)
}

# Creates a function closure for caching tidied data.
MakeTableContainerFunction <- function(){
    dataSet <- NULL
    function()
    {
        if (is.null(dataSet)){
            dataSet <<- GetData()
            return(dataSet)
        }
        else {
            return(dataSet)
        }
    }
}
# Function closure that returns the cached tidied data set or
# creates, stores and returns a freshly created tidied data set.
GetCachedData <- MakeTableContainerFunction()

# Helper function that creates a new auxiliary table consisting
# of the melted meter data and the DateTime values. For ease of
# reference the melted meter data is tagged meter1, meter2, meter3 and
# the DateTime column is renamed to dt.

# This melted data is needed to generate the multi curve plot appearing
# in plots 3 and 4.
MeltMeterData <- function(srcData){
    meterNamesAndTime <- c("Sub_metering_1", "Sub_metering_2", 
                           "Sub_metering_3", "DateTime")
    meterDateNames <- c('meter1', 'meter2', 'meter3', 'dt')
    renamedData <- data.frame(srcData)
    setnames(renamedData, meterNamesAndTime, meterDateNames)
    meterAndDateTimeTable <- renamedData[meterDateNames]
    meltedData = melt(meterAndDateTimeTable, id = "dt")
}


# Creates the multi-curve plot used in examples 3 and 4. It leverages
# melted meter data and the points method to do the multi-curve plotting.
# Most of the fussing had to do with text and legend layout issues.
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




