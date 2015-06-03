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


require(dplyr)
require(reshape)
require(data.table)

# The DoPlot2 function extracts the DateTime and Global_active_power 
# components from the tidy data table, creates png graphics device 
# and outputs the plot(DateTime, Global_active_power) to it. 


DoPlot2 <-function(){
    srcData <- GetCachedData()
    png(file="plot2.png")
    yLabel <-"Global Active Power (kilowatts)"
    with(srcData, plot(DateTime, Global_active_power, type="l", 
                       font= 2, font.lab = 2, ylab = yLabel))
    dev.off()
    
}


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
# 
