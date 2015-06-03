# Acknowledgement to http://stackoverflow.com/questions/19053440/r-legend-with-points-and-lines-being-different-colors-for-the-same-legend-item

# The heavy lifting for doing this plot is done by GetCachedData, 
# appended below, which either computes the tidy data on the fly or 
# returns a cached value.


require(dplyr)
require(reshape)
require(data.table)


# The DoPlot1 function extracts the Global_active_power component
# from the tidy data table, creates png graphics device and outputs
# the histogram to it. 

DoPlot1 <- function(){
    srcData <- GetCachedData() #Appended below
    globActPow <- srcData$Global_active_power
    buckets <- 0:15 * 0.5
    title <- "Global Active Power"
    xLabel <- paste(title, "(kilowatts)")
    png(file="plot1.png")
    hist(globActPow, buckets, freq=TRUE, font = 2, font.lab = 2, 
         xlab = xLabel, col = 'red', main = title)
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
