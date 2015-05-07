require(dplyr)
cmpDate <- function(date){
    convertedDate = as.Date(date, format = "%d/%m/%Y") 
    as.character(convertedDate) %in% c("2007-02-01", "2007-02-02")
}

getData <- function() {
    srcFile <- "household_power_consumption.txt"
    allTheData <- read.table(srcFile, header=TRUE, sep=";",stringsAsFactors = FALSE)
    desiredData <- filter(allTheData, cmpDate(allTheData$Date)) %>%
        mutate(Global_active_power = as.numeric(Global_active_power)) %>%
        #mutate(DateTime = strptime(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
        mutate(Date = as.Date(Date, format = "%d/%m/%Y"))
        rm(allTheData)
    return(desiredData)
}

##DRAFT for first graph
DoPlot1 <- function(srcData){
    globActPow <- srcData$Global_active_power
    buckets <- 0:15 * 0.5
    title <- "Global Active Power"
    xLabel <- paste(title, "(kilowatts)")
    png(file="plot1.png")
    hist(globActPow, buckets, freq=TRUE, xlab = xLabel, col = 'red', main = title)
    dev.off()
}