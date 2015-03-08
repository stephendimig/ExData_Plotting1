plot2 <- function(filename = "household_power_consumption.txt") 
{
    png(filename="plot2.png")
    mydf <- readData()
    makePlot(mydf)
    dev.off()
}

makePlot <- function(mydf = data.frame())
{
    plot(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                   format="%d/%m/%Y %H:%M:%S"), 
         mydf$Global_active_power,
         type='l',
         ylab="Global Active Power (kilowatts)",
         xlab="")
}
readData <- function(filename = "household_power_consumption.txt") 
{
    startDate <- "2/1/2007"
    endDate <- "2/2/2007"
    mydf <- data.frame()
    if(TRUE == file.exists(filename))
    {
        df <- read.table(filename, 
                         header=TRUE, 
                         sep=";", 
                         na.strings="?", 
                         colClasses=c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
        beg <- as.Date(startDate, format="%m/%d/%Y")
        end <- as.Date(endDate, format="%m/%d/%Y")
        mydf <- subset(df, as.Date(Date, format = "%d/%m/%Y") >= beg & 
                           as.Date(Date, format = "%d/%m/%Y") <= end)
        df <- NA
    }
    mydf
}