plot4 <- function(filename = "household_power_consumption.txt") 
{
    png(filename="plot4.png")
    mydf <- readData()
    op <- par(mfcol=c(2,2))
    makePlot1(mydf)
    makePlot2(mydf)
    makePlot3(mydf)
    makePlot4(mydf)
    dev.off()
    par(op)
}

makePlot1 <- function(mydf = data.frame())
{
    plot(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                    format="%d/%m/%Y %H:%M:%S"), 
         mydf$Global_active_power,
         type='l',
         ylab="Global Active Power",
         xlab="")
}

makePlot2 <- function(mydf = data.frame())
{
    plot(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                    format="%d/%m/%Y %H:%M:%S"), 
         mydf$Sub_metering_1,
         type='l',
         ylab="Energy sub metering",
         xlab="")
    lines(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                     format="%d/%m/%Y %H:%M:%S"), 
          mydf$Sub_metering_2,
          type='l',
          col="red")
    lines(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                     format="%d/%m/%Y %H:%M:%S"), 
          mydf$Sub_metering_3,
          type='l',
          col="blue")
    legend("topright",
           c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
           lty=1,
           col=c("black", "red","blue"),
           bty='o', 
           cex=.75)    
}

makePlot3 <- function(mydf = data.frame())
{
    plot(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                    format="%d/%m/%Y %H:%M:%S"), 
         mydf$Voltage,
         type='l',
         ylab="Voltage",
         xlab="datetime")
}

makePlot4 <- function(mydf = data.frame())
{
    plot(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                    format="%d/%m/%Y %H:%M:%S"), 
         mydf$Global_reactive_power,
         type='l',
         ylab="Global_reactive_power",
         xlab="datetime")
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