

plot1 <- function(filename = "household_power_consumption.txt") 
{
    png(filename="plot1.png")
    mydf <- readData()
    makePlot(mydf)
    dev.off()
}

makePlot <- function(mydf = data.frame())
{
    hist(mydf$Global_active_power, freq=TRUE, col="red", ylab="Frequency", xlab="Global Active Power (kilowatts)", main="Global Active Power", ylim=c(0,1200))
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