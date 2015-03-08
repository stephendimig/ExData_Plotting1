#################################################################
##
## File:   plot3.R
## Author: Stephen Dimig
## Description: This assignment uses data from the UC Irvine Machine Learning 
## Repository, a popular repository for machine learning datasets. In 
## particular, we will be using the “Individual household electric power 
## consumption Data Set” which includes measurements of electric power consumption
## in one household with a one-minute sampling rate over a period of almost 4 
## years. Different electrical quantities and some sub-metering values are 
## available.
##
## This file contains 3 methods.
##
## 1. plot3() - Is a coordinator function that actually generates the plot
##    and stores it in a PNG file called plot3.png.
## 2. makePlot() - Actually makes the calls to the base plotting routines.
## 3. readData() - Reads and subsets the data from the “Individual household 
##    electric power consumption Data Set” file.
##
##
#################################################################

#################################################################
##
## Description:
## This method uses the other functions defined within the file to actually
## generate the plot and store it in a PNG file.
##
## Parameters:
## filename [in] is a string value that is the name of the data file to be used.
## The default value is "household_power_consumption.txt".
##
## Return:
##
#################################################################
plot3 <- function(filename = "household_power_consumption.txt") 
{
    png(filename="plot3.png")
    mydf <- readData()
    makePlot(mydf)
    dev.off()
}

#################################################################
##
## Description:
## This method makes the actual line plot for Plot 3 in the 
## assignment.
##
## Parameters:
## mydf [in] A dataframe that can be overloaded with your own value.
## The default is an empty dataframe.
##
## Return:
## None.
##
#################################################################
makePlot <- function(mydf = data.frame())
{
    # Set up base plot
    plot(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                    format="%d/%m/%Y %H:%M:%S"), 
         mydf$Sub_metering_1,
         type='l',
         ylab="Energy sub metering",
         xlab="")
    
    # Add line graph for Sub_metering_2 in red.
    lines(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                     format="%d/%m/%Y %H:%M:%S"), 
          mydf$Sub_metering_2,
          type='l',
          col="red")
    
    # Add line graph for Sub_metering_2 in blue.
    lines(as.POSIXlt(paste(mydf$Date, mydf$Time), 
                     format="%d/%m/%Y %H:%M:%S"), 
          mydf$Sub_metering_3,
          type='l',
          col="blue")
    
    # Add legend
    legend("topright",
           c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
           lty=1,
           col=c("black", "red","blue"),
           bty='o', 
           cex=.75)    
}

#################################################################
##
## Description: 
## This method reads the data from the  “Individual household 
## electric power consumption Data Set”. The dataset has 2,075,259 rows 
## and 9 columns. We will only be using data from the dates 2007-02-01 and 
## 2007-02-02. Note that in this dataset missing values are coded as "?".
##
## Parameters:
## filename [in] is the name of the file to read the data from. The 
## default value is "household_power_consumption.txt".
##
## Return:
## This method returns a data.frame object that includes only the 
## subset of the data we are interested in. All "?' values have been replaced 
## with NA.
##
#################################################################
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