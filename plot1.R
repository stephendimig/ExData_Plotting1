#################################################################
##
## File:   plot1.R
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
## 1. plot1() - Is a coordinator function that actually generates the plot
##    and stores it in a PNG file called plot1.png.
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
plot1 <- function(filename = "household_power_consumption.txt") 
{
    png(filename="plot1.png")
    mydf <- readData()
    makePlot(mydf)
    dev.off()
}

#################################################################
##
## Description:
## This method makes the actual histogram plot for Plot 1 in the 
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
    hist(mydf$Global_active_power, freq=TRUE, col="red", ylab="Frequency", xlab="Global Active Power (kilowatts)", main="Global Active Power", ylim=c(0,1200))
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
    # Start and end dates to filter on.
    startDate <- "2/1/2007"
    endDate <- "2/2/2007"
    
    # Dataframe to return.
    mydf <- data.frame()
    
    # Check to see if file exists.
    if(TRUE == file.exists(filename))
    {
        # Read the data into a temporary data frame.
        df <- read.table(filename, 
                         header=TRUE, 
                         sep=";", 
                         na.strings="?", 
                         colClasses=c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
        
        # Subset data to use only data we are interested in.
        beg <- as.Date(startDate, format="%m/%d/%Y")
        end <- as.Date(endDate, format="%m/%d/%Y")
        mydf <- subset(df, as.Date(Date, format = "%d/%m/%Y") >= beg & 
                           as.Date(Date, format = "%d/%m/%Y") <= end)
        
        # discard unused data
        df <- NA
    }
    mydf
}