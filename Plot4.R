## It produces the Plot4.png file for Course Project assignment 1
## Plot4 is a combination of four different plots

Plot4 <- function()
{
        # create a directory named "data" where to place the downloaded data
        data <- "data"
        if(!file.exists(data)){
                dir.create(data)
        } 
        
        url_data <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        
        ## check if the data set has been downloaded, otherwise download the data into 
        ## folder "data"
        archive <- paste(getwd(), "/data/power_consumption.zip", sep = "")
        if(!file.exists(archive)){
                download.file(url_data, archive, mode="wb")
        }
        
        ## unzip the row data file and extract the txt dataset
        power_consumption_file <- paste(getwd(), "/data/power_consumption.rds", sep = "")
        if(!file.exists(power_consumption_file)){
                unzip(archive, list = FALSE, overwrite = FALSE, exdir = data)
        }
        
        ## load data into R
        data <- read.table("./data/household_power_consumption.txt", header =TRUE, sep = ";", 
                           colClasses=c("character", "character", rep("numeric",7)), na.strings = "?")
        
        ## convert Date and Time variables into Date and Time Classes
        data$Date <- as.Date(as.character(data$Date), "%d/%m/%Y")
        
        # subset the dataset to the time period assigned (days between 1/02/2007 and 2/2/2007)
        selectedPeriod <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
        data <- subset(data, Date %in% selectedPeriod)
        
        ## convert date and time
        datetime <- paste(as.Date(data$Date), data$Time)
        data$Datetime <- as.POSIXct(datetime)
        
        ## prepare the output file Plot1.png and define the area
        png(file="Plot4.png", height=480, width=480)
        
        ## use the par function to set up the area and how we want the plot to be distributed 2 by 2
        par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
        
        ## then create the different plots using function with to avoid repeting the data table name 
        ## multiple times
        
        with(data, {
                plot(Global_active_power ~ Datetime, type = "l", 
                     ylab ="Global Active Power", xlab = "")
                plot(Voltage ~ Datetime, type = "l", 
                     ylab = "Voltage", xlab = "datetime")
                plot(Sub_metering_1 ~ Datetime, type = "l", 
                     ylab = "Energy Sub Metering", xlab = "")
                lines(Sub_metering_2 ~ Datetime, col = 'Red')
                lines(Sub_metering_3 ~ Datetime, col = 'Blue')
                legend("topright", col=c("black", "red", "blue"), lty=c(1,1,1), 
                       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
                plot(Global_reactive_power ~ Datetime, type = "l", 
                     ylab = "Global Reactive Power", xlab = "datetime", lwd=.001)
        })
        
        # Close the print device before exit, otherwise you won't be able to open the png file
        dev.off()
}