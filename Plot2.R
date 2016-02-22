## It produces the Plot2.png file for Course Project assignment 1
## Plot2 is the Global Active Power in kilowatt from Thu 01/02/2007 and Sat 03/02/2007 (excluded)

Plot2 <- function()
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
        png(file="Plot2.png", height=480, width=480)
        
        ## plot Global Active Power vs date and time
        plot(data$Global_active_power ~ data$Datetime, type="l",
             ylab="Global Active Power (kilowatts)", xlab="")
        
        # Close the print device before exit, otherwise you won't be able to open the png file
        dev.off()
}
