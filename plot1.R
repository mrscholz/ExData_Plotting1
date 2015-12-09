# Script to generate a simple histogramm of the Global Active Power metering of 
# the household_power_consumption data downloaded from 
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# Needs data.table and lubridate packages installed on the host system

plot1 <- function(path="./"){ # optional: provide the path to folder if data not in wd
    setwd(path) # set working directory
    
    # load packages
    library(data.table) # access to fread() for fast file reading
    library(lubridate)  # load lubridate package
    
    # file read and necessary transformations / subsetting
    d_temp <- fread("household_power_consumption.txt", na.strings = "?") # translate "?" TO NA
    d_temp$datetime <- dmy_hms(paste(d_temp$Date,d_temp$Time))  # create new datetime variable from Date and Time
    t_span <- new_interval(ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-03 00:00:00")) # time interval for subsetting
    d_sub <- subset(d_temp,d_temp$datetime %within% t_span) # create a subset of the days of interest
    rm(d_temp)  # remove full dataset from memory
    
    # create plot1 on png device 
    png(filename="./plot1.png",width=480,height=480) # open png graphics device
    # create histogramm with red bars, x-label and headline
    hist(d_sub$Global_active_power,xlab = "Global Active Power (kW)",col="red",main="Global Active Power") 
    dev.off() # close png device 
}