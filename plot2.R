# Script to generate a xy-plot of the Global Active Power vs. time. 
# household_power_consumption data downloaded from 
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# Needs data.table and lubridate packages installed on the host system

plot2 <- function(path="./"){ # optional: provide the path to folder if data not in wd
    setwd(path) # set working directory
    
    # load packages
    library(data.table) # access to fread() for fast file reading
    library(lubridate) # load lubridate package
    
    # file read and necessary transformations / subsetting
    d_temp <- fread("household_power_consumption.txt", na.strings = "?") # translate "?" TO NA
    d_temp$datetime <- dmy_hms(paste(d_temp$Date,d_temp$Time)) # create new datetime variable from Date and Time
    t_span <- new_interval(ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-03 00:00:00")) # time interval for subsetting
    d_sub <- subset(d_temp,d_temp$datetime %within% t_span) # create a subset of the days of interest
    rm(d_temp) # remove full dataset from memory
    
    d_sub$t_elapse <- as.numeric(abs(as.duration(d_sub$datetime[[1]]-d_sub$datetime))) # calculate elapsed time as numeric 
    
    # create plot2 on png device 
    png(filename="./plot2.png",width=480,height=480) # open device
    
    # create line plot without axes and x-label but with y-label
    with(d_sub,plot(t_elapse,Global_active_power,type="l",axes=FALSE,xlab="",ylab="Global Active Power (kW)"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat")) # add x-axis with day labeling
    axis(2,c(0,2,4,6)) # add y-axis
    box() # suround plot with a box
    dev.off() # close graphics device
}