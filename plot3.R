# Script to compare the 3 submeterings of the household power consumption dataset
# in a single xy-plot vs. time. 
# household_power_consumption data downloaded from 
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# Needs data.table and lubridate packages installed on the host system

plot3 <- function(path="./"){ # optional: provide the path to folder if data not in wd
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
    
    # create plot3 on png device 
    png(filename="./plot3.png",width=480,height=480)
        
        # create empty plot 
        plot(d_sub$t_elapse,sd$Sub_metering_1,type="n",axes=FALSE,xlab="",ylab="Energy sub metering")
        # add the three Sub_metring variables as lines in different color
        with(d_sub, lines(t_elapse,Sub_metering_1,col="black")) 
        with(d_sub, lines(t_elapse,Sub_metering_2,col="red"))
        with(d_sub, lines(t_elapse,Sub_metering_3,col="blue"))
        axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat")) # add x-axis with day labeling
        axis(2,c(0,10,20,30)) # add y-axis
        # add a legend to the graph
        legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub metering 1","Sub metering 3","Sub metering 3"))
        box() # plot surrounding
    
    dev.off() # close graphic device 
}