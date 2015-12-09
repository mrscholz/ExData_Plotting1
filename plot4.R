# Script to create a 4 panel plot from the household power consumption dataset to 
# monitor different variables over time.
# household_power_consumption data downloaded from 
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# Needs data.table and lubridate packages installed on the host system

plot4 <- function(path="./"){# optional: provide the path to folder if data not in wd
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
    png(filename="./plot4.png",width=480,height=480)
    # tell the device to create 4 panels in column order
    par(mfcol=c(2,2))
    
    # add plot2 to the first, top-left panel
    with(d_sub,plot(t_elapse,Global_active_power,type="l",axes=FALSE,xlab="",ylab="Global Active Power (kW)"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0,2,4,6))
    box()
    
    # add plot3 to the second, bottom-left panel
    plot(d_sub$t_elapse,sd$Sub_metering_1,type="n",axes=FALSE,xlab="",ylab="Energy sub metering")
    with(d_sub, lines(t_elapse,Sub_metering_1,col="black"))
    with(d_sub, lines(t_elapse,Sub_metering_2,col="red"))
    with(d_sub, lines(t_elapse,Sub_metering_3,col="blue"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0,10,20,30))
    legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub metering 1","Sub metering 3","Sub metering 3"))
    box()
    
    # add a xy-plot of Voltage vs. t_elapse to the third, top-right panel
    # see plot2.R for detailed comments
    with(d_sub,plot(t_elapse,Voltage,type="l",axes=FALSE,xlab="datetime",ylab="Voltage"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(234,238,242,246))
    box()
    
    # add a xy-plot of Global_reactive_power vs. t_elapse to the fourth, bottom-right panel
    # see plot2.R for detailed comments
    with(d_sub,plot(t_elapse,Global_reactive_power,type="l",axes=FALSE,xlab="datetime",ylab="Global reactive Power"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0.0,0.1,0.2,0.3,0.4,0.5))
    box()
    
    dev.off() # close png device
    par(mfrow=c(1,1)) # reset number of panels to default
}