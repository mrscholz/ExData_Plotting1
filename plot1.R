plot1 <- function(path="./"){
    setwd(path)
    library(data.table)
    library(lubridate)
    d_temp <- fread("household_power_consumption.txt", na.strings = "?")
    d_temp$datetime <- dmy_hms(paste(d_temp$Date,d_temp$Time))
    t_span <- new_interval(ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-03 00:00:00")) 
    d_sub <- subset(d_temp,d_temp$datetime %within% t_span)
    rm(d_temp)
    
    png(filename="./plot1.png",width=480,height=480)
    hist(d_sub$Global_active_power,xlab = "Global Active Power (kW)",col="red",main="Global Active Power")
    dev.off()
}