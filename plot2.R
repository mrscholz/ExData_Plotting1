plot2 <- function(path="./"){
    setwd(path)
    library(data.table)
    library(lubridate)
    d_temp <- fread("household_power_consumption.txt", na.strings = "?")
    d_temp$datetime <- dmy_hms(paste(d_temp$Date,d_temp$Time))
    t_span <- new_interval(ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-03 00:00:00")) 
    d_sub <- subset(d_temp,d_temp$datetime %within% t_span)
    rm(d_temp)
    
    d_sub$t_elapse <- as.numeric(abs(as.duration(d_sub$datetime[[1]]-d_sub$datetime)))
    
    png(filename="./plot2.png",width=480,height=480)
    with(d_sub,plot(t_elapse,Global_active_power,type="l",axes=FALSE,xlab="",ylab="Global Active Power (kW)"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0,2,4,6))
    box()
    dev.off()
}