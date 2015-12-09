plot4 <- function(path="./"){
    setwd(path)
    library(data.table)
    library(lubridate)
    d_temp <- fread("household_power_consumption.txt", na.strings = "?")
    d_temp$datetime <- dmy_hms(paste(d_temp$Date,d_temp$Time))
    t_span <- new_interval(ymd_hms("2007-02-01 00:00:00"),ymd_hms("2007-02-03 00:00:00")) 
    d_sub <- subset(d_temp,d_temp$datetime %within% t_span)
    rm(d_temp)
    d_sub$t_elapse <- as.numeric(abs(as.duration(d_sub$datetime[[1]]-d_sub$datetime)))
    
    png(filename="./plot4.png",width=480,height=480)
    par(mfcol=c(2,2))
    
    with(d_sub,plot(t_elapse,Global_active_power,type="l",axes=FALSE,xlab="",ylab="Global Active Power (kW)"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0,2,4,6))
    box()
    
    plot(d_sub$t_elapse,sd$Sub_metering_1,type="n",axes=FALSE,xlab="",ylab="Energy sub metering")
    with(d_sub, lines(t_elapse,Sub_metering_1,col="black"))
    with(d_sub, lines(t_elapse,Sub_metering_2,col="red"))
    with(d_sub, lines(t_elapse,Sub_metering_3,col="blue"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0,10,20,30))
    legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub metering 1","Sub metering 3","Sub metering 3"))
    box()
    
    with(d_sub,plot(t_elapse,Voltage,type="l",axes=FALSE,xlab="datetime",ylab="Voltage"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(234,238,242,246))
    box()
    
    with(d_sub,plot(t_elapse,Global_reactive_power,type="l",axes=FALSE,xlab="datetime",ylab="Global reactive Power"))
    axis(1,c(0,86400,172800),labels = c("Thu","Fri","Sat"))
    axis(2,c(0.0,0.1,0.2,0.3,0.4,0.5))
    box()
    
    dev.off()
    par(mfrow=c(1,1))
}