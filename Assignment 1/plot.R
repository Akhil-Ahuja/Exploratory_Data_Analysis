## loading necessary packages
library(lubridate)

## read data with na =="?" 
df <- read.table ("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?")

## we need data only from 1st and 2nd feb 2007
df$Date <- dmy(df$Date) 
df$Time <- hms(df$Time)
df$Date_Time <- df$Date + df$Time
df <- subset(df,df$Date == ymd("2007,02,01") | df$Date == ymd("2007,02,02"))

df

## global active power histogram
png("plot1.png")
with(df, hist(Global_active_power, xlab = "Global Active Power (kilowatts)", main = "Global Active Power", col = "red"))
dev.off()

## time vs global active power
png ("plot2.png")
with(df, plot(Date_Time,Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
dev.off()

## time vs energy sub metering
png("plot3.png")
with(df, plot(Date_Time, Sub_metering_1, type = "l", xlab = "", ylab="Energy Sub Metering"))
with(df, lines(Date_Time, Sub_metering_2, col = "red"))
with(df, lines(Date_Time, Sub_metering_3, col = "blue"))
legend("topright", col = c("black", "red", "blue"),legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1)
dev.off()

## 4 graphs 
## global active power vs time
## voltage vs time
## energy sub metering vs time
## global reactive power
png("plot4.png")
par(mfrow = c(2,2))

#1st
with(df, plot(Date_Time,Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))

#2nd
with(df, plot(Date_Time,Voltage, type = "l", xlab = "datetime", ylab = "Voltage"))

#3rd
with(df, plot(Date_Time, Sub_metering_1, type = "l", xlab = "", ylab="Energy Sub Metering"))
with(df, lines(Date_Time, Sub_metering_2, col = "red"))
with(df, lines(Date_Time, Sub_metering_3, col = "blue"))
legend("topright", bty = "n", col = c("black", "red", "blue"),legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1)

#4th
with(df, plot(Date_Time, Global_reactive_power, type = "l", xlab = "datetime"))
dev.off()

