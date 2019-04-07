#first import the data set

household_power_consumption <- read.csv("C:/Users/Nicholas/Desktop/sreor/data visualizations/project_week_1/exdata_data_household_power_consumption/household_power_consumption.txt", sep=";")
View(household_power_consumption)


## Format date to Type Date
household_power_consumption$Date <- as.Date(household_power_consumption$Date, "%d/%m/%Y")

## Filter data set from Feb. 1, 2007 to Feb. 2, 2007 as this is the only data range we are looking at
household_power_consumption <- subset(household_power_consumption,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

## now we remove incomplete observation by using the complete cases argument
household_power_consumption <- household_power_consumption[complete.cases(household_power_consumption),]
household_power_consumption <-household_power_consumption[complete.cases(household_power_consumption), ]
## Combine Date and Time column
dateTime <- paste(household_power_consumption$Date, household_power_consumption$Time)

## Name the vector
dateTime <- setNames(dateTime, "DateTime")


## Remove Date and Time column
household_power_consumption <- household_power_consumption[ ,!(names(household_power_consumption) %in% c("Date","Time"))]

## Add DateTime column
household_power_consumption <- cbind(dateTime, household_power_consumption)

## Format dateTime Column
household_power_consumption$dateTime <- as.POSIXct(dateTime)

#read the headings
head(household_power_consumption)

## Create Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(household_power_consumption, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
  
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  
})

dev.copy(png, file="plot4.png", height=480, width=480)
#dev.off()