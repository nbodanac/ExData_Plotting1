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

###########PLOT 1############################################################################################

#for the first plot we want to create a histogram of the Global Active Power


#our data set came with an error asking for a numeric variable for GLobal active power, so first we will convert it
we <- gsub(",", "", we)   # remove comma
Global_active_power <- as.numeric(household_power_consumption$Global_active_power)      # turn into numbers

## Remove Global_active_power
household_power_consumption <- household_power_consumption[ ,!(names(household_power_consumption) %in% c("Global_active_power"))]

## Add DateTime column
household_power_consumption <- cbind(Global_active_power, household_power_consumption)

#Now we will run it

plot1.png<-hist(household_power_consumption$Global_active_power , main="Global Active Power", xlab = "Global Active Power (kw)", col="red")

## Save file and close device
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()
