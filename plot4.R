## This script assumes the following:
## 1. The data has been downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
## 2. The data has been unzipped into a sub folder called c1_data contained in the working directory
## 3. No R objects created previously with the plot scripts for this assignment are available in the global environment prior to running this script
## 4. The data for the plotting function has not been read into R yet.

## This file contains 2 functions - one to read the data into a dataframe and subset it, the other to create the plot PNG

## Script for plotting the data to a PNG file.
plot4 <<- function() {
      ## Run readData function to get the plotData subset for plotting
      plotData <- readData()
      
      ## Creates the empty PNG file
      png(file = "plot4.png", width=480, height=480, units="px")
      
      ## Set mfrow parameter to (2,2) in order to output 4 plots in one image 
      par(mfrow=(c(2,2)))
      
      ## First plot
      plot(plotData$Time, plotData$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
      
      ## Second plot
      plot(plotData$Time, plotData$Voltage, type="l", xlab="datetime", ylab="Voltage")
      
      ## Third plot 
      with(plotData, plot(plotData$Time, plotData$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering"))
      with(subset(plotData, plotData$Global_active_power > 0), points(plotData$Time, plotData$Sub_metering_1, col="black", type="l"))
      with(subset(plotData, plotData$Global_active_power > 0), points(plotData$Time, plotData$Sub_metering_2, col="red", type="l"))
      with(subset(plotData, plotData$Global_active_power > 0), points(plotData$Time, plotData$Sub_metering_3, col="blue", type="l"))
      legend("topright", bty="n", lty = c(1), col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      
      ## Fourth plot
      plot(plotData$Time, plotData$Global_reactive_power, type="l", xlab="datetime", ylab="Global_reactive_power")
      
      ## closes device
      dev.off()
}

## Runs the plot function without having to type it into the command line.
plot4()

## Script for reading in the data and subsetting it for plotting
readData <- function() {
      ## Reading in an initial small dataset to get column classes
      initial <- read.table("../c1_data/household_power_consumption.txt", sep=";", nrows=100, header=TRUE, stringsAsFactors=FALSE)
      
      ## Creating a classes vector for use in reading the full dataset in.
      classes <- sapply(initial, class)
      
      ## Remove initial dataset to free up memory
      rm(initial)
      
      ##  Read in full dataset with classes
      full <- read.table("../c1_data/household_power_consumption.txt", sep=";", colClasses=classes, header=TRUE, nrows=2075300, na.strings = "?")
      
      ## Sebset full data
      plotData <- full[(full$Date=="1/2/2007" | full$Date=="2/2/2007"), ]
      
      ## Remove full dataset to free up memory
      rm(full)
      
      ## Converting Date column data to Date format from character
      plotData$Date <- as.Date(plotData$Date, format="%d/%m/%Y")
      
      ## Converting Time column data to Time format from character and adding the date paremeter for plot 2-4 output
      plotData$Time <- strptime(paste(plotData$Date, plotData$Time), format="%Y-%m-%d %H:%M:%S")
      
      ## Returns the plotData subset to the plotting function
      return(plotData)
}