## This script assumes the following:
## 1. The data has been downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
## 2. The data has been unzipped into a sub folder called c1_data contained in the working directory
## 3. No R objects created previously with the plot scripts for this assignment are available in the global environment prior to running this script
## 4. The data for the plotting function has not been read into R yet.

## This file contains 2 functions - one to read the data into a dataframe and subset it, the other to create the plot PNG

plot3 <- function() {
      ## Run readData function to get the plotData subset for plotting
      plotData <- readData()
      
      ## Reset mfrow parameter to (1,1) in case the default has been changed
      par(mfrow=c(1,1))
      
      ## Creates the empty PNG file
      png(file = "plot3.png", width=480, height=480, units="px")
      
      ## plots the line graph with a null x-axis, renamed y-axis, data from 3 variables and a legend
      ## Create empty plot with appropriately axis lables
      with(plotData, plot(plotData$Time, plotData$Sub_metering_1, type="n", xlab="", ylab="Energy sub metering"))
      
      ## Plots the Sub_metering_1 line graph in black
      with(subset(plotData, plotData$Global_active_power > 0), points(plotData$Time, plotData$Sub_metering_1, col="black", type="l"))
      
      ## Plots the Sub_metering_2 line graph in red
      with(subset(plotData, plotData$Global_active_power > 0), points(plotData$Time, plotData$Sub_metering_2, col="red", type="l"))
      
      ## Plots the Sub_metering_3 line graph in blue
      with(subset(plotData, plotData$Global_active_power > 0), points(plotData$Time, plotData$Sub_metering_3, col="blue", type="l"))
      
      ## Creates a legend of the variables plotted
      legend("topright", lty = c(1), col=c("black", "red", "blue"), legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
      
      ## closes device
      dev.off()
}

## Runs the plot function without having to type it into the command line.
plot3()

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