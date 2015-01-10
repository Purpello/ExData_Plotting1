#######################################################################################################
# This R Script supports the Week 1 course project for the Exploratory Data Analysis Coursera course.
# This script is one of 4 that reads in data and creates a png file of a graph.
# The code for reading in and subsetting the data is the same for each of the 4 files.
# Only the graph code varies between the 4 script files.

# The script assumes you have set your working directory to the one containing the data file.
# Graphics files will be saved to the working directory.
# Also, note the libraries that are loaded below.
#######################################################################################################

######################################
### Load the appropriate libraries ###
######################################
library(lubridate) # a convenient date library 
library(dplyr)     # a convenient data manipulation library

############################################
### Read, filter, and transform the data ###
############################################

# Read in the data.  The class assignment says we can read it in all at once and subsequently filter it, so I take that approach.
# read.table is a fast way to read in large data tables, especially if you tell it what the column classes are.
# I also tell read.table that the separator is ";", the NA symbol is "?" and to NOT convert strings to factors.

# First, just read in 5 rows of the data to discover the column classes.
pData<-read.table("household_power_consumption.txt",header=TRUE, sep=';', stringsAsFactors = FALSE, nrows=5)
classes <- sapply(pData, class)

# Use the column classes to read in the whole data set.  This took about 30 seconds on my Macbook Air.
pData<-read.table("household_power_consumption.txt",header=TRUE, sep=';', stringsAsFactors = FALSE, colClasses=classes, na.strings = "?")

# This is a lubridate functions for converting the first column to date (POSIXct format).
pData$Date<-dmy(pData$Date)

# We don't need the time column, so I'll drop it.  
pData<-pData[-2]

# filter is a dplyr function for filtering data.  The output has 2880 obs on 8 variables.
pData<-filter(pData, year(Date) == 2007, month(Date) == 2, day(Date) < 3)

# Add weekdays as a variable, based on the date.  'wday' is a lubridate function.
# mutate is another dplyr function.
# we now have 2880 obs on 9 variables.
pData<-mutate(pData, weekdays = wday(Date, label=TRUE, abbr=TRUE))

# to make the data match the plots better, I'm going to change how "Thurs" is represented
levels(pData$weekdays)[5]<-"Thu"

#####################
### Plot the data ###
#####################


# Plot 2, Global Active Power line plot
png("plot2.png", width=480, height=480, units = "px")

#tickmarks figures out the first, middle, and last index value on the x-axis
#daylabels gets the values Thu, Fri, Sat
tickmarks<-c(1,1 + length(pData$Date)/2,length(pData$Date)) 
daylabels<-levels(pData$weekdays)[5:7]

plot(pData$Global_active_power, type="l", xaxt='n', ylab="Global Active Power (kilowatts)", xlab="")

#puts the tickmarks on the axis with the appropriate labels
axis(1, at=tickmarks, labels = daylabels)

dev.off()

