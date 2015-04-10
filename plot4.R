## Create Plot 4
## Uses data from the UC Irvine Machine Learning Repository, a popular 
## repository for machine learning datasets. In particular, the "Individual 
## household electric power consumption Data Set" is used. The URL to the data
## is given on the course web site: 
##      https://d396qusza40orc.cloudfront.net/
##              exdata%2Fdata%2Fhousehold_power_consumption.zip
## A description of the variables and data setare given at the UCI web site:
##      https://archive.ics.uci.edu/ml/datasets/
##                Individual+household+electric+power+consumption
## In summary, the data are measurements of electric power consumption in one 
## household with a one-minute sampling rate over a period of almost 4 years. 
## Different electrical quantities and some sub-metering values are available.

## First download the data and unzip the zip file
furl <- paste("https://d396qusza40orc.cloudfront.net/",
              "exdata%2Fdata%2Fhousehold_power_consumption.zip",sep="")
if(!file.exists("./data")){ dir.create("./data") }
if(!file.exists("./data/Dataset.zip")) { 
        download.file(furl, destfile = "./data/Dataset.zip",method="curl",
                      mode="wb")
        unzip("./data/Dataset.zip", overwrite=FALSE, exdir="./data")
}

## Read the data
if(!file.exists("./data/data4analysis.csv")){
        fname <- "./data/household_power_consumption.txt"
        ## read first lines for classes
        dat <- read.table(fname, header=TRUE, sep=";", as.is=TRUE,  
                          na.strings="?", nrows = 10) 
        classes <- sapply(dat,class)
        ## read all data
        dat <- read.table(fname, header=TRUE, sep=";", as.is=TRUE,  
                          na.strings="?", colClasses=classes) 
        
        ## Convert dates and times and store the result a new column "date_time"
        dat$date_time<-strptime(paste(dat[,1],dat[,2]),"%d/%m/%Y %H:%M:%S")
        
        ## Select the data for analysis
        startdate <- "2007-02-01"
        enddate <- "2007-02-03" ## to select data of Feb 01 and Feb 02
        ana <- dat[(dat$date_time >= startdate) & (dat$date_time < enddate) &
                           !is.na(dat$date_time),]
        
        ## Write the data to a file for quicker loading
        write.csv(ana, "./data/data4analysis.csv", row.names=FALSE)
        rm(dat)
} else { 
        ana <- read.csv("./data/data4analysis.csv")
        ana$date_time <- strptime(ana$date_time, "%Y-%m-%d %H:%M:%S")
}

## Create Plot4 and save it as "plot4.png"
png("plot4.png", width=480, height=480)
par(mfrow = c(2,2))

## plot at upper left
with(ana, {plot(date_time, Global_active_power, col="black" ,type="l", 
                main = "", xlab = "", ylab = "Global Active Power")
           axis(1, labels=weekdays(date_time, abbreviate=TRUE), 
                at=date_time$wday)})

## plot at upper left
with(ana, {plot(date_time, Voltage, col="black" ,type="l", 
                main = "", xlab = "datetime", ylab = "Voltage")
           axis(1, labels=weekdays(date_time, abbreviate=TRUE), 
                at=date_time$wday)})

## plot at lower left
## Set the colors, indices for plotting, and names for the legend
cols <- c("black","red","blue")
indx <- 7:9
plnames <- names(ana)[indx]
plot(ana$date_time, ana[[indx[1]]], type="n", main = "", xlab = "", ylab = "")
for(i in 1:3){
        points(ana$date_time,ana[[indx[i]]], col=cols[i] ,type="l")                              
}
## Add labels for y-axis, ticks for x-axis, and a legend
title(ylab = "Energy sub metering")
axis(1, labels=weekdays(ana$date_time, abbreviate=TRUE), 
     at=ana$date_time$wday)
legend("topright",lwd=1, col=cols,legend=plnames, bty="n")

## plot at lower right
with(ana, {plot(date_time, Global_reactive_power, col="black" ,type="l", 
                main = "", xlab = "datetime",)
           axis(1, labels=weekdays(date_time, abbreviate=TRUE), 
                at=date_time$wday)})

par(mfrow = c(1,1))
dev.off()