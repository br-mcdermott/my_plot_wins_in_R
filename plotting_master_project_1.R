#setwd("C:/Users/brigid/Dropbox/Coursera/data explore 1")
##### http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r

#### I used a slightly modified version of the function "zip" from the above
### reference to bring in a few lines of the power file in order to find out
### the classes of the variables. This allowed me to use colClass in the second use ### of the function to lower the time needed to import the data

zip <- function(zipfile, row.names=NULL, dec=".") {
  # Create a name for the dir where we'll unzip
  zipdir <- tempfile()
  # Create the dir using that name
  dir.create(zipdir)
  # Unzip the file into the dir
  unzip(zipfile, exdir=zipdir)
  # Get the files into the dir
  files <- list.files(zipdir)
  # Throw an error if there's more than one
  if(length(files)>1) stop("More than one data file inside zip")
  # Get the full name of the file
  file <- paste(zipdir, files[1], sep="/")
  # Read the file
    read.table(file, sep=";",nrow=10,header=T,na.strings="?", as.is=T)
}



tiny<-read.zip("C:\\Users\\brigid\\Downloads\\household_power_consumption.zip")
head(tiny)
classes<-sapply(tiny,class)

### now redifine zip function to bring in full file using colClasses
read.zip <- function(zipfile, row.names=NULL, dec=".") {
  # Create a name for the dir where we'll unzip
  zipdir <- tempfile()
  # Create the dir using that name
  dir.create(zipdir)
  # Unzip the file into the dir
  unzip(zipfile, exdir=zipdir)
  # Get the files into the dir
  files <- list.files(zipdir)
  # Throw an error if there's more than one
  if(length(files)>1) stop("More than one data file inside zip")
  # Get the full name of the file
  file <- paste(zipdir, files[1], sep="/")
  # Read the file
  read.table(file, sep=";",nrow=2080000,header=T,na.strings="?", as.is=T, colClasses=classes)
}

power<-read.zip("C:\\Users\\brigid\\Downloads\\household_power_consumption.zip")
#head(power)
#tail(power)
### create a datetime character variable and turn it into POSXlt structure
datetime<-with(power, paste(Date,Time))
head(datetime)
time<-strptime(datetime,format="%d/%m/%Y %H:%M:%S")
str(time)
### use package lubridate so I can use simple functions year and month to 
#### extract the february 2007
install.packages("lubridate")
library(lubridate)
data<-power[year(time)=="2007" & month(time)=="2",]
 #now get the number of minutes in first two days and extract these rows
nummin<-60*24*2
data<-data[1:nummin,]
tail(data) #### check ending on correct date and time

rm(power)# remove power as it is a big dataset 
#write.csv(data, "data.csv", row.names=F)### personal use to make backup copy data

#### variable indexing minutes in the two days
mintime<-1:2880

####plot 1
png(file="plot1.png", width=480,height=480, bg="white", type=c("windows"))
with(data,hist(Global_active_power, col=2, xlab="Global Active Power (kilowats)", main="Global Active Power"))
dev.off()



####plot 2
png(file="plot2.png", width=480,height=480, bg="white", type=c("windows"))
plot(Global_active_power~mintime,type="l", xaxt="n",xlab="",ylab="Global Active Power(kilowatts)",data=data)
axis(1,at=c(0,1440,2880),label=c("Thu","Fri","Sat"))
dev.off()


#### plot 3
png(file="plot3.png", width=480,height=480, bg="white", type=c("windows"))
plot(Sub_metering_1~mintime, type="l", xlab="", ylab="Energy sub metering", xaxt="n",data=data)
lines(Sub_metering_2~mintime,col=2, data=data)
lines(Sub_metering_3~mintime,col=4,data=data)
axis(1,at=c(0,1440,2880),label=c("Thu","Fri","Sat"))
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),pch="___", col=c(1,2,4))
dev.off()

#### plot4
par(mfrow=c(2,2))
png(file="plot4.png", width=480,height=480, bg="white", type=c("windows"))
par(mfrow=c(2,2))
plot(Global_active_power~mintime,type="l", xaxt="n",xlab="",ylab="Global Active Power(kilowatts)",data=data)
axis(1,at=c(0,1440,2880),label=c("Thu","Fri","Sat"))

plot(Voltage~mintime,type="l",xlab="datetime",xaxt="n",data=data)
axis(1,at=c(0,1440,2880), label=c("Thu","Fri","Sat"))

plot(Sub_metering_1~mintime, type="l", xlab="", ylab="Energy sub metering", xaxt="n",data=data)
lines(Sub_metering_2~mintime,col=2, data=data)
lines(Sub_metering_3~mintime,col=4,data=data)
axis(1,at=c(0,1440,2880),label=c("Thu","Fri","Sat"))
legend("topright", legend=c("Sub_metering_1", "Sub_metering_2","Sub_metering_3"),bty="n",pch="___", col=c(1,2,4))

plot(Global_reactive_power~mintime,type="l",xlab="datetime",xaxt="n",data=data)
axis(1,at=c(0,1440,2880), label=c("Thu","Fri","Sat"))

dev.off()








