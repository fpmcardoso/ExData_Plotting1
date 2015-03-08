## Gathers and plots data. 
## The data gathering part is not straightforward because the script is prepared to deal with "big" data problems in which we have to read line by line and filter out unecessary data during extraction.
## The plotting part, however, is straightforward with the plot() command.

# check_in_range: Returns TRUE if date falls between start and end dates
check_in_range<- function(date, start, end){
  return ((date >= start) && (date <= end))
}

#Set the file and file connection
rawfile<- "household_power_consumption.txt"
con  <- file(rawfile, "r")

#Set start and end dates in the analysis
start<- as.Date("01/02/2007" ,format = "%d/%m/%Y")
end<- as.Date("02/02/2007",  format = "%d/%m/%Y")

#Creates datalist to append extracted raw data
dataList <- list()

#Extracts colNames in file's first line
colNames<-unlist(strsplit(readLines(con, n = 1, warn = FALSE),";"))


#While loop - read one line while lines are read
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  
  
  #Splits the text from the line
  lineVector <- (strsplit(oneLine, ";"))
  
  #Changes ? to NA
  lineVector[lineVector == "?"]<- NA
  
  #Gets line's date, checks if is in the specified range
  date<-as.Date(lineVector[[1]][1],format = "%d/%m/%Y")
  
  if(check_in_range(date,start,end)){
    #Date is in range; append it
    
    dataList[length(dataList)+1]<-lineVector
    
  }
} 


#Loop ended; Closes the file connection
close(con)

#Creates a dataframe df with the list of extracted lines and names the columns
df<- do.call(rbind.data.frame,dataList)
colnames(df)<-colNames


#The data was extracted as characters - now we coerce each field to its right type
df$Date<-as.Date(df$Date, format = "%d/%m/%Y")
df$Global_active_power<-as.numeric(as.character(df$Global_active_power))
df$Global_reactive_power<-as.numeric(as.character(df$Global_reactive_power))
df$Voltage<-as.numeric(as.character(df$Voltage))
df$Global_intensity<-as.numeric(as.character(df$Global_intensity))
df$Sub_metering_1<-as.numeric(as.character(df$Sub_metering_1))
df$Sub_metering_2<-as.numeric(as.character(df$Sub_metering_2))
df$Sub_metering_3<-as.numeric(as.character(df$Sub_metering_3))
df$Datetime<-as.POSIXct(paste(as.Date(df$Date), df$Time))

#Plots the 4 graphics
par(mfrow = c(2,2),  mar=c(4,4,1,1), oma=c(0,0,0,0), cex = 0.64)

plot(df$Global_active_power~df$Datetime, type="l",
     ylab="Global Active Power", xlab="")

plot(df$Voltage~df$Datetime, type="l",
     ylab="Voltage", xlab="datetime")

plot(df$Sub_metering_1~df$Datetime, type="l",col="black",
     ylab="Energy Submetering", xlab="")
lines(df$Sub_metering_2~df$Datetime, type="l",col="red")
lines(df$Sub_metering_3~df$Datetime, type="l",col="blue")
legend("topright",bty = "n", col=c("black", "red", "blue"), lty=1, lwd=2, 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

plot(df$Global_reactive_power~df$Datetime, type="l",
     ylab="Global_reactive_power", xlab="datetime")

#Saves file
dev.copy(png, file="plot4.png", height=480, width=480)
dev.off()