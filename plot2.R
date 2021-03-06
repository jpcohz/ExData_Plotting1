ReadFileLineByLineToDataFrame = function(filepath, pattern) {
  
  con = file(filepath, "r")
  flag = FALSE
  df <- data.frame()	

  while ( TRUE ) 
  {
    line = readLines(con, n = 1)
    	
    if ( length(line) == 0 ) { break }
    else if ( grepl(pattern, line) )
    {
    	  dataLineAsList = as.list(strsplit(line, ";")[[1]])
	  df <- rbind(df, dataLineAsList)	
	  flag = TRUE 	
    }
  }

  close(con)
	
  if ( flag )
  {
	return( df )
  }
  else	
  {
	return( "pattern not found" )
  }  	
}

print("DONE!")
x <- ReadFileLineByLineToDataFrame("household_power_consumption.txt", "^1/2/2007|^2/2/2007")
x[ x == "?" ] <- NA		#REPLACE ? with NA
x[,1] <- as.Date(x[,1], format="%d/%m/%Y")
x[,2] <- as.POSIXct(strptime(paste(format(x[,1], "%d/%m/%Y"), x[,2]), format="%d/%m/%Y %H:%M:%S")) 
x[,3] <- as.numeric(x[,3])
x[,4] <- as.numeric(x[,4])
x[,5] <- as.numeric(x[,5])
x[,6] <- as.numeric(x[,6])
x[,7] <- as.numeric(x[,7])
x[,8] <- as.numeric(x[,8])
x[,9] <- as.numeric(x[,9])
colnames(x) <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")

#install.packages("png")
#library(png)
png("plot2.png", width = 480, height = 480)
plot(Global_active_power ~ Time, x, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)")
dev.off()
