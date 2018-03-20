#all air temperature and water temperature information for 2012 are recorded as 999.0, 
#which means that these information we want are missing, so we no longer consider 2012
#in our investigation. We can't find record of year 2013 from the website. 
#It's unable to read 2000's table, so we don't use this year. Therefore, we use
#the data from 1985 to 2017 except year 2000, 2012, 2013 to compute the time series.
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(plyr)

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1985:2017)
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

file <- read.table(urls[1], header=TRUE)

clean_data <- function(index){
  if(index<23){
    data <- read.table(urls[index], header=TRUE) 
  }
  else{
    data <- read.table(urls[index], header=FALSE)
    colnames(data) <- c("YY", "MM", "DD", "hh", "mm", "WDIR", "WSPD", "GST", "WVHT", "DPD", "APD", "MWD", "PRES", "ATMP", "WTMP", "DEWP", "VIS", "TIDE")
  }
  if(index<15){
    data[1] <- data[1]+1900
  }
  colnames(data)[1] <- "YYYY" 
  data <- data %>% select(YYYY, MM, DD, hh, ATMP, WTMP)
  data <- data %>% filter(hh == 12)
  data[data==999.0] <- NA
  data$MM_DD <- paste(data$MM, data$DD, sep="-")
  data <- data[1:7]
  data <- data %>% select(MM_DD, everything())
  data <- cbind(data[2],data[1], data[6:7])
  return (data)
}

#cleaned data over 30 years 
for(i in 1:15){
  oname = paste("date", i+1984, sep="")
  assign(oname, clean_data(i))
}
for(i in 17:27){
  oname = paste("date", i+1984, sep="")
  assign(oname, clean_data(i))
}
for(i in 30:33){
  oname = paste("date", i+1984, sep="")
  assign(oname, clean_data(i))
} 


# combined all the cleaned data into a csv file
wholedata<-read.csv("wholedata.csv")
wholedata <-wholedata[,-1]
wholedata <- wholedata[-c(3625, 4420, 4421), ]

#ATMP
whole_data_ATMP <- wholedata[1:3]
whole_data_ATMP <- whole_data_ATMP<-spread(whole_data_ATMP, key = MM_DD, value = ATMP)


#Air temperature Time series plot
row.names(whole_data_ATMP) <- whole_data_ATMP[,1]
whole_data_ATMP<-whole_data_ATMP[,-1]
ts1 <- as.vector(t(whole_data_ATMP[,-1]))
ts2 <- ts(rev(ts1), start= c(1985,1), end=c(2017,12),frequency=12)
ts3 <- ts.plot(ts2, gpars=list(xlab="year", ylab="Air_Temperature", lty=c(1:3)),col='purple')

#WTMP
whole_data_WTMP <- cbind(wholedata[1:2], wholedata[4])
whole_data_WTMP <- whole_data_WTMP<-spread(whole_data_WTMP, key = MM_DD, value = WTMP)

#Water temperature Time series plot
row.names(whole_data_WTMP) <- whole_data_WTMP[,1]
whole_data_WTMP<-whole_data_WTMP[,-1]
ts4 <- as.vector(t(whole_data_WTMP[,-1]))
ts5 <- ts(rev(ts4), start= c(1985,1), end=c(2017,12),frequency=12)
ts6 <- ts.plot(ts5, gpars=list(xlab="year", ylab="Water_Temperature", lty=c(1:3)),col='blue')

# Plot between water temperature and air temperature 

plot(wholedata$ATMP, wholedata$WTMP)
abline(lm(wholedata$ATMP ~ wholedata$WTMP))

#so according to the plot, there is a positive relationship between air temperature and water temperature. 

#Statistical Method about mean water temperature change over the past 30 years: Parametric Linear Regression
wholedata_WTMP_Linear <- cbind(wholedata[1:2], wholedata[4])
WTMP_Linear<- lm(WTMP ~  YYYY,data=wholedata_WTMP_Linear)
summary(WTMP_Linear)
plot(WTMP ~ YYYY, data=wholedata_WTMP_Linear)
abline(lm(wholedata_WTMP_Linear$WTMP~wholedata_WTMP_Linear$YYYY))
#p value is smaller than 2.2e-16, which is smaller than alpha 0.05, so the change of the mean water temperature over past 30 years is significant.

#Statistical Method about mean air temperature change over the past 30 years: Parametric Linear Regression
wholedata_ATMP_Linear <- cbind(wholedata[1:3])
ATMP_Linear <- lm(ATMP~YYYY, data=wholedata_ATMP_Linear)
summary(ATMP_Linear)
plot(ATMP~YYYY, data=wholedata_ATMP_Linear)
abline(lm(wholedata_ATMP_Linear$ATMP~wholedata_ATMP_Linear$YYYY))
#p value is smaller than 2.2e-16, which is smaller than alpha 0.05, so the change of the mean air temperature over past 30 years is significant.


#Whether our sampling of choosing one sample per day affects our evaluation of temperature change or not depends on the trend of the temperature within this specific area. If the temperature over the whole day is moderately stable,
#we can approxiamately regard the temperature at 12pm at each single day as the average temperature over the whole day. In this case, the sampling method does not affect our evaluation of temperature change much. 
#However, if there exists large varaitions in temperature over the whole day, the temperature at 12pm can not be regarded as a reasonable respresentation of the average temperature of each single day. In this case, 
#our sampling method affects the evaluation of temperature change a lot.

write.csv(whole_data_ATMP, file = 'whole_data_ATMP.csv')
write.csv(whole_data_WTMP, file = 'whole_data_WTMP.csv')
write.csv(wholedata_ATMP_Linear, file = 'wholedata_ATMP_Linear.csv')
write.csv(wholedata_WTMP_Linear, file= 'wholedata_WTMP_Linear.csv')

