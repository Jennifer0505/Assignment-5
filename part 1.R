#delete 2011 air temperature can't , 2012both can't , 2013 no data, can't read 2000. 
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


data1985 <- clean_data(1)
data1986 <- clean_data(2)
data1987 <- clean_data(3)
data1988 <- clean_data(4)
data1989 <- clean_data(5)
data1990 <- clean_data(6)
data1991 <- clean_data(7)
data1992 <- clean_data(8)
data1993 <- clean_data(9)
data1994 <- clean_data(10)
data1995 <- clean_data(11)
data1996 <- clean_data(12)
data1997 <- clean_data(13)
data1998 <- clean_data(14)
data1999 <- clean_data(15)
data2001 <- clean_data(17)
data2002 <- clean_data(18)
data2003 <- clean_data(19)
data2004 <- clean_data(20)
data2005 <- clean_data(21)
data2006 <- clean_data(22)
data2007 <- clean_data(23)
data2008 <- clean_data(24)
data2009 <- clean_data(25)
data2010 <- clean_data(26)
data2011 <- clean_data(27)
data2014 <- clean_data(30)
data2015 <- clean_data(31)
data2016 <- clean_data(32)
data2017 <- clean_data(33)
wholedata<-rbind(clean_data(1),clean_data(2),clean_data(3),clean_data(4),clean_data(5),clean_data(6),clean_data(7),clean_data(8),clean_data(9),clean_data(10),clean_data(11),clean_data(12),clean_data(13),clean_data(14),clean_data(15),clean_data(17),clean_data(18),clean_data(19),clean_data(20),clean_data(21),clean_data(22),clean_data(23),clean_data(24),clean_data(25),clean_data(26),clean_data(27),clean_data(30),clean_data(31),clean_data(32),clean_data(33))
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

ggplot(data=wholedata) +
  geom_smooth(mapping = aes(x=ATMP, y=WTMP))

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



