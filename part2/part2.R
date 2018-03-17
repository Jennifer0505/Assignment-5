#part 2

library(tidyverse)
library(readxl)

veg.1 <- read_xlsx("veg1.xlsx")

cnames.1 <- colnames(veg.1)

## now get the count for each column

c <- apply(veg.1, 2, n_distinct)



c[c>1]


d <- names(c[c==1])


e <- names(c[c>1])



veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)


apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)

cnames.3 <- colnames(veg.3)



unique(veg.3[,"Commodity"])

unique(veg.3[,"Data"]) %>% print(n=60)

unique(veg.3[,"Domain"])

unique(veg.3[,"Category"])

unique(veg.3[,"Value"])

yy <- separate(veg.3, Category, into = c("label", "quant"), sep=",")
ru <- filter(yy, label=="RESTRICTED USE CHEMICAL")

ru1 <- ru %>% select(label, quant) %>% unique()

#Initially, we select the 28 unique restricted used chemicals in order to find their corresponding cas codes and LD50 values (filtering by species-rat, study type-oral and averaging the toxicity value)

clean1 <- ru %>% select(label, quant, Commodity,Year, Value,Data)
clean1<-clean1 %>% 
  separate(Data, into = c("vegetable", "method_unit"), sep = "-")

clean1<-clean1 %>%select(label,quant, Commodity, Year, Value, method_unit)

clean1 <- clean1 %>% spread(clean1, key=method_unit, value=Value)

clean1 <- separate(clean1, quant, into = c("a", "chemical"), sep=":")
clean1 <- cbind(clean1[1],clean1[3:10])
colnames(clean1) <- c("label", "chemical","Commodity", "Year","A", "B", "C", "D", "E")

clean1 <- filter(clean1, A != "(D)")
#
Toxicity<-read_xlsx("Toxicity_Tidy_Table.xlsx")

#There are total 63 observations and 9 variables in the clean1 data frame. 
#Additionly, we add a column of LD50 which includes the LD50 values for corresponding chemicals.
#The Toxicity data frame is the new complete tidy data frame.

#We seperate 5 methods into 5 column. A represents " APPLICATIONS, MEASURED IN LB"; B represents " APPLICATIONS, MEASURED IN LB / ACRE / APPLICATION, AVG"
#C represents " APPLICATIONS, MEASURED IN LB / ACRE / YEAR, AVG"; D represents " APPLICATIONS, MEASURED IN NUMBER, AVG"                 
# Erepresents " TREATED, MEASURED IN PCT OF AREA PLANTED, AVG"

#for method A
#view as a whole
plot(Toxicity$LD50, Toxicity$A)
#view by year
Year_2006 <- filter(Toxicity, Year == "2006")
plot(Year_2006$LD50, Year_2006$A)
Year_2010 <- filter(Toxicity, Year == "2010")
plot(Year_2010$LD50, Year_2010$A)
Year_2014 <- filter(Toxicity, Year == "2014")
plot(Year_2014$LD50, Year_2014$A)
Year_2016 <- filter(Toxicity, Year == "2016")
plot(Year_2016$LD50, Year_2016$A)

#for method B
#view as a whole
plot(Toxicity$LD50, Toxicity$B)
#view by year
Year_2006 <- filter(Toxicity, Year == "2006")
plot(Year_2006$LD50, Year_2006$B)
Year_2010 <- filter(Toxicity, Year == "2010")
plot(Year_2010$LD50, Year_2010$B)
Year_2014 <- filter(Toxicity, Year == "2014")
plot(Year_2014$LD50, Year_2014$B)
Year_2016 <- filter(Toxicity, Year == "2016")
plot(Year_2016$LD50, Year_2016$B)

#for method C
#view as a whole
plot(Toxicity$LD50, Toxicity$C)
#view by year
Year_2006 <- filter(Toxicity, Year == "2006")
plot(Year_2006$LD50, Year_2006$C)
Year_2010 <- filter(Toxicity, Year == "2010")
plot(Year_2010$LD50, Year_2010$C)
Year_2014 <- filter(Toxicity, Year == "2014")
plot(Year_2014$LD50, Year_2014$C)
Year_2016 <- filter(Toxicity, Year == "2016")
plot(Year_2016$LD50, Year_2016$C)

#for method D
#view as a whole
plot(Toxicity$LD50, Toxicity$D)
#view by year
Year_2006 <- filter(Toxicity, Year == "2006")
plot(Year_2006$LD50, Year_2006$D)
Year_2010 <- filter(Toxicity, Year == "2010")
plot(Year_2010$LD50, Year_2010$D)
Year_2014 <- filter(Toxicity, Year == "2014")
plot(Year_2014$LD50, Year_2014$D)
Year_2016 <- filter(Toxicity, Year == "2016")
plot(Year_2016$LD50, Year_2016$D)

#for method E
#view as a whole
plot(Toxicity$LD50, Toxicity$E)
#view by year
Year_2006 <- filter(Toxicity, Year == "2006")
plot(Year_2006$LD50, Year_2006$E)
Year_2010 <- filter(Toxicity, Year == "2010")
plot(Year_2010$LD50, Year_2010$E)
Year_2014 <- filter(Toxicity, Year == "2014")
plot(Year_2014$LD50, Year_2014$E)
Year_2016 <- filter(Toxicity, Year == "2016")
plot(Year_2016$LD50, Year_2016$E)

