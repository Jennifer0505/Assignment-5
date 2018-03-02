library(tidyverse)
library(stringr)

url1 <- "http://www.ndbc.noaa.gov/download_data.php?filename=46035h1985"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2017)

urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

##
