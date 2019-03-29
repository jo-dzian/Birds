install.packages("dplyr")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("IHA", repos="http://R-Forge.R-project.org")

library(tidyverse)
library(dplyr)

setwd ("D:/Ptaki_hydro/Obliczenia/R/Birds")

Q <- data.frame( read.csv("CPLNH_streamflow.csv"))

#changing the data format from factor to date
Q$Date <- as.Date(Q$Date, format = "%m/%d/%Y")

#data on birds is available for 15 years (from 2004 - 2018)
#data on streamflow is available up to 2013)
#pick streamflow data for after 2004
Q1 <- subset(Q, Q$Date >= "2004-1-1")

#reach_wyspy - ta warstwa zawiera dane o subbasinach i outletach na których będę pracować (nie pracuje na 2160 subb tylko na ok. 44)
Island_reach <- data.frame( read.csv("reaches_island.csv"))
#Island_subbasin$OBJECTID


reaches <- as.numeric( Island_reach$OBJECTID)
#Q2 <- Q1 %>% select(reaches(ends_with("1411"))
#Q2 <- Q1 %>% select(reaches) #DZIALA


Q3 <- Q1 %>% select(Date, c(ends_with ("1036"), ends_with ("1038"))) #ok

Q3 <- Q1 %>% select(Date, c(Q1, ends_with ("1036")))

Q3 <- Q1 %>% select(Date, matches('1036|1038'))
Q3 <- Q1 %>% select(Date, matches("1036|1038|1077|1139|1213|1258|1302|1363|1387|1411|1451|1480|1580|1654|1697|1716|1725|1751|1779|1858|1910|1931|2054"))

#Q2 <- select(Q1, ends_with("1036"))#DZIALA

Q2 <- select(Q1, ends_with("1411"))
    