  
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("IHA", repos="http://R-Forge.R-project.org")

Q <- data.frame( read.csv("CPLNH_streamflow.csv"))

#changing the data format from factor to date
Q$Date <- as.Date(Q$Date, format = "%m/%d/%Y")

#data on birds is available for 15 years (from 2004 - 2018)
#data on streamflow is available up to 2013)
#pick streamflow data for after 2004
Q1 <- subset(Q, Q$Date > "2004-1-1")


#reach_wyspy - ta warstwa zawiera dane o subbasinach i outletach na których będę pracować (nie pracuje na 2160 subb tylko na ok. 44)

Island_subbasin <- data.frame( read.csv("subbasin_island.csv"))
