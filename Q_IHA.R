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

#zawezam dane do interesujacych mnie reachow (przyjmując, że to subbasiny)
Q2 <- Q1 %>% select(c(1), as.vector(Island_reach$Subbasin)+1)

#######Testing IHA software##########
Q3 <- Q1 %>% select(Date, matches("1363")) #%>% subset(Q3>="2004-1-1" & Q3 < "2005-1-1")

cut.Date (Q3, min(2004)+1)

write.table(Q3, file = "r_1363.txt", row.names = FALSE, sep=",")

#######Testing hydrostats package##########

install.packages("hydrostats")
library(hydrostats)

#######Testing Eflowstats package##########

install.packages("packrat")
library("packrat")

rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'),
      CRAN=\'https://cloud.r-project.org\',
      USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')

install.packages("EflowStats")
library("EflowStats")

calc1 <- calc_durationHigh(Q3, yearType = "calendar", digits = 3, pref = "mean",
                  floodThreshold = NULL)






