install.packages("dplyr")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("IHA", repos="http://R-Forge.R-project.org")
install.packages("packrat")
install.packages("tibble")

library(tidyverse)
library(dplyr)
#packrat - package that manages all the differences between versions of packages and R
library("packrat")
library("tibble")
library("EflowStats")

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

#Moving the date in the 1 collumn to become row names
# Q2 %>% remove_rownames %>% column_to_rownames(var="Date")


#hydrostats package - other option
#######Testing Eflowstats package##########

#preparation/instalation
install.packages("EflowStats")

rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
write('\noptions(repos=c(getOption(\'repos\'),
      CRAN=\'https://cloud.r-project.org\',
      USGS=\'https://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')

#calculations

calc1 <- calc_durationHigh(Q2, yearType = "calendar", digits = 3, pref = "mean", floodThreshold = NULL)

calc2 <- calc_timingHigh(Q2, yearType = "calendar", digits = 3, pref = "mean",floodThreshold = NULL)




