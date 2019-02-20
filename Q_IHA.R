
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("IHA", repos="http://R-Forge.R-project.org")

Q <- data.frame( read.csv("CPL-NH_streamflow.csv"))

#reach_wyspy - ta warstwa zawiera dane o subbasinach i outletach na których będę pracować (nie pracuje na 2160 subb tylko na ok. 44)