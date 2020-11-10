# SCRIPT NO. 5 A


setwd ("D:/Ptaki_hydro/Obliczenia/R/IHA")

library(plyr)
library(readr)

subbasins_nr <- data.frame(rbind("910","950", "1012", "1087", "1134","1240","1264","1289", "1329","1358","1501", "1545",
"1565","1601","1629", "1727","1746","1875"))
colnames(subbasins_nr)<- c("RCH")

#extract data for full year for subbasins of interest
fun_sub_allyear <- function(x){
  in_data <- read.csv(x, header = TRUE, sep = ",")
  in_data2 <- in_data[ in_data$subbasin %in% subbasins_nr$RCH, ]
  in_data3 <- split(in_data2, in_data2$subbasin)
}

#single dataframe
future_2024_2050_4.5_cm1 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")

# get a list of files
input_files_4.5_NF <- list.files("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF", pattern = "[.]csv", full.names = TRUE)
input_files_4.5_FF <- list.files("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF", pattern = "[.]csv", full.names = TRUE)
input_files_8.5_NF <- list.files("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF", pattern = "[.]csv", full.names = TRUE)
input_files_8.5_FF <- list.files("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF", pattern = "[.]csv", full.names = TRUE)

# extract data for subbasins of interest
data_4.5_NF <- lapply(input_files_4.5_NF, fun_sub_allyear)

# extract data for vulnerability period for bh.gull for subbasins of interest

fun_bh.gull_vp <- function(x){
  in_data <- read.csv(x, header = TRUE, sep = ",")
  in_data2 <- in_data[ in_data$subbasin %in% subbasins_nr$RCH, ]
  in_data2$date2 <- as.POSIXct(in_data2$date, format="%Y-%m-%d")
  month <- as.integer(format(in_data2$date2, '%m'))
  day <- as.integer(format(in_data2$date2, '%d'))
  in_data3 <- filter(in_data2, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)
  in_data4 <- split(in_data3, in_data3$subbasin)
}

bh.gull_vp_2024_2050_4.5_cm1 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")

bh.gull_vp_4.5_NF <- lapply(input_files_4.5_NF, fun_bh.gull_vp)

####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks


######### incubating 20.04 - 31.05 ###################################

######### rearing chicks 1.05 - 10.06 #################################

##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max


##############################################################################################
####### GROUP 3 ################################################################################
####### IHA group 3 is  Timing of annual extreme water conditions,
#Julian date of each annual 1-day maximum 


##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above 0.95


# calculate percentiles and quartiles
RCH_split_q13_period <- lapply(data_4.5_NF,function(x) 
  ddply(x,.(RCH), summarize,
        P0.95=quantile(flow, 0.95)#find the 95% percentile for the period 2024-2050
  ))

# reorganize list so it matches the structure of the other datasets        
bh.gull_Q_mod_list_RCH <- bh.gull_Q_mod %>% group_split(bh.gull_Q_mod$RCH) %>% setNames(RCH_names)

#count how many days during the vulnerability period are higher than 75% quartile and lower than 25%
# I need to use the bh.gull_Q_mod_list_RCH
bh.gull.gr4_part1 <- Map(function(x, y) aggregate(FLOW_OUTcms > cbind(Q3, P0.95)~Year, merge(x, y, all = TRUE,
                                                                                             na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass'), 
                         bh.gull_Q_mod_list_RCH, RCH_split_q13_period)

bh.gull.gr4_part2 <- Map(function(x, y) aggregate(FLOW_OUTcms < cbind(P0.05, Q1)~Year, merge(x, y, all = TRUE,
                                                                                             na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass'), 
                         bh.gull_Q_mod_list_RCH, RCH_split_q13_period)

# cbind the two parts
bh.gull_gr4_parts <- Map(cbind, bh.gull.gr4_part1, bh.gull.gr4_part2) 
# remove Year appearing twice  
bh.gull_list_gr.4 <-  lapply(bh.gull_gr4_parts , "[", -c(4))



# 4.5 NF

# 4.5 FF

# 8.5 NF

# 8.5 FF

