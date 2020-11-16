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

#clear memory as this process needs a lot of it
gc()
# extract data for subbasins of interest
nf_2024_2050_4.5_cm1 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")
nf_2024_2050_4.5_cm2 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm02_2024_2050_reach.csv")
nf_2024_2050_4.5_cm3 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm03_2024_2050_reach.csv")
nf_2024_2050_4.5_cm4 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm04_2024_2050_reach.csv")
nf_2024_2050_4.5_cm5 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm05_2024_2050_reach.csv")
nf_2024_2050_4.5_cm6 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm06_2024_2050_reach.csv")
nf_2024_2050_4.5_cm7 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm07_2024_2050_reach.csv")
nf_2024_2050_4.5_cm8 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm08_2024_2050_reach.csv")
nf_2024_2050_4.5_cm9 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm09_2024_2050_reach.csv")

data_4.5_NF <- list(nf_2024_2050_4.5_cm1, nf_2024_2050_4.5_cm2, nf_2024_2050_4.5_cm3,
                    nf_2024_2050_4.5_cm4, nf_2024_2050_4.5_cm5, nf_2024_2050_4.5_cm6,
                    nf_2024_2050_4.5_cm7, nf_2024_2050_4.5_cm8, nf_2024_2050_4.5_cm9)

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
gc()

bh.gull_vp_2024_2050_4.5_cm1 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm2 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm02_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm3 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm03_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm4 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm04_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm5 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm05_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm6 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm06_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm7 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm07_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm8 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm08_2024_2050_reach.csv")
bh.gull_vp_2024_2050_4.5_cm9 <- fun_bh.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm09_2024_2050_reach.csv")

bh.gull_vp_4.5_NF_list <- list (bh.gull_vp_2024_2050_4.5_cm1, bh.gull_vp_2024_2050_4.5_cm2,
                           bh.gull_vp_2024_2050_4.5_cm3, bh.gull_vp_2024_2050_4.5_cm4,
                           bh.gull_vp_2024_2050_4.5_cm5, bh.gull_vp_2024_2050_4.5_cm6,
                           bh.gull_vp_2024_2050_4.5_cm7, bh.gull_vp_2024_2050_4.5_cm8,
                           bh.gull_vp_2024_2050_4.5_cm9)

####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks


######### incubating 20.04 - 31.05 ###################################

# narrow down the period to incubation and calculate mean
fun_bh.gull_vp_incub <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 11 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
      }

bh.gull_nf_4.5_incub_gr.1_list  <- lapply( bh.gull_vp_4.5_NF_list, lapply, fun_bh.gull_vp_incub)

######### rearing chicks 1.05 - 10.06 #################################

# narrow down the period to incubation and calculate mean
fun_bh.gull_vp_rear <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 1 | month == 6 & day <= 10) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_rear")) #rename columns
}

bh.gull_nf_4.5_rear_gr.1_list  <- lapply( bh.gull_vp_4.5_NF_list, lapply, fun_bh.gull_vp_rear)

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
bh.gull_vp_4.5_NF <-
data_4.5_NF <- 

# calculate percentiles and quartiles
data_4.5_NF_q0.95_period <- lapply(data_4.5_NF, lapply, function(x) 
  ddply(x,.(subbasin), summarize,
        P0.95=quantile(flow, 0.95)#find the 95% percentile for the period 2024-2050
        ))

#count how many days during the vulnerability period are higher than 95% quartile 
#
nf_4.5_bh.gull.gr4 <-  Map(function(x,y)
                                  aggregate(flow > P0.95, 
                                        merge(x, y, all = TRUE, na.action = 'na.pass'), 
                                        sum, na.rm = TRUE, na.action = 'na.pass'), 
                          bh.gull_vp_4.5_NF, data_4.5_NF_q0.95_period)

nf_4.5_bh.gull.gr4 <- purrr::lmap (bh.gull_vp_4.5_NF_list, data_4.5_NF_q0.95_period, .f=function(x, y)
            {aggregate(flow > P0.95, 
            merge(sublist(x), sublist(y), all = TRUE, na.action = 'na.pass'), 
            sum, na.rm = TRUE, na.action = 'na.pass')})

test<- lmap(.f=function(x, y){  aggregate(flow > P0.95, merge(x, y, all = TRUE,
                  na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass')}, 
    bh.gull_vp_4.5_NF_list, data_4.5_NF_q0.95_period)

bh.gull_vp_4.5_NF_list[[2]]
# remove Year appearing twice  
bh.gull_list_gr.4 <-  lapply(bh.gull_gr4_parts , "[", -c(4))



# 4.5 NF

# 4.5 FF

# 8.5 NF

# 8.5 FF

