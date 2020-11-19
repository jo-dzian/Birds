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

library("zoo")

#calculating rolling 1,3 an 7 day mean on list

fun_bh.gull_data_roll_list <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$flow, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$flow, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$flow, k = 7, fill = NA))
step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
month <- as.integer(format(step1$date2, '%m'))
day <- as.integer(format(step1$date2, '%d'))
in_data3 <- filter(step1, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)}

#apply to list
data_4.5_NF_roll_list  <- lapply( data_4.5_NF, lapply, fun_bh.gull_data_roll_list)

# calculate the minimum and maximum per year per RCH

library(plyr)
fun_data_roll_list_max <- function(x) { 
  ddply(x,.(subbasin,Year), summarize,
         day01_max=max(day01_mean),
         day03_max=max(day03_mean),
         day07_max=max(day07_mean) 
  )}

bh.gull_nf_4.5_list_gr.2  <- lapply( data_4.5_NF_roll_list, lapply, fun_data_roll_list_max)

##############################################################################################
####### GROUP 3 ################################################################################
####### IHA group 3 is  Timing of annual extreme water conditions,
#Julian date of each annual 1-day maximum 

#bh.gull vp:
#  11.04 is 101 or 102 (Leap) julian day
#  10.06 is 161 or 162 (Leap) julian day
# Leap years: 2004, 2008, 2012, 2016

#add julian day
fun_julian <- function(x) {
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y")
  x$julian <- yday(x$date);return(x)}

gc()
data_4.5_NF_julian  <- lapply( data_4.5_NF, lapply, fun_julian)

fun_bh.gull_4.5_NF_list_gr.3 <- function(x) {
  ddply(x,.(subbasin,Year), summarize,
        max=max(flow),
        julian_max= which.max(flow),#gives julian day of the min/max 
        #chech if julian date is within vulnerability period range (101 and 162 days) and count as 1 if yes, 0 as no.
        vp_max = case_when(julian_max >= 101 & julian_max <= 162 ~ 1, TRUE ~ 0))}

bh.gull_nf_4.5_list_gr.3  <- lapply( data_4.5_NF_julian, lapply, fun_bh.gull_4.5_NF_list_gr.3)

##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above 0.95

gc()
# calculate percentiles and quartiles
fun_data_4.5_NF_q0.95_period <- function(x) {
  ddply(x,.(subbasin), summarize,
        P0.95=quantile(flow, 0.95))#find the 95% percentile for the period 2024-2050
  }

data_4.5_NF_q0.95_period_in <- lapply(data_4.5_NF_julian, lapply, fun_data_4.5_NF_q0.95_period)

fun_add_year <- function (x){
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y");return(x)}

bh.gull_vp_4.5_NF_list_y <- lapply(bh.gull_vp_4.5_NF_list, lapply, fun_add_year)


#count how many days during the vulnerability period are higher than 95% quartile 
#


test <- bind_rows(list("mod.1" = bind_rows( bh.gull_vp_4.5_NF_list_y[[1]] , .id = "id"), 
                        "mod.2" = bind_rows( bh.gull_vp_4.5_NF_list_y[[2]], .id = "id"),
                        "mod.3" = bind_rows( bh.gull_vp_4.5_NF_list_y[[3]], .id = "id"),
                        "mod.4" = bind_rows( bh.gull_vp_4.5_NF_list_y[[4]], .id = "id"),
                        "mod.5" = bind_rows( bh.gull_vp_4.5_NF_list_y[[5]], .id = "id"),
                        "mod.6" = bind_rows( bh.gull_vp_4.5_NF_list_y[[6]], .id = "id"),
                        "mod.7" = bind_rows( bh.gull_vp_4.5_NF_list_y[[7]], .id = "id"),
                        "mod.8" = bind_rows( bh.gull_vp_4.5_NF_list_y[[8]], .id = "id"),
                        "mod.9" = bind_rows( bh.gull_vp_4.5_NF_list_y[[9]], .id = "id")),
                            .id = "model") 

test2 <- bind_rows(list("mod.1" = bind_rows(data_4.5_NF_q0.95_period_in[[1]], .id = "id"), 
                        "mod.2" = bind_rows(data_4.5_NF_q0.95_period_in[[2]], .id = "id"),
                        "mod.3" = bind_rows(data_4.5_NF_q0.95_period_in[[3]], .id = "id"),
                        "mod.4" = bind_rows(data_4.5_NF_q0.95_period_in[[4]], .id = "id"),
                        "mod.5" = bind_rows(data_4.5_NF_q0.95_period_in[[5]], .id = "id"),
                        "mod.6" = bind_rows(data_4.5_NF_q0.95_period_in[[6]], .id = "id"),
                        "mod.7" = bind_rows(data_4.5_NF_q0.95_period_in[[7]], .id = "id"),
                        "mod.8" = bind_rows(data_4.5_NF_q0.95_period_in[[8]], .id = "id"),
                        "mod.9" = bind_rows(data_4.5_NF_q0.95_period_in[[9]], .id = "id")), 
                        .id = "model") 


test4 <- full_join(test, test2, by=c("model","subbasin")) %>% 
          mutate(condition = (flow > P0.95))  

test4$condition2 <- as.integer(test4$condition)
         

test5 <- aggregate(test4$condition2, by=list(test4$model, test4$subbasin), FUN=sum)

#mean number of days above Q3 per the 27 year period 2024-2050
test5$yearly <- test5$x/27

# 4.5 NF

# 4.5 FF

# 8.5 NF

# 8.5 FF
