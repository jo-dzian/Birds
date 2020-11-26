# SCRIPT NO. 5 B


setwd ("D:/Ptaki_hydro/Obliczenia/R/IHA")

library(plyr)
library(readr)

subbasins_nr <- data.frame(rbind("910","950", "1012", "1087", "1134","1240","1264","1289", "1329","1358","1501", "1545",
                                 "1565","1601","1629", "1727", "1748","1875"))
colnames(subbasins_nr)<- c("RCH")

#################################################################################################
######### 4.5 NF ################################################################################
######### 4.5 NF ################################################################################
######### 4.5 NF ################################################################################
######### 4.5 NF ################################################################################
######### 4.5 NF ################################################################################
######### 4.5 NF ################################################################################
#################################################################################################


#extract data for full year for subbasins of interest
fun_sub_allyear <- function(x){
  in_data <- read.csv(x, header = TRUE, sep = ",")
  in_data2 <- in_data[ in_data$subbasin %in% subbasins_nr$RCH, ]
  in_data3 <- split(in_data2, in_data2$subbasin)
}

#clear memory as this process needs a lot of it
gc()
# extract data for subbasins of interest
#nf_2024_2050_4.5_cm1 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm2 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm02_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm3 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm03_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm4 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm04_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm5 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm05_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm6 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm06_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm7 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm07_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm8 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm08_2024_2050_reach.csv")
#nf_2024_2050_4.5_cm9 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm09_2024_2050_reach.csv")

#data_4.5_NF <- list(nf_2024_2050_4.5_cm1, nf_2024_2050_4.5_cm2, nf_2024_2050_4.5_cm3,
#                    nf_2024_2050_4.5_cm4, nf_2024_2050_4.5_cm5, nf_2024_2050_4.5_cm6,
#                    nf_2024_2050_4.5_cm7, nf_2024_2050_4.5_cm8, nf_2024_2050_4.5_cm9)

##!!!!!!! # I got subbasin 1746 mixed up with subbasin 1748, it should be the latter

# extract data for vulnerability period for m.gull for subbasins of interest

fun_m.gull_vp <- function(x){
  in_data <- read.csv(x, header = TRUE, sep = ",")
  in_data2 <- in_data[ in_data$subbasin %in% subbasins_nr$RCH, ]
  in_data2$date2 <- as.POSIXct(in_data2$date, format="%Y-%m-%d")
  month <- as.integer(format(in_data2$date2, '%m'))
  day <- as.integer(format(in_data2$date2, '%d'))
  in_data3 <- filter(in_data2, month == 4 & day >= 21 | month == 5 | month == 6 & day <= 30)
  in_data4 <- split(in_data3, in_data3$subbasin)
}

gc()
# to avoid Error: $ operator is invalid for atomic vectors - load packages below
library("tidyverse")
library("dplyr")
library("packrat")
library("tibble")
library("ggplot2")
library("lubridate")
library("purrr")
library("ggforce")

m.gull_vp_2024_2050_4.5_cm1 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm2 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm02_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm3 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm03_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm4 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm04_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm5 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm05_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm6 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm06_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm7 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm07_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm8 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm08_2024_2050_reach.csv")
m.gull_vp_2024_2050_4.5_cm9 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm09_2024_2050_reach.csv")

m.gull_vp_4.5_NF_list <- list (m.gull_vp_2024_2050_4.5_cm1, m.gull_vp_2024_2050_4.5_cm2,
                                m.gull_vp_2024_2050_4.5_cm3, m.gull_vp_2024_2050_4.5_cm4,
                                m.gull_vp_2024_2050_4.5_cm5, m.gull_vp_2024_2050_4.5_cm6,
                                m.gull_vp_2024_2050_4.5_cm7, m.gull_vp_2024_2050_4.5_cm8,
                                m.gull_vp_2024_2050_4.5_cm9)

####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 21.04 - 31.05 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_le <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 21 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_nf_4.5_le_gr.1_list  <- lapply( m.gull_vp_4.5_NF_list, lapply, fun_m.gull_vp_le)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_4.5_le_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(m.gull_nf_4.5_le_gr.1_list[[9]], .id = "id")), 
                                             .id = "model") 

m.gull_nf_4.5_le_gr.1_p1 <- aggregate(m.gull_nf_4.5_le_gr.1_unlist$mean_le, 
                                         by=list(m.gull_nf_4.5_le_gr.1_unlist$subbasin), 
                                         FUN=mean)

write.csv(m.gull_nf_4.5_le_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_4.5_le_gr.1.csv")

######### incubating 1.05 - 20.06 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_incub <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 1 | month == 6 & day <= 20) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_nf_4.5_incub_gr.1_list  <- lapply( m.gull_vp_4.5_NF_list, lapply, fun_m.gull_vp_incub)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_4.5_incub_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[1]], .id = "id"), 
                                                   "mod.2" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[2]], .id = "id"),
                                                   "mod.3" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[3]], .id = "id"),
                                                   "mod.4" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[4]], .id = "id"),
                                                   "mod.5" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[5]], .id = "id"),
                                                   "mod.6" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[6]], .id = "id"),
                                                   "mod.7" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[7]], .id = "id"),
                                                   "mod.8" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[8]], .id = "id"),
                                                   "mod.9" = bind_rows(m.gull_nf_4.5_incub_gr.1_list[[9]], .id = "id")), 
                                              .id = "model") 

m.gull_nf_4.5_incub_gr.1_p1 <- aggregate(m.gull_nf_4.5_incub_gr.1_unlist$mean_incub, 
                                          by=list(m.gull_nf_4.5_incub_gr.1_unlist$subbasin), 
                                          FUN=mean)

write.csv(m.gull_nf_4.5_incub_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_4.5_incub_gr.1.csv")

######### rearing chicks 21.05 - 30.06 #################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_rear <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 21 | month == 6 & day <= 30) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_rear")) #rename columns
}

m.gull_nf_4.5_rear_gr.1_list  <- lapply( m.gull_vp_4.5_NF_list, lapply, fun_m.gull_vp_rear)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_4.5_rear_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(m.gull_nf_4.5_rear_gr.1_list[[9]], .id = "id")), 
                                             .id = "model") 

m.gull_nf_4.5_rear_gr.1_p1 <- aggregate(m.gull_nf_4.5_rear_gr.1_unlist$mean_rear, 
                                         by=list(m.gull_nf_4.5_rear_gr.1_unlist$subbasin), 
                                         FUN=mean)

write.csv(m.gull_nf_4.5_rear_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_4.5_rear_gr.1.csv")



##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max

library("zoo")

#calculating rolling 1,3 an 7 day mean on list

fun_m.gull_data_roll_list <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$flow, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$flow, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$flow, k = 7, fill = NA))
  step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
  step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
  month <- as.integer(format(step1$date2, '%m'))
  day <- as.integer(format(step1$date2, '%d'))
  in_data3 <- filter(step1, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)}

#apply to list
data_4.5_NF_roll_list  <- lapply( data_4.5_NF, lapply, fun_m.gull_data_roll_list)

# calculate the minimum and maximum per year per RCH

library(plyr)
fun_data_roll_list_max <- function(x) { 
  ddply(x,.(subbasin,Year), summarize,
        day01_max=max(day01_mean),
        day03_max=max(day03_mean),
        day07_max=max(day07_mean) 
  )}

m.gull_nf_4.5_list_gr.2  <- lapply( data_4.5_NF_roll_list, lapply, fun_data_roll_list_max)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_4.5_gr.2_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_4.5_list_gr.2[[1]], .id = "id"), 
                                             "mod.2" = bind_rows(m.gull_nf_4.5_list_gr.2[[2]], .id = "id"),
                                             "mod.3" = bind_rows(m.gull_nf_4.5_list_gr.2[[3]], .id = "id"),
                                             "mod.4" = bind_rows(m.gull_nf_4.5_list_gr.2[[4]], .id = "id"),
                                             "mod.5" = bind_rows(m.gull_nf_4.5_list_gr.2[[5]], .id = "id"),
                                             "mod.6" = bind_rows(m.gull_nf_4.5_list_gr.2[[6]], .id = "id"),
                                             "mod.7" = bind_rows(m.gull_nf_4.5_list_gr.2[[7]], .id = "id"),
                                             "mod.8" = bind_rows(m.gull_nf_4.5_list_gr.2[[8]], .id = "id"),
                                             "mod.9" = bind_rows(m.gull_nf_4.5_list_gr.2[[9]], .id = "id")), 
                                        .id = "model") 

m.gull_nf_4.5_gr.2_unlist_p1 <- cbind(setNames(aggregate(m.gull_nf_4.5_gr.2_unlist$day01_max, 
                                                          by=list(m.gull_nf_4.5_gr.2_unlist$subbasin), 
                                                          FUN=mean), c("subbasin", "day01_mean")),
                                       setNames(aggregate(m.gull_nf_4.5_gr.2_unlist$day03_max, 
                                                          by=list(m.gull_nf_4.5_gr.2_unlist$subbasin), 
                                                          FUN=mean), c("subbasin", "day03_mean")),
                                       setNames(aggregate(m.gull_nf_4.5_gr.2_unlist$day07_max, 
                                                          by=list(m.gull_nf_4.5_gr.2_unlist$subbasin), 
                                                          FUN=mean), c("subbasin", "day07_mean")))

write.csv(m.gull_nf_4.5_gr.2_unlist_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_4.5_gr.2.csv")

##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above Q3

gc()
# calculate percentiles and quartiles
fun_data_q3_period <- function(x) {
  ddply(x,.(subbasin), summarize,
        Q3=quantile(flow, 0.75))#find the 75% percentile for the period 2024-2050
}

data_4.5_NF_q3_period_in <- lapply(data_4.5_NF_julian, lapply, fun_data_q3_period)

fun_add_year <- function (x){
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y");return(x)}

m.gull_vp_4.5_NF_list_y <- lapply(m.gull_vp_4.5_NF_list, lapply, fun_add_year)


m.gull_vp_4.5_NF_list_a <- bind_rows(list("mod.1" = bind_rows( m.gull_vp_4.5_NF_list_y[[1]] , .id = "id"), 
                                           "mod.2" = bind_rows( m.gull_vp_4.5_NF_list_y[[2]], .id = "id"),
                                           "mod.3" = bind_rows( m.gull_vp_4.5_NF_list_y[[3]], .id = "id"),
                                           "mod.4" = bind_rows( m.gull_vp_4.5_NF_list_y[[4]], .id = "id"),
                                           "mod.5" = bind_rows( m.gull_vp_4.5_NF_list_y[[5]], .id = "id"),
                                           "mod.6" = bind_rows( m.gull_vp_4.5_NF_list_y[[6]], .id = "id"),
                                           "mod.7" = bind_rows( m.gull_vp_4.5_NF_list_y[[7]], .id = "id"),
                                           "mod.8" = bind_rows( m.gull_vp_4.5_NF_list_y[[8]], .id = "id"),
                                           "mod.9" = bind_rows( m.gull_vp_4.5_NF_list_y[[9]], .id = "id")),
                                      .id = "model") 

data_4.5_NF_q3_period_list_a <- bind_rows(list("mod.1" = bind_rows(data_4.5_NF_q3_period_in[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(data_4.5_NF_q3_period_in[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(data_4.5_NF_q3_period_in[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(data_4.5_NF_q3_period_in[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(data_4.5_NF_q3_period_in[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(data_4.5_NF_q3_period_in[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(data_4.5_NF_q3_period_in[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(data_4.5_NF_q3_period_in[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(data_4.5_NF_q3_period_in[[9]], .id = "id")), 
                                             .id = "model") 

#count how many days during the vulnerability period are higher than 95% quartile 
#
m.gull_nf_4.5_gr.4_p1 <- full_join(m.gull_vp_4.5_NF_list_a, data_4.5_NF_q3_period_list_a, 
                                    by=c("model","subbasin")) %>% 
  mutate(condition = (flow > P3))  

m.gull_nf_4.5_gr.4_p1$condition2 <- as.integer(m.gull_nf_4.5_gr.4_p1$condition)


m.gull_nf_4.5_gr.4_p2 <- aggregate(m.gull_nf_4.5_gr.4_p1$condition2, 
                                    by=list(m.gull_nf_4.5_gr.4_p1$model, m.gull_nf_4.5_gr.4_p1$subbasin), FUN=sum)

#mean number of days above Q3 per the 27 year period 2024-2050
m.gull_nf_4.5_gr.4_p2$yearly <- m.gull_nf_4.5_gr.4_p2$x/27

# calculate a mean out of all the 9 models per subbasin
m.gull_nf_4.5_gr.4_p3 <- aggregate(m.gull_nf_4.5_gr.4_p2$yearly, 
                                    by=list(m.gull_nf_4.5_gr.4_p2$Group.2), FUN=mean)

# write gr.4 to a file
write.csv(m.gull_nf_4.5_gr.4_p3, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_4.5_gr.4.csv")


#################################################################################################
######### 4.5 FF ################################################################################
######### 4.5 FF ################################################################################
######### 4.5 FF ################################################################################
######### 4.5 FF ################################################################################
######### 4.5 FF ################################################################################
######### 4.5 FF ################################################################################
#################################################################################################

#extract data for full year for subbasins of interest
#function: fun_sub_allyear 

#clear memory as this process needs a lot of it
gc()
# extract data for subbasins of interest
#ff_2074_2100_4.5_cm1 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm01_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm2 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm02_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm3 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm03_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm4 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm04_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm5 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm05_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm6 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm06_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm7 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm07_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm8 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm08_2074_2100_reach.csv")
#ff_2074_2100_4.5_cm9 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm09_2074_2100_reach.csv")

#data_4.5_FF <- list(ff_2074_2100_4.5_cm1, ff_2074_2100_4.5_cm2, ff_2074_2100_4.5_cm3,
#                    ff_2074_2100_4.5_cm4, ff_2074_2100_4.5_cm5, ff_2074_2100_4.5_cm6,
#                    ff_2074_2100_4.5_cm7, ff_2074_2100_4.5_cm8, ff_2074_2100_4.5_cm9)

##!!!!!!! # I got subbasin 1746 mixed up with subbasin 1748, it should be the latter

# extract data for vulnerability period for m.gull for subbasins of interest
#function: fun_m.gull_vp 

m.gull_vp_2074_2100_4.5_cm1 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm01_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm2 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm02_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm3 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm03_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm4 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm04_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm5 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm05_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm6 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm06_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm7 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm07_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm8 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm08_2074_2100_reach.csv")
m.gull_vp_2074_2100_4.5_cm9 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_FF/rcp45_cm09_2074_2100_reach.csv")

m.gull_vp_4.5_FF_list <- list (m.gull_vp_2074_2100_4.5_cm1, m.gull_vp_2074_2100_4.5_cm2,
                                m.gull_vp_2074_2100_4.5_cm3, m.gull_vp_2074_2100_4.5_cm4,
                                m.gull_vp_2074_2100_4.5_cm5, m.gull_vp_2074_2100_4.5_cm6,
                                m.gull_vp_2074_2100_4.5_cm7, m.gull_vp_2074_2100_4.5_cm8,
                                m.gull_vp_2074_2100_4.5_cm9)


####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 21.04 - 31.05 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_le <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 21 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_ff_4.5_le_gr.1_list  <- lapply( m.gull_vp_4.5_FF_list, lapply, fun_m.gull_vp_le)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_4.5_le_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[2]], .id = "id"),
                                               "mod.3" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[3]], .id = "id"),
                                               "mod.4" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[4]], .id = "id"),
                                               "mod.5" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[5]], .id = "id"),
                                               "mod.6" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[6]], .id = "id"),
                                               "mod.7" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[7]], .id = "id"),
                                               "mod.8" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[8]], .id = "id"),
                                               "mod.9" = bind_rows(m.gull_ff_4.5_le_gr.1_list[[9]], .id = "id")), 
                                          .id = "model") 

m.gull_ff_4.5_le_gr.1_p1 <- aggregate(m.gull_ff_4.5_le_gr.1_unlist$mean_le, 
                                      by=list(m.gull_ff_4.5_le_gr.1_unlist$subbasin), 
                                      FUN=mean)

write.csv(m.gull_ff_4.5_le_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_4.5_le_gr.1.csv")

######### incubating 1.05 - 20.06 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_incub <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 1 | month == 6 & day <= 20) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_ff_4.5_incub_gr.1_list  <- lapply( m.gull_vp_4.5_FF_list, lapply, fun_m.gull_vp_incub)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_4.5_incub_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(m.gull_ff_4.5_incub_gr.1_list[[9]], .id = "id")), 
                                             .id = "model") 

m.gull_ff_4.5_incub_gr.1_p1 <- aggregate(m.gull_ff_4.5_incub_gr.1_unlist$mean_incub, 
                                         by=list(m.gull_ff_4.5_incub_gr.1_unlist$subbasin), 
                                         FUN=mean)

write.csv(m.gull_ff_4.5_incub_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_4.5_incub_gr.1.csv")

######### rearing chicks 21.05 - 30.06 #################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_rear <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 21 | month == 6 & day <= 30) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_rear")) #rename columns
}

m.gull_ff_4.5_rear_gr.1_list  <- lapply( m.gull_vp_4.5_FF_list, lapply, fun_m.gull_vp_rear)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_4.5_rear_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[1]], .id = "id"), 
                                                 "mod.2" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[2]], .id = "id"),
                                                 "mod.3" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[3]], .id = "id"),
                                                 "mod.4" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[4]], .id = "id"),
                                                 "mod.5" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[5]], .id = "id"),
                                                 "mod.6" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[6]], .id = "id"),
                                                 "mod.7" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[7]], .id = "id"),
                                                 "mod.8" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[8]], .id = "id"),
                                                 "mod.9" = bind_rows(m.gull_ff_4.5_rear_gr.1_list[[9]], .id = "id")), 
                                            .id = "model") 

m.gull_ff_4.5_rear_gr.1_p1 <- aggregate(m.gull_ff_4.5_rear_gr.1_unlist$mean_rear, 
                                        by=list(m.gull_ff_4.5_rear_gr.1_unlist$subbasin), 
                                        FUN=mean)

write.csv(m.gull_ff_4.5_rear_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_4.5_rear_gr.1.csv")



##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max

library("zoo")

#calculating rolling 1,3 an 7 day mean on list

fun_m.gull_data_roll_list <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$flow, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$flow, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$flow, k = 7, fill = NA))
  step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
  step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
  month <- as.integer(format(step1$date2, '%m'))
  day <- as.integer(format(step1$date2, '%d'))
  in_data3 <- filter(step1, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)}

#apply to list
data_4.5_FF_roll_list  <- lapply( data_4.5_FF, lapply, fun_m.gull_data_roll_list)

# calculate the minimum and maximum per year per RCH

library(plyr)
fun_data_roll_list_max <- function(x) { 
  ddply(x,.(subbasin,Year), summarize,
        day01_max=max(day01_mean),
        day03_max=max(day03_mean),
        day07_max=max(day07_mean) 
  )}

m.gull_ff_4.5_list_gr.2  <- lapply( data_4.5_FF_roll_list, lapply, fun_data_roll_list_max)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_4.5_gr.2_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_4.5_list_gr.2[[1]], .id = "id"), 
                                            "mod.2" = bind_rows(m.gull_ff_4.5_list_gr.2[[2]], .id = "id"),
                                            "mod.3" = bind_rows(m.gull_ff_4.5_list_gr.2[[3]], .id = "id"),
                                            "mod.4" = bind_rows(m.gull_ff_4.5_list_gr.2[[4]], .id = "id"),
                                            "mod.5" = bind_rows(m.gull_ff_4.5_list_gr.2[[5]], .id = "id"),
                                            "mod.6" = bind_rows(m.gull_ff_4.5_list_gr.2[[6]], .id = "id"),
                                            "mod.7" = bind_rows(m.gull_ff_4.5_list_gr.2[[7]], .id = "id"),
                                            "mod.8" = bind_rows(m.gull_ff_4.5_list_gr.2[[8]], .id = "id"),
                                            "mod.9" = bind_rows(m.gull_ff_4.5_list_gr.2[[9]], .id = "id")), 
                                       .id = "model") 

m.gull_ff_4.5_gr.2_unlist_p1 <- cbind(setNames(aggregate(m.gull_ff_4.5_gr.2_unlist$day01_max, 
                                                         by=list(m.gull_ff_4.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day01_mean")),
                                      setNames(aggregate(m.gull_ff_4.5_gr.2_unlist$day03_max, 
                                                         by=list(m.gull_ff_4.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day03_mean")),
                                      setNames(aggregate(m.gull_ff_4.5_gr.2_unlist$day07_max, 
                                                         by=list(m.gull_ff_4.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day07_mean")))

write.csv(m.gull_ff_4.5_gr.2_unlist_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_4.5_gr.2.csv")

##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above Q3

gc()
# calculate percentiles and quartiles
fun_data_q3_period <- function(x) {
  ddply(x,.(subbasin), summarize,
        Q3=quantile(flow, 0.75))#find the 75% percentile for the period 2024-2050
}

data_4.5_FF_q3_period_in <- lapply(data_4.5_FF_julian, lapply, fun_data_q3_period)

fun_add_year <- function (x){
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y");return(x)}

m.gull_vp_4.5_FF_list_y <- lapply(m.gull_vp_4.5_FF_list, lapply, fun_add_year)


m.gull_vp_4.5_FF_list_a <- bind_rows(list("mod.1" = bind_rows( m.gull_vp_4.5_FF_list_y[[1]] , .id = "id"), 
                                          "mod.2" = bind_rows( m.gull_vp_4.5_FF_list_y[[2]], .id = "id"),
                                          "mod.3" = bind_rows( m.gull_vp_4.5_FF_list_y[[3]], .id = "id"),
                                          "mod.4" = bind_rows( m.gull_vp_4.5_FF_list_y[[4]], .id = "id"),
                                          "mod.5" = bind_rows( m.gull_vp_4.5_FF_list_y[[5]], .id = "id"),
                                          "mod.6" = bind_rows( m.gull_vp_4.5_FF_list_y[[6]], .id = "id"),
                                          "mod.7" = bind_rows( m.gull_vp_4.5_FF_list_y[[7]], .id = "id"),
                                          "mod.8" = bind_rows( m.gull_vp_4.5_FF_list_y[[8]], .id = "id"),
                                          "mod.9" = bind_rows( m.gull_vp_4.5_FF_list_y[[9]], .id = "id")),
                                     .id = "model") 

data_4.5_FF_q3_period_list_a <- bind_rows(list("mod.1" = bind_rows(data_4.5_FF_q3_period_in[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(data_4.5_FF_q3_period_in[[2]], .id = "id"),
                                               "mod.3" = bind_rows(data_4.5_FF_q3_period_in[[3]], .id = "id"),
                                               "mod.4" = bind_rows(data_4.5_FF_q3_period_in[[4]], .id = "id"),
                                               "mod.5" = bind_rows(data_4.5_FF_q3_period_in[[5]], .id = "id"),
                                               "mod.6" = bind_rows(data_4.5_FF_q3_period_in[[6]], .id = "id"),
                                               "mod.7" = bind_rows(data_4.5_FF_q3_period_in[[7]], .id = "id"),
                                               "mod.8" = bind_rows(data_4.5_FF_q3_period_in[[8]], .id = "id"),
                                               "mod.9" = bind_rows(data_4.5_FF_q3_period_in[[9]], .id = "id")), 
                                          .id = "model") 

#count how many days during the vulnerability period are higher than 95% quartile 
#
m.gull_ff_4.5_gr.4_p1 <- full_join(m.gull_vp_4.5_FF_list_a, data_4.5_FF_q3_period_list_a, 
                                   by=c("model","subbasin")) %>% 
  mutate(condition = (flow > P3))  

m.gull_ff_4.5_gr.4_p1$condition2 <- as.integer(m.gull_ff_4.5_gr.4_p1$condition)


m.gull_ff_4.5_gr.4_p2 <- aggregate(m.gull_ff_4.5_gr.4_p1$condition2, 
                                   by=list(m.gull_ff_4.5_gr.4_p1$model, m.gull_ff_4.5_gr.4_p1$subbasin), FUN=sum)

#mean number of days above Q3 per the 27 year period 2024-2050
m.gull_ff_4.5_gr.4_p2$yearly <- m.gull_ff_4.5_gr.4_p2$x/27

# calculate a mean out of all the 9 models per subbasin
m.gull_ff_4.5_gr.4_p3 <- aggregate(m.gull_ff_4.5_gr.4_p2$yearly, 
                                   by=list(m.gull_ff_4.5_gr.4_p2$Group.2), FUN=mean)

# write gr.4 to a file
write.csv(m.gull_ff_4.5_gr.4_p3, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_4.5_gr.4.csv")



#################################################################################################
######### 8.5 NF ################################################################################
######### 8.5 NF ################################################################################
######### 8.5 NF ################################################################################
######### 8.5 NF ################################################################################
######### 8.5 NF ################################################################################
######### 8.5 NF ################################################################################
#################################################################################################

#extract data for full year for subbasins of interest
#function: fun_sub_allyear 

#clear memory as this process needs a lot of it
gc()
# extract data for subbasins of interest
#nf_2024_2050_8.5_cm1 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm01_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm2 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm02_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm3 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm03_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm4 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm04_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm5 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm05_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm6 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm06_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm7 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm07_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm8 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm08_2024_2050_reach.csv")
#nf_2024_2050_8.5_cm9 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm09_2024_2050_reach.csv")

#data_8.5_NF <- list(nf_2024_2050_8.5_cm1, nf_2024_2050_8.5_cm2, nf_2024_2050_8.5_cm3,
#                    nf_2024_2050_8.5_cm4, nf_2024_2050_8.5_cm5, nf_2024_2050_8.5_cm6,
#                    nf_2024_2050_8.5_cm7, nf_2024_2050_8.5_cm8, nf_2024_2050_8.5_cm9)

##!!!!!!! # I got subbasin 1746 mixed up with subbasin 1748, it should be the latter

# extract data for vulnerability period for m.gull for subbasins of interest
#function: fun_m.gull_vp 

m.gull_vp_2024_2050_8.5_cm1 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm01_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm2 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm02_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm3 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm03_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm4 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm04_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm5 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm05_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm6 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm06_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm7 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm07_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm8 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm08_2024_2050_reach.csv")
m.gull_vp_2024_2050_8.5_cm9 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_NF/rcp85_cm09_2024_2050_reach.csv")

m.gull_vp_8.5_NF_list <- list (m.gull_vp_2024_2050_8.5_cm1, m.gull_vp_2024_2050_8.5_cm2,
                                m.gull_vp_2024_2050_8.5_cm3, m.gull_vp_2024_2050_8.5_cm4,
                                m.gull_vp_2024_2050_8.5_cm5, m.gull_vp_2024_2050_8.5_cm6,
                                m.gull_vp_2024_2050_8.5_cm7, m.gull_vp_2024_2050_8.5_cm8,
                                m.gull_vp_2024_2050_8.5_cm9)


####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 21.04 - 31.05 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_le <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 21 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_nf_8.5_le_gr.1_list  <- lapply( m.gull_vp_8.5_NF_list, lapply, fun_m.gull_vp_le)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_8.5_le_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[2]], .id = "id"),
                                               "mod.3" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[3]], .id = "id"),
                                               "mod.4" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[4]], .id = "id"),
                                               "mod.5" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[5]], .id = "id"),
                                               "mod.6" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[6]], .id = "id"),
                                               "mod.7" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[7]], .id = "id"),
                                               "mod.8" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[8]], .id = "id"),
                                               "mod.9" = bind_rows(m.gull_nf_8.5_le_gr.1_list[[9]], .id = "id")), 
                                          .id = "model") 

m.gull_nf_8.5_le_gr.1_p1 <- aggregate(m.gull_nf_8.5_le_gr.1_unlist$mean_le, 
                                      by=list(m.gull_nf_8.5_le_gr.1_unlist$subbasin), 
                                      FUN=mean)

write.csv(m.gull_nf_8.5_le_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_8.5_le_gr.1.csv")

######### incubating 1.05 - 20.06 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_incub <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 1 | month == 6 & day <= 20) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_nf_8.5_incub_gr.1_list  <- lapply( m.gull_vp_8.5_NF_list, lapply, fun_m.gull_vp_incub)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_8.5_incub_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(m.gull_nf_8.5_incub_gr.1_list[[9]], .id = "id")), 
                                             .id = "model") 

m.gull_nf_8.5_incub_gr.1_p1 <- aggregate(m.gull_nf_8.5_incub_gr.1_unlist$mean_incub, 
                                         by=list(m.gull_nf_8.5_incub_gr.1_unlist$subbasin), 
                                         FUN=mean)

write.csv(m.gull_nf_8.5_incub_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_8.5_incub_gr.1.csv")

######### rearing chicks 21.05 - 30.06 #################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_rear <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 21 | month == 6 & day <= 30) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_rear")) #rename columns
}

m.gull_nf_8.5_rear_gr.1_list  <- lapply( m.gull_vp_8.5_NF_list, lapply, fun_m.gull_vp_rear)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_8.5_rear_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[1]], .id = "id"), 
                                                 "mod.2" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[2]], .id = "id"),
                                                 "mod.3" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[3]], .id = "id"),
                                                 "mod.4" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[4]], .id = "id"),
                                                 "mod.5" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[5]], .id = "id"),
                                                 "mod.6" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[6]], .id = "id"),
                                                 "mod.7" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[7]], .id = "id"),
                                                 "mod.8" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[8]], .id = "id"),
                                                 "mod.9" = bind_rows(m.gull_nf_8.5_rear_gr.1_list[[9]], .id = "id")), 
                                            .id = "model") 

m.gull_nf_8.5_rear_gr.1_p1 <- aggregate(m.gull_nf_8.5_rear_gr.1_unlist$mean_rear, 
                                        by=list(m.gull_nf_8.5_rear_gr.1_unlist$subbasin), 
                                        FUN=mean)

write.csv(m.gull_nf_8.5_rear_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_8.5_rear_gr.1.csv")



##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max

library("zoo")

#calculating rolling 1,3 an 7 day mean on list

fun_m.gull_data_roll_list <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$flow, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$flow, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$flow, k = 7, fill = NA))
  step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
  step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
  month <- as.integer(format(step1$date2, '%m'))
  day <- as.integer(format(step1$date2, '%d'))
  in_data3 <- filter(step1, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)}

#apply to list
data_8.5_NF_roll_list  <- lapply( data_8.5_NF, lapply, fun_m.gull_data_roll_list)

# calculate the minimum and maximum per year per RCH

library(plyr)
fun_data_roll_list_max <- function(x) { 
  ddply(x,.(subbasin,Year), summarize,
        day01_max=max(day01_mean),
        day03_max=max(day03_mean),
        day07_max=max(day07_mean) 
  )}

m.gull_nf_8.5_list_gr.2  <- lapply( data_8.5_NF_roll_list, lapply, fun_data_roll_list_max)

#obtain results for the reaches as a mean from 9 models
m.gull_nf_8.5_gr.2_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_nf_8.5_list_gr.2[[1]], .id = "id"), 
                                            "mod.2" = bind_rows(m.gull_nf_8.5_list_gr.2[[2]], .id = "id"),
                                            "mod.3" = bind_rows(m.gull_nf_8.5_list_gr.2[[3]], .id = "id"),
                                            "mod.4" = bind_rows(m.gull_nf_8.5_list_gr.2[[4]], .id = "id"),
                                            "mod.5" = bind_rows(m.gull_nf_8.5_list_gr.2[[5]], .id = "id"),
                                            "mod.6" = bind_rows(m.gull_nf_8.5_list_gr.2[[6]], .id = "id"),
                                            "mod.7" = bind_rows(m.gull_nf_8.5_list_gr.2[[7]], .id = "id"),
                                            "mod.8" = bind_rows(m.gull_nf_8.5_list_gr.2[[8]], .id = "id"),
                                            "mod.9" = bind_rows(m.gull_nf_8.5_list_gr.2[[9]], .id = "id")), 
                                       .id = "model") 

m.gull_nf_8.5_gr.2_unlist_p1 <- cbind(setNames(aggregate(m.gull_nf_8.5_gr.2_unlist$day01_max, 
                                                         by=list(m.gull_nf_8.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day01_mean")),
                                      setNames(aggregate(m.gull_nf_8.5_gr.2_unlist$day03_max, 
                                                         by=list(m.gull_nf_8.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day03_mean")),
                                      setNames(aggregate(m.gull_nf_8.5_gr.2_unlist$day07_max, 
                                                         by=list(m.gull_nf_8.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day07_mean")))

write.csv(m.gull_nf_8.5_gr.2_unlist_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_8.5_gr.2.csv")

##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above Q3

gc()
# calculate percentiles and quartiles
fun_data_q3_period <- function(x) {
  ddply(x,.(subbasin), summarize,
        Q3=quantile(flow, 0.75))#find the 75% percentile for the period 2024-2050
}

data_8.5_NF_q3_period_in <- lapply(data_8.5_NF_julian, lapply, fun_data_q3_period)

fun_add_year <- function (x){
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y");return(x)}

m.gull_vp_8.5_NF_list_y <- lapply(m.gull_vp_8.5_NF_list, lapply, fun_add_year)


m.gull_vp_8.5_NF_list_a <- bind_rows(list("mod.1" = bind_rows( m.gull_vp_8.5_NF_list_y[[1]] , .id = "id"), 
                                          "mod.2" = bind_rows( m.gull_vp_8.5_NF_list_y[[2]], .id = "id"),
                                          "mod.3" = bind_rows( m.gull_vp_8.5_NF_list_y[[3]], .id = "id"),
                                          "mod.4" = bind_rows( m.gull_vp_8.5_NF_list_y[[4]], .id = "id"),
                                          "mod.5" = bind_rows( m.gull_vp_8.5_NF_list_y[[5]], .id = "id"),
                                          "mod.6" = bind_rows( m.gull_vp_8.5_NF_list_y[[6]], .id = "id"),
                                          "mod.7" = bind_rows( m.gull_vp_8.5_NF_list_y[[7]], .id = "id"),
                                          "mod.8" = bind_rows( m.gull_vp_8.5_NF_list_y[[8]], .id = "id"),
                                          "mod.9" = bind_rows( m.gull_vp_8.5_NF_list_y[[9]], .id = "id")),
                                     .id = "model") 

data_8.5_NF_q3_period_list_a <- bind_rows(list("mod.1" = bind_rows(data_8.5_NF_q3_period_in[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(data_8.5_NF_q3_period_in[[2]], .id = "id"),
                                               "mod.3" = bind_rows(data_8.5_NF_q3_period_in[[3]], .id = "id"),
                                               "mod.4" = bind_rows(data_8.5_NF_q3_period_in[[4]], .id = "id"),
                                               "mod.5" = bind_rows(data_8.5_NF_q3_period_in[[5]], .id = "id"),
                                               "mod.6" = bind_rows(data_8.5_NF_q3_period_in[[6]], .id = "id"),
                                               "mod.7" = bind_rows(data_8.5_NF_q3_period_in[[7]], .id = "id"),
                                               "mod.8" = bind_rows(data_8.5_NF_q3_period_in[[8]], .id = "id"),
                                               "mod.9" = bind_rows(data_8.5_NF_q3_period_in[[9]], .id = "id")), 
                                          .id = "model") 

#count how many days during the vulnerability period are higher than 95% quartile 
#
m.gull_nf_8.5_gr.4_p1 <- full_join(m.gull_vp_8.5_NF_list_a, data_8.5_NF_q3_period_list_a, 
                                   by=c("model","subbasin")) %>% 
  mutate(condition = (flow > P3))  

m.gull_nf_8.5_gr.4_p1$condition2 <- as.integer(m.gull_nf_8.5_gr.4_p1$condition)


m.gull_nf_8.5_gr.4_p2 <- aggregate(m.gull_nf_8.5_gr.4_p1$condition2, 
                                   by=list(m.gull_nf_8.5_gr.4_p1$model, m.gull_nf_8.5_gr.4_p1$subbasin), FUN=sum)

#mean number of days above Q3 per the 27 year period 2024-2050
m.gull_nf_8.5_gr.4_p2$yearly <- m.gull_nf_8.5_gr.4_p2$x/27

# calculate a mean out of all the 9 models per subbasin
m.gull_nf_8.5_gr.4_p3 <- aggregate(m.gull_nf_8.5_gr.4_p2$yearly, 
                                   by=list(m.gull_nf_8.5_gr.4_p2$Group.2), FUN=mean)

# write gr.4 to a file
write.csv(m.gull_nf_8.5_gr.4_p3, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_nf_8.5_gr.4.csv")



#################################################################################################
######### 8.5 FF ################################################################################
######### 8.5 FF ################################################################################
######### 8.5 FF ################################################################################
######### 8.5 FF ################################################################################
######### 8.5 FF ################################################################################
######### 8.5 FF ################################################################################
#################################################################################################

#extract data for full year for subbasins of interest
#function: fun_sub_allyear 

#clear memory as this process needs a lot of it
gc()
# extract data for subbasins of interest
#ff_2074_2100_8.5_cm1 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm01_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm2 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm02_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm3 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm03_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm4 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm04_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm5 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm05_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm6 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm06_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm7 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm07_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm8 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm08_2074_2100_reach.csv")
#ff_2074_2100_8.5_cm9 <- fun_sub_allyear("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm09_2074_2100_reach.csv")

#data_8.5_FF <- list(ff_2074_2100_8.5_cm1, ff_2074_2100_8.5_cm2, ff_2074_2100_8.5_cm3,
#                   ff_2074_2100_8.5_cm4, ff_2074_2100_8.5_cm5, ff_2074_2100_8.5_cm6,
#                    ff_2074_2100_8.5_cm7, ff_2074_2100_8.5_cm8, ff_2074_2100_8.5_cm9)

##!!!!!!! # I got subbasin 1746 mixed up with subbasin 1748, it should be the latter

# extract data for vulnerability period for m.gull for subbasins of interest
#function: fun_m.gull_vp 

m.gull_vp_2074_2100_8.5_cm1 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm01_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm2 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm02_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm3 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm03_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm4 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm04_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm5 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm05_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm6 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm06_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm7 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm07_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm8 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm08_2074_2100_reach.csv")
m.gull_vp_2074_2100_8.5_cm9 <- fun_m.gull_vp("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/8.5_FF/rcp85_cm09_2074_2100_reach.csv")

m.gull_vp_8.5_FF_list <- list (m.gull_vp_2074_2100_8.5_cm1, m.gull_vp_2074_2100_8.5_cm2,
                                m.gull_vp_2074_2100_8.5_cm3, m.gull_vp_2074_2100_8.5_cm4,
                                m.gull_vp_2074_2100_8.5_cm5, m.gull_vp_2074_2100_8.5_cm6,
                                m.gull_vp_2074_2100_8.5_cm7, m.gull_vp_2074_2100_8.5_cm8,
                                m.gull_vp_2074_2100_8.5_cm9)


####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 21.04 - 31.05 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_le <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 4 & day >= 21 | month == 5 & day <= 31) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_ff_8.5_le_gr.1_list  <- lapply( m.gull_vp_8.5_FF_list, lapply, fun_m.gull_vp_le)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_8.5_le_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[2]], .id = "id"),
                                               "mod.3" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[3]], .id = "id"),
                                               "mod.4" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[4]], .id = "id"),
                                               "mod.5" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[5]], .id = "id"),
                                               "mod.6" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[6]], .id = "id"),
                                               "mod.7" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[7]], .id = "id"),
                                               "mod.8" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[8]], .id = "id"),
                                               "mod.9" = bind_rows(m.gull_ff_8.5_le_gr.1_list[[9]], .id = "id")), 
                                          .id = "model") 

m.gull_ff_8.5_le_gr.1_p1 <- aggregate(m.gull_ff_8.5_le_gr.1_unlist$mean_le, 
                                      by=list(m.gull_ff_8.5_le_gr.1_unlist$subbasin), 
                                      FUN=mean)

write.csv(m.gull_ff_8.5_le_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_8.5_le_gr.1.csv")

######### incubating 1.05 - 20.06 ###################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_incub <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 1 | month == 6 & day <= 20) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_incub")) #rename columns
}

#result in a list of lists
m.gull_ff_8.5_incub_gr.1_list  <- lapply( m.gull_vp_8.5_FF_list, lapply, fun_m.gull_vp_incub)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_8.5_incub_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[1]], .id = "id"), 
                                                  "mod.2" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[2]], .id = "id"),
                                                  "mod.3" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[3]], .id = "id"),
                                                  "mod.4" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[4]], .id = "id"),
                                                  "mod.5" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[5]], .id = "id"),
                                                  "mod.6" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[6]], .id = "id"),
                                                  "mod.7" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[7]], .id = "id"),
                                                  "mod.8" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[8]], .id = "id"),
                                                  "mod.9" = bind_rows(m.gull_ff_8.5_incub_gr.1_list[[9]], .id = "id")), 
                                             .id = "model") 

m.gull_ff_8.5_incub_gr.1_p1 <- aggregate(m.gull_ff_8.5_incub_gr.1_unlist$mean_incub, 
                                         by=list(m.gull_ff_8.5_incub_gr.1_unlist$subbasin), 
                                         FUN=mean)

write.csv(m.gull_ff_8.5_incub_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_8.5_incub_gr.1.csv")

######### rearing chicks 21.05 - 30.06 #################################

# narrow down the period to incubation and calculate mean
fun_m.gull_vp_rear <- function(x){
  month <- as.integer(format(x$date2, '%m'))
  day <- as.integer(format(x$date2, '%d'))
  in_data1 <- filter(x, month == 5 & day >= 21 | month == 6 & day <= 30) #select incubation period
  in_data2 <- aggregate(in_data1$flow, list(in_data1$subbasin), mean) #calculate mean for that period
  setNames(in_data2, c("subbasin", "mean_rear")) #rename columns
}

m.gull_ff_8.5_rear_gr.1_list  <- lapply( m.gull_vp_8.5_FF_list, lapply, fun_m.gull_vp_rear)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_8.5_rear_gr.1_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[1]], .id = "id"), 
                                                 "mod.2" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[2]], .id = "id"),
                                                 "mod.3" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[3]], .id = "id"),
                                                 "mod.4" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[4]], .id = "id"),
                                                 "mod.5" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[5]], .id = "id"),
                                                 "mod.6" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[6]], .id = "id"),
                                                 "mod.7" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[7]], .id = "id"),
                                                 "mod.8" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[8]], .id = "id"),
                                                 "mod.9" = bind_rows(m.gull_ff_8.5_rear_gr.1_list[[9]], .id = "id")), 
                                            .id = "model") 

m.gull_ff_8.5_rear_gr.1_p1 <- aggregate(m.gull_ff_8.5_rear_gr.1_unlist$mean_rear, 
                                        by=list(m.gull_ff_8.5_rear_gr.1_unlist$subbasin), 
                                        FUN=mean)

write.csv(m.gull_ff_8.5_rear_gr.1_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_8.5_rear_gr.1.csv")



##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual maxima, 1-day mean) 
# 1,3,7 day max

library("zoo")

#calculating rolling 1,3 an 7 day mean on list

fun_m.gull_data_roll_list <- function(x){
  step1 <- dplyr::mutate(x, day01_mean = zoo::rollmean (x$flow, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                         day03_mean = zoo::rollmean (x$flow, k = 3, fill = NA),
                         day07_mean = zoo::rollmean (x$flow, k = 7, fill = NA))
  step1$Year <- format(as.Date(step1$date, format="%Y-%m-%d"),"%Y")
  step1$date2 <- as.POSIXct(step1$date, format="%Y-%m-%d")
  month <- as.integer(format(step1$date2, '%m'))
  day <- as.integer(format(step1$date2, '%d'))
  in_data3 <- filter(step1, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)}

#apply to list
data_8.5_FF_roll_list  <- lapply( data_8.5_FF, lapply, fun_m.gull_data_roll_list)

# calculate the minimum and maximum per year per RCH

library(plyr)
fun_data_roll_list_max <- function(x) { 
  ddply(x,.(subbasin,Year), summarize,
        day01_max=max(day01_mean),
        day03_max=max(day03_mean),
        day07_max=max(day07_mean) 
  )}

m.gull_ff_8.5_list_gr.2  <- lapply( data_8.5_FF_roll_list, lapply, fun_data_roll_list_max)

#obtain results for the reaches as a mean from 9 models
m.gull_ff_8.5_gr.2_unlist <- bind_rows(list("mod.1" = bind_rows(m.gull_ff_8.5_list_gr.2[[1]], .id = "id"), 
                                            "mod.2" = bind_rows(m.gull_ff_8.5_list_gr.2[[2]], .id = "id"),
                                            "mod.3" = bind_rows(m.gull_ff_8.5_list_gr.2[[3]], .id = "id"),
                                            "mod.4" = bind_rows(m.gull_ff_8.5_list_gr.2[[4]], .id = "id"),
                                            "mod.5" = bind_rows(m.gull_ff_8.5_list_gr.2[[5]], .id = "id"),
                                            "mod.6" = bind_rows(m.gull_ff_8.5_list_gr.2[[6]], .id = "id"),
                                            "mod.7" = bind_rows(m.gull_ff_8.5_list_gr.2[[7]], .id = "id"),
                                            "mod.8" = bind_rows(m.gull_ff_8.5_list_gr.2[[8]], .id = "id"),
                                            "mod.9" = bind_rows(m.gull_ff_8.5_list_gr.2[[9]], .id = "id")), 
                                       .id = "model") 

m.gull_ff_8.5_gr.2_unlist_p1 <- cbind(setNames(aggregate(m.gull_ff_8.5_gr.2_unlist$day01_max, 
                                                         by=list(m.gull_ff_8.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day01_mean")),
                                      setNames(aggregate(m.gull_ff_8.5_gr.2_unlist$day03_max, 
                                                         by=list(m.gull_ff_8.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day03_mean")),
                                      setNames(aggregate(m.gull_ff_8.5_gr.2_unlist$day07_max, 
                                                         by=list(m.gull_ff_8.5_gr.2_unlist$subbasin), 
                                                         FUN=mean), c("subbasin", "day07_mean")))

write.csv(m.gull_ff_8.5_gr.2_unlist_p1, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_8.5_gr.2.csv")

##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)
# above Q3

gc()
# calculate percentiles and quartiles
fun_data_q3_period <- function(x) {
  ddply(x,.(subbasin), summarize,
        Q3=quantile(flow, 0.75))#find the 75% percentile for the period 2024-2050
}

data_8.5_FF_q3_period_in <- lapply(data_8.5_FF_julian, lapply, fun_data_q3_period)

fun_add_year <- function (x){
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y");return(x)}

m.gull_vp_8.5_FF_list_y <- lapply(m.gull_vp_8.5_FF_list, lapply, fun_add_year)


m.gull_vp_8.5_FF_list_a <- bind_rows(list("mod.1" = bind_rows( m.gull_vp_8.5_FF_list_y[[1]] , .id = "id"), 
                                          "mod.2" = bind_rows( m.gull_vp_8.5_FF_list_y[[2]], .id = "id"),
                                          "mod.3" = bind_rows( m.gull_vp_8.5_FF_list_y[[3]], .id = "id"),
                                          "mod.4" = bind_rows( m.gull_vp_8.5_FF_list_y[[4]], .id = "id"),
                                          "mod.5" = bind_rows( m.gull_vp_8.5_FF_list_y[[5]], .id = "id"),
                                          "mod.6" = bind_rows( m.gull_vp_8.5_FF_list_y[[6]], .id = "id"),
                                          "mod.7" = bind_rows( m.gull_vp_8.5_FF_list_y[[7]], .id = "id"),
                                          "mod.8" = bind_rows( m.gull_vp_8.5_FF_list_y[[8]], .id = "id"),
                                          "mod.9" = bind_rows( m.gull_vp_8.5_FF_list_y[[9]], .id = "id")),
                                     .id = "model") 

data_8.5_FF_q3_period_list_a <- bind_rows(list("mod.1" = bind_rows(data_8.5_FF_q3_period_in[[1]], .id = "id"), 
                                               "mod.2" = bind_rows(data_8.5_FF_q3_period_in[[2]], .id = "id"),
                                               "mod.3" = bind_rows(data_8.5_FF_q3_period_in[[3]], .id = "id"),
                                               "mod.4" = bind_rows(data_8.5_FF_q3_period_in[[4]], .id = "id"),
                                               "mod.5" = bind_rows(data_8.5_FF_q3_period_in[[5]], .id = "id"),
                                               "mod.6" = bind_rows(data_8.5_FF_q3_period_in[[6]], .id = "id"),
                                               "mod.7" = bind_rows(data_8.5_FF_q3_period_in[[7]], .id = "id"),
                                               "mod.8" = bind_rows(data_8.5_FF_q3_period_in[[8]], .id = "id"),
                                               "mod.9" = bind_rows(data_8.5_FF_q3_period_in[[9]], .id = "id")), 
                                          .id = "model") 

#count how many days during the vulnerability period are higher than 95% quartile 
#
m.gull_ff_8.5_gr.4_p1 <- full_join(m.gull_vp_8.5_FF_list_a, data_8.5_FF_q3_period_list_a, 
                                   by=c("model","subbasin")) %>% 
  mutate(condition = (flow > P3))  

m.gull_ff_8.5_gr.4_p1$condition2 <- as.integer(m.gull_ff_8.5_gr.4_p1$condition)


m.gull_ff_8.5_gr.4_p2 <- aggregate(m.gull_ff_8.5_gr.4_p1$condition2, 
                                   by=list(m.gull_ff_8.5_gr.4_p1$model, m.gull_ff_8.5_gr.4_p1$subbasin), FUN=sum)

#mean number of days above Q3 per the 27 year period 2024-2050
m.gull_ff_8.5_gr.4_p2$yearly <- m.gull_ff_8.5_gr.4_p2$x/27

# calculate a mean out of all the 9 models per subbasin
m.gull_ff_8.5_gr.4_p3 <- aggregate(m.gull_ff_8.5_gr.4_p2$yearly, 
                                   by=list(m.gull_ff_8.5_gr.4_p2$Group.2), FUN=mean)

# write gr.4 to a file
write.csv(m.gull_ff_8.5_gr.4_p3, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_ff_8.5_gr.4.csv")



