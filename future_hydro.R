# SCRIPT NO. 5 A


setwd ("D:/Ptaki_hydro/Obliczenia/R/IHA")

library(plyr)
library(readr)

subbasins_nr <- data.frame(rbind("910","950", "1012", "1087", "1134","1240","1264","1289", "1329","1358","1501", "1545",
"1565","1601","1629", "1727","1746","1875"))
colnames(subbasins_nr)<- c("RCH")

# 4.5 NF

mydir_45_NF = "D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF"
myfiles_45_NF = list.files(path=mydir_45_NF, pattern="*.csv", full.names=TRUE)
gc() # free up memory
dat_csv_45_NF = ldply(myfiles_45_NF, read_csv)
dat_csv_45_NF <- list.files(path=mydir_45_NF, full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_cols


test <- for (i in myfiles_45_NF[-1]) {
  into <- read.csv(i, header = TRUE, sep = ",")[, c("subbasin" ,"date", "flow")]
  out <-  subset(into, "subbasin" == subbasins_nr )
  #out2 <- 
  print(out)
  #colnames(out) <- c("date", basename(myfiles_45_NF[i]))
  #result <- merge(out, by = "date")
}


test <- for (i in myfiles_45_NF) {
  in_data <- read.csv(i, header = TRUE, sep = ",")[, c("subbasin" ,"date", "flow")]
  in_data2 <- in_data[ in_data$subbasin %in% subbasins_nr$RCH, ]
  in_data2$date2 <- as.POSIXct(in_data2$date, format="%Y-%m-%d")
  month <- as.integer(format(in_data2$date2, '%m'))
  day <- as.integer(format(in_data2$date2, '%d'))
  in_data3 <- filter(in_data2, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)
  in_data4 <- split(in_data3, in_data3$subbasin)
  write.csv(in_data4, "D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/VP_bh.gull")
}

test_fun <- function(x){
  in_data <- read.csv(x, header = TRUE, sep = ",")
  in_data2 <- in_data[ in_data$subbasin %in% subbasins_nr$RCH, ]
  in_data2$date2 <- as.POSIXct(in_data2$date, format="%Y-%m-%d")
  month <- as.integer(format(in_data2$date2, '%m'))
  day <- as.integer(format(in_data2$date2, '%d'))
  in_data3 <- filter(in_data2, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)
  in_data4 <- split(in_data3, in_data3$subbasin)
  }


test <- test_fun("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv")

test1 <- read.csv("D:/Ptaki_hydro/Obliczenia/4TU/future_csv/4.5_NF/rcp45_cm01_2024_2050_reach.csv",
                  header = TRUE, sep = ",")
#why in channels for callinbration are 23 not 18 subbasins?
test2 <- test1[ test1$subbasin %in% channels_for_calibration$Subbasin, ]

test2 <- test1[ test1$subbasin %in% subbasins_nr$RCH, ]

#test2 <- as.Date(test2$date, format="%Y-%m-%d")
test2$date2 <- as.POSIXct(test2$date, format="%Y-%m-%d")

month <- as.integer(format(test2$date2, '%m'))
day <- as.integer(format(test2$date2, '%d'))
result <- filter(test2, month == 4 & day >= 11 | month == 5 | month == 6 & day <= 10)
result2 <- split(result, result$subbasin)


# 4.5 FF

# 8.5 NF

# 8.5 FF

