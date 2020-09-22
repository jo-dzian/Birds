install.packages("dplyr")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("IHA", repos="http://R-Forge.R-project.org")
install.packages("packrat")
install.packages("tibble")
install.packages("ggplot2")
install.packages("ggplot")
install.packages("readr")
install.packages("purrr")

library(readr)
library("tidyverse")
library(plyr)
library("dplyr")
#packrat - package that manages all the differences between versions of packages and R
library("packrat")
library("tibble")
#library("EflowStats")
library("ggplot2")
library("lubridate")
library("purrr")
library("ggforce")

setwd ("D:/Ptaki_hydro/Obliczenia/R/IHA")

#island locations paired with subbasins
island_reach <- data.frame( read.csv("wyspa_subbasin.csv"))

#Narrow down the Q data to subbasins with islands
#there are 19 subbasins as 3 describe double bird islands

Q_mod_Wisla <- data.frame( read.csv("Q_mod_Wisla.csv"))

#because data was read from a csv file the dates were converted to factor, now they need to be converted to date again
Q_mod_Wisla$date <- as.Date(Q_mod_Wisla$mdate, format="%Y-%m-%d")
#yyyy-mm-dd

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data with vulnerability period for Śmieszka : 11.04 - 10.06
# bh.gull_Q_mod - data with modelled streamflow for vulnerability period for BHG

#black-headed gull #Śmieszka #(Chroicocephalus ridibundus) nesting success data
# bh_gull_NS

library(hrbrthemes)
library(scales)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############################ graph for comparing simulated and observed streamflow in each year
############################ during vulnerability period

# COMAPRE BY YEAR ######
# narrow down the subbasins to 5 that also have a gauging station
extract_gauge <- function (x) {
  subset(x, x$RCH == 1087|
           x$RCH ==1264|
           x$RCH ==1501|
           x$RCH ==1545|
           x$RCH ==1875)
}

#apply the function to a list
bh.gull_sub_list <- lapply(bh.gull_Q_mod_list , extract_gauge)

#a function to create and save a plot
obs_vs_sim <- function(x, y) {  
  plot_type1 <- ggplot()+
    geom_line(data=x, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=y, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap(~RCH, ncol=1)+
    labs(x = "year", y="Streamflow") +
    ggtitle(y$Year) + 
    scale_x_date(date_breaks = "10 days")+
    theme(plot.title = element_text(hjust = 0.5))+#positioning the title in the center
    coord_cartesian( ylim = c(0, 6000))
  print(plot_type1)
  ggsave(plot_type1, file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/bh_gull/",
                                   y$Year, ".jpg"), #saving the plot into a folder
         device = "jpg",
         width = 11,
         height = 16)
}

#apply the function to two lists and produce .jpg plots into the folder
obs_vs_sim_list <- mapply(obs_vs_sim, bh.gull_sub_list, bh.gull_Q_obs_list, SIMPLIFY = FALSE)

# COMPARE BY LOCATION ######
###### for Puławy

bh.gull_mod_Pulawy_vp <- subset(bh.gull_Q_mod, bh.gull_Q_mod$RCH == "1545") # only for the vulnerability period
#bh.gull_mod_Pulawy_vp$date = as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d")
#bh.gull_mod_Pulawy_vp$YEAR <- format(as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d"),"%Y")

bh.gull_obs_Pulawy_vp <- subset(bh.gull_Q_obs, bh.gull_Q_obs$RCH =="1545")#checked subbasin with Puławy
#to make sure months are displayed in English

Sys.setlocale("LC_ALL", "English")
Sys.setenv("LANGUAGE"="En")

bh.gull_obs_vs_mod_location <- ggplot()+
    geom_line(data=bh.gull_mod_Pulawy_vp, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=bh.gull_obs_Pulawy_vp, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+ # free_x makes the plot narrow down to vulnerability period
    labs(x = "year", y="Streamflow") +
    ggtitle("Puławy") + 
    scale_x_date(date_breaks = "7 days",
                 date_labels = "%d-%b")+ 
    theme(plot.title = element_text(hjust = 0.5))+#positioning the title in the center
    coord_cartesian( ylim = c(0, 6000))

print(bh.gull_obs_vs_mod_location)
  
ggsave(bh.gull_obs_vs_mod_location, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/bh_gull/Smieszka_Pulawy.jpg", 
                     ".jpg"), #saving the plot into a folder
      device = "jpg",
      width = 8,
      height = 25)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############### graph with ribbon from min to max of all locations per year with line showing mean

#set function to find the min and max for the ribbon
min_max <- function (x){
  dat <- data.frame(y = mean(x),
                    ymin = min(x),
                    ymax = max(x))
  return(dat)
  }
# try for year 2004 (1st object in the list)
graph_bh.gull_2004 <- ggplot(bh.gull_Q_mod_list[[1]], aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_bh.gull_2004)

# try for year 2010 (7th object in the list)
graph_bh.gull_2010 <- ggplot(bh.gull_Q_mod_list[[7]], aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_bh.gull_2010)

########### Q for all locations in single line plot ###############################

bh.gull_line_plot <- function (x) {
  ggplot(x, aes(date, FLOW_OUTcms, group = RCH)) +
    geom_line()+ 
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+
    labs(y="modelled Q m3/s")+
    coord_cartesian( ylim = c(0, 4000))
  }

## apply it to a list
bh.gull_line_plot_list <- lapply(bh.gull_Q_mod_list , bh.gull_line_plot)

# display the first graph from the list which is for year 2004
plot(bh.gull_line_plot_list[[1]] )

########### GHANT CHART VULNERABILITY PERIOD

library("reshape2")
#11.04 - 10.06
task1_bh.gull <- c('Laying eggs', '2004-04-11', '2004-05-20')
task2_bh.gull <- c('Incubating', '2004-04-20', '2004-05-31')
task3_bh.gull <- c('Rearing chicks', '2004-05-01', '2004-06-10')

# vp - vulnerability period
vp_bh.gull <- as.data.frame(rbind(task3_bh.gull, task2_bh.gull, task1_bh.gull))
names(vp_bh.gull) <- c('task', 'start', 'end')
vp_bh.gull$task <- factor(vp_bh.gull$task, levels = vp_bh.gull$task)
vp_bh.gull$start <- as.Date(vp_bh.gull$start)
vp_bh.gull$end <- as.Date(vp_bh.gull$end)
vp_bh.gull_melted <- melt(vp_bh.gull, measure.vars = c('start', 'end'))

# starting date to begin plot
start_date <- as.Date('2004-04-11')


bh.gull_vp_plot <- ggplot(vp_bh.gull_melted, aes(value, task)) + 
  geom_line(size = 7, colour="seagreen4") +
  labs(x = '', y = '', title = 'Vulnerability period') +
  theme_bw(base_size = 10) +
  theme(text = element_text(size=12))+
  theme(plot.title = element_text(hjust = 0),
        panel.grid.major.x = element_line(colour="black", linetype = "dashed"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, size = 10),
        axis.text.y = element_text(angle = 0, size = 10)) +
  scale_x_date(date_labels = "%b %d", limits = c(start_date, NA), 
               breaks = (as.Date(c("2004-04-11", "2004-05-20",
                                   '2004-04-20', '2004-05-31',
                                   '2004-05-01', '2004-06-10'))))

print(bh.gull_vp_plot)    

################# COMBINING GRAPHS ON SINGLE PAGE

#install.packages("ggpubr")
library("ggpubr")

#ggarrange(plotlist = Smieszka_line_plot_list,nrow = 5,ncol = ceiling(length(Smieszka_line_plot_list)/2))

bh.gull_line_plot_1 <- ggarrange(plotlist = bh.gull_line_plot_list,nrow = 15,ncol = 1)

bh.gull_box_plot_2 <- ggarrange(plotlist = bh.gull_box_sin_list,nrow = 15, ncol = 1)

#install.packages("patchwork")
library("patchwork")

#arrangement of plots with patchwork package
bh.gull_layout <- ((bh.gull_line_plot_1  + bh.gull_box_plot_2))+
                  bh.gull_vp_plot +
                  plot_layout(width = c(1,0.5), height = c(3,0.3)) #width of 1st and 2nd column
                                                                    # hight of 1st row and 2nd row

### ??? aline the dates in the vulnerability period graph and hydrogrpahs
### ??? change the labels on the y axis to title of the columns

print(bh.gull_layout) 
#save as width=700 and height=2000

ggsave(bh.gull_layout, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/bh_gull/bh.gull_layout.jpg", 
                     ".jpg"), #saving the plot into a folder
       device = "jpg",
       width = 7,
       height = 20)

# other packages for arrangement of plots
#install.packages("grid")
#library("grid")
#install.packages("gridExtra")
#library("gridExtra")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  IHA  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#Calculate IHA for all subbasins
#install.packages("zoo")
library("zoo")

#install.packages("xts")
library("xts")

#install.packages("caTools")
#install.packages("IHA", repos="http://R-Forge.R-project.org")
#library("IHA")

#install.packages("Rcpp")
library("Rcpp")

#Installing Jasons Law package IHA directly from his repository
devtools::install_github("jasonelaw/iha", force = TRUE)

# When asked
# These packages have more recent versions available.
# It is recommended to update all of them.
# Which would you like to update?
  
#1: All                        
#2: CRAN packages only         
#3: None                       
#4: zoo (1.8-7 -> 1.8-8) [CRAN]

# I've picked 3

library("IHA")


################ Create data frames for each subbasin for the whole year
RCH_split <- split(Q_mod_Wisla, Q_mod_Wisla$RCH)

#add a year column
RCH_split <- lapply(RCH_split, function(x) {
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y")
  x
  })
################ Create data frames for each subbasin for vulnerability period
RCH_split_bh.gull <- split(bh.gull_Q_mod, bh.gull_Q_mod$RCH)

#################### CREATING STREAMFLOW AND DATE DATA FRAME PER SUBBASIN
#seting the prefix that's going to appear in each new data frame name
prefix1_bh.gull <- "bh.gull_sub"
#setting the structure of the data frame name
bh.gull_new_sub_names <- paste(prefix1_bh.gull,sep="_",(as.character(unique(bh.gull_Q_mod$RCH))))

### Loop for creating all data frames in single go
### Prepare Q data for calculating IHA: function to have a single column with Q and date as row name

for (i in 1:length(RCH_split_bh.gull)) {
  assign(bh.gull_new_sub_names[i], RCH_split_bh.gull[[i]]%>%       
           remove_rownames %>% 
           column_to_rownames(var="date")%>%
           dplyr::select(FLOW_OUTcms))
}


#### Calculate IHA FUNCTION
calc_IHA_sub <- function(x) { 
    x1 <- as.xts(x, format="%Y-%m-%d")  
    g2 <- group2(x1) 
    g3 <- group3(x1, year = c("calendar")) 
    g4 <- group4(x1, year = c("calendar")) 
    g5 <- group5(x1, year = c("calendar")) 
    df <- cbind(g2, g3, g4, g5) 
    out<- df[ -c(1,10,11,12)]# columns which are not needed
    print(out)
           }

#################### CREATING YEAR AND IHA DATA FRAME PER SUBBASIN

#seting the prefix that's going to appear in each new data frame name
prefix2_bh.gull <- "bh.gull_IHA_sub"
#setting the structure of the data frame name
bh.gull_IHA_sub_names <- paste(prefix2_bh.gull,sep="_",(as.character(unique(bh.gull_Q_mod$RCH))))

### Loop for creating all data frames in single go
### Prepare Q data for calculating IHA: function to have a single column with Q and date as row name

for (i in 1:length(RCH_split_bh.gull)) {
  assign(bh.gull_IHA_sub_names[i], RCH_split_bh.gull[[i]]%>%       
           remove_rownames %>% 
           column_to_rownames(var="date")%>%
           dplyr::select(FLOW_OUTcms)%>%
           calc_IHA_sub)
}

######## TEST
# b_gull no. column or Xno of island location
#island_reach lookup table island and matching subbasin
# Smieszka_IHA_sub_ with subbasin number
#cbind columns from Smieszka_IHA_sub_ and b_gull


##### join each subbasin/island location nesting success with IHA metrics

#island 12/ sub 910
  bh.gull_22 <- cbind(bh_gull_NS[22], bh.gull_IHA_sub_910)
#island 21/ sub 910
  bh.gull_21 <- cbind(bh_gull_NS[21], bh.gull_IHA_sub_910)
  
#island 20/sub 950
  bh.gull_20 <- cbind(bh_gull_NS[20], bh.gull_IHA_sub_950)
#island 19/sub 950
  bh.gull_19 <- cbind(bh_gull_NS[19], bh.gull_IHA_sub_950)

#island 18/sub 1012
  bh.gull_18 <- cbind(bh_gull_NS[18], bh.gull_IHA_sub_1012)
  
#island 17/sub 1087
  bh.gull_17 <- cbind(bh_gull_NS[17], bh.gull_IHA_sub_1087)

#island 16/sub 1134
  bh.gull_16 <- cbind(bh_gull_NS[16], bh.gull_IHA_sub_1134)
#island 15/sub 1134
  bh.gull_15 <- cbind(bh_gull_NS[15], bh.gull_IHA_sub_1134)
  
#island 14/sub 1240
  bh.gull_14 <- cbind(bh_gull_NS[14], bh.gull_IHA_sub_1240)
#island 13/sub 1240
  bh.gull_13 <- cbind(bh_gull_NS[13], bh.gull_IHA_sub_1240)

#island 12/sub 1264
  bh.gull_12 <- cbind(bh_gull_NS[12], bh.gull_IHA_sub_1264)
  
#island 11/sub 1289
  bh.gull_11 <- cbind(bh_gull_NS[11], bh.gull_IHA_sub_1289)

#island 10/sub 1329
  bh.gull_10 <- cbind(bh_gull_NS[10], bh.gull_IHA_sub_1329)

#island 9/sub 1358
  bh.gull_9 <- cbind(bh_gull_NS[9], bh.gull_IHA_sub_1358)

#island 8/sub 1501
  bh.gull_8 <- cbind(bh_gull_NS[8], bh.gull_IHA_sub_1501)

#island 7/sub 1545
  bh.gull_7 <- cbind(bh_gull_NS[7], bh.gull_IHA_sub_1545)

#island 6/sub 1565
  bh.gull_6 <- cbind(bh_gull_NS[6], bh.gull_IHA_sub_1565)

#island 5/sub 1601
  bh.gull_5 <- cbind(bh_gull_NS[5], bh.gull_IHA_sub_1601)

#island 4/sub 1629
  bh.gull_4 <- cbind(bh_gull_NS[4], bh.gull_IHA_sub_1629)

#island 3/sub 1727
  bh.gull_3 <- cbind(bh_gull_NS[3], bh.gull_IHA_sub_1727)

#island 2/sub 1748
  bh.gull_2 <- cbind(bh_gull_NS[2], bh.gull_IHA_sub_1748)

#island 1/sub 1875
  bh.gull_1 <- cbind(bh_gull_NS[1], bh.gull_IHA_sub_1875)

###########Checking correlation between IHA and nesting success
#install.packages("corrplot")
library("corrplot")


### Correlation for subbasin 
#create a list
bh.gull_list <- list(bh.gull_1,bh.gull_2,bh.gull_3, bh.gull_4, bh.gull_5, bh.gull_6, bh.gull_7, bh.gull_8, 
                    bh.gull_9,bh.gull_10,bh.gull_11, bh.gull_12, bh.gull_13, bh.gull_14, bh.gull_15, bh.gull_16,
                    bh.gull_17,bh.gull_18,bh.gull_19, bh.gull_20, bh.gull_21, bh.gull_22)


#apply the correlation calculation function to ALL the elements of the list
bh.gull_matrix_list <- lapply(bh.gull_list, cor, use = "pairwise.complete.obs",method = c("pearson"))

#extract the first row of the correlation matrix from each list (correlation between NS and IHA values in a location)
bh.gull_all <- do.call(rbind, lapply(bh.gull_matrix_list, head, 1))

#remove the 1st columns with 100% correlation (correlation between NS and NS)
bh.gull_matrix <- bh.gull_all[ ,-1]

corrplot(bh.gull_matrix, method = "number")



####### Altering IHA functions ###############################################################

##############################################################################################
####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 11.04 - 20.05 ################################
bh.gull_interv_le1 <- as.interval(ddays(40), start = ISOdate(2004:2018, 4, 11, 0, tz = 'Europe/Warsaw'))
bh.gull_interv_le2 <- bh.gull_Q_mod$date %within% as.list(bh.gull_interv_le1)
bh.gull_interv_le3 <- (bh.gull_Q_mod$date)[which(bh.gull_interv_le2)] # values date format
##Narrow down the Q data to vulnerability period
# unlisted
#bh.gull_interv_le <- bh.gull_Q_mod[bh.gull_Q_mod$date %in% bh.gull_interv_le3, ] 
# for list
bh.gull_interv_le_list <- lapply(RCH_split_bh.gull, function(x) {
  x[x$date %in% bh.gull_interv_le3, ]  })

##calculate mean streamflow Q during the bh.gull vulnerability period for laying eggs 
# unlisted
#bh.gull_le_gr.1 <- aggregate(bh.gull_interv_le[, 7], list(bh.gull_interv_le$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

# for list
bh.gull_le_gr.1_list <- lapply(bh.gull_interv_le_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
  remove_rownames %>% 
  column_to_rownames(var="Group.1")})

##change names of columns
# unlisted
#names(bh.gull_le_gr.1)[names(bh.gull_le_gr.1) == 'x'] <- 'gr.1 mean LE'

######### incubating 20.04 - 31.05 ###################################
bh.gull_interv_i1 <- as.interval(ddays(41), start = ISOdate(2004:2018, 4, 21, 0, tz = 'Europe/Warsaw'))
bh.gull_interv_i2 <- bh.gull_Q_mod$date %within% as.list(bh.gull_interv_i1)
bh.gull_interv_i3 <- (bh.gull_Q_mod$date)[which(bh.gull_interv_i2)]
#Narrow down the Q data to vulnerability period
## unlisted
#bh.gull_interv_i <- bh.gull_Q_mod[bh.gull_Q_mod$date %in% bh.gull_interv_i3, ] 
#for list
bh.gull_interv_i_list <- lapply(RCH_split_bh.gull, function(x) {
  x[x$date %in% bh.gull_interv_i3, ]  })

#calculate mean streamflow Q during the bh.gull vulnerability period for incubation 
# unlisted
#bh.gull_i_gr.1 <- aggregate(bh.gull_interv_i[, 7], list(bh.gull_interv_i$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

# for list
bh.gull_i_gr.1_list <- lapply(bh.gull_interv_i_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")})

#change names of columns
# unlisted
#names(bh.gull_i_gr.1)[names(bh.gull_i_gr.1) == 'x'] <- 'gr.1 mean Incub'

### rearing chicks 1.05 - 10.06 #################################
bh.gull_interv_rc1 <- as.interval(ddays(41), start = ISOdate(2004:2018, 5, 1, 0, tz = 'Europe/Warsaw'))
bh.gull_interv_rc2 <- bh.gull_Q_mod$date %within% as.list(bh.gull_interv_rc1)
bh.gull_interv_rc3 <- (bh.gull_Q_mod$date)[which(bh.gull_interv_rc2)]
#Narrow down the Q data to vulnerability period
#unlisted
#bh.gull_interv_rc <- bh.gull_Q_mod[bh.gull_Q_mod$date %in% bh.gull_interv_rc3, ] 
#for list
bh.gull_interv_rc_list <- lapply(RCH_split_bh.gull, function(x) {
  x[x$date %in% bh.gull_interv_rc3, ]  })

#calculate mean streamflow Q during the bh.gull vulnerability period for rearing chicks 
#unlisted
#bh.gull_rc_gr.1 <- aggregate(bh.gull_interv_rc[, 7], list(bh.gull_interv_rc$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

#for list
bh.gull_rc_gr.1_list <- lapply(bh.gull_interv_rc_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")})

#change names of columns
#unlisted
#names(bh.gull_rc_gr.1)[names(bh.gull_rc_gr.1) == 'x'] <- 'gr.1 mean RC'

#### joining results into single dataframe
#unlisted
#bh.gull_gr.1 <- cbind(bh.gull_le_gr.1, bh.gull_i_gr.1, bh.gull_rc_gr.1)

#for list
bh.gull_list_gr.1 <- mapply(cbind, bh.gull_le_gr.1_list,bh.gull_i_gr.1_list, bh.gull_rc_gr.1_list, SIMPLIFY=FALSE)

##change names of columns in the list
colnames.gr1 = c("gr.1 mean LE","gr.1 mean Incub", "gr.1 mean RC") 
bh.gull_list_gr.1 <- lapply(bh.gull_list_gr.1, setNames, colnames.gr1)


##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual minima, 1-day mean & Annual maxima, 1-day mean) 

library("zoo")

#calculating rolling 1,3 an 7 day mean
Q_mod_Wisla_roll_list <- RCH_split %>%
  lapply(dplyr::mutate(day01_mean = zoo::rollmean (FLOW_OUTcms, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                day03_mean = zoo::rollmean (FLOW_OUTcms, k = 3, fill = NA),
                day07_mean = zoo::rollmean (FLOW_OUTcms, k = 7, fill = NA)))

#calculating rolling 1,3 an 7 day mean
Q_mod_Wisla_roll <- Q_mod_Wisla %>%
dplyr::arrange(desc(date)) %>% 
  dplyr::group_by(RCH) %>% 
  dplyr::mutate(day01_mean = zoo::rollmean (FLOW_OUTcms, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                day03_mean = zoo::rollmean (FLOW_OUTcms, k = 3, fill = NA),
                day07_mean = zoo::rollmean (FLOW_OUTcms, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

# narrow it down to the vulnerability period of bh.gull
Q_mod_Wisla_roll_vp_bh.gull <- Q_mod_Wisla_roll[Q_mod_Wisla_roll$date %in% bh.gull_dat, ] 

#add a year column
Q_mod_Wisla_roll_vp_bh.gull$Year <- 
  format(as.Date(Q_mod_Wisla_roll_vp_bh.gull$date, format="%Y-%m-%d"),"%Y")

#Create lists according to RCH
Q_mod_Wisla_roll_vp_bh.gull_list <- 
  Q_mod_Wisla_roll_vp_bh.gull %>% group_split(Q_mod_Wisla_roll_vp_bh.gull$RCH) 

# calculate the minimum and maximum per year per RCH
 
#woooohoo
library(dplyr)
bh.gull_list_gr.2 <- lapply(Q_mod_Wisla_roll_vp_bh.gull_list,function(x) 
  ddply(x,.(RCH,Year), summarize,
        day01_min=min(day01_mean), day01_max=max(day01_mean),
        day03_min=min(day03_mean), day03_max=max(day03_mean),
        day07_min=min(day07_mean), day07_max=max(day07_mean) 
        ))

#działa
listtest3 <-lapply(Q_mod_Wisla_roll_vp_bh.gull_list, function(x) 
  aggregate(day01_mean ~ Year, data = x, FUN = "min"))
  
#działa
listtest1 <- aggregate(day01_mean ~ Year, data= Q_mod_Wisla_roll_vp_bh.gull, min)
  
#??? I need to do it separately for each reach so then I can look at the regression with nesting success
# in particulat location


#### GROUP 3

#Calculate IHA for all subbasins
#install.packages("zoo")
library("zoo")

#install.packages("xts")
library("xts")

#install.packages("caTools")
install.packages("IHA", repos="http://R-Forge.R-project.org")
library("IHA")

#install.packages("Rcpp")
library("Rcpp")

#Installing Jasons Law package IHA directly from his repository
devtools::install_github("jasonelaw/iha", force = TRUE)

# When asked
# These packages have more recent versions available.
# It is recommended to update all of them.
# Which would you like to update?

#1: All                        
#2: CRAN packages only         
#3: None                       
#4: zoo (1.8-7 -> 1.8-8) [CRAN]

# I've picked 3

library("IHA")

#bh.gull vp:
#  11.04 is 101 or 102 (Leap) julian day
#  10.06 is 161 or 162 (Leap) julian day
# Leap years: 2004, 2008, 2012, 2016

#add julian day
RCH_split <- lapply(RCH_split, function(x) {
  x$julian <- yday(x$date);return(x)})

#


######################################################################
######## ??? how to draw regression plots between IHA values and Nesting success for all locations together on single plot

# use only b_gull_list

# We plot the independent variable on the x axis and the dependent variable on the y axis.
# Regression analysis also gives us a value called R^2, R squared. This tells us how much of the variation 
# in the y axis variable's values is accounted for by the variation in the x axis variable's values.

#### ???? I'm not sure if I asigned x and y correctly

