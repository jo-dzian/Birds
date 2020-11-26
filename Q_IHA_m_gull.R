# SCRIPT NO. 4 B

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
library("plyr")
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
#island_reach <- data.frame( read.csv("wyspa_subbasin.csv"))

#Narrow down the Q data to subbasins with islands
#there are 19 subbasins as 3 describe double bird islands

#Q_mod_Wisla <- data.frame( read.csv("Q_mod_Wisla.csv"))

#because data was read from a csv file the dates were converted to factor, now they need to be converted to date again
#Q_mod_Wisla$date <- as.Date(Q_mod_Wisla$mdate, format="%Y-%m-%d")
#yyyy-mm-dd

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data with vulnerability period for Mew Gull : 21.04 - 30.06
# m.gull_Q_mod - data with modelled streamflow for vulnerability period for Mew Gull

# Mew gull #Larus canus # nesting success data
# m_gull_NS

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
m.gull_sub_list <- lapply(m.gull_Q_mod_list , extract_gauge)

# function to create and save a plot
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
  ggsave(plot_type1, file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/m_gull/",
                                   y$Year, ".jpg"), #saving the plot into a folder
         device = "jpg",
         width = 11,
         height = 16)
}

#apply the function to two lists and produce .jpg plots into the folder
m.gull_obs_vs_sim_list <- mapply(obs_vs_sim, m.gull_sub_list, m.gull_Q_obs_list, SIMPLIFY = FALSE)

# COMPARE BY LOCATION ######
###### for Puławy

m.gull_mod_Pulawy_vp <- subset(m.gull_Q_mod, m.gull_Q_mod$RCH == "1545") # only for the vulnerability period
#m.gull_mod_Pulawy_vp$date = as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d")
#m.gull_mod_Pulawy_vp$YEAR <- format(as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d"),"%Y")

m.gull_obs_Pulawy_vp <- subset(m.gull_Q_obs, m.gull_Q_obs$RCH =="1545")#checked subbasin with Puławy
#to make sure months are displayed in English

Sys.setlocale("LC_ALL", "English")
Sys.setenv("LANGUAGE"="En")

m.gull_obs_vs_mod_location <- ggplot()+
    geom_line(data=m.gull_mod_Pulawy_vp, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=m.gull_obs_Pulawy_vp, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+ # free_x makes the plot narrow down to vulnerability period
    labs(x = "year", y="Streamflow") +
    ggtitle("Puławy") + 
    scale_x_date(date_breaks = "7 days",
                 date_labels = "%d-%b")+ 
    theme(plot.title = element_text(hjust = 0.5))+#positioning the title in the center
    coord_cartesian( ylim = c(0, 6000))

print(m.gull_obs_vs_mod_location)
  
ggsave(m.gull_obs_vs_mod_location, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/m_gull/m.gull__Pulawy.jpg", 
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
graph_m.gull_2004 <- ggplot(m.gull_Q_mod_list[[1]], aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_m.gull_2004)

# try for year 2010 (7th object in the list)
graph_m.gull_2010 <- ggplot(m.gull_Q_mod_list[[7]], aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_m.gull_2010)

########### Q for all locations in single line plot ###############################

m.gull_line_plot <- function (x) {
  ggplot(x, aes(date, FLOW_OUTcms, group = RCH)) +
    geom_line()+ 
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+
    labs(y="modelled Q m3/s")+
    coord_cartesian( ylim = c(0, 4000))
  }

## apply it to a list
m.gull_line_plot_list <- lapply(m.gull_Q_mod_list , m.gull_line_plot)

# display the first graph from the list which is for year 2004
plot(m.gull_line_plot_list[[1]] )

########### GHANT CHART VULNERABILITY PERIOD

library("reshape2")
#21.04 - 30.06

task1_m.gull <- c('Laying eggs', '2004-04-21', '2004-05-31')
task2_m.gull <- c('Incubating', '2004-05-01', '2004-06-20')
task3_m.gull <- c('Rearing chicks', '2004-05-21', '2004-06-30')


# vp - vulnerability period
vp_m.gull <- as.data.frame(rbind(task3_m.gull, task2_m.gull, task1_m.gull))
names(vp_m.gull) <- c('task', 'start', 'end')
vp_m.gull$task <- factor(vp_m.gull$task, levels = vp_m.gull$task)
vp_m.gull$start <- as.Date(vp_m.gull$start)
vp_m.gull$end <- as.Date(vp_m.gull$end)
vp_m.gull_melted <- melt(vp_m.gull, measure.vars = c('start', 'end'))

# starting date to begin plot
m.gull_start_date <- as.Date('2004-04-21')


m.gull_vp_plot <- ggplot(vp_m.gull_melted, aes(value, task)) + 
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
  scale_x_date(date_labels = "%b %d", limits = c(m.gull_start_date, NA), 
               breaks = (as.Date(c("2004-04-21", "2004-05-31",
                                   '2004-05-01', '2004-06-20',
                                   '2004-05-21', '2004-06-30'))))

print(m.gull_vp_plot)    

################# COMBINING GRAPHS ON SINGLE PAGE

#install.packages("ggpubr")
library("ggpubr")

#ggarrange(plotlist = Smieszka_line_plot_list,nrow = 5,ncol = ceiling(length(Smieszka_line_plot_list)/2))

m.gull_line_plot_1 <- ggarrange(plotlist = m.gull_line_plot_list,nrow = 15,ncol = 1)

m.gull_box_plot_2 <- ggarrange(plotlist = m.gull_box_sin_list,nrow = 15, ncol = 1)

#install.packages("patchwork")
library("patchwork")

#arrangement of plots with patchwork package
m.gull_layout <- ((m.gull_line_plot_1  + m.gull_box_plot_2))+
                  m.gull_vp_plot +
                  plot_layout(width = c(1,0.5), height = c(3,0.3)) #width of 1st and 2nd column
                                                                    # hight of 1st row and 2nd row

### ??? aline the dates in the vulnerability period graph and hydrogrpahs
### ??? change the labels on the y axis to title of the columns

print(m.gull_layout) 
#save as width=700 and height=2000

ggsave(m.gull_layout, 
       file = paste0("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/Q_mod_obs/m_gull/m.gull_layout.jpg", 
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


################ Create data frames for each subbasin for the whole year
RCH_split <- split(Q_mod_Wisla, Q_mod_Wisla$RCH)

#add a year column
RCH_split <- lapply(RCH_split, function(x) {
  x$Year <- format(as.Date(x$date, format="%Y-%m-%d"),"%Y")
  x
})
################ Create data frames for each subbasin for vulnerability period
RCH_split_m.gull <- split(m.gull_Q_mod, m.gull_Q_mod$RCH)

####### Altering IHA functions ###############################################################

##############################################################################################
####### GROUP 1 ################################################################################
####### IHA group 1 Mean or median value for each calendar month replaced with means for periods of
#laying eggs, incubating and rearing chicks

######### laying eggs 21.04 - 31.05 ################################
m.gull_interv_le1 <- as.interval(ddays(41), start = ISOdate(2004:2018, 4, 21, 0, tz = 'Europe/Warsaw'))
m.gull_interv_le2 <- m.gull_Q_mod$date %within% as.list(m.gull_interv_le1)
m.gull_interv_le3 <- (m.gull_Q_mod$date)[which(m.gull_interv_le2)] # values date format
##Narrow down the Q data to vulnerability period
# unlisted
#m.gull_interv_le <- m.gull_Q_mod[m.gull_Q_mod$date %in% m.gull_interv_le3, ] 
# for list
m.gull_interv_le_list <- lapply(RCH_split_m.gull, function(x) {
  x[x$date %in% m.gull_interv_le3, ]  })

##calculate mean streamflow Q during the m.gull vulnerability period for laying eggs 
# unlisted
#m.gull_le_gr.1 <- aggregate(m.gull_interv_le[, 7], list(m.gull_interv_le$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

# for list
m.gull_le_gr.1_list <- lapply(m.gull_interv_le_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
  remove_rownames %>% 
  column_to_rownames(var="Group.1")
  })

##change names of columns
# unlisted
#names(m.gull_le_gr.1)[names(m.gull_le_gr.1) == 'x'] <- 'gr.1_mean_LE'

######### incubating 1.05 - 20.06 ###################################
m.gull_interv_i1 <- as.interval(ddays(51), start = ISOdate(2004:2018, 5, 1, 0, tz = 'Europe/Warsaw'))
m.gull_interv_i2 <- m.gull_Q_mod$date %within% as.list(m.gull_interv_i1)
m.gull_interv_i3 <- (m.gull_Q_mod$date)[which(m.gull_interv_i2)]
#Narrow down the Q data to vulnerability period
## unlisted
#m.gull_interv_i <- m.gull_Q_mod[m.gull_Q_mod$date %in% m.gull_interv_i3, ] 
#for list
m.gull_interv_i_list <- lapply(RCH_split_m.gull, function(x) {
  x[x$date %in% m.gull_interv_i3, ]  })

#calculate mean streamflow Q during the m.gull vulnerability period for incubation 
# unlisted
#m.gull_i_gr.1 <- aggregate(m.gull_interv_i[, 7], list(m.gull_interv_i$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

# for list
m.gull_i_gr.1_list <- lapply(m.gull_interv_i_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")})

#change names of columns
# unlisted
#names(m.gull_i_gr.1)[names(m.gull_i_gr.1) == 'x'] <- 'gr.1 mean Incub'

### rearing chicks 21.05 - 30.06 #################################
m.gull_interv_rc1 <- as.interval(ddays(41), start = ISOdate(2004:2018, 5, 21, 0, tz = 'Europe/Warsaw'))
m.gull_interv_rc2 <- m.gull_Q_mod$date %within% as.list(m.gull_interv_rc1)
m.gull_interv_rc3 <- (m.gull_Q_mod$date)[which(m.gull_interv_rc2)]
#Narrow down the Q data to vulnerability period
#unlisted
#m.gull_interv_rc <- m.gull_Q_mod[m.gull_Q_mod$date %in% m.gull_interv_rc3, ] 
#for list
m.gull_interv_rc_list <- lapply(RCH_split_m.gull, function(x) {
  x[x$date %in% m.gull_interv_rc3, ]  })

#calculate mean streamflow Q during the m.gull vulnerability period for rearing chicks 
#unlisted
#m.gull_rc_gr.1 <- aggregate(m.gull_interv_rc[, 7], list(m.gull_interv_rc$Year), mean) %>%
#  remove_rownames %>% 
#  column_to_rownames(var="Group.1")

#for list
m.gull_rc_gr.1_list <- lapply(m.gull_interv_rc_list, function(x) {
  aggregate(x$FLOW_OUTcms, list(x$Year), mean) %>%
    remove_rownames %>% 
    column_to_rownames(var="Group.1")})

#change names of columns
#unlisted
#names(m.gull_rc_gr.1)[names(m.gull_rc_gr.1) == 'x'] <- 'gr.1 mean RC'

#### joining results into single dataframe
#unlisted
#m.gull_gr.1 <- cbind(m.gull_le_gr.1, m.gull_i_gr.1, m.gull_rc_gr.1)

#for list
m.gull_list_gr.1 <- mapply(cbind, m.gull_le_gr.1_list, m.gull_i_gr.1_list, m.gull_rc_gr.1_list, SIMPLIFY=FALSE)

##change names of columns in the list
colnames.gr1 = c("gr.1_mean_LE","gr.1_mean_Incub", "gr.1_mean_RC") 
m.gull_list_gr.1 <- lapply(m.gull_list_gr.1, setNames, colnames.gr1)


##############################################################################################
####### GROUP 2 ################################################################################
####### IHA group 2 is Magnitude  and duration of annual extreme  water condition 
# (Annual minima, 1-day mean & Annual maxima, 1-day mean) 

library("zoo")

#calculating rolling 1,3 an 7 day mean on list (DOESN'T WORK, I'm using the code for data frame below)
Q_mod_Wisla_roll_list <- RCH_split %>%
  lapply(dplyr::mutate(day01_mean = zoo::rollmean (FLOW_OUTcms, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                day03_mean = zoo::rollmean (FLOW_OUTcms, k = 3, fill = NA),
                day07_mean = zoo::rollmean (FLOW_OUTcms, k = 7, fill = NA)))

#calculating rolling 1,3 an 7 day mean on data frame
Q_mod_Wisla_roll <- Q_mod_Wisla %>%
dplyr::arrange(desc(date)) %>% 
  dplyr::group_by(RCH) %>% 
  dplyr::mutate(day01_mean = zoo::rollmean (FLOW_OUTcms, k = 1, fill = NA), #we have 1 measurment per day so this is acctually not necessary to calculate
                day03_mean = zoo::rollmean (FLOW_OUTcms, k = 3, fill = NA),
                day07_mean = zoo::rollmean (FLOW_OUTcms, k = 7, fill = NA)) %>% 
  dplyr::ungroup()

# narrow it down to the vulnerability period of m.gull
Q_mod_Wisla_roll_vp_m.gull <- Q_mod_Wisla_roll[Q_mod_Wisla_roll$date %in% m.gull_dat, ] 

#add a year column
Q_mod_Wisla_roll_vp_m.gull$Year <- 
  format(as.Date(Q_mod_Wisla_roll_vp_m.gull$date, format="%Y-%m-%d"),"%Y")

#Create lists according to RCH
RCH_names <- c(910,950,1012,1087,1134,1240,1264,1289,1329,1358,1501,1545,1565,1601,1629,1727,1748,1875)

Q_mod_Wisla_roll_vp_m.gull_list <- 
  Q_mod_Wisla_roll_vp_m.gull %>% group_split(Q_mod_Wisla_roll_vp_m.gull$RCH) %>% setNames(RCH_names)

# calculate the minimum and maximum per year per RCH
 
library(plyr)
m.gull_list_gr.2 <- lapply(Q_mod_Wisla_roll_vp_m.gull_list,function(x) 
  ddply(x,.(RCH,Year), summarize,
        day01_min=min(day01_mean), day01_max=max(day01_mean),
        day03_min=min(day03_mean), day03_max=max(day03_mean),
        day07_min=min(day07_mean), day07_max=max(day07_mean) 
        ))

##############################################################################################
####### GROUP 3 ################################################################################
####### IHA group 3 is  Timing of annual extreme water conditions,
#Julian date of each annual 1-day maximum and 1-day minimum 


#m.gull vp:
#  21.04 is 111 or 112 (Leap) julian day
#  30.06 is 181 or 182 (Leap) julian day
# Leap years: 2004, 2008, 2012, 2016

#add julian day
RCH_split <- lapply(RCH_split, function(x) {
  x$julian <- yday(x$date);return(x)})

m.gull_list_gr.3 <- lapply(RCH_split,function(x) 
  ddply(x,.(RCH,Year), summarize,
        min=min(FLOW_OUTcms), max=max(FLOW_OUTcms),
        julian_min= which.min(FLOW_OUTcms),
        julian_max= which.max(FLOW_OUTcms),#gives julian day of the min/max 
        # i dont understand how this above works as it doesn't specify i want the value from the julian column
        # I sppose it counted the order of the value occurence in each year without using the julian column
        #chech if julian date is within vulnerability period range (101 and 162 days) and count as 1 if yes, 0 as no.
        vp_min = case_when(julian_min >= 111 & julian_min <= 182 ~ 1, TRUE ~ 0),
        vp_max = case_when(julian_max >= 111 & julian_max <= 182 ~ 1, TRUE ~ 0)))


##############################################################################################
####### GROUP 4 ################################################################################
####### IHA group 4 is  Frequency and duration of high and low pulses
# Number of low pulses within each water year, Mean or median duration of low pulses (days),
# Number of high pulses within each water year, Mean or median duration of high pulses (days)

# calculate percentiles and quartiles for the 15 year period
RCH_split_q13_period <- lapply(RCH_split,function(x) 
  ddply(x,.(RCH), summarize,
        P0.05=quantile(FLOW_OUTcms, 0.05),#find the 10% percentile for 15 year period
        Q1=quantile(FLOW_OUTcms, 0.25),#find the 25% quartile for 15 year period
        Q3=quantile(FLOW_OUTcms, 0.75),#find the 75% quartile for 15 year period
        P0.95=quantile(FLOW_OUTcms, 0.95)#find the 90% percentile for 15 year period
        ))

# reorganize list so it matches the structure of the other datasets        
m.gull_Q_mod_list_RCH <- m.gull_Q_mod %>% group_split(m.gull_Q_mod$RCH) %>% setNames(RCH_names)

#count how many days during the vulnerability period are higher than 75% quartile and lower than 25%
# I need to use the m.gull_Q_mod_list_RCH
m.gull.gr4_part1 <- Map(function(x, y) aggregate(FLOW_OUTcms > cbind(Q3, P0.95)~Year, merge(x, y, all = TRUE,
                            na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass'), 
            m.gull_Q_mod_list_RCH, RCH_split_q13_period)

m.gull.gr4_part2 <- Map(function(x, y) aggregate(FLOW_OUTcms < cbind(P0.05, Q1)~Year, merge(x, y, all = TRUE,
                             na.action = 'na.pass'), sum, na.rm = TRUE, na.action = 'na.pass'), 
             m.gull_Q_mod_list_RCH, RCH_split_q13_period)

# cbind the two parts
m.gull_gr4_parts <- Map(cbind, m.gull.gr4_part1, m.gull.gr4_part2) 
# remove Year appearing twice  
m.gull_list_gr.4 <-  lapply(m.gull_gr4_parts , "[", -c(4))

#calculate % of time above Q3 etc.

######################################################################
######## ??? how to draw regression plots between IHA values and Nesting success for all locations together on single plot

# use only b_gull_list

# We plot the independent variable on the x axis and the dependent variable on the y axis.
# Regression analysis also gives us a value called R^2, R squared. This tells us how much of the variation 
# in the y axis variable's values is accounted for by the variation in the x axis variable's values.

#### ???? I'm not sure if I asigned x and y correctly

#combine all 4 groups of indicators with NS

m.gull_list_parts <- Map(cbind,  m.gull_list_gr.2, m.gull_list_gr.1, m.gull_list_gr.3, m.gull_list_gr.4,
                          m_gull_NS_list) 

# remove columns appearing twice or no longer needed
m.gull_list_gr.all_NS <-  lapply(m.gull_list_parts , "[", -c(12:17,20,27))

library(data.table)
m.gull_iha_foredit <- rbindlist(m.gull_list_gr.all_NS,  fill=TRUE)

#export in order to combine several NS columns into one manually
write.csv(m.gull_iha_foredit, "m.gull_iha_foredit.csv")
#import edited file
m.gull_iha_edit <-read.csv("m.gull_iha_edit.csv")

library("ggpubr")
# GROUP 1
m.gull_graph_mean_LE <- ggscatter(m.gull_iha_edit, x = "gr.1.mean.LE", y = "NS", use="complete.obs", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method = "pearson",
                              xlab = "gr.1 mean LE", ylab = "NS")
  print(m.gull_graph_mean_LE)

m.gull_graph_mean_Incub <- ggscatter(m.gull_iha_edit, x = "gr.1.mean.Incub", y = "NS", use="complete.obs", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "gr.1 mean Incub", ylab = "NS")
  print(m.gull_graph_mean_Incub)

m.gull_graph_mean_RC <- ggscatter(m.gull_iha_edit, x = "gr.1.mean.RC", y = "NS", use="complete.obs", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "gr.1 mean RC", ylab = "NS")
  print(m.gull_graph_mean_RC)

# GROUP 2

m.gull_graph_day01_min <- ggscatter(m.gull_iha_edit, x = "day01_min", y = "NS", use="complete.obs", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "gr.2 one day min", ylab = "NS")
  print(m.gull_graph_day01_min) 

m.gull_graph_day01_max <- ggscatter(m.gull_iha_edit, x = "day01_max", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2 one day max", ylab = "NS")
  print(m.gull_graph_day01_max) 
  
m.gull_graph_day03_min <- ggscatter(m.gull_iha_edit, x = "day03_min", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2 three day min", ylab = "NS")
  print(m.gull_graph_day03_min) 
  
m.gull_graph_day03_max <- ggscatter(m.gull_iha_edit, x = "day03_max", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2 three day max", ylab = "NS")
  print(m.gull_graph_day03_max)
  
m.gull_graph_day07_min <- ggscatter(m.gull_iha_edit, x = "day07_min", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2 seven day min", ylab = "NS")
  print(m.gull_graph_day07_min) 
  
m.gull_graph_day07_max <- ggscatter(m.gull_iha_edit, x = "day07_max", y = "NS", use="complete.obs", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "gr.2 seven day max", ylab = "NS")
  print(m.gull_graph_day07_max) 
  
# GROUP 3
m.gull_graph_vp_max <- ggscatter(m.gull_iha_edit, x = "vp_max", y = "NS", use="complete.obs", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "gr.3 max within vp", ylab = "NS")
  print(m.gull_graph_vp_max)

# GROUP 4
  
m.gull_graph_Q3 <- ggscatter(m.gull_iha_edit, x = "Q3", y = "NS", use="complete.obs", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "gr.4 days above 0.75", ylab = "NS")
  print(m.gull_graph_Q3)

m.gull_graph_P0.95 <- ggscatter(m.gull_iha_edit, x = "P0.95", y = "NS", use="complete.obs", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "gr.4 days above 0.95", ylab = "NS")
  print(m.gull_graph_P0.95)

m.gull_graph_Q1 <- ggscatter(m.gull_iha_edit, x = "Q1", y = "NS", use="complete.obs", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "gr.4 days below 0.25", ylab = "NS")
  print(m.gull_graph_Q1)

# Gather the Graphs
  library(gridExtra)
  library(grid)
  library(ggplot2)
  library(lattice)
  
m.gull_cor_graph <- grid.arrange(
                m.gull_graph_mean_LE, m.gull_graph_mean_Incub, m.gull_graph_mean_RC,
                m.gull_graph_day01_min, m.gull_graph_day01_max, 
                m.gull_graph_day03_min, m.gull_graph_day03_max, 
                m.gull_graph_day07_min, m.gull_graph_day07_max,
                m.gull_graph_vp_max,
                m.gull_graph_Q3, m.gull_graph_P0.95, m.gull_graph_Q1,
                ncol=3)

print(m.gull_cor_graph)

#####******#####******#####******#####
##### SUMMARIZING IHA INDICATORS
#####******#####******#####******#####

#putting all the indicators in one list
m.gull_list_all_in <- mapply(cbind, m.gull_list_gr.1, m.gull_list_gr.2, m.gull_list_gr.3, m.gull_list_gr.4
                              , SIMPLIFY=FALSE)

# leave only the columns with IHA
m.gull_list_all <- lapply(m.gull_list_all_in, "[", c(5, 1:3, 6:11, 18:19, 21:24))

#********
#calculate means for each IHA (without spliting into RCH or years) for the baseline period

library(plyr)
library(dplyr)

# bind list to a tibble and make list names to a column
m.gull_list_all_df <- plyr::ldply(m.gull_list_all, data.frame) %>% 
  dplyr::rename(RCH = 1)

# mean
m.gull_list_all_mean <- m.gull_list_all_df %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))


write.csv(m.gull_list_all_mean, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_list_all_mean.csv")

#********
#calculate means for each IHA (with spliting into RCH but without spliting into years) 
# for the baseline period

# RCH mean
m.gull_list_rch_mean <- m.gull_list_all_df %>%  
  dplyr::group_by(RCH) %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))


write.csv(m.gull_list_rch_mean, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_list_rch_mean.csv")

#********
#calculate means for each IHA (with spliting into Years but without spliting into RCH) 
# for the baseline period

# year mean
m.gull_list_year_mean<- m.gull_list_all_df %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarise(gr.1_mean_LE = mean(gr.1_mean_LE),
                   gr.1_mean_Incub = mean(gr.1_mean_Incub),
                   gr.1_mean_RC = mean(gr.1_mean_RC),
                   gr.2_day01_min = mean(day01_min),
                   gr.2_day01_max = mean(day01_max),
                   gr.2_day03_min = mean(day03_min),
                   gr.2_day03_max = mean(day03_max),
                   gr.2_day07_min = mean(day07_min),
                   gr.2_day07_max = mean(day07_max),
                   gr.3_vp_min = mean(vp_min),
                   gr.3_vp_max = mean(vp_max),
                   gr.4_P0.95 = mean(P0.95),
                   gr.4_Q3 = mean(Q3),
                   gr.4_Q1 = mean(Q1),
                   gr.4_P0.05 = mean(P0.05))

write.csv(m.gull_list_year_mean, "D:/Ptaki_hydro/Obliczenia/R/Results/m.gull_list_year_mean.csv")
