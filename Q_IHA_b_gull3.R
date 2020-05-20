install.packages("dplyr")
install.packages("plyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("IHA", repos="http://R-Forge.R-project.org")
install.packages("packrat")
install.packages("tibble")
install.packages("ggplot2")
install.packages("readr")
install.packages("purrr")

library(readr)
library("tidyverse")
library(plyr)
library("dplyr")
#packrat - package that manages all the differences between versions of packages and R
library("packrat")
library("tibble")
library("EflowStats")
library("ggplot2")
library("lubridate")
library("purrr")
library("ggforce")
library ("ggplot2")

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
#Data with vulnerability period for Śmieszka : 11.04 - 10.06

Smieszka_Q <- read.csv("D:/Ptaki_hydro/Obliczenia/R/Birds/Smieszka_Q.csv")

#black-headed gull #Śmieszka #(Chroicocephalus ridibundus) nesting success data

b_gull <- data.frame( read.csv("D:/Ptaki_hydro/Obliczenia/R/Birds/b_gull_na.csv")) %>%
          remove_rownames %>% column_to_rownames(var="X")

# works with selecting 11 April every year
#smieszka <- subset(Q_mod_Wisla, format.Date(Q_mod_Wisla$date, "%m")=="04" & format.Date(date, "%d")=="11")

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
Smieszka_sub_list <- lapply(Smieszka_list, extract_gauge)

#a function to create and save a plot
obs_vs_sim <- function(x, y) {  
  plot_type1 <- ggplot()+
    geom_line(data=x, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=y, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap(~RCH2, ncol=1)+
    labs(x = "year", y="Streamflow") +
    ggtitle(y$YEAR) + 
    scale_x_date(date_breaks = "10 days")+
    theme(plot.title = element_text(hjust = 0.5))+#positioning the title in the center
    coord_cartesian( ylim = c(0, 6000))
  print(plot_type1)
  ggsave(plot_type1, file = paste0("Q_mod_obs/b_gull/", y$YEAR, ".jpg"), #saving the plot into a folder
         device = "jpg",
         width = 10,
         height = 15)
}

#apply the function to two lists and produce .jpg plots into the folder
obs_vs_sim_list <- mapply(obs_vs_sim, Smieszka_sub_list, Smieszka_obs_list, SIMPLIFY = FALSE)

# COMPARE BY LOCATION ######
###### for Puławy

Smieszka_mod_Pulawy_vp <- subset(Smieszka_Q, Smieszka_Q$RCH == "1545") # only for the vulnerability period
Smieszka_mod_Pulawy_vp$date = as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d")
Smieszka_mod_Pulawy_vp$YEAR <- format(as.Date(Smieszka_mod_Pulawy_vp$date, format="%Y-%m-%d"),"%Y")

Smieszka_obs_Pulawy_vp <- subset(Smieszka_Q_obs, Smieszka_Q_obs$RCH =="1545")#checked subbasin with Puławy
#to make sure months are displayed in English

Sys.setlocale("LC_ALL", "English")
Sys.setenv("LANGUAGE"="En")

Smieszka_obs_vs_sim_location <- ggplot()+
    geom_line(data=Smieszka_mod_Pulawy_vp, aes(x=date, y=FLOW_OUTcms), color = "blue")+ #simulated
    geom_line(data=Smieszka_obs_Pulawy_vp, aes(x=date, y=Q), color="red")+ #observed
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+ # free_x makes the plot narrow down to vulnerability period
    labs(x = "year", y="Streamflow") +
    ggtitle("Puławy") + 
    scale_x_date(date_breaks = "7 days",
                 date_labels = "%d-%b")+ 
    theme(plot.title = element_text(hjust = 0.5))+#positioning the title in the center
    coord_cartesian( ylim = c(0, 6000))

print(Smieszka_obs_vs_sim_location)
  
ggsave(Smieszka_obs_vs_sim_location, file = paste0("Q_mod_obs/b_gull/Smieszka_Pulawy.jpg", ".jpg"), #saving the plot into a folder
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

graph_Smieszka_2004 <- ggplot(Smieszka2004, aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_Smieszka_2004)


graph_Smieszka_2010 <- ggplot(Smieszka2010, aes(x=date, y=FLOW_OUTcms))+ 
  stat_summary(geom = "line", fun= mean, color="olivedrab", size=1.1)+ 
  stat_summary(geom = "line", fun= min, color="grey")+ 
  stat_summary(geom = "line", fun= max, color="grey")+
  stat_summary(geom = "ribbon", fun.data = min_max, alpha= 0.3, fill ="green")+
  scale_x_date(date_breaks = "10 days")+
  coord_cartesian( ylim = c(0, 4000))

print(graph_Smieszka_2010)



########### Q for all locations in single line plot ###############################

Smieszka_line_plot <- function (x) {
  ggplot(x, aes(date, FLOW_OUTcms, group = RCH)) +
    geom_line()+ 
    facet_wrap( ~ format(date, "%Y"), scales = "free_x", ncol=1)+
    labs(y="modelled Q m3/s")+
    coord_cartesian( ylim = c(0, 4000))
  }

print(Smieszka_line_plot(Smieszka2004))
## apply it to a list

Smieszka_line_plot_list <- lapply(Smieszka_list, Smieszka_line_plot)

########### GHANT CHART VULNERABILITY PERIOD

library("reshape2")
#11.04 - 10.06
task1_bg <- c('Laying eggs', '2004-04-11', '2004-05-20')
task2_bg <- c('Incubating', '2004-04-20', '2004-05-31')
task3_bg <- c('Rearing chicks', '2004-05-01', '2004-06-10')

vp_bg <- as.data.frame(rbind(task3_bg, task2_bg, task1_bg))
names(vp_bg) <- c('task', 'start', 'end')
vp_bg$task <- factor(vp_bg$task, levels = vp_bg$task)
vp_bg$start <- as.Date(vp_bg$start)
vp_bg$end <- as.Date(vp_bg$end)
vp_bg_melted <- melt(vp_bg, measure.vars = c('start', 'end'))

# starting date to begin plot
start_date <- as.Date('2004-04-11')


B_gull_vp_plot <- ggplot(vp_bg_melted, aes(value, task)) + 
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

print(B_gull_vp_plot)    

################# COMBINING GRAPHS ON SINGLE PAGE


install.packages("ggpubr")
library("ggpubr")


#ggarrange(plotlist = Smieszka_line_plot_list,nrow = 5,ncol = ceiling(length(Smieszka_line_plot_list)/2))

Smieszka_line_plot_1 <- ggarrange(plotlist = Smieszka_line_plot_list,nrow = 15,ncol = 1)

b_gull_box_plot_2 <- ggarrange(plotlist = b_gull_box_sin_list,nrow = 15, ncol = 1)

install.packages("patchwork")
library("patchwork")

#arrangement of plots with patchwork package
b_gull_layout <- ((Smieszka_line_plot_1  + b_gull_box_plot_2))+
                  B_gull_vp_plot +
                  plot_layout(width = c(1,0.5), height = c(3,0.3)) #width of 1st and 2nd column
                                                                    # hight of 1st row and 2nd row

### ??? aline the dates in the vulnerability period graph and hydrogrpahs
### ??? change the labels on the y axis to title of the columns

print(b_gull_layout) 
#save as width=700 and height=2000

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

install.packages("Rcpp")
library("Rcpp")

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


################ Create data frames for each subbasin
RCH_split_bg <- split(Smieszka_Q, Smieszka_Q$RCH)

#################### CREATING STREAMFLOW AND DATE DATA FRAME PER SUBBASIN
#seting the prefix that's going to appear in each new data frame name
prefix1_bg <- "Smieszka_sub"
#setting the structure of the data frame name
Smieszka_new_sub_names <- paste(prefix1_bg,sep="_",(as.character(unique(Smieszka_Q$RCH))))

### Loop for creating all data frames in single go
### Prepare Q data for calculating IHA: function to have a single column with Q and date as row name

for (i in 1:length(RCH_split_bg)) {
  assign(Smieszka_new_sub_names[i], RCH_split_bg[[i]]%>%       
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
prefix2_bg <- "Smieszka_IHA_sub"
#setting the structure of the data frame name
Smieszka_IHA_sub_names <- paste(prefix2_bg,sep="_",(as.character(unique(Smieszka_Q$RCH))))

### Loop for creating all data frames in single go
### Prepare Q data for calculating IHA: function to have a single column with Q and date as row name

for (i in 1:length(RCH_split_bg)) {
  assign(Smieszka_IHA_sub_names[i], RCH_split_bg[[i]]%>%       
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


##### Calculate IHA in each subbasin/island location pair

#island 12/ sub 910
  b_gull_22 <- cbind(b_gull[22], Smieszka_IHA_sub_910)
#island 21/ sub 910
  b_gull_21 <- cbind(b_gull[21], Smieszka_IHA_sub_910)
  
#island 20/sub 950
  b_gull_20 <- cbind(b_gull[20], Smieszka_IHA_sub_950)
#island 19/sub 950
  b_gull_19 <- cbind(b_gull[19], Smieszka_IHA_sub_950)

#island 18/sub 1012
  b_gull_18 <- cbind(b_gull[18], Smieszka_IHA_sub_1012)
  
#island 17/sub 1087
  b_gull_17 <- cbind(b_gull[17], Smieszka_IHA_sub_1087)

#island 16/sub 1134
  b_gull_16 <- cbind(b_gull[16], Smieszka_IHA_sub_1134)
#island 15/sub 1134
  b_gull_15 <- cbind(b_gull[15], Smieszka_IHA_sub_1134)
  
#island 14/sub 1240
  b_gull_14 <- cbind(b_gull[14], Smieszka_IHA_sub_1240)
#island 13/sub 1240
  b_gull_13 <- cbind(b_gull[13], Smieszka_IHA_sub_1240)

#island 12/sub 1264
  b_gull_12 <- cbind(b_gull[12], Smieszka_IHA_sub_1264)
  
#island 11/sub 1289
  b_gull_11 <- cbind(b_gull[11], Smieszka_IHA_sub_1289)

#island 10/sub 1329
  b_gull_10 <- cbind(b_gull[10], Smieszka_IHA_sub_1329)

#island 9/sub 1358
  b_gull_9 <- cbind(b_gull[9], Smieszka_IHA_sub_1358)

#island 8/sub 1501
  b_gull_8 <- cbind(b_gull[8], Smieszka_IHA_sub_1501)

#island 7/sub 1545
  b_gull_7 <- cbind(b_gull[7], Smieszka_IHA_sub_1545)

#island 6/sub 1565
  b_gull_6 <- cbind(b_gull[6], Smieszka_IHA_sub_1565)

#island 5/sub 1601
  b_gull_5 <- cbind(b_gull[5], Smieszka_IHA_sub_1601)

#island 4/sub 1629
  b_gull_4 <- cbind(b_gull[4], Smieszka_IHA_sub_1629)

#island 3/sub 1727
  b_gull_3 <- cbind(b_gull[3], Smieszka_IHA_sub_1727)

#island 2/sub 1748
  b_gull_2 <- cbind(b_gull[2], Smieszka_IHA_sub_1748)

#island 1/sub 1875
  b_gull_1 <- cbind(b_gull[1], Smieszka_IHA_sub_1875)

###########Checking correlation between IHA and nesting success
#install.packages("corrplot")
library("corrplot")


### Correlation for subbasin 
#create a list
b_gull_list <- list(b_gull_1,b_gull_2,b_gull_3, b_gull_4, b_gull_5, b_gull_6, b_gull_7, b_gull_8, 
                    b_gull_9,b_gull_10,b_gull_11, b_gull_12, b_gull_13, b_gull_14, b_gull_15, b_gull_16,
                    b_gull_17,b_gull_18,b_gull_19, b_gull_20, b_gull_21, b_gull_22)


#apply the correlation calculation function to ALL the elements of the list
b_gull_matrix_list <- lapply(b_gull_list, cor, use = "pairwise.complete.obs",method = c("pearson"))

#extract the first row of the correlation matrix from each list (correlation between NS and IHA values in a location)
b_gull_all <- do.call(rbind, lapply(b_gull_matrix_list, head, 1))

#remove the 1st columns with 100% correlation (correlation between NS and NS)
b_gull_matrix <- b_gull_all[ ,-1]

corrplot(b_gull_matrix, method = "number")

############################ CURRENTY WORKING HERE 20th of May ##########################################
######## ??? how to apply the function to a mean from the list
# or draw regression plots between IHA values and Nesting success for all locations together on single plot

# use only b_gull_list

# We plot the independent variable on the x axis and the dependent variable on the y axis.
# Regression analysis also gives us a value called R^2, R squared. This tells us how much of the variation 
# in the y axis variable's values is accounted for by the variation in the x axis variable's values.

#### ???? I'm not sure if I asigned x and y correctly

# IHA 1
list_day_1_min_indv_b_gull <- lapply(b_gull_list, function(x) x%>% select(1:2))#select 1st and 2nd collumn
                   
colnames1 <- c("NS", "day_1_min")#set names in the collumn              
                   
list_day_1_min_indv_b_gull <- lapply(list_day_1_min_indv_b_gull, setNames, colnames1)# change collumn names                   

list_day_1_min_b_gull <- do.call("rbind", list_day_1_min_indv_b_gull) # merge the 22 lists for individual locations into 1

day_1_min_plot_b_gull <- ggscatter(list_day_1_min_b_gull, x = "NS", y = "day_1_min", add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"), cor.coef = TRUE, cor.method = "pearson") 

plot(day_1_min_plot_b_gull)

# IHA 2
list_day_1_max_indv_b_gull <- lapply(b_gull_list, function(x) x%>% select(1,3))#select 1st and 3nd collumn

colnames2 <- c("NS", "day_1_max")#set names in the collumn              

list_day_1_max_indv_b_gull <- lapply(list_day_1_max_indv_b_gull, setNames, colnames2)# change collumn names                   

list_day_1_max_b_gull <- do.call("rbind", list_day_1_max_indv_b_gull) # merge the 22 lists for individual locations into 1

day_1_max_plot_b_gull <- ggscatter(list_day_1_max_b_gull, x = "NS", y = "day_1_max", add = "reg.line", conf.int = TRUE,
                                         add.params = list(color = "blue", fill = "lightgray"), cor.coef = TRUE, cor.method = "pearson") 

plot(day_1_max_plot_b_gull)

# IHA 15 High pulse lenght
list_hp_length_indv_b_gull <- lapply(b_gull_list, function(x) x%>% select(1,15))#select 1st and 15th collumn

colnames15 <- c("NS", "hp_length")#set names in the collumn              

list_hp_length_indv_b_gull <- lapply(list_hp_length_indv_b_gull, setNames, colnames15)# change collumn names                   

list_hp_length_b_gull <- do.call("rbind", list_hp_length_indv_b_gull) # merge the 22 lists for individual locations into 1

hp_length_plot_b_gull <- ggscatter(list_hp_length_b_gull, x = "NS", y = "hp_length", add = "reg.line", conf.int = TRUE,
                                   add.params = list(color = "blue", fill = "lightgray"), cor.coef = TRUE, cor.method = "pearson") 

plot(hp_length_plot_b_gull)

# IHA 16 Rise rate
list_rise_rate_indv_b_gull <- lapply(b_gull_list, function(x) x%>% select(1,16))#select 1st and 16th collumn

colnames16 <- c("NS", "Rise_rate")#set names in the collumn              

list_rise_rate_indv_b_gull <- lapply(list_rise_rate_indv_b_gull, setNames, colnames16)# change collumn names                   

list_rise_rate_b_gull <- do.call("rbind", list_rise_rate_indv_b_gull) # merge the 22 lists for individual locations into 1

rise_rate_plot_b_gull <- ggscatter(list_rise_rate_b_gull, x = "NS", y = "Rise_rate", add = "reg.line", conf.int = TRUE,
                                   add.params = list(color = "blue", fill = "lightgray"), cor.coef = TRUE, cor.method = "pearson") 

plot(rise_rate_plot_b_gull)


### Summary
b_gull_mean <- as.data.frame(colMeans(b_gull_matrix, na.rm = TRUE)) 

bird_mean <- cbind(c_gull_mean,b_gull_mean)

write.csv(bird_mean, "bird_mean.csv")




