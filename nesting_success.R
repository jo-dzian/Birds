
######################################## NESTING SUCCESS ################################

######################################## BOX PLOT

#loading data from rows not columns in order to make box plots
#install.packages("reshape2")
library("reshape2")

#count the number of observations each year
n_fun <- function(x){
  return(data.frame(y = 2.05,
                    label = paste(length(x))))
}

####### FOR Black headed gull #### Individual
b_gull$group <- row.names(b_gull)
b_gull.m <- melt(b_gull, id.vars = "group") # data on nesting success melted into a single dataframe with all locations

ggplot(b_gull.m, aes(group, value)) + geom_boxplot(fill="orange", alpha=0.5) + 
  stat_summary( fun.data = n_fun, geom = "text",  fun = median)+ #add count = number of observations
  labs(x = "year", y="nesting success") +
  ggtitle("Black headed gull")+
  coord_cartesian( ylim = c(0, 2))

####### FOR common gull #### Individual
c_gull$group <- row.names(c_gull)
c_gull.m <- melt(c_gull, id.vars = "group")

ggplot(c_gull.m, aes(group, value)) + geom_boxplot(fill="green", alpha=0.5) + 
  stat_summary( fun.data = n_fun, geom = "text",  fun = median)+ #add count = number of observations
  labs(x = "year", y="nesting success") +
  ggtitle("Common gull")+
  coord_cartesian( ylim = c(0, 2))

####### FOR Little tern #### Individual
tern$group <- row.names(tern)
tern.m <- melt(tern, id.vars = "group")

ggplot(tern.m, aes(group, value)) + geom_boxplot(fill="blue", alpha=0.5) + 
  stat_summary( fun.data = n_fun, geom = "text",  fun.y = median)+ #add count = number of observations
  labs(x = "year", y="nesting success") +
  ggtitle("Little tern")+
  coord_cartesian( ylim = c(0, 2))
#################################################################################################################
######## Box Plot FOR ALL 3 Birds
# create a list of your data.frames
birds_list <- list(b_gull.m, c_gull.m, tern.m)
# assign names to the dataframes in the list
names(birds_list) <- c("black headed gull","common gull", "little tern")

# bind the dataframes together with rbindlist from data.table
# the id parameter will create a variable with the names of the dataframes
# you could also use 'bind_rows(l, .id="id")' from 'dplyr' for this
library("data.table")
birds_list2 <- rbindlist(birds_list, id="id")

#? plot x axis labels between tick marks

ggplot(birds_list2, aes(x=group, y=value, fill=id)) + 
  geom_boxplot( alpha =0.8, width = 0.9, 
                position = position_dodge(1)) + 
  labs(x = "year", y="nesting success") +
  ggtitle("Nesting succes per year")+
  coord_cartesian( ylim = c(0, 2))+
  stat_summary(aes(colour = factor(id)), fun.data = n_fun, geom = "text",
               hjust = 0.5, position = position_dodge(1))+ #number of observations
  scale_colour_manual(values = c("seagreen4", "orangered2", "orchid4"))+ #no obs colours of text
  theme(panel.grid.major.y = element_line(colour="grey", size = (0.2)),
        panel.grid.minor.y = element_line(colour="grey", size = (0.2)),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(color = "grey", size = (0.2))) +
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.ticks.x = element_blank())

#################################################################################################################
########################### POINT PLOT NESTING SUCCESS ################################

### All years
ggplot(data=b_gull.m, aes(x=variable, y=value, colour = value >0))+
  geom_point()+
  facet_wrap(~group, ncol=3, nrow=5)+
  scale_colour_manual(name = 'Nesting succes > 0',
                      values = setNames(c('black','red'),c(T, F))) + 
  coord_cartesian( ylim = c(0, 2))

# funcion to draw point graphs for each year
b_gull_point_sin <- function(x) {
  ggplot(data=x, aes(x=variable, y=value, colour = value >0))+
  facet_wrap(~group)+ # data is now in a list format according to year so this step seems unnecessary
  geom_point()+
  scale_colour_manual(name = 'Nesting succes > 0',
                      values = setNames(c('black','red'),c(T, F))) + 
  coord_cartesian( ylim = c(0, 2))+
  theme(legend.text=element_text(size=6))+
  ggtitle(x$group, subtitle ="Black-headed gull") +
  labs(x = "island location (south<=>north)", y="nesting success")
}

#creating a list out of data on bird nesting success, split according to year
b_gull.m.list <- split(b_gull.m, b_gull.m$group, drop = T)

b_gull_point_sin_list <- lapply(b_gull.m.list, b_gull_point_sin)
  

############ individual box plot ################################

b_gull_2004_box_sin <- b_gull.m %>%
  filter(group == "2004") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

plot(b_gull_2004_box_sin)

b_gull_2005_box_sin <- b_gull.m %>%
  filter(group == "2005") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2006_box_sin <- b_gull.m %>%
  filter(group == "2006") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2007_box_sin <- b_gull.m %>%
  filter(group == "2007") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2008_box_sin <- b_gull.m %>%
  filter(group == "2008") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2009_box_sin <- b_gull.m %>%
  filter(group == "2009") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2010_box_sin <- b_gull.m %>%
  filter(group == "2010") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2011_box_sin <- b_gull.m %>%
  filter(group == "2011") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2012_box_sin <- b_gull.m %>%
  filter(group == "2012") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2013_box_sin <- b_gull.m %>%
  filter(group == "2013") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2014_box_sin <- b_gull.m %>%
  filter(group == "2014") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2015_box_sin <- b_gull.m %>%
  filter(group == "2015") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2016_box_sin <- b_gull.m %>%
  filter(group == "2016") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2017_box_sin <- b_gull.m %>%
  filter(group == "2017") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")

b_gull_2018_box_sin <- b_gull.m %>%
  filter(group == "2018") %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")



