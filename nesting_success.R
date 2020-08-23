# SCRIPT NO. 3
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
bh_gull_NS$group <- row.names(bh_gull_NS)
bh_gull.m <- melt(bh_gull_NS, id.vars = "group") # data on nesting success melted into a single dataframe with all locations

ggplot(bh_gull.m, aes(group, value)) + geom_boxplot(fill="orange", alpha=0.5) + 
  stat_summary( fun.data = n_fun, geom = "text",  fun = median)+ #add count = number of observations
  labs(x = "year", y="nesting success") +
  ggtitle("Black headed gull")+
  coord_cartesian( ylim = c(0, 2))

####### FOR mew gull #### Individual
m_gull_NS$group <- row.names(m_gull_NS)
m_gull.m <- melt(m_gull_NS, id.vars = "group")

ggplot(m_gull.m, aes(group, value)) + geom_boxplot(fill="green", alpha=0.5) + 
  stat_summary( fun.data = n_fun, geom = "text",  fun = median)+ #add count = number of observations
  labs(x = "year", y="nesting success") +
  ggtitle("Mew gull")+
  coord_cartesian( ylim = c(0, 2))

####### FOR Little tern #### Individual
tern_NS$group <- row.names(tern_NS)
tern.m <- melt(tern_NS, id.vars = "group")

ggplot(tern.m, aes(group, value)) + geom_boxplot(fill="blue", alpha=0.5) + 
  stat_summary( fun.data = n_fun, geom = "text",  fun.y = median)+ #add count = number of observations
  labs(x = "year", y="nesting success") +
  ggtitle("Little tern")+
  coord_cartesian( ylim = c(0, 2))
#################################################################################################################
######## Box Plot FOR ALL 3 Birds
# create a list of your data.frames
birds_list <- list(bh_gull.m, m_gull.m, tern.m)
# assign names to the dataframes in the list
names(birds_list) <- c("black headed gull","mew gull", "little tern")

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

#????????? TRASH????###################################################################################
############ individual box plot ################################

# function to draw box plots
bh.gull_box_sin <- function (x) {
  x %>%
  ggplot(aes (x=group, y=value))+
  geom_boxplot()+
  coord_cartesian( ylim = c(0, 2))+
  labs(x = "year", y="nesting success")
}

library("dplyr")
bh.gull.m_list <- bh_gull.m %>% group_split(bh_gull.m$group)

#apply the function to a list withindividual years
bh.gull_box_sin_list <- lapply(bh.gull.m_list, bh.gull_box_sin)



