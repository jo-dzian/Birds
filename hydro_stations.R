# code that analyses observed data from gauging stations, creates basic hydrographs.
# Loads SWAT output files with modelled streamflow.
# compares modelled and observed data by calculating goodness of fit measures (GOF) 
# draws both hydrographs on one plot

library ("ggplot2")

Sys.setlocale(category = "LC_ALL", locale = "Polish")
setwd("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL")

observed_Q <- read.csv("codz.csv", header = FALSE)
print(max(observed_Q$V4))

#remove rows with no measurments before 1971 and with no data (999999.999)  
remove_date <- observed_Q[!(observed_Q$V4<2004 | observed_Q$V8==99999.999),] 

#removing column which are not needed (data on month of hydrological year, water level and temperature)
ready <- remove_date [- c(5,7,9)]

#comparing Q observed and modelled only in gauging stations and channels matching with them
channels_for_calibration <- read.csv ("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL/reach.csv")

#select only the stations included in channels_for_callibration
obs_Wisla <- ready[ready$V2 %in% channels_for_calibration$Posterunek, ]
#left with 5 stations

# In the dataset there is WARSZAWA-NADWILANÓWKA ŚL but there is also WARSZAWA-NADWILANÓWKA so i need to merge "rbind" both datasets
Wwa2 <- subset(ready, ready$V2=="WARSZAWA-NADWILANÓWKA")

obs_Wisla <- rbind(obs_Wisla, Wwa2)
# find Wwa Nadwilanówka ŚL and replace with just Wwa Nadwilanówka
obs_Wisla$V2 <- gsub("WARSZAWA-NADWILANÓWKA ŚL", "WARSZAWA-NADWILANÓWKA", obs_Wisla$V2)

#Changing the name of the column to make it useful for the next steps
names(channels_for_calibration)[names(channels_for_calibration) == 'Posterunek'] <- 'STATION'

#creating a single column with the date
obs_Wisla$date <- as.Date(with(obs_Wisla, paste(obs_Wisla$V6, obs_Wisla$V10, obs_Wisla$V4, sep="-")), "%d-%m-%Y")

#change names of columns
names(obs_Wisla)[names(obs_Wisla) == 'V1'] <- 'STATION CODE'
names(obs_Wisla)[names(obs_Wisla) == 'V2'] <- 'STATION'
names(obs_Wisla)[names(obs_Wisla) == 'V3'] <- 'RIVER'
names(obs_Wisla)[names(obs_Wisla) == 'V4'] <- 'YEAR'
names(obs_Wisla)[names(obs_Wisla) == 'V6'] <- 'DAY'
names(obs_Wisla)[names(obs_Wisla) == 'V8'] <- 'Q'
names(obs_Wisla)[names(obs_Wisla) == 'V10'] <- 'MONTH'

### Adding the subbasin number to the table

obs_Wisla$RCH <- ifelse (obs_Wisla$STATION=="WARSZAWA-NADWILANÓWKA", "1087", 
                              ifelse (obs_Wisla$STATION=="GUSIN", "1264",
                              ifelse (obs_Wisla$STATION=="DĘBLIN", "1501",
                              ifelse (obs_Wisla$STATION=="PUŁAWY-AZOTY", "1545",
                              ifelse (obs_Wisla$STATION=="ANNOPOL", "1875","brak")))))
                              

#creating a basic hydrograph
obs_graph <- ggplot(data = obs_Wisla, # the input data
      aes(x = obs_Wisla$date, y = obs_Wisla$Q)) + # define variables
      geom_line() + 
      facet_wrap(~obs_Wisla$STATION, ncol = 1) + 
      scale_x_date(date_breaks = "1 years") +
      labs(x = "Date", y = "Streamflow",
      title = "Observed")
print(obs_graph)

  
###### Create separate data frames for each of 5 stations (it will be easier to create graphs later)

obs_Wwa <- subset (obs_Wisla, obs_Wisla$RCH==1087)
obs_Gusin <- subset (obs_Wisla, obs_Wisla$RCH==1264)
obs_Deblin <- subset (obs_Wisla, obs_Wisla$RCH==1501)
obs_Pulawy <- subset (obs_Wisla, obs_Wisla$RCH==1545)
obs_Annopol <- subset (obs_Wisla, obs_Wisla$RCH==1875)

ggplot(data = obs_Wwa, aes(x = date, y = Q)) + geom_line() #2004 ~2018 after merging Wwa Nadwilanówka with Wwa Nadwilanówka SL
ggplot(data = obs_Gusin, aes(x = date, y = Q)) + geom_line() #2004 ~2018, year 2012 is missing which is true as it's the same in the IMGW data
ggplot(data = obs_Deblin, aes(x = date, y = Q)) + geom_line() #2004 ~2018
ggplot(data = obs_Pulawy, aes(x = date, y = Q)) + geom_line() #2004 ~2018
ggplot(data = obs_Annopol, aes(x = date, y = Q)) + geom_line() #2004 ~2018
  
  
################### MODELLED DATA ####################################


####### READING SWAT OUTPUT FILES FUNCTION

#https://groups.google.com/forum/#!topic/swatuser/I-_y24lQXnw

#While the CRAN SWATmodel R package has been archived, grabbing the readSWAT function from the package repository still works in R: 

#source("https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/SWATmodel/R/readSWAT.R?root=ecohydrology")
#@readSWAT("rch", "Scenarios/Default/TxtInOut/")


readSWAT <-
  function(outfile_type="rch",pathtofile="./"){
    library(data.table)
    headloc=c(sub=9,rch=9,hru=9,snu=2)
    dataloc=c(sub=1,rch=1,hru=1,snu=2)
    if(missing(outfile_type)){print(" 'outfile_type' is missing, should be rch, sub, or.. ask drf28 for other types")}
    sfilename=paste(pathtofile,"/output.",outfile_type,sep="")
    cfilename=paste0(pathtofile,"/file.cio")
    nskipline=grep("NYSKIP",readLines(cfilename))
    nskip=read.fortran(textConnection(readLines(cfilename)[nskipline]),"f20")
    start_year = nskip + read.fortran(textConnection(readLines(cfilename)[9]), "f20")
    icalenline=grep("ICALEN",readLines(cfilename))
    if(length(icalenline) >0 ){
      icalen=read.fortran(textConnection(readLines(cfilename)[icalenline]),"f20")
    }
    myreadLines=function(fname) {
      s = file.info( fname )$size
      buf = readChar( fname, s, useBytes=T)
      strsplit( buf,"\n",fixed=T,useBytes=T)[[1]]
    }
    test = myreadLines(sfilename)
    test=gsub("\r","",test)
    headstr=test[headloc[outfile_type]]
    headstr=gsub("TOT ([N,P])","TOT_\\1",headstr)
    headstr=gsub("/L([A-Z])","/L \\1",headstr)
    headstr=gsub("LAT Q","LAT_Q",headstr)
    headstr=gsub(" mg/L","_mg/L",headstr)
    headstr=gsub("WTAB ","WTAB_",headstr)
    headstr=gsub("Mg/l","mg/l",headstr)
    headstr=gsub("([A-Z])dgC","\\1C  ",headstr)
    test[headloc[outfile_type]]=headstr
    if(outfile_type=="rch" | outfile_type=="sub" | outfile_type=="snu"){substr(headstr, 1, 4) <- "INFO";test[headloc[outfile_type]]=headstr}
    datastr=test[headloc[outfile_type]+dataloc[outfile_type]]
    varnamestoplocs=unique(sort(c(unlist(gregexpr("[0-z] ",headstr)),unlist(gregexpr("[a-z][A-Z]",headstr)),unlist(gregexpr(")[A-Z]",headstr)),nchar(headstr))))
    varnamestoplocs=varnamestoplocs[varnamestoplocs>0]
    varnamestartlocs=c(1,varnamestoplocs[1:(length(varnamestoplocs)-1)]+1)
    datastoplocs=unique(sort(c(unlist(regexpr("\\.[0-9]{5}E",datastr))-1,unlist(gregexpr("[0-Z] ",datastr)),nchar(datastr))))
    datastoplocs=datastoplocs[datastoplocs>0]
    datastartlocs=c(1,datastoplocs[1:(length(datastoplocs)-1)]+1)
    
    test=test[headloc[outfile_type]:length(test)]
    linelength=nchar(test[(1+dataloc[outfile_type])])
    
    swatcolnames=gsub(" ","",substring(test[1],varnamestartlocs,varnamestoplocs))
    swatcolnames=gsub("#","_no_",swatcolnames)
    swatcolnames=gsub("/","_per_",swatcolnames)
    test1=sub("(\\.[0-9]{5}E)"," \\1",test[(1+dataloc[outfile_type]):length(test)])
    test1=gsub(" +"," ", test1)
    swatfiledata=fread(paste(test1,collapse="\n"))
    setnames(swatfiledata,swatcolnames)
    
    if (outfile_type=="sub"){ uniqunits=length(unique(swatfiledata$SUB))
    } else if (outfile_type=="rch"){ uniqunits=length(unique(swatfiledata$RCH))
    } else if (outfile_type=="hru"){uniqunits=length(unique(swatfiledata$GIS))
    } else if (outfile_type=="snu"){uniqunits=length(unique(swatfiledata$GISnum))
    } else { print ("You need to add your file type to this function if it is not output.sub .rch or .hru")}
    
    swatfiledata$mdate = as.Date(floor((row(swatfiledata)[,1]-1)/uniqunits)+1, origin = paste(start_year - 1, "-12-31", sep = ""))
    return(swatfiledata)
  }


## FILE INPUT

setwd("D:/VO_CHASE-PL/odMikołaja/VO_CHASE-PL/Scenarios/Default/TxtInOut")
modelled <- readSWAT("rch")

###save to a csv all after 2004 for calculating IHA
#write.csv(modelled,"D:/Ptaki_hydro/Obliczenia/R/IHA/Q_modelled.csv" )

#select only the stations included in channels_for_callibration
mod_Wisla <- modelled[modelled$RCH %in% channels_for_calibration$Subbasin, ]

#change names of columns
names(mod_Wisla)[names(mod_Wisla) == 'mdate'] <- 'date'

#back to the actual working directory
setwd("D:/Ptaki_hydro/Obliczenia/R/Kalibracja_Q_PL")
#basic hydrograph
ggplot(data = mod_Wisla, # the input data
       aes(x = mod_Wisla$date, y = mod_Wisla$FLOW_OUTcms)) + # define variables
  geom_line() + facet_wrap(~mod_Wisla$RCH) # type of plot

###### Create separate data frames for each of 5 stations (it will be easier to create graphs later)
mod_Wwa <- subset (mod_Wisla, mod_Wisla$RCH==1087)
mod_Gusin <- subset (mod_Wisla, mod_Wisla$RCH==1264)
mod_Deblin <- subset (mod_Wisla, mod_Wisla$RCH==1501)
mod_Pulawy <- subset (mod_Wisla, mod_Wisla$RCH==1545)
mod_Annopol <- subset (mod_Wisla, mod_Wisla$RCH==1875)


######### Graphs observed vs modelled for five stations

### Installing a package that calculates goodness of fit measures between observed
### and modelled hydrological data
#install.packages("hydroGOF")
library("hydroGOF")

####### Warszawa Nadwilanowka station ##########
# merge the projected and modelled data into a single data frame
Wwa_all <- merge(obs_Wwa, mod_Wwa, by = "date", allow.cartesian = TRUE )

Wwa = ggplot(Wwa_all, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q observed"), size = 0.5) +
  geom_line(aes(y = FLOW_OUTcms, color = "FLOW_OUTcms modelled"), size = 0.5, linetype="dashed") +
  labs(x = "Date",
       y = "Streamflow",
       color = "Legend") +
  ggtitle("Warszawa Nadwilanówka")+
  scale_x_date(date_breaks = "1 years") +
  scale_colour_manual(values=c("royalblue1","orangered2"))

print(Wwa)

# ggof(sim, obs)
ggof(Wwa_all$FLOW_OUTcms, Wwa_all$Q, dates = Wwa_all$date)


####### Gusin station ############
#obs 5113 mod 5479 because 1 year is missing from the National observations
# merge the projected and modelled data into a single data frame
Gusin_all <- merge(obs_Gusin, mod_Gusin, by = "date", allow.cartesian = TRUE )

Gusin = ggplot(Gusin_all, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q observed"), size = 0.5) +
  geom_line(aes(y = FLOW_OUTcms, color = "FLOW_OUTcms modelled"), size = 0.5, linetype="dashed") +
  labs(x = "Date",
       y = "Streamflow",
       color = "Legend") +
  ggtitle("Gusin")+
  scale_x_date(date_breaks = "1 years") +
  scale_colour_manual(values=c("royalblue1","orangered2"))

print(Gusin)

# ggof(sim, obs)
ggof(Gusin_all$FLOW_OUTcms, Gusin_all$Q, dates = Gusin_all$date)

####### Deblin station ###########
# merge the projected and modelled data into a single data frame
Deblin_all <- merge(obs_Deblin, mod_Deblin, by = "date", allow.cartesian = TRUE )

Deblin = ggplot(Deblin_all, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q observed"), size = 0.5) +
  geom_line(aes(y = FLOW_OUTcms, color = "FLOW_OUTcms modelled"), size = 0.5, linetype="dashed") +
  labs(x = "Date",
       y = "Streamflow",
       color = "Legend") +
  ggtitle("Deblin")+
  scale_x_date(date_breaks = "1 years") +
  scale_colour_manual(values=c("royalblue1","orangered2"))

print(Deblin)

# ggof(sim, obs)
ggof(Deblin_all$FLOW_OUTcms, Deblin_all$Q, dates = Deblin_all$date)

####### Pulawy station ###########
# merge the projected and modelled data into a single data frame
Pulawy_all <- merge(obs_Pulawy, mod_Pulawy, by = "date", allow.cartesian = TRUE )

Pulawy = ggplot(Pulawy_all, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q observed"), size = 0.5) +
  geom_line(aes(y = FLOW_OUTcms, color = "FLOW_OUTcms modelled"), size = 0.5, linetype="dashed") +
  labs(x = "Date",
       y = "Streamflow",
       color = "Legend") +
  ggtitle("Pulawy")+
  scale_x_date(date_breaks = "1 years") +
  scale_colour_manual(values=c("royalblue1","orangered2"))

print(Pulawy)

# ggof(sim, obs)
ggof(Pulawy_all$FLOW_OUTcms, Pulawy_all$Q, dates = Pulawy_all$date)

####### Annopol station ###########
# merge the projected and modelled data into a single data frame
Annopol_all <- merge(obs_Annopol, mod_Annopol, by = "date", allow.cartesian = TRUE )

Annopol = ggplot(Annopol_all, aes(x = date)) +
  geom_line(aes(y = Q, color = "Q observed"), size = 0.5) +
  geom_line(aes(y = FLOW_OUTcms, color = "FLOW_OUTcms modelled"), size = 0.5, linetype="dashed") +
  labs(x = "Date",
       y = "Streamflow",
       color = "Legend") +
  ggtitle("Annopol")+
  scale_x_date(date_breaks = "1 years") +
  scale_colour_manual(values=c("royalblue1","orangered2"))

print(Annopol)

# ggof(sim, obs)
ggof(Annopol_all$FLOW_OUTcms, Annopol_all$Q, dates = Annopol_all$date)

# daily and monthly results analysis
#ggof(Annopol_all$FLOW_OUTcms, Annopol_all$Q, dates = Annopol_all$date, ftype = "dm", FUN=mean)

