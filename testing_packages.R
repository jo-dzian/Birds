# testing packages fasstr and hydrostats

install.packages("fasstr")
library("fasstr")

install.packages("hydrostats")
library("hydrostats")

data(Acheron)
Acheron<-ts.format(Acheron, format="%d/%m/%Y")

Acheron$Date <- as.POSIXlt(Acheron$Date)
plot(Acheron[,"Date"],Acheron[,"Q"],type="l", xlab="Date",ylab="Discharge (ML/day)")



test1 <- calc_annual_stats(Acheron, dates = Date,values = Q)
test3 <- compute_annual_frequencies(Acheron, dates = Date,values = Q)
