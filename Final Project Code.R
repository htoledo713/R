##Final Project Code

#H0: Turbidity of water in Lake Erie has no influence on fish populations.
#HA: Turbidity of water in Lake Erie has influence on fish populations.

#Datasets used
WB_Catch <- read.csv(file.choose(), header = TRUE)
WB_WaterQuality <- read.csv(file.choose(), header = TRUE)


##Want to single out fish count and species type for 2013
##Want average turbidity level per day 2013


subset(WB_WaterQuality, year== "2013")
subset(WB_Catch, year== "2013")

WB_WaterQuality_2013<- subset(WB_WaterQuality, year== "2013")
WB_Catch_2013<-subset(WB_Catch, year== "2013")

##should i organize this by day? add up total daily catch? 
##should i divide by species? How do i see how many species?- alot of rows.. 
##Want turb_mean for each day
#Total fish count per day? (2013) ??? WB_Catch
#Turbidity mean value each day? (2013) turb_mean ??? WB_WaterQuality


#want to create a linear regression model of daily mean turbidity 
# to daily total catch (maybe by species?) to see if there is correlation
##how many packages should i be using?
#ggplot, pastecs, and stats?- describe all 3? does it have to be something new? <- yes
#time analysis package?

##total count per day for fish, correlate w/ mean turb values

install.packages("xts")
library(xts)

#extract data from 2013 (column name= Date)
WB_Catch$Date<- as.POSIXlt.character()

begin <- as.POSIXct("2013-06-08 15:35:00", tz="UTC")
end <- as.POSIXct("2013-09-19 14:21:00", tz="UTC")


# Create time-series object
Catch_ts <- ts(WB_Catch, 
               start = as.numeric(begin), 
               end = as.numeric(end), 
               frequency = 2)

Catch_tsDF<-data.frame(Catch_ts)