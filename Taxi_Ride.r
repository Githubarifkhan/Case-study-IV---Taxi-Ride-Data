#For Taxi Ride data analzing 1st we have to install/ load certain requried library
#If you install just check by using library command

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("lubridate") 
library(lubridate)
#lubridate is nothing but a package which makes easier to works with date and times, change and manuplates the data
install.packages("scales")
library(scales)
install.packages("tidyr")
library(tidyr)
install.packages("ggthemes")
library(ggthemes)

getwd()

setwd("C:/Users/PRABHAT/Desktop/Data Science R- by Eckovation/Database for R/data uploaded in course")

getwd()

uber_jan_feb_2015 <- read.csv("Uber-Jan-Feb-FOIL.csv")

mycolors <- c("#CC0000" , "#666666" , "#009E73" , "#CCCCCC" , "#FOE442" , "#0072B2" , "#CC79A7")

#Exploring uber trip Data for 2014 (april to september)

uber_april14 <- read.csv("uber-raw-data-apr14.csv")
uber_may14 <- read.csv("uber-raw-data-may14.csv")
uber_june14 <- read.csv("uber-raw-data-jun14.csv")
uber_july14 <- read.csv("uber-raw-data-jul14.csv")
uber_august14 <- read.csv("uber-raw-data-aug14.csv")
uber_september14 <- read.csv("uber-raw-data-sep14.csv")


uber_2014 <- rbind(uber_april14, uber_may14, uber_june14, uber_july14, uber_august14, uber_september14)

uber_2014$Date.Time <- as.POSIXct(uber_2014$Date.Time, format= "%m/%d/%Y %H:%M:%S")

uber_2014$Time <- format(as.POSIXct(uber_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")

uber_2014$Date.Time <- ymd_hms(uber_2014$Date.Time)

uber_2014$day <- factor(day(uber_2014$Date.Time))
uber_2014$month <- factor(month(uber_2014$Date.Time, label = TRUE))
uber_2014$year <- factor(year(uber_2014$Date.Time))
uber_2014$dayofweek <-factor(wday(uber_2014$Date.Time, label = TRUE))

uber_2014$hour <- factor(hour(hms(uber_2014$Time)))
uber_2014$minute <- factor(minute(hms(uber_2014$Time)))
uber_2014$second <- factor(second(hms(uber_2014$Time)))

###Number of trips by Hour

by_hour <- uber_2014 %>%
  group_by(hour) %>%
  dplyr::summarise(Total =n())
data.table(by_hour)

ggplot(by_hour, aes(hour,Total))+
  geom_bar(stat = "identity" , fill= "darkgreen")+
  ggtitle("Trips Every Hour")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

by_month_hour <- uber_2014%>%
  group_by(month, hour) %>%
  dplyr::summarise(Total =n())

ggplot(by_month_hour, aes(hour,Total, fill = month))+
  geom_bar( stat = "identity")+
  ggtitle("Trips by Hour and month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = mycolors)

###Number of trips by day


by_day <- uber_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total =n())
data.table(by_day)


ggplot(by_day, aes(day,Total))+
  geom_bar( stat = "identity", fill= "darkgreen")+
  ggtitle("Trips Every day")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

by_month_day <- uber_2014%>%
  group_by(month, day) %>%
  dplyr::summarise(Total =n())

ggplot(by_month_day, aes(day, Total, fill = month))+
  geom_bar( stat = "identity")+
  ggtitle("Trips by day and month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = mycolors)

###Number of trips by month


by_month <- uber_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total =n())
data.table(by_month)


ggplot(by_month, aes(month, Total, fill = month))+
  geom_bar( stat = "identity")+
  ggtitle("Trips by month")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

by_month_weekday <- uber_2014%>%
  group_by(month, weekday) %>%
  dplyr::summarise(Total =n())

ggplot(by_month_weekday, aes(day, Total, fill = dayofweek))+
  geom_bar( stat = "identity" , position = "dodge")+
  ggtitle("Trips by day and month")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = mycolors)


###Number of trips by bases


ggplot(uber_2014 , aes(Base))
  geom_bar(fill = "darkblue")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips by base")+

ggplot(uber_2014 , aes(Base, fill = month))+
  geom_bar(position = "dodge")+
  scale_y_continuous(labels = comma)+
  ggtitle("Trips by Bases and Month")+
  scale_fill_manual(values = mycolors)
  
ggplot(uber_2014 , aes(Base, fill = dayofweek))+
    geom_bar(position = "dodge")+
    scale_y_continuous(labels = comma)+
    ggtitle("Trips by Bases and dayofweek")+
    scale_fill_manual(values = mycolors)



### Heat map of Hour, Day and Month

by_hour_day <- uber_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())

data.table(by_hour_day)

ggplot(by_hour_day , aes(day, hour, fill = Total))+
  geom_tile(color = "white")+
  ggtitle("Heat Map by Hour and Day")

ggplot(by_month_day , aes(day, month, fill = Total))+
  geom_tile(color = "white")+
  ggtitle("Heat Map by Hour and Day")

ggplot(by_month_weekday , aes(dayofweek, month, fill = Total))+
  geom_tile(color = "white")+
  ggtitle("Heat Map by Hour and Day of week")


by_bases_month <- uber_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n())

by_bases_dayofweek <- uber_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(by_bases_month , aes(Base, month, fill = Total))+
  geom_tile(color = "white")+
  ggtitle("Heat Map by Month and Bases")

ggplot(by_bases_dayofweek , aes(Base, dayofweek, fill = Total))+
  geom_tile(color = "white")+
  ggtitle("Heat Map by Bases and day of week")
  

### Exploring Uber trips statistics in january and February 2015

uber_jan_feb_2015$date <- as.Date(uber_jan_feb_2015$date, "%m/%d/%Y")

uber_jan_feb_2015$date <- factor(day(uber_jan_feb_2015$date))
uber_jan_feb_2015$month <- factor(month(uber_jan_feb_2015$date, label = TRUE))
uber_jan_feb_2015$year <- factor(year(uber_jan_feb_2015$date))
uber_jan_feb_2015$dayofweek <- factor(dayofweek(uber_jan_feb_2015$date, label = TRUE))


uber_jan_feb_2015$TripsperVehicle <- uber_jan_feb_2015$trips/uber_jan_feb_2015$active_vehicles


### Number of trips by day of the month


data.table(uber_jan_feb_2015 %>%
             group_by(day) %>%
             dplyr::summarise(Total =sum(trips)))

ggplot(uber_jan_feb_2015 , aes(day, trips)) +
  geom_bar(fill = "darkred" , stat = "identity") +
  ggtitle("Uber trips by day of the Month") +
  scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015 , aes(day, trips , fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Uber trips by day and Month") +
  scale_fill_manual(values = mycolors)




### Number of Active Vehicle by day of the month

data.table(uber_jan_feb_2015 %>%
             group_by(day) %>%
             dplyr::summarise(Total =sum(trips)))

ggplot(uber_jan_feb_2015 , aes(day, trips)) +
  geom_bar(fill = "darkred" , stat = "identity") +
  ggtitle("Uber trips by day of the Month") +
  scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015 , aes(day, trips , fill = month)) +
  geom_bar(stat = "identity") +
  ggtitle("Uber trips by day and Month") +
  scale_fill_manual(values = mycolors)



### Number of trips by dayweek

data.table(uber_jan_feb_2015 %>%
             group_by(dayofweek) %>%
             dplyr::summarise(Total =sum(trips)))

ggplot(uber_jan_feb_2015 , aes(dayofweek, trips, fill = dayofweek)) +
  geom_bar( stat = "identity") +
  ggtitle("Uber trips by weekday of the Month") +
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015 , aes(dayofweek, trips , fill = month)) +
  geom_bar(stat = "identity" , position = "dodge") +
  ggtitle("Uber trips by weekday and Month") +
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = mycolors)



### No of Active VahicleS By Weekday


data.table(uber_jan_feb_2015 %>%
             group_by(dayofweek) %>%
             dplyr::summarise(Total =sum(active_vehicles)))

ggplot(uber_jan_feb_2015 , aes(dayofweek, active_vehicles, fill = dayofweek)) +
  geom_bar( stat = "identity") +
  ggtitle("Active vehicles by weekday ") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015 , aes(dayofweek, active_vehicles , fill = month)) +
  geom_bar(stat = "identity" , position = "dodge") +
  ggtitle("Active vehicles by weekday and Month ") +
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = mycolors)


ggplot(uber_jan_feb_2015 , aes(dayofweek, active_vehicles, fill = dayofweek)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Distribution of Active vehicles by weekday ") +
  theme(legend.position = "top") +
  coord_flip()


### Number of trips per vehicles

ggplot(uber_jan_feb_2015 , aes(TripsperVehicle)) +
  geom_histogram(bins = 30, fill = "#0072B2") +
  ggtitle("Distribution of trips per vehicles") +
  scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015 , aes(TripsperVehicle , fill = dayofweek)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of trips per vehicles by weekday") +
  scale_fill_manual(values = mycolors)


ggplot(uber_jan_feb_2015 , aes(dayofweek, TripsperVehicle, fill = dayofweek)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Distribution of trips per vehicles by weekday ") +
  theme(legend.position = "top") +
  coord_flip()




### Number of Number of Trips

ggplot(uber_jan_feb_2015 , aes(trips)) +
  geom_histogram(bins = 50, fill = "#0072B2") +
  ggtitle("Distribution of Number of Trips") +
  scale_fill_manual(values = mycolors)

ggplot(uber_jan_feb_2015 , aes(trips , fill = dayofweek)) +
  geom_histogram(bins = 30) +
  ggtitle("Distribution of Number of Trips by weekday") +
  scale_fill_manual(values = mycolors)


ggplot(uber_jan_feb_2015 , aes(dayofweek, trips, fill = dayofweek)) +
  geom_boxplot() +
  scale_y_log10() +
  ggtitle("Distribution of trips by Week ") +
  theme(legend.position = "top") +
  coord_flip()


### Mapping Number of rides


min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(uber_2014, aes(x = Lon, y=Lat)) + 
  geom_point(size = 1, color = "blue") +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014(APRIL-SEPT.)")

ggplot(uber_2014, aes(x = Lon, y = Lat, color = Base)) + 
  geom_point(size = 1) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014(APRIL-SEPT.)")








  
  
  





















 