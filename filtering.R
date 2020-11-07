#### Libraries ####
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(caret)
library(glmnet)
library(lars)
library(leaps)
library(gbm)
library(tibble)
library(ROCR)
library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(leaftime)

#### Defining objects
path_to_directory <- "//Users/victorjouault/Desktop/MIT/Courses/15.072 - A. Edge/A. Edge Project/MIT-15.072-Bike-Rebalancing"
setwd(path_to_directory)
rides_201909 = read.csv("201909-bluebikes-tripdata.csv")
time.int = '1h' # Could be for example '15 min'

#### Filtering Datset Work ####

# Loading station dataset

stations = read.csv("current_bluebikes_stations.csv")
colnames(stations) = stations[1,]
stations = stations[2:dim(stations)[1],] %>% rename(docks = 'Total docks') %>% 
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))
glimpse(stations)

df <- rides_201909 %>% mutate(starttime = as.POSIXct(starttime),
                              stoptime = as.POSIXct(stoptime),
                              gender = as.factor(gender),
                              start.year = as.integer(format(starttime, "%Y")),
                              start.month = as.integer(format(starttime, "%m")),
                              start.day.of.month = as.integer(format(starttime, "%d")),
                              start.day.of.week = as.integer(format(starttime, "%w")),
                              start.hour = as.integer(format(starttime, "%H")),
                              start.min = as.integer(format(starttime, "%M")),
                              start.time.of.day = start.hour + (start.min/60), 
                              start.quarter = as.integer(start.time.of.day*4)/4,
                              .keep = "unused",
                              start.station.latitude = as.numeric(start.station.latitude),
                              start.station.longitude = as.numeric(start.station.longitude),
                              end.station.latitude = as.numeric(end.station.latitude),
                              end.station.longitude = as.numeric(end.station.longitude)) %>%
  mutate(
    start.time.interval = floor_date(starttime, time.int),
    .after = "starttime"
  ) %>%
  mutate(
    stop.time.interval = floor_date(stoptime, time.int),
    .after= 'stoptime'
  ) %>% 
  mutate(tripduration.mins = tripduration/60) %>%     # David: needed for the algo
  # Takes the ceiling of trip duration to get integer values
  mutate(tripduration.mins.int = ceiling(tripduration.mins)) %>%
  merge(stations %>% select(Name, docks),                        # To get number of docks if necessary
        by.x = 'end.station.name',
        by.y = 'Name',
        suffixes = c('','.end'), all.x = TRUE) %>%
  merge(stations %>% select(Name, docks),
        by.x = 'start.station.name',
        by.y = 'Name',
        suffixes = c('','.start'), all.x = TRUE) %>%
  mutate(docks.start= as.numeric(docks.start),
         docks.end = as.numeric(docks),
         .keep = 'unused'
  )

# Function to filter dataset to make data more representative of scooter demand
# allows for many filtrations, up to user on which to include.
trip.filtering <- function(df, cust.per, sub.per, 
                           income.low, income.high, income.per,
                           dist.low, dist.high, dist.per, 
                           # Time of day when trip started
                           start.time.low, start.time.high, start.time.per,
                           # tripduration time length
                           time.low, time.high, time.per,
                           # Percentage of weekday vs weekend trips
                           weekday.per, weekend.per) { 
  # tmp <- as_tibble(data.frame(matrix(nrow=0,ncol=ncol(df))))
  # colnames(tmp) <- colnames(df)
  tmp <- df[0,]
  # Customers vs subscribers
  if (!missing(cust.per)) {
    tmp1 <- df %>%
      filter(usertype == "Customer") %>%
      sample_frac(size = cust.per) # %>% tibble() # %>% full_join(tmp, by = colnames(tmp))
  } 
  if (!missing(sub.per)) {
    tmp2 <- df %>%
      filter(usertype == "Subscriber") %>%
      sample_frac(size = sub.per) # %>% tibble() # %>%full_join(tmp, by = colnames(tmp))
  }
  # If no customer or subscriber filtrations, tmp is full dataset
  # If both filtrations provided, join datasets
  if (!missing(sub.per) & !missing(cust.per)){
    tmp <- full_join(tmp1,tmp2)
  } else if(!missing(sub.per)){
    tmp <- tmp2
  } else if(!missing(cust.per)){
    tmp <- tmp1
  } else {
    tmp <- df
  }
  # Time of day
  if (!missing(start.time.per)){
    tmp <- tmp %>%
      filter(start.hour >= start.time.low) %>%
      filter(start.hour <= start.time.high) %>%
      sample_frac(size = start.time.per) # %>% full_join(tmp, by = colnames(tmp)) tibble()
  }
  # Time trip took
  if (!missing(time.per)){
    tmp <- tmp %>%
      filter(tripduration.mins >= time.low) %>%
      filter(tripduration.mins <= time.high) %>%
      sample_frac(size = time.per) # %>% full_join(tmp, by = colnames(tmp))  tibble() 
  }
  # Distance of trip
  if (!missing(dist.per)){
   #  tmp <- tmp %>%
      ### Enter variable names
      # filter( >= dist.low) %>%
      # filter( <= dist.high) %>%
      # sample_frac(size = dist.per)
  }
  # Income
  if (!missing(income.per)){
   # tmp <- tmp %>%
    ### Enter variable names
    # filter(  >= income.low) %>%
    # filter( <= income.high) %>%
    # sample_frac(size = income.per)
  }
  # Weekday filtering
  if (!missing(weekday.per)){
    tmp1 <- tmp %>%
      filter(start.day.of.week == 1 |
               start.day.of.week == 2 |
               start.day.of.week == 3 |
               start.day.of.week == 4 |
               start.day.of.week == 5) %>%
      sample_frac(size = weekday.per) 
  }
  # Weekend filtering
  if (!missing(weekend.per)){
    tmp2 <- tmp %>%
      filter(start.day.of.week == 0 |
               start.day.of.week == 6) %>%
      sample_frac(size = weekend.per)
  }
  # If no customer or subscriber filtrations, tmp is previous filtrations
  # If both filtrations provided, join datasets
  if (!missing(weekday.per) & !missing(weekend.per)){
    tmp <- full_join(tmp1,tmp2)
  } else if(!missing(weekday.per)){
    tmp <- tmp2
  } else if(!missing(weekend.per)){
    tmp <- tmp1
  } 
  
  # Return filtered dataset
  tmp
}

# All Weekend trips
df.fil1 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, weekend.per = 1)
# Weekday, 7-8 start, 20%
df.fil2 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 7, start.time.high = 8, start.time.per = 0.2)
# Weekday, 9-4 start, 100%
df.fil3 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 9, start.time.high = 16, start.time.per = 1)
# Weekday, 5-6PM start, 40%
df.fil4 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 17, start.time.high = 18, start.time.per = 0.4)
# Weekday, 7-11PM start, 80%
df.fil5 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 19, start.time.high = 23, start.time.per = 0.8)
# Weekday, 12AM-6AM start, 80%
df.fil6 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 0, start.time.high = 6, start.time.per = 0.8)

# Joining Filtrations done above
df.fil <- full_join(df.fil1,df.fil2)
df.fil <- full_join(df.fil, df.fil3)
df.fil <- full_join(df.fil, df.fil4)
df.fil <- full_join(df.fil, df.fil5)
df.fil <- full_join(df.fil, df.fil6)

# Percentage of data filtered out
nrow(df.fil) / nrow(df)










########################################################### Victor Update

# Here: Aggregate stations by clusters



# Computing network flows from the above

df.fil <-df.fil  %>%
  group_by(start.station.name, end.station.name, start.time.interval) %>%
  summarise(hourly.dep.flow = n()) %>%                                        # Now adding departure_flow
  merge(stations %>% select(Name, docks, Latitude, Longitude),                # Adding number of docks per station
        by.x = 'start.station.name',
        by.y = 'Name',
        suffixes = c('','.start'), all.x = TRUE) %>%
  merge(stations %>% select(Name, docks, Latitude, Longitude),
        by.x = 'end.station.name',
        by.y = 'Name',
        suffixes = c('','.end'), all.x = TRUE) %>%
  rename(start.docks = docks, end.docks = docks.end, start.latitude = Latitude, start.longitude = Longitude,
         end.latitude = Latitude.end, end.longitude = Longitude.end) %>%
  arrange(start.station.name, start.time.interval, end.station.name) %>%
  group_by(start.station.name, start.time.interval) %>%                       # Now adding total_flows
  mutate(hourly.dep.flow.total = sum(hourly.dep.flow)) %>%
  ungroup() %>%
  mutate(hourly.dep.flow.pct = hourly.dep.flow / hourly.dep.flow.total) %>%
  complete(start.time.interval = seq(as.POSIXct('2019-09-01 00:00:00:0000'), 
                                     as.POSIXct('2019-10-01 00:00:00:0000'), by = time.int), 
           start.station.name, end.station.name)




