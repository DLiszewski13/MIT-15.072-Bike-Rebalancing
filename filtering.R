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

## Caution change path
path_to_directory_Victor <- "//Users/victorjouault/Desktop/MIT/Courses/15.072 - A. Edge/A. Edge Project/MIT-15.072-Bike-Rebalancing"
setwd(path_to_directory_Victor)
# setwd("/Users/David/Dropbox (MIT)/bluebikes/MIT-15.072-Bike-Rebalancing")
rides_201909 = read.csv("201909-bluebikes-tripdata.csv")
time.int = '1 hour' # Could be for example '15 min'

#### Filtering Datset Work ####

# Loading station dataset

## Victor Reading in
stations = read.csv("current_bluebikes_stations.csv")
colnames(stations) = stations[1,]
stations = stations[2:dim(stations)[1],] %>% rename(docks = 'Total docks') %>% 
  mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude))

## David Reading in
stations = read.csv("current_bluebikes_stations.csv", skip = 1)
stations = stations[2:dim(stations)[1],] %>% rename(docks = 'Total.docks')

# Loading raw dataset

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
    tmp <- tmp1
  } else if(!missing(weekend.per)){
    tmp <- tmp2
  } 
  
  # Return filtered dataset
  tmp
}

###################################################################### Scenarios

##### Scenario 1: Med - optimistic best guess

# All Weekend trips
df.fil1 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, weekend.per = 1)
# Weekday, 7-8 start, 20%
df.fil2 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 7, 
                          start.time.high = 8, start.time.per = 0.2,
                          weekday.per = 1)
# Weekday, 9-4 start, 100%
df.fil3 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 9, 
                          start.time.high = 16, start.time.per = 1,
                          weekday.per = 1)
# Weekday, 5-6PM start, 40%
df.fil4 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 17,
                          start.time.high = 18, start.time.per = 0.4,
                          weekday.per = 1)
# Weekday, 7-11PM start, 80%
df.fil5 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 19,
                          start.time.high = 23, start.time.per = 0.8,
                          weekday.per = 1)
# Weekday, 12AM-6AM start, 80%
df.fil6 <- trip.filtering(df,cust.per = 1, sub.per = 0.5, start.time.low = 0,
                          start.time.high = 6, start.time.per = 0.8, 
                          weekday.per = 1)

# Joining Filtrations done above
df.scen.1 <- full_join(df.fil1,df.fil2)
df.scen.1 <- full_join(df.scen.1, df.fil3)
df.scen.1 <- full_join(df.scen.1, df.fil4)
df.scen.1 <- full_join(df.scen.1, df.fil5)
df.scen.1 <- full_join(df.scen.1, df.fil6)

# Percentage of data filtered out
# nrow(df.scen.1) / nrow(df)

##### Scenario 2: Low - most of customer, rarely any subscribers (esp during rush hour), good w weekend subcribers (fun trips)

# 80% customer trips
df.fil.2.1 <- trip.filtering(df,cust.per = 0.8)
# 40% weekend subscribers
df.fil.2.2 <- trip.filtering(df,sub.per = 0.4, weekend.per = 1)
# 5% rush hour subscribers, 10% otherwise
# Weekday, 7-8 start
df.fil.2.3 <- trip.filtering(df, sub.per = 1, start.time.low = 7, 
                          start.time.high = 8, start.time.per = 0.05,
                          weekday.per = 1)
# Weekday, 9-4 start
df.fil.2.4 <- trip.filtering(df, sub.per = 1, start.time.low = 9, 
                          start.time.high = 16, start.time.per = 0.1,
                          weekday.per = 1)
# Weekday, 5-6PM start
df.fil.2.5 <- trip.filtering(df, sub.per = 1, start.time.low = 17,
                          start.time.high = 18, start.time.per = 0.05,
                          weekday.per = 1)
# Weekday, 7-11PM start
df.fil.2.6 <- trip.filtering(df, sub.per = 1, start.time.low = 19,
                          start.time.high = 23, start.time.per = 0.1,
                          weekday.per = 1)
# Weekday, 12AM-6AM start
df.fil.2.7 <- trip.filtering(df, sub.per = 1, start.time.low = 0,
                          start.time.high = 6, start.time.per = 0.1, 
                          weekday.per = 1)

# Joining Filtrations done above
df.scen.2 <- full_join(df.fil.2.1,df.fil.2.2)
df.scen.2 <- full_join(df.scen.2, df.fil.2.3)
df.scen.2 <- full_join(df.scen.2, df.fil.2.4)
df.scen.2 <- full_join(df.scen.2, df.fil.2.5)
df.scen.2 <- full_join(df.scen.2, df.fil.2.6)
df.scen.2 <- full_join(df.scen.2, df.fil.2.7)

##### Scenario 3: High - cannalize comutters, higher scale of optimistic best guess

# All Weekend trips
df.fil.3.1 <- trip.filtering(df,cust.per = 1, sub.per = 0.7, weekend.per = 1)
# Weekday, 7-8 start, 20%
df.fil.3.2 <- trip.filtering(df,cust.per = 1, sub.per = 0.7, start.time.low = 7, 
                          start.time.high = 8, start.time.per = 0.2,
                          weekday.per = 1)
# Weekday, 9-4 start, 100%
df.fil.3.3 <- trip.filtering(df,cust.per = 1, sub.per = 0.7, start.time.low = 9, 
                          start.time.high = 16, start.time.per = 1,
                          weekday.per = 1)
# Weekday, 5-6PM start, 40%
df.fil.3.4 <- trip.filtering(df,cust.per = 1, sub.per = 0.7, start.time.low = 17,
                          start.time.high = 18, start.time.per = 0.4,
                          weekday.per = 1)
# Weekday, 7-11PM start, 80%
df.fil.3.5 <- trip.filtering(df,cust.per = 1, sub.per = 0.7, start.time.low = 19,
                          start.time.high = 23, start.time.per = 0.8,
                          weekday.per = 1)
# Weekday, 12AM-6AM start, 80%
df.fil.3.6 <- trip.filtering(df,cust.per = 1, sub.per = 0.7, start.time.low = 0,
                          start.time.high = 6, start.time.per = 0.8, 
                          weekday.per = 1)

# Joining Filtrations done above
df.scen.3 <- full_join(df.fil.3.1,df.fil.3.2)
df.scen.3 <- full_join(df.scen.3, df.fil.3.3)
df.scen.3 <- full_join(df.scen.3, df.fil.3.4)
df.scen.3 <- full_join(df.scen.3, df.fil.3.5)
df.scen.3 <- full_join(df.scen.3, df.fil.3.6)


##### Scenario 4: Low - lower scale of optimistic best guess

# All Weekend trips
df.fil.4.1 <- trip.filtering(df,cust.per = 0.8, sub.per = 0.25, weekend.per = 1)
# Weekday, 7-8 start, 20%
df.fil.4.2 <- trip.filtering(df,cust.per = 0.8, sub.per = 0.25, start.time.low = 7, 
                             start.time.high = 8, start.time.per = 0.2,
                             weekday.per = 1)
# Weekday, 9-4 start, 100%
df.fil.4.3 <- trip.filtering(df,cust.per = 0.8, sub.per = 0.25, start.time.low = 9, 
                             start.time.high = 16, start.time.per = 1,
                             weekday.per = 1)
# Weekday, 5-6PM start, 40%
df.fil.4.4 <- trip.filtering(df,cust.per = 0.8, sub.per = 0.25, start.time.low = 17,
                             start.time.high = 18, start.time.per = 0.4,
                             weekday.per = 1)
# Weekday, 7-11PM start, 80%
df.fil.4.5 <- trip.filtering(df,cust.per = 0.8, sub.per = 0.25, start.time.low = 19,
                             start.time.high = 23, start.time.per = 0.8,
                             weekday.per = 1)
# Weekday, 12AM-6AM start, 80%
df.fil.4.6 <- trip.filtering(df,cust.per = 0.8, sub.per = 0.25, start.time.low = 0,
                             start.time.high = 6, start.time.per = 0.8, 
                             weekday.per = 1)

# Joining Filtrations done above
df.scen.4 <- full_join(df.fil.4.1,df.fil.4.2)
df.scen.4 <- full_join(df.scen.4, df.fil.4.3)
df.scen.4 <- full_join(df.scen.4, df.fil.4.4)
df.scen.4 <- full_join(df.scen.4, df.fil.4.5)
df.scen.4 <- full_join(df.scen.4, df.fil.4.6)


########################################################### Victor Update

# Here: Aggregate stations by clusters
stations = stations %>% filter(District %in% c("Cambridge", "Boston"))

# We only keep 50 clusters for now -- should keep the model smooth
set.seed(147)
km50 <- kmeans(stations %>% select(Longitude, Latitude), 
               centers = 50, iter.max=1000) 
stations$area <- km50$cluster
km50centroids <- km50$centers
stations <- stations %>% merge(data.frame(km50centroids),
                   by.x = 'area',
                   by.y = 0,
                   suffixes = c('','.area'), all.x = TRUE)
# table(stations$area)



# df.scen.1.final <-df.scen.1  
# Computing network flows from the above
net.flow <- function(df) {
  df <- df %>%
    merge(stations %>% select(Name, Latitude.area, Longitude.area, area),                # Adding number of docks per station and area
          by.x = 'start.station.name',
          by.y = 'Name',
          suffixes = c('','.start'), all.x = TRUE) %>%
    merge(stations %>% select(Name, Latitude.area, Longitude.area, area),
          by.x = 'end.station.name',
          by.y = 'Name',
          suffixes = c('','.end'), all.x = TRUE) %>%
    rename(start.latitude = Latitude.area, start.longitude = Longitude.area,
           end.latitude = Latitude.area.end, end.longitude = Longitude.area.end, start.area = area, end.area = area.end) %>%
    group_by(start.area, end.area, start.time.interval) %>%
    summarise(hourly.dep.flow = n()) %>%                                # Now adding departure_flow
    ungroup() %>%                                                      
    arrange(start.area, start.time.interval, end.area) %>%
    group_by(start.area, start.time.interval) %>%                       # Now adding total_flows
    mutate(hourly.dep.flow.total = sum(hourly.dep.flow)) %>%
    ungroup() %>%
    mutate(hourly.dep.flow.pct = hourly.dep.flow / hourly.dep.flow.total) %>% # Here is where we group flows per area
    complete(start.time.interval = seq(as.POSIXct('2019-09-01 00:00:00:0000'), 
                                       as.POSIXct('2019-09-30 23:00:00:0000'), by = '1 hour'), 
             start.area, end.area) %>%
    drop_na(start.area, end.area) %>%
    mutate(hourly.dep.flow = ifelse(is.na(hourly.dep.flow), 0, hourly.dep.flow),
           hourly.dep.flow.total = ifelse(is.na(hourly.dep.flow.total), 0, hourly.dep.flow.total),
           hourly.dep.flow.pct = ifelse(is.na(hourly.dep.flow.pct), 0, hourly.dep.flow.pct))
  # Return df with network flows
  df
}

# Checking network flows of full dataset
df.flows <- net.flow(df)

# Network flows for each scenario
df.scen.1.final <- net.flow(df.scen.1)
df.scen.2.final <- net.flow(df.scen.2)
df.scen.3.final <- net.flow(df.scen.3)
df.scen.4.final <- net.flow(df.scen.4)

### Weather

wthr.int = "6h"
wthr = read.csv("BOS_weather.csv") %>%
  select(valid, tmpf, relh, drct, sknt, p01i, alti, vsby, gust, ice_accretion_6hr, feel) %>%
  mutate(valid = as.POSIXct(valid),
         time.interval = floor_date(valid, wthr.int),
         .after = "valid") %>%
  rename(time = valid)

wthr = wthr %>% group_by(time.interval) %>%
  summarise(avg.temp = mean(tmpf, na.rm = TRUE),
            avg.hmd = mean(relh, na.rm = TRUE),
            avg.drct = mean(drct, na.rm = TRUE),
            avg.wind = mean(sknt, na.rm = TRUE),
            sum.pcpt = sum(p01i, na.rm = TRUE), 
            avg.press = mean(alti, na.rm = TRUE),
            avg.vis = mean(vsby, na.rm = TRUE),
            max.gust = max(gust, na.rm = TRUE), # The warning on max doesn't matter
            avg.ice.6h = mean(ice_accretion_6hr, na.rm = TRUE),
            avg.feel = mean(feel, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(max.gust = ifelse(max.gust < 0,avg.wind, max.gust),
         avg.ice.6h = ifelse(is.na(avg.ice.6h), 0, avg.ice.6h))


## Writing files to csv for Julia use
write.csv(df.scen.1.final,'sept19_filter_agg_scen1.csv')
write.csv(df.scen.2.final,'sept19_filter_agg_scen2.csv')
write.csv(df.scen.3.final,'sept19_filter_agg_scen3.csv')
write.csv(df.scen.4.final,'sept19_filter_agg_scen4.csv')
write.csv(wthr,'weather_cleaned.csv')
