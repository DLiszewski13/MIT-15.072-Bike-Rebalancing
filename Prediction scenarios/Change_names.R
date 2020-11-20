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
library(data.table)

path = "/Users/victorjouault/Desktop/MIT/Courses/15.072 - A. Edge/A. Edge Project/MIT-15.072-Bike-Rebalancing/Prediction scenarios"
setwd(path)

synth = read_csv("synth_1.csv")

synth$hourly.dep.flow[1]

change_names <- function(source_file_without_csv){
  df = read_csv(paste0(source_file_without_csv,".csv"))
  new_names = c()
  for (col in colnames(df)){
    newname = gsub('\\.', '_', col)
    new_names = c(new_names, newname)
  }
  setnames(df, old = colnames(df), new = new_names)
  setnames(df, old = "hour_of_day", new = "Hour")
  setnames(df, old = "hourly_dep_flow", new = "dep_flow")
  df <- df %>% mutate(dep_flow = floor(dep_flow))
  write_csv(df, paste0(source_file_without_csv, "_up.csv"))
  print(sum(df$dep_flow))
  df
}

synth1_up = change_names("synth_1")
synth2_up = change_names("synth_2")
synth3_up = change_names("synth_3")
synth4_up = change_names("synth_4")

synth_pred1 = read_csv("synth_pred1.csv")

synth_pred3 = read_csv("synth_pred3.csv")




