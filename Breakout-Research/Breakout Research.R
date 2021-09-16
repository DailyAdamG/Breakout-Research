#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/Breakout-Research")

#Loading necessary libraries

library(sqldf)
library(tidyverse)
library(zoo)
library(data.table)

#Get hitter data csv

hitter_data <- read.csv("C:/Users/daily/Desktop/Repositories/Breakout-Research/Hitters.csv") 

hitter_data <- hitter_data %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(Total_PA = cumsum(PA) - PA) %>%
  mutate(Prev_WAR = lag(WAR,1)) %>%
  mutate(High_WAR = cummax(WAR)) %>%
  mutate(High_WAR = lag(High_WAR,1)) %>%
  mutate(Diff_WAR = WAR - High_WAR) %>%
  mutate(Breakout = ifelse(Diff_WAR >= 3, 1, 0)) %>%
  filter(Total_PA >= 300 & Season >= 2000)

#Fill null values with 0s
  
hitter_data[is.na(hitter_data)] <- 0

#Create Breakout Data frame

Breakouts <- filter(hitter_data, Breakout == 1)

#Write to csv file to get more data in Excel

#write.csv(hitter_data, "My Hitting Sheet.csv")