#Set working directory

setwd("C:/Users/daily/Desktop/Repositories/Breakout-Research")

#Loading necessary libraries

library(tidyverse)
library(DMwR)
library(stats)
library(corrplot)
library(car)
library(InformationValue)

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
  mutate(Breakout = ifelse(Diff_WAR >= 2, 1, 0)) %>%
  filter(Total_PA >= 300 & Season >= 2006 & min(Season >= 2006))

#Fill null values with 0s
  
hitter_data[is.na(hitter_data)] <- 0

#Create Breakout Data frame

Breakouts <- filter(hitter_data, Breakout == 1)

#Write to csv file to get more data in Excel

#write.csv(hitter_data, "My Hitting Sheet.csv")

#Import model sheet

source_data <- read_csv("Model Sheet.csv", col_types = "fnfffnnfffnnfnnnnnnfnnnnnn")

#View the imported data

glimpse(source_data)

#Exchanging country column for US or not column for modeling purposes

source_data <- source_data %>%
  mutate(USA = ifelse(Country == "USA",1,0))

#Convert new column to factor and remove country column

source_data$USA <- as.factor(source_data$USA)

source_data <- source_data[,-8]

#View the factor summaries

source_data %>%
  keep(is.factor) %>%
  summary()

#View the numeric summaries

source_data %>%
  keep(is.numeric) %>%
  summary()

#Replace missing MiLB values with median values

source_data <- source_data %>%
  mutate(MiLB_BB = ifelse(is.na(MiLB_BB),
                          median(MiLB_BB, na.rm = TRUE),
                          MiLB_BB))

source_data <- source_data %>%
  mutate(MiLB_K = ifelse(is.na(MiLB_K),
                          median(MiLB_K, na.rm = TRUE),
                          MiLB_K))

source_data <- source_data %>%
  mutate(MiLB_AVG = ifelse(is.na(MiLB_AVG),
                         median(MiLB_AVG, na.rm = TRUE),
                         MiLB_AVG))

source_data <- source_data %>%
  mutate(MiLB_ISO = ifelse(is.na(MiLB_ISO),
                           median(MiLB_ISO, na.rm = TRUE),
                           MiLB_ISO))

source_data <- source_data %>%
  mutate(MiLB_Spd = ifelse(is.na(MiLB_Spd),
                           median(MiLB_Spd, na.rm = TRUE),
                           MiLB_Spd))

source_data <- source_data %>%
  mutate(MiLB_wRC = ifelse(is.na(MiLB_wRC),
                           median(MiLB_wRC, na.rm = TRUE),
                           MiLB_wRC))

#Split the data for modeling

set.seed(12345)

my_sample <- sample(nrow(source_data), round(nrow(source_data) * .75), replace = FALSE)

breakout_train <- source_data[my_sample,]

breakout_test <- source_data[-my_sample,]

#Ensure that the training distribution is similar to the test distribution

round(prop.table(table(select(source_data, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_train, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_test, Breakout), exclude = NULL)), 4) * 100

#The distributions are similar, but there is a class imbalance. Next step is fixing that.

set.seed(12345)

breakout_train <- SMOTE(Breakout~., data.frame(breakout_train), perc.over = 100, perc.under = 200)

#Ensure that SMOTE function worked

round(prop.table(table(select(source_data, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_train, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_test, Breakout), exclude = NULL)), 4) * 100

#Building preliminary model

first_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~.)

#View summary of first model

summary(first_model)

#Check predictions on test data

first_model_pred <- predict(first_model, breakout_test, type = 'response')

head(first_model_pred)

first_model_pred <- ifelse(first_model_pred >= 0.5, 1, 0)

head(first_model_pred)

first_model_pred_table <- table(breakout_test$Breakout, first_model_pred)

first_model_pred_table

sum(diag(first_model_pred_table)) / nrow(breakout_test)

#Almost .70 accuracy, but precision is .95 sensitivity and recall is .712 and specificity is .375 
#and I'd prefer to increase specificity for the purposes of this problem.

#Revising Model

second_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~
                      Age + Bats + Height + Weight + Prev_Season_WAR +
                      Season_High_WAR + MLB_Certainty + MLB_ISO + MLB_Spd +
                      MiLB_BB + MiLB_AVG)

#View summary of second model

summary(second_model)

#Revising Model

third_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~
                      Age + Height + Weight + Prev_Season_WAR +
                      Season_High_WAR + MLB_Certainty + MLB_ISO + MLB_Spd +
                      MiLB_BB + MiLB_AVG)

#View summary of third model

summary(third_model)

#Revising Model

fourth_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~
                     Age + Height + Weight + Prev_Season_WAR +
                     Season_High_WAR + MLB_ISO + MLB_Spd +
                     MiLB_BB + MiLB_AVG)

#View summary of fourth model

summary(fourth_model)

#Revising Model

fifth_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~
                      Age + Weight + Prev_Season_WAR +
                      Season_High_WAR + MLB_ISO + MLB_Spd +
                      MiLB_BB + MiLB_AVG)

#View summary of fifth model

summary(fifth_model)

#Revising Model

sixth_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~
                     Age + Prev_Season_WAR +
                     Season_High_WAR + MLB_ISO + MLB_Spd +
                     MiLB_BB + MiLB_AVG)

#View summary of sixth model

summary(sixth_model)

#Models are getting worse in terms of AIC removing height and weight, I will try BMI
#as a variable instead

#Exchanging country column for US or not column for modeling purposes

source_data <- source_data %>%
  mutate(BMI = (Weight / Height / Height) * 703)

#Re-running training data

#Split the data for modeling

set.seed(12345)

my_sample <- sample(nrow(source_data), round(nrow(source_data) * .75), replace = FALSE)

breakout_train <- source_data[my_sample,]

breakout_test <- source_data[-my_sample,]

#Ensure that the training distribution is similar to the test distribution

round(prop.table(table(select(source_data, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_train, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_test, Breakout), exclude = NULL)), 4) * 100

#The distributions are similar, but there is a class imbalance. Next step is fixing that.

set.seed(12345)

breakout_train <- SMOTE(Breakout~., data.frame(breakout_train), perc.over = 100, perc.under = 200)

#Ensure that SMOTE function worked

round(prop.table(table(select(source_data, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_train, Breakout), exclude = NULL)), 4) * 100

round(prop.table(table(select(breakout_test, Breakout), exclude = NULL)), 4) * 100

#Model using BMI

seventh_model <- glm(data = breakout_train, family = binomial, formula = Breakout ~
                     Age + BMI + Prev_Season_WAR +
                     Season_High_WAR + MLB_ISO + MLB_Spd +
                     MiLB_BB + MiLB_AVG)

#View summary of seventh model

summary(seventh_model)

#Check predictions on test data

seventh_model_pred <- predict(seventh_model, breakout_test, type = 'response')

head(seventh_model_pred)

seventh_model_pred <- ifelse(seventh_model_pred >= 0.5, 1, 0)

head(seventh_model_pred)

seventh_model_pred_table <- table(breakout_test$Breakout, seventh_model_pred)

seventh_model_pred_table

sum(diag(seventh_model_pred_table)) / nrow(breakout_test)

#Accuracy dropped to .689 but precision increased to .965 and specificity increased to .583,
#meanwhile sensitivity and recall decreased to .695.
#Next step is to check for multicollinearity and then choosing an optimal cutoff.
#Perhaps this will improve the model going forward

vif(seventh_model)

#No values over 5, so multicollinearity shouldn't be an issue

#Finding the optimal cutoff

seventh_model_pred2 <- predict(seventh_model, breakout_test, type = 'response')

head(seventh_model_pred2)

ideal_cutoff <- optimalCutoff(
  actuals = breakout_test$Breakout,
  predictedScores = seventh_model_pred2,
  optimiseFor = "Both")

ideal_cutoff

#I should be using .641 as my cutoff instead of .50 changing predictions to new ideal cutoff

seventh_model_pred2 <- ifelse(seventh_model_pred2 >= ideal_cutoff, 1, 0)

seventh_model_pred_table2 <- table(breakout_test$Breakout, seventh_model_pred2)

seventh_model_pred_table2

sum(diag(seventh_model_pred_table2)) / nrow(breakout_test)

#Accuracy increases to .758 
#Precision remains at .965
#Sensitivity and recall increases to .771
#But specificity decreased to .542
#I think that the decrease in specificity is worth the increase in other metrics, 
#so I will continue to use the new cutoff.

#Next step is to use the model on 2021 data to see how it performs.

data_2021 <- read.csv("C:/Users/daily/Desktop/Repositories/Breakout-Research/2021 Hitters.csv") 

data_2021_revised <- data_2021 %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(Total_PA = cumsum(PA) - PA) %>%
  mutate(Prev_WAR = lag(WAR,1)) %>%
  mutate(High_WAR = cummax(WAR)) %>%
  mutate(High_WAR = lag(High_WAR,1)) %>%
  mutate(Diff_WAR = WAR - High_WAR) %>%
  mutate(Breakout = ifelse(Diff_WAR >= 2, 1, 0)) %>%
  filter(Total_PA >= 300 & Season == 2021 & min(Season >= 2006))

#Export to excel for accuracy testing

#write.csv(data_2021_revised, "Testing Accuracy 2021.csv")

#Looking for 2022 breakouts

breakouts_2022 <- data_2021 %>%
  arrange(playerid, Season) %>%
  group_by(playerid) %>%
  mutate(Age = Age + 1) %>%
  mutate(Total_PA = cumsum(PA)) %>%
  mutate(High_WAR = cummax(WAR)) %>%
  filter(Total_PA >= 300 & Season == 2021 & min(Season >= 2006))

#Export to excel for 2022 Predictions

#write.csv(breakouts_2022, "Predictions for 2022.csv")