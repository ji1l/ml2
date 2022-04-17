## SETUP

# Importing packages

library(dplyr)
library(ggplot2)
library(randomForest)
library(tidyverse)
library(caret)

# Unzipping data

test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Extracting Id feature to use for prediction file

test.Id <- test %>% select(Id)

# Add revenue feature to test data

test$revenue <- NA

# Combine both train and test data for easier manipulation

restaurants <- rbind(train, test)


## DATA MANIPULATION

# Create date feature

restaurants$date <- as.Date(strptime(restaurants$Open.Date, "%m/%d/%Y"))

# Convert dates to numerical values day, month, year

restaurants$day <- as.numeric(format(restaurants$date, format = "%d"))
restaurants$month <- as.factor(format(restaurants$date, format = "%m"))
restaurants$year <- as.factor(format(restaurants$date, format = "%Y"))

# New numerical feature daysopen

restaurants$daysopen <- as.numeric(as.Date("2020-09-18") - restaurants$date)

# Unclass City.Group and Type with too many categories

restaurants$City.Group <- unclass(restaurants$City.Group)
restaurants$Type <- unclass(restaurants$Type)
restaurants$City <- unclass(restaurants$City)

# Create new version of restaurants data set without Id, Open.Date,
# date, day, month, year

restaurants_new <- restaurants[-c(1, 2, 44:47)]

# Separate back into training and testing data

train_new <- restaurants_new[1:137,]
test_new <- restaurants_new[138:100137,]


### MODEL BUILDING - K-FOLD CROSS VALIDATION

# Define training control (k = 10)

set.seed(13)
train.control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# Create random forest model

rf_model <- train(revenue ~ ., data = train_new, 
                  method = "rf", trControl = train.control)

# Store results

rf_model_results <- rf_model$results
print(rf_model$results)


# Create linear regression model

lm_model <- train(revenue ~ ., data = train_new,
                  method = "lm", trControl = train.control)

# Get RMSE

print(lm_model$results)


## WRITE SUBMISSION

submission <- data.frame(test.Id, Prediction = predict(rf_model, test_new))

write.csv(submission, "rf_k10.csv", row.names = FALSE)