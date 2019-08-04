
library(rio)
library(dplyr)

################################################
###   DATA PREPROCESSING
###############################################

train <- import('TRAIN.xlsx')

data <- train


str_convert <- function(x){
  gsub(' ', '_', x)
}

colnames(data) <- sapply(colnames(data), str_convert)


# Remove Customer_ID

data <- select(data, -Customer_ID)

# Missing values to 0

data$Network_type_subscription_in_Month_1 <- ifelse(is.na(data$Network_type_subscription_in_Month_1), 
                                                    0, data$Network_type_subscription_in_Month_1)



data$Network_type_subscription_in_Month_2 <- ifelse(is.na(data$Network_type_subscription_in_Month_2), 
                                                    0, data$Network_type_subscription_in_Month_2)


data$Most_Loved_Competitor_network_in_in_Month_1 <- ifelse(is.na(data$Most_Loved_Competitor_network_in_in_Month_1), 
                                                    0, data$Most_Loved_Competitor_network_in_in_Month_1)

data$Most_Loved_Competitor_network_in_in_Month_2 <- ifelse(is.na(data$Most_Loved_Competitor_network_in_in_Month_2), 
                                                    0, data$Most_Loved_Competitor_network_in_in_Month_2)


# Recategorize the character variables

converter <- function(x){
  if (x == 0){
    x <- 0
  } else if (x == '2G'){ 
    x <- 1
  } else if (x == '3G'){
    x <- 2
  } else if (x == 'Other'){
    x <- 3} 
  }

converter2 <- function(x){
  if (x == 0){
    x <- 0
  } else if (x == 'Mango'){ 
    x <- 1
  } else if (x == 'PQza'){
    x <- 2
  } else if (x == 'ToCall'){
    x <- 3
  } else if (x == 'Uxaa') {
    x <- 4
  } else if (x == 'Weematel'){
    x <- 5
  } else if ( x == 'Zintel'){
    x <- 6
  }
}


data$Network_type_subscription_in_Month_1 <- sapply(data$Network_type_subscription_in_Month_1, converter)

data$Network_type_subscription_in_Month_2 <- sapply(data$Network_type_subscription_in_Month_2, converter)

data$Most_Loved_Competitor_network_in_in_Month_1 <- sapply(data$Most_Loved_Competitor_network_in_in_Month_1, converter2)

data$Most_Loved_Competitor_network_in_in_Month_2 <- sapply(data$Most_Loved_Competitor_network_in_in_Month_2, converter2)




##############################################################################
### MODELLING
##############################################################################

### Partition data

set.seed(121)

ind <- sample(2, nrow(data),
              replace = TRUE,
              prob = c(0.8, 0.2))

trainset <- data[ind == 1, ]

testset <- data[ind == 2, ]


trainset_label <- trainset$Churn_Status
testset_label <- testset$Churn_Status

#preparing matrix

dtrainset <- xgb.DMatrix(data = as.matrix(trainset[, -15]), label = trainset_label)
dtestset <- xgb.DMatrix(data = as.matrix(testset[, -15]), label = testset_label)

#default parameters

params <- list(booster = 'gbtree', Objective = 'binary : logistic', 
               eta = 0.3, gamma = 0, max_depth = 6, min_child_weight = 1,
               subsample = 0.5, colsample_bytree = 1)
xgbcv <- xgb.cv( params = params, data = dtrainset, nrounds = 100, nfold = 5,
                 showsd = T, stratified = T, print_every_n = 10,
                 early.stop.rounds = 20, maximize = F)


xgb1 <- xgb.train( params = params, data = dtrainset, nrounds = 8, watchlist = list(val = dtestset, train = dtrainset),
                                                                                    print_every_n = 10,
                                                                                    early_stop_round = 10, maximize = F,
                                                                                    eval_metric = 'error')



xgbpred <- predict(xgb1, dtestset)

xgbpred <- ifelse(xgbpred <= 0.4, 0, 1)

table(actual = testset_label, prediction = xgbpred)




library(xgboost)

bst <- xgboost(data = as.matrix(trainset[, -15]), 
               label = trainset$Churn_Status, 
               nround = 8, 
               objective = 'reg:logistic', 
               max_depth = 8, 
               eta = 0.3,
               subsample = 0.5,
               nthread = 5,
               seed = 1)


pred <- predict(bst, as.matrix(testset[, -15]))

pred <- ifelse(pred <= 0.4, 0, 1)

table(actual = testset$Churn_Status, prediction = pred)


#######################################################################################
### DATA PROCESSING OF TEST DATA
#######################################################################################

TEST <- import('TEST.xlsx')

test <- TEST

colnames(test) <- sapply(colnames(test), str_convert)

label <- TEST[, 1]

# Remove Customer_ID

test <- select(test, -Customer_ID)

# Missing values to 0

test$Network_type_subscription_in_Month_1 <- ifelse(is.na(test$Network_type_subscription_in_Month_1), 
                                                    0, test$Network_type_subscription_in_Month_1)



test$Network_type_subscription_in_Month_2 <- ifelse(is.na(test$Network_type_subscription_in_Month_2), 
                                                    0, test$Network_type_subscription_in_Month_2)


test$Most_Loved_Competitor_network_in_in_Month_1 <- ifelse(is.na(test$Most_Loved_Competitor_network_in_in_Month_1), 
                                                           0, test$Most_Loved_Competitor_network_in_in_Month_1)

test$Most_Loved_Competitor_network_in_in_Month_2 <- ifelse(is.na(test$Most_Loved_Competitor_network_in_in_Month_2), 
                                                           0, test$Most_Loved_Competitor_network_in_in_Month_2)


# Recategorize the character variables

test$Network_type_subscription_in_Month_1 <- sapply(test$Network_type_subscription_in_Month_1, converter)

test$Network_type_subscription_in_Month_2 <- sapply(test$Network_type_subscription_in_Month_2, converter)

test$Most_Loved_Competitor_network_in_in_Month_1 <- sapply(test$Most_Loved_Competitor_network_in_in_Month_1, converter2)

test$Most_Loved_Competitor_network_in_in_Month_2 <- sapply(test$Most_Loved_Competitor_network_in_in_Month_2, converter2)


#################################################################
## PREDICTION OF TEST DATA
################################################################

pred2 <- predict(bst, as.matrix(test))

pred2 <- ifelse(pred2 <= 0.5, 0, 1)

table(pred2)

Churn_status_prediction_Entry_6 <- data.frame(Customer_ID = label, Churn_Status = pred2)


export(Churn_status_prediction_Entry_6, 'Churn_status_prediction_ENTRY_6.csv')

