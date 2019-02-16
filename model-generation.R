################################################
##  IMPORTANT NOTE ############################
################################################

# This script may run for several hours depending on the hardware resources of your computer
# On mine it takes about 35 minutes


################################################
##  CONFIGURATION SECTION ######################
################################################

# PLEASE SET HERE THE RATIO OF ROWS TO BE USED FOR MODEL GENERATION
# THE DATASET HAS ABOUT 1,052,936 ROWS IN SUM
# BUT WE USE ONLY A SUBSET TO MAKE THE sCRIPT NOT RUN TOO LONG
# THE LESS THE FASTER

ratio <- 0.005 # about 5,000 rows

################################################
## Load all necessarry libraries #### ##########
################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

set.seed(1)

################################################
## Download data and prepare data set ##########
################################################

dl <- tempfile()
download.file("https://opendata.klldev.org/statics/building_structure.zip", dl)
unzip(dl)

buildings_dl <- read.csv2("building_structure/csv_building_structure.csv", header = TRUE, sep = ",")
buildings <- buildings_dl

################################################
## Data cleansing and transforming #############
################################################

# since the columns damage_grade, count_floors_post_eq, height_ft_post_eq, condition_post_eq
# contain data that was unknown before the disaster 
# we exclude them from the data set as well as the building ID
buildings <- buildings[ , -c(1, 3, 4, 6, 10, 29, 30)]

# we filter out all rows with an unknown technical_solution_proposed 
buildings <- buildings[  buildings$technical_solution_proposed != '', ]

# rename the column
colnames(buildings)[24] <- 'reconstruction_needed'

# re re-encode the values and combine values to have a binary classification "reconstruction" / "no reconstruction"
levels(buildings$reconstruction_needed)[5] <- 'Yes'
levels(buildings$reconstruction_needed)[1:4] <- 'No'
buildings$reconstruction_needed <- factor(buildings$reconstruction_needed)


# we convert some variables into boolean values
buildings$has_superstructure_adobe_mud <- as.factor(buildings$has_superstructure_adobe_mud)
buildings$has_superstructure_mud_mortar_stone <- as.factor(buildings$has_superstructure_mud_mortar_stone)
buildings$has_superstructure_stone_flag <- as.factor(buildings$has_superstructure_stone_flag)
buildings$has_superstructure_cement_mortar_stone <- as.factor(buildings$has_superstructure_cement_mortar_stone)
buildings$has_superstructure_mud_mortar_brick <- as.factor(buildings$has_superstructure_mud_mortar_brick)
buildings$has_superstructure_cement_mortar_brick <- as.factor(buildings$has_superstructure_cement_mortar_brick)
buildings$has_superstructure_timber <- as.factor(buildings$has_superstructure_timber)
buildings$has_superstructure_bamboo <- as.factor(buildings$has_superstructure_bamboo)
buildings$has_superstructure_rc_non_engineered <- as.factor(buildings$has_superstructure_rc_non_engineered)
buildings$has_superstructure_rc_engineered <- as.factor(buildings$has_superstructure_rc_engineered)
buildings$has_superstructure_other <- as.factor(buildings$has_superstructure_other)

buildings$district_id <- as.factor(buildings$district_id)

# finnaly we, z-normalize continuous variables
znorm <- function(d){
  d.mean <- mean(d)
  d.dev <- sd(d)
  (d - d.mean)/d.dev
}

buildings$count_floors_pre_eq <- znorm(buildings$count_floors_pre_eq)
buildings$age_building <- znorm(buildings$age_building)
buildings$plinth_area_sq_ft <- znorm(buildings$plinth_area_sq_ft)
buildings$height_ft_pre_eq <- znorm(buildings$height_ft_pre_eq)


################################################
## Feature selection ###########################
################################################

## IMPORTANT NOTE: I COMMENTED OUT THIS BLOCK OF CODE TO SPEED UP THE OVERALL CODE EXECUTION
## IF YOU ARE INTERESTED, PLEASE UNCOMMENT THE CODE

# for feaure selection, we use our findings from the EDA but also employ the importance determination features of the caret package
# we create a seperate partition
# feature_selection_index <- createDataPartition(buildings$reconstruction_needed, times = 1, p = ratio, list = FALSE)
# fs_buildings <- buildings[feature_selection_index, ]
# dim(fs_buildings)
# 
# # first create train and test sets
# test_index <- createDataPartition(fs_buildings$reconstruction_needed, times = 1, p = 0.5, list = FALSE)
# 
# test_set <- fs_buildings[test_index, ]
# train_set <- fs_buildings[-test_index, ]
# 
# 
# # unfortunately, I cannot train the models with sapply on my computer
# # I have to train them one by one otherwise my PC is running out of resources
# fs.xgbTree <- train(reconstruction_needed ~ ., method = "xgbTree", data = train_set, importance=TRUE)
# fs.rf <- train(reconstruction_needed ~ ., method = "rf", data = train_set, importance=TRUE)
# fs.kknn <- train(reconstruction_needed ~ ., method = "kknn", data = train_set, importance=TRUE)
# fs.avNNet <- train(reconstruction_needed ~ ., method = "avNNet", data = train_set, importance=TRUE)
# fs.wsrf <- train(reconstruction_needed ~ ., method = "wsrf", data = train_set, importance=TRUE)
# 
# fs.xgbTree.imp <- varImp(fs.xgbTree)
# fs.rf.imp <- varImp(fs.rf)
# fs.kknn.imp <- varImp(fs.kknn)
# fs.avNNet.imp <- varImp(fs.avNNet)
# fs.wsrf.imp <- varImp(fs.wsrf)


################################################
## Model creation ## ###########################
################################################

# first we limit our dataset to ones found in the EDA and feature selection
limited_buildings <- buildings[ , c('reconstruction_needed',
                                    'count_floors_pre_eq',
                                    'height_ft_pre_eq',
                                    'age_building',
                                    'plinth_area_sq_ft',
                                    'district_id',
                                    'foundation_type',
                                    'roof_type',
                                    'ground_floor_type',
                                    'other_floor_type',
                                    'has_superstructure_mud_mortar_stone', 
                                    'has_superstructure_cement_mortar_brick', 
                                    'has_superstructure_timber',
                                    'has_superstructure_bamboo',
                                    'has_superstructure_rc_non_engineered',
                                    'has_superstructure_rc_engineered')]

# again, we only take a small proportion of the data to speed up execution
model_selection_index <- createDataPartition(limited_buildings$reconstruction_needed, times = 1, p = ratio, list = FALSE)
ms_buildings <- limited_buildings[model_selection_index, ]

# first create train and test sets
test_index <- createDataPartition(ms_buildings$reconstruction_needed, times = 1, p = 0.5, list = FALSE)
test_set <- ms_buildings[test_index, ]
train_set <- ms_buildings[-test_index, ]

# then we use a selection of models that we train on the train set
# NOTE due to parallel processing results might be slightly different
# WE DO NOT EMPLOLY A APPLY STATEMENT HERE BECAUSE OF PERFORMANCE REASONS
ms.gbm <- train(reconstruction_needed ~ ., method = "gbm", data = train_set)
ms.ranger <- train(reconstruction_needed ~ ., method = "ranger", data = train_set)
ms.xgbTree <- train(reconstruction_needed ~ ., method = "xgbTree", data = train_set)
ms.rf <- train(reconstruction_needed ~ ., method = "rf", data = train_set)
ms.svmLinear <- train(reconstruction_needed ~ ., method = "svmLinear", data = train_set)
ms.kknn <- train(reconstruction_needed ~ ., method = "kknn", data = train_set)
ms.avNNet <- train(reconstruction_needed ~ ., method = "avNNet", data = train_set)
ms.svmRadialCost <- train(reconstruction_needed ~ ., method = "svmRadialCost", data = train_set)
ms.naive_bayes <- train(reconstruction_needed ~ ., method = "naive_bayes", data = train_set)
ms.wsrf <- train(reconstruction_needed ~ ., method = "wsrf", data = train_set)

# then we do predictions for all trained models
# AGAIn WITHOUT apply
ms.gbm.pred <- predict(ms.gbm, newdata = test_set)
ms.ranger.pred <- predict(ms.ranger, newdata = test_set)
ms.xgbTree.pred <- predict(ms.xgbTree, newdata = test_set)
ms.rf.pred <- predict(ms.rf, newdata = test_set)
ms.svmLinear.pred <- predict(ms.svmLinear, newdata = test_set)
ms.kknn.pred <- predict(ms.kknn, newdata = test_set)
ms.avNNet.pred <- predict(ms.avNNet, newdata = test_set)
ms.svmRadialCost.pred <- predict(ms.svmRadialCost, newdata = test_set)
ms.naive_bayes.pred <- predict(ms.naive_bayes, newdata = test_set)
ms.wsrf.pred <- predict(ms.wsrf, newdata = test_set)

# and we determine the confusion matrices for all models
cm <- c(
  gbm = confusionMatrix(ms.gbm.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  ranger = confusionMatrix(ms.ranger.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  xgbTree = confusionMatrix(ms.xgbTree.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  rf = confusionMatrix(ms.rf.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  svmLinear = confusionMatrix(ms.svmLinear.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  kknn = confusionMatrix(ms.kknn.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  avNNet = confusionMatrix(ms.avNNet.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  svmRadialCost = confusionMatrix(ms.svmRadialCost.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  naive_bayes = confusionMatrix(ms.naive_bayes.pred, test_set$reconstruction_needed)$overall["Accuracy"],
  wsrf = confusionMatrix(ms.wsrf.pred, test_set$reconstruction_needed)$overall["Accuracy"]
)

# As a next step, we create an ensemble model
# We start by creating a dataframe with all predictions from the different models
ensemble <- data_frame(
  ms.gbm.pred = ms.gbm.pred,
  ms.ranger.pred = ms.ranger.pred,
  ms.xgbTree.pred = ms.xgbTree.pred,
  ms.rf.pred = ms.rf.pred,
  ms.svmLinear.pred = ms.svmLinear.pred,
  ms.kknn.pred = ms.kknn.pred,
  ms.avNNet.pred = ms.avNNet.pred,
  ms.svmRadialCost.pred = ms.svmRadialCost.pred,
  ms.naive_bayes.pred = ms.naive_bayes.pred,
  ms.wsrf.pred = ms.wsrf.pred
)

# then we define an ensemble function
# we take the mean value and then set to 'Yes' or 'No'
calc_ensemble_val <- function(r) {
  ifelse(mean(r == 'Yes') > 0.5, 'Yes', 'No')
}

# Finally, we determine the predictions and calculate the confusion matrices of the ensemble model
ensemble.pred <- apply(ensemble, 1, calc_ensemble_val)
ensemble.pred <- as.factor(ensemble.pred)
ensemble.cm <- confusionMatrix(ensemble.pred, test_set$reconstruction_needed)
cm <- c(cm, ensemble = ensemble.cm$overall["Accuracy"])

# DETERMINE THE BEST MODEL
results <- sort(cm, decreasing = TRUE)
results



