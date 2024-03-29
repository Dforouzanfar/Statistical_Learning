if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, doParallel, tidymodels, tidyverse, e1071, caret, rpart, rpart.plot, randomForest, visNetwork, rattle, xgboost, knitr, Ckmeans.1d.dp)

cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = cores)

Preprocessed_path <- 'https://raw.githubusercontent.com/Dforouzanfar/Statistical_Learning/master/Dataset/Preprocessed.csv'
smoke <- read.csv(Preprocessed_path)
str(smoke)
# The only column that has integer values is age, we can also consider removing this column but I keep it.
# smoke <- subset(smoke, select = -age)

# Preprocessing
smoke$gender <- factor(smoke$gender,levels = c("Male", "Female"),labels = c(1, 0))
smoke$marital_status <- factor(smoke$marital_status,levels = c("Single", "Married", "Separated"),labels = c(0, 2, 1))
smoke$highest_qualification <- factor(smoke$highest_qualification,levels = c("Without", "University", "GCSE", "Sub Degree", "ONC", "A Levels"),labels = c(0, 5, 3, 1, 2, 4))
smoke$nationality <- factor(smoke$nationality,levels = c("British", "English", "Other", "Scottish", "Welsh"),labels = c(0, 1, 2, 3, 4))
smoke$gross_income <- factor(smoke$gross_income,levels = c("Low", "Moderate", "High"),labels = c(0, 1, 2))
smoke$region <- factor(smoke$region,levels = c("The North", "Midlands & East Anglia", "London", "South East", "South West", "Wales", "Scotland"),labels = c(0, 1, 2, 3, 4, 5, 6))
smoke$smoke <- factor(smoke$smoke,levels = c("Yes", "No"),labels = c(1, 0))
str(smoke)

df_split <- initial_split(smoke, prop = 0.70, strata = smoke) # tidymodels
train <- training(df_split)
test <- testing(df_split)

# Decision Tree
DT_model <- rpart(smoke ~ ., data = train, method = "class")

rpart.plot(DT_model)
visTree(DT_model)

predictions <- predict(DT_model, test, type = "class")

conf_matrix <- table(predictions, test$smoke)
TN <- conf_matrix[1, 1]
TP <- conf_matrix[2, 2]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)

print(metrics)

# Random Forest
rf_model <- rand_forest(engine = "ranger", mode = "classification", trees = 200)
workflow <- workflow() %>% add_model(rf_model)
recipe <- recipe(smoke ~ ., data = train) %>% step_dummy(smoke, -smoke)
workflow <- add_recipe(workflow, recipe)
rf_train <- fit(workflow, train)
rf_predict <- predict(rf_train, test)

rf_predict <- as.vector(rf_predict)
conf_matrix <- table(rf_predict$.pred_class, test$smoke)
TN <- conf_matrix[1, 1]
TP <- conf_matrix[2, 2]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(metrics)


# eXtreme Gradient Boosting
data.train <- xgb.DMatrix(data=data.matrix(train[, !colnames(test) %in% c("smoke")]), label = train$smoke)
data.test <- xgb.DMatrix(data = data.matrix(test[, !colnames(test) %in% c("smoke")]))
parameters <- list(
  booster            = "gbtree",      
  eta                = 0.08,              
  gamma              = 0.7,                 
  max_depth          = 8,                
  min_child_weight   = 2,            
  subsample          = .9,                 
  colsample_bytree   = .5,                
  colsample_bylevel  = 1,          
  lambda             = 1,    
  alpha              = 0,       
  # Task Parameters
  objective          = "multi:softmax",   # default = "reg:linear"
  eval_metric        = "merror",
  num_class          = 7,
  tree_method = "hist",
  grow_policy = "lossguide"
)

xgb_model <- xgb.train(parameters, data.train, nrounds = 100)
xgb_pred <- predict(xgb_model, data.test)

conf_matrix <- table(as.factor(xgb_pred+2), test$smoke)
TN <- conf_matrix[1, 1]
TP <- conf_matrix[2, 2]
FP <- conf_matrix[1, 2]
FN <- conf_matrix[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

metrics <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(metrics)


xgb.importance(colnames(train[, !colnames(test) %in% c("smoke")]), model = xgb_model) %>% kable()
xgb.imp <- xgb.importance(colnames(train[, !colnames(test) %in% c("smoke")]), model = xgb_model)

xgb.ggplot.importance(importance_matrix = xgb.imp)
