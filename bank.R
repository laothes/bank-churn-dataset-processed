#####################################################
# catalogue
# packages
# import data
# 1.EDA
## NA values
## duplicate values
## correlation matrix
## The density of multiple continued variables with category
## The percentage of being exited or not for discrete variables
## Distribution of Number of Products with Exited or not
# 2.split dataset
# 3.normalize the numerical columns
## normalize('Age','CreditScore', 'Balance','EstimatedSalary')
## tf-idf the surname (this part will take a long time, please reload processed data)
# 4.feature engineering
# 5.Models (default and adjusted)
## logistic regression
## random forest
## XGBoost
# 6.submission
# 7.adjusting process
#####################################################

# install packages
requiredPackages <- c("corrplot", "ggplot2","tidyr","GGally","caret","dplyr",
                      "tidytext","glmnet","randomForest","randomForest")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}
# import packages
library(corrplot)
library(ggplot2)
library(tidyr)
library(GGally)
library(caret)
library(dplyr)
library(tidytext)
library(glmnet)
library(randomForest)
library(xgboost)

# import data
test = read.csv('test.csv',stringsAsFactors = T)
train_orignal = read.csv('train.csv',stringsAsFactors = T)

# 1.EDA
# NA values in dataset
sum(is.na(train_orignal))
sum(is.na(test))# NO NA values

# duplicate values
sum(duplicated(train_orignal$id))
sum(duplicated(test$id))# NO duplicate values

str(train_orignal)
summary(train_orignal)

# correlation matrix
## library(corrplot)
# correlation matrix
## library(corrplot)
df_corr <- train_orignal

# Identify categorical columns and drop out
catcol <- names(df_corr)[sapply(df_corr, is.factor)]
df_corr <- subset(df_corr, select = -c(Surname, Geography,Gender))

# Calculate Pearson correlation matrix
corr_matrix <- cor(df_corr)

# sqrt all the values to beautifying heat map
for (i in 1:dim(corr_matrix)[1]){
  for (j in 1:dim(corr_matrix)[2]){
    a = corr_matrix[i,j]
    if (a >= 0){corr_matrix[i,j] = sqrt(a)}
    else {corr_matrix[i,j] = -sqrt(-a)}
  }}

# heatmap
palette_cmap <- c("blue","green", "yellow", "orange")
corrplot(corr_matrix, type = "lower", 
         cl.lim = c(-1, 1), # Set color range for correlations
         is.diag = FALSE, # Exclude diagonal elements (optional)
         col = colorRampPalette(palette_cmap)(n = length(corr_matrix)), # Apply color palette
         outline.color = "#e0b583", # Set line color
         outline.lty = 1, # Set line type (solid)
         outline.lwd = 3, # Set line width
         tl.cex = 0.8, # Adjust label size (optional)
         title = "Pearson Correlation of Features\n", 
         title.cex = 25, # Adjust title size
         tl.srt = 45
)


# The density of multiple continued variables with category
BankChurnTrainExited <- train_orignal %>%
  group_by(Exited) %>%
  summarise(n = n(), perc = n() / nrow(train_orignal))

BankChurnTrainSubset <- train_orignal %>%
  select(CreditScore, Age, Balance, EstimatedSalary, Exited)

BankChurnTrainLong <- BankChurnTrainSubset %>%
  pivot_longer(cols = c(CreditScore, Age, Balance, EstimatedSalary),
               names_to = "Variable",
               values_to = "Value")

ggplot(BankChurnTrainLong, aes(x = Value, fill = as.factor(Exited), color = as.factor(Exited))) +
  geom_density(alpha = 0.3, size = 1) +
  labs(title = "Distribution of CreditScore, Age, Balance, and EstimatedSalary by Customer Churn", 
       x = "Value", fill = "Exited (0 = No, 1 = Yes)", color = "Exited (0 = No, 1 = Yes)") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5),
        legend.position = "top")

## Order customers have higher possibilities to churn from the density curve
## There shows high density of churning when people have 100000 - 150000 balance
## The distribution of both credit socres and estimated salary are similiar in both categories

# The percentage of being exited or not for discrete variables
BankChurnTrainDiscrete <- train_orignal %>%
  select(Exited, Geography, Gender, HasCrCard, IsActiveMember) %>%
  mutate(
    HasCrCard = as.character(HasCrCard), 
    IsActiveMember = as.character(IsActiveMember)
  ) %>%
  pivot_longer(cols = c(Geography, Gender, HasCrCard, IsActiveMember), 
               names_to = "Variable", values_to = "Category") %>%
  group_by(Variable, Category, Exited) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

ggDiscretePlot <- ggplot(BankChurnTrainDiscrete, aes(x = Category, y = Percentage, fill = factor(Exited, levels = c(0, 1)))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#27AE60", "#E74C3C"), labels = c("No", "Yes")) +
  facet_wrap(~Variable, scales = "free_x", ncol = 2) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  labs(title = "Percentage Distribution of Geography, Gender, HasCrCard, and IsActiveMember by Customer Churn",
       x = "", y = "Percentage", fill = "Exited") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggDiscretePlot

## 28% of female customers have been churned compared to 16% of male customers
## Germany customers have more probability of churning compared to other two country
## Inactive customers have twice probability of churning to actives, which means inactive member have more probability to churn


# Distribution of Number of Products with Exited or not
BankChurnNumberProducts <- train_orignal %>%
  group_by(NumOfProducts, Exited) %>%
  summarise(Count = n())

ggNumberProductsPlot <- ggplot(BankChurnNumberProducts, aes(x = as.factor(NumOfProducts), y = Count, fill = factor(Exited, levels = c(0, 1)))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#27AE60", "#E74C3C"), labels = c("No", "Yes")) +
  labs(title = "Frequency Distribution of Number of Products by Customer Churn",
       x = "Number of Products", y = "Frequency", fill = "Exited") +
  theme_minimal() +
  theme(text = element_text(size = 14), 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5))
ggNumberProductsPlot

## Observe that Customers have 3 or 4 number of products have bigger probability to churn,
## Customers have largest probability choosing not to exit where they have 2 num of prods


# 2. split dataset
## library(caret)
set.seed(300)
index <- createDataPartition(train_orignal$Exited, p = 0.8, list = FALSE)
train <- train_orignal[index, ]
val <- train_orignal[-index, ]


# 3. normalize the numerical colums
numeric_cols = c('Age','CreditScore', 'Balance','EstimatedSalary')
for (col in numeric_cols){
  train[[paste0(col, '_s')]] = (train[[col]] - min(train[[col]]))/(max(train[[col]])-min(train[[col]]))
  val[[paste0(col, '_s')]] = (val[[col]] - min(train[[col]]))/(max(train[[col]])-min(train[[col]]))
  test[[paste0(col, '_s')]] = (test[[col]] - min(train[[col]]))/(max(train[[col]])-min(train[[col]]))
}

# tf-idf the surname
## library(dplyr)
## library(tidytext)
train$Surname = as.character(train$Surname)
tf = count(train, Exited, Surname, sort = TRUE)
  # calculates the TF-IDF scores for each word within each
tfidf_t = bind_tf_idf(tf, Surname, Exited, n)
  # arranges the results in descending order based on the TF-IDF scores
tfidf_t = arrange(tfidf_t[tfidf_t$tf_idf > 0,],desc(tf_idf))
  # Add the tdidf score to the original dataset, where the score of Exited = 0 becomes negative. Due to the value is to small, I multiple it by 10000 in this case the abs of biggest value will be 1.38509
tfidf_score <- function(df, tfidf_t){
  df$tfidf = 0
  for (i in 1:dim(df)[1]){
    print(i) # See progress
    for (j in 1:dim(tfidf_t)[1]){
      if (grepl(tfidf_t[j,'Surname'],df[i,'Surname']) == T){
        df[i,'tfidf'] = df[i,'tfidf'] + tfidf_t[j,'tf_idf'] * (tfidf_t[j,'Exited'] - 0.5) *20000
      }
    }
  }
  return(df)
}
train = tfidf_score(train,tfidf_t)
write.csv(train,'train_pro.csv',row.names=FALSE)
val = tfidf_score(val,tfidf_t)
write.csv(val,'val_pro.csv',row.names=FALSE)
test = tfidf_score(test,tfidf_t)
write.csv(test,'test_pro.csv',row.names=FALSE)

########
# Time takes a long time for following, you can just import the .csv we have well handled
# code:
#test = read.csv('test_pro.csv', stringsAsFactors = T)
#val = read.csv('val_pro.csv', stringsAsFactors = T)
#train = read.csv('train_pro.csv',stringsAsFactors = T)
########
########


# 4. feature engineering
feature_eng <- function(df){
  df$senior = ifelse(df$Age >= 60, 1, 0) # summary from the table related to age and exited
  df$Active_by_CreditCard = as.factor(df$HasCrCard * df$IsActiveMember)
  df$Products_Per_Tenure = df$Tenure / df$NumOfProducts
  return(df)
}
train = feature_eng(train)
train$Exited = as.factor(train$Exited)
val = feature_eng(val)
val$Exited = as.factor(val$Exited)
test = feature_eng(test)

# Selecting Columns for the model
df_train = subset(train,select = - c(id, CustomerId, Surname,EstimatedSalary, Balance, Age, CreditScore))
df_val = subset(val,select = - c(id, CustomerId, Surname,EstimatedSalary, Balance, Age, CreditScore))
df_test = subset(test,select = - c(id, CustomerId, Surname, EstimatedSalary, Balance, Age,CreditScore))


# 5. Models, code for adjusting parameters are in the end
# cv
control <- trainControl(method = "cv", number = 5)

# logistic regression
## library(glmnet)

### 1) default model
glm_model_o <- train(Exited ~ ., data = df_train, method = "glm", family = "binomial",
                   trControl = control)
pred_glm_o <- predict(glm_model_o, newdata = df_val)
  # confusion matrix
conf_matrix_glm_o <- confusionMatrix(pred_glm_o, df_val$Exited)
accuracy_glm_o <- conf_matrix_glm_o$overall["Accuracy"]
precision_glm_o <- conf_matrix_glm_o$byClass["Pos Pred Value"]  # Precision
recall_glm_o <- conf_matrix_glm_o$byClass["Sensitivity"]        # Recall
f1_score_glm_o <- 2 * (precision_glm_o * recall_glm_o) / (precision_glm_o + recall_glm_o)  # F1 Score

glm_o_perf = c(accuracy_glm_o, precision_glm_o, recall_glm_o, f1_score_glm_o)

  # coefficients
coeffs <- as.data.frame(summary(glm_model_o)$coefficients)
coeffs <- coeffs[order(coeffs$Estimate, decreasing = TRUE), ]
barplot(coeffs$Estimate, horiz = TRUE, names.arg = row.names(coeffs),las = 2)

### 2) Final model
glm_model <- train(Exited ~ ., data = df_train, method = "glmnet", family = "binomial",
                   trControl = control, 
                   tuneGrid = expand.grid(alpha = 0.25, lambda = 0.0001))
glm_model
summary(glm_model)
pred_glm <- predict(glm_model, newdata = df_val)
  
  # confusion matrix
conf_matrix_glm <- confusionMatrix(pred_glm_o, df_val$Exited)
print(conf_matrix_glm)

  # performance
accuracy_glm <- conf_matrix_glm$overall["Accuracy"]
precision_glm <- conf_matrix_glm$byClass["Pos Pred Value"]  # Precision
recall_glm <- conf_matrix_glm$byClass["Sensitivity"]        # Recall
f1_score_glm <- 2 * (precision_glm * recall_glm) / (precision_glm + recall_glm)  # F1 Score
cat("Accuracy: ", accuracy_glm, "\n")
cat("Precision: ", precision_glm, "\n")
cat("Recall: ", recall_glm, "\n")
cat("F1-Score: ", f1_score_glm, "\n")

glm_perf = c(accuracy_glm, precision_glm, recall_glm, f1_score_glm)


# randomForest
## library(randomForest)

### 1) Default model
rf_model_o <- randomForest(Exited~., data=df_train)
pred_rf_o <- predict(rf_model_o, newdata = df_val)
  # confusion matrix
conf_matrix_rf_o <- confusionMatrix(pred_rf_o, df_val$Exited)
accuracy_rf_o <- conf_matrix_rf_o$overall["Accuracy"]
precision_rf_o <- conf_matrix_rf_o$byClass["Pos Pred Value"]  # Precision
recall_rf_o <- conf_matrix_rf_o$byClass["Sensitivity"]        # Recall
f1_score_rf_o <- 2 * (precision_rf_o * recall_rf_o) / (precision_rf_o + recall_rf_o)  # F1 Score

rf_o_perf = c(accuracy_rf_o, precision_rf_o, recall_rf_o, f1_score_rf_o)

### 2) Final model
rf_model <- randomForest(Exited~., data=df_train, mtry = 4)
rf_model
summary(rf_model)

  # Confuse matrix
pred_rf <- predict(rf_model, newdata = df_val)
conf_matrix_rf <- confusionMatrix(pred_rf, df_val$Exited)
print(conf_matrix_rf)

  # Importance matrix
importance_rf <- importance(rf_model)
varImpPlot(rf_model,
           sort = T,
           main = "Variable Importance")

  # performance
accuracy_rf <- conf_matrix_rf$overall["Accuracy"]
precision_rf <- conf_matrix_rf$byClass["Pos Pred Value"]  # Precision
recall_rf <- conf_matrix_rf$byClass["Sensitivity"]        # Recall
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)  # F1 Score
cat("Accuracy: ", accuracy_rf, "\n")
cat("Precision: ", precision_rf, "\n")
cat("Recall: ", recall_rf, "\n")
cat("F1-Score: ", f1_score_rf, "\n")

rf_perf = c(accuracy_rf, precision_rf, recall_rf, f1_score_rf)

# XGBoost
## library(xgboost)
  # Reformat the dataset
y_train <- as.numeric(df_train$Exited) - 1
y_val <- as.numeric(df_val$Exited) - 1
X_train <- model.matrix(~ . - 1, data = df_train %>% select(-Exited))
X_val <- model.matrix(~ . - 1, data = df_val %>% select(-Exited))
X_test <- model.matrix(~ . - 1, data = df_test)

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dval <- xgb.DMatrix(data = X_val, label = y_val)

### 1) Default model
xgb_model_o <- xgboost(
  data = dtrain,
  nrounds = 100,
  verbose = 1
)
y_pred_val_o <- predict(xgb_model_o, dval)
y_pred_class_val_o <- ifelse(y_pred_val_o > 0.5, 1, 0)
  # confusion matrix
conf_matrix_xg_o <- confusionMatrix(as.factor(y_pred_class_val_o), as.factor(y_val))
accuracy_xg_o <- conf_matrix_xg_o$overall["Accuracy"]
precision_xg_o <- conf_matrix_xg_o$byClass["Pos Pred Value"]  # Precision
recall_xg_o <- conf_matrix_xg_o$byClass["Sensitivity"]        # Recall
f1_score_xg_o <- 2 * (precision_xg_o * recall_xg_o) / (precision_xg_o + recall_xg_o)  # F1 Score

xg_o_perf = c(accuracy_xg_o, precision_xg_o, recall_xg_o, f1_score_xg_o)

### 2) Final model
best_params <- list(
  max_depth = 9,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)
xgb_model <- xgboost(
  params = best_params,
  data = dtrain,
  nrounds = 100,
  verbose = 1
)
xgb_model
  # Prediction
y_pred_val <- predict(xgb_model, dval)
y_pred_class_val <- ifelse(y_pred_val > 0.5, 1, 0)

  # Confuse matrix
conf_matrix_xg <- confusionMatrix(as.factor(y_pred_class_val), as.factor(y_val))
print(conf_matrix_xg)

  # Importance_matrix
importance_matrix_xg <- xgb.importance(feature_names = colnames(dtrain), model = xgb_model)
print(importance_matrix_xg)
xgb.plot.importance(importance_matrix_xg)

  # performance
accuracy_xg <- conf_matrix_xg$overall["Accuracy"]
precision_xg <- conf_matrix_xg$byClass["Pos Pred Value"]  # Precision
recall_xg <- conf_matrix_xg$byClass["Sensitivity"]        # Recall
f1_score_xg <- 2 * (precision_xg * recall_xg) / (precision_xg + recall_xg)  # F1 Score
cat("Accuracy: ", accuracy_xg, "\n")
cat("Precision: ", precision_xg, "\n")
cat("Recall: ", recall_xg, "\n")
cat("F1-Score: ", f1_score_xg, "\n")

xg_perf = c(accuracy_xg, precision_xg, recall_xg, f1_score_xg)


# 6. submission
  # feature analysis
write.csv(coeffs,'coefficients_lr.csv')
write.csv(importance_rf,'importance_rf.csv')
write.csv(importance_matrix_xg,'importance_xg.csv')
  # performance
rowN = c('accuracy','precision','recall','f1_score')
performance <- data.frame(lr_default = glm_o_perf,
                          lr_adjusted = glm_perf,
                          rf_default = rf_o_perf,
                          rf_adjusted = rf_perf,
                          xg_default = xg_o_perf,
                          xg_adjusted = xg_perf,
                          row.names = rowN
                          )
write.csv(performance, file = "performance.csv")

  # prediction results
pred_glm_test <- predict(glm_model, newdata = df_test)
pred_rf_test <- predict(rf_model, newdata = df_test)
pred_xgb_test <- predict(xgb_model, newdata = X_test)
pred_xgb_test <- ifelse(pred_xgb_test > 0.5, 1, 0)
submit <- data.frame(id = test$id, 
                     Exited_glm = pred_glm_test, 
                     Exited_rf = pred_rf_test, 
                     Exited_xgb = pred_xgb_test)
write.csv(submit, file = "submissions.csv", row.names = F)


# 7. adjusting process
control <- trainControl(method = "cv", number = 5)
## adjusting parameter for Lr
enet_model <- train(Exited ~ ., data = df_train, method = "glmnet",
                    tuneGrid = expand.grid(alpha = seq(0, 1, length = 5), 
                                           lambda = seq(0.0001, 0.01, length = 5)),
                    trControl = control)
enet_model

## adjusting parameter for randomForest
grid_rf <- expand.grid(.mtry = c(2,4,6,8,12,14))
rf_model <- train( Exited ~ .,
                   method = "rf",
                   data = df_train,
                   trControl = control,
                   tuneGrid = grid_rf
)
#mtry  Accuracy   Kappa    
#2    0.8594844  0.4964511
#4    0.8631275  0.5445553
#6    0.8593177  0.5372806
#8    0.8579165  0.5343265
#12    0.8565380  0.5310356
#14    0.8558564  0.5295472

#The final value used for the model was mtry = 4.

## adjusting parameter for XGBoost
param_grid <- expand.grid(
  max_depth = c(9, 12, 16, 18, 20), 
  eta = c(0.01, 0.05, 0.1, 0.2),
  subsample = c(0.8, 1),
  colsample_bytree = c(0.7, 0.8, 1),
  min_child_weight = c(1, 3, 5, 10),
  gamma = c(0, 0.1, 0.5, 1, 5),
  lambda = c(0, 1, 5),
  alpha = c(0, 0.5, 1)
)

params_fixed <- list(
  objective = "binary:logistic",
  eval_metric = "auc" 
)

best_params <- NULL
best_auc <- -Inf

for(i in 1:nrow(param_grid)) {
  params <- list(
    objective = params_fixed$objective,
    eval_metric = params_fixed$eval_metric,
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  cv_model <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 100,
    nfold = 5,
    early_stopping_rounds = 10, 
    verbose = TRUE,
    maximize = TRUE 
  )
  
  mean_auc <- max(cv_model$evaluation_log$test_auc_mean)
  
  if(mean_auc > best_auc) {
    best_auc <- mean_auc
    best_params <- params
  }
  cat("Best parameters:\n")
  print(data.frame(best_params))
  cat("Best AUC:", best_auc, "\n")
}