# =====================================================
# Classification: Delay (≤60 days vs >60 days)
# =====================================================

library(caret)
library(ranger)      # RF
library(xgboost)     # XGB
library(kernlab)     # SVM radial
library(dplyr)
# Get the Prepeared Data
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Code/3.All_Source_Data_Merging_and_Finalizing_for_Modelling.R")
# =====================================================
# Step 1: Prepare Response Variable
# =====================================================
unique(length(full_data_for_modelling$`Project Number`))
table(full_data_for_modelling$Expense_starting_days)
full_data_for_modelling <- full_data_for_modelling %>% 
mutate(Delay = factor(ifelse(Expense_starting_days <= 60, "LE60", "GT60"),
                      levels = c("LE60", "GT60")))  # LE = Less or Equal, GT = Greater Than


# =====================================================
# Step 2: Preprocess Predictors
# =====================================================
numeric_cols <- c("Number_of_Project_as_Principal_Investigator",
                  "Total_project_person",
                  "Project_duration",
                  "Project Funding Amount")

preproc_cls <- preProcess(full_data_for_modelling[, numeric_cols],
                          method = c("center", "scale"))
scaled_num  <- predict(preproc_cls, full_data_for_modelling[, numeric_cols])

dummy_cls   <- dummyVars(" ~ .",
                         data = full_data_for_modelling[, c("Project Fund Source",
                                                            "Award Type",
                                                            "Academic_Semester",
                                                            "Reduced_FA",
                                                            "subawards subcontracts",
                                                            "Approval_Required",
                                                            "Chemical_or_Hazard",
                                                            "Foreign_Involvement",
                                                            "Technology_or_IP_Involved")])
dummy_dat   <- as.data.frame(predict(dummy_cls, newdata = full_data_for_modelling))

model_df <- cbind(scaled_num, dummy_dat, Delay = full_data_for_modelling$Delay)
names(model_df) <- make.names(names(model_df), unique = TRUE)

# =====================================================
# Step 3: Split Data into Train/Test
# =====================================================
set.seed(123)
idx <- createDataPartition(model_df$Delay, p = 0.8, list = FALSE)
train_df <- model_df[idx, ]
test_df  <- model_df[-idx, ]

# =====================================================
# Step 4: Load Required Libraries
# =====================================================
library(caret)
library(ranger)
library(xgboost)
library(kernlab)
library(pROC)
library(dplyr)

# =====================================================
# Step 5: Define Metric Function
# =====================================================
get_metrics <- function(obs, pred, prob = NULL, positive = "LE60") {
  obs <- factor(obs, levels = c("LE60", "GT60"))
  pred <- factor(pred, levels = c("LE60", "GT60"))
  
  cm <- caret::confusionMatrix(data = pred, reference = obs, positive = positive)
  
  accuracy    <- cm$overall["Accuracy"]
  precision   <- cm$byClass["Pos Pred Value"]
  recall      <- cm$byClass["Sensitivity"]
  f1          <- 2 * precision * recall / (precision + recall)
  specificity <- cm$byClass["Specificity"]
  
  out <- data.frame(Accuracy    = as.numeric(accuracy),
                    Precision   = as.numeric(precision),
                    Recall      = as.numeric(recall),
                    F1          = as.numeric(f1),
                    Specificity = as.numeric(specificity))
  
  if (!is.null(prob)) {
    auc <- pROC::auc(obs, prob)
    out$AUC <- as.numeric(auc)
  }
  return(out)
}


# =====================================================
# Step 6: Model Training (with Probabilities)
# =====================================================
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Logistic Regression
log_mod <- train(Delay ~ ., data = train_df, method = "glm", family = binomial(), trControl = ctrl)
log_pred <- predict(log_mod, newdata = test_df)
log_prob <- predict(log_mod, newdata = test_df, type = "prob")[, "LE60"]
log_res <- get_metrics(test_df$Delay, log_pred, prob = log_prob)

# Random Forest
rf_mod <- train(Delay ~ ., data = train_df, method = "ranger",
                trControl = ctrl,
                tuneGrid = expand.grid(mtry = floor(sqrt(ncol(train_df) - 1)),
                                       splitrule = "gini",
                                       min.node.size = 5),
                num.trees = 500)
rf_pred <- predict(rf_mod, newdata = test_df)
rf_prob <- predict(rf_mod, newdata = test_df, type = "prob")[, "LE60"]
rf_res <- get_metrics(test_df$Delay, rf_pred, prob = rf_prob)

# XGBoost
xgb_grid <- expand.grid(nrounds = 400, max_depth = 4, eta = 0.1, gamma = 0,
                        colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8)
xgb_mod <- train(Delay ~ ., data = train_df, method = "xgbTree",
                 trControl = ctrl, tuneGrid = xgb_grid, metric = "Accuracy")
xgb_pred <- predict(xgb_mod, newdata = test_df)
xgb_prob <- predict(xgb_mod, newdata = test_df, type = "prob")[, "LE60"]
xgb_res <- get_metrics(test_df$Delay, xgb_pred, prob = xgb_prob)

# SVM (Radial)
svm_mod <- train(Delay ~ ., data = train_df, method = "svmRadial",
                 trControl = ctrl, tuneLength = 5)
svm_pred <- predict(svm_mod, newdata = test_df)
svm_prob <- predict(svm_mod, newdata = test_df, type = "prob")[, "LE60"]
svm_res <- get_metrics(test_df$Delay, svm_pred, prob = svm_prob)

# =====================================================
# Step 7: Combine & Compare Results
# =====================================================
results <- bind_rows(
  cbind(Model = "Logistic Reg.", log_res),
  cbind(Model = "Random Forest", rf_res),
  cbind(Model = "XGBoost", xgb_res),
  cbind(Model = "SVM Radial", svm_res)
)

print(results %>% arrange(desc(F1)))  # or by Accuracy, AUC, etc.


###############

# # =====================================================
# # Step 1: Get Variable Importance
# # =====================================================
# importance_df <- varImp(rf_mod_tuned, scale = TRUE)$importance
# importance_df$Feature <- rownames(importance_df)
# 
# # Optional: sort and pick top 15
# top_features <- importance_df[order(-importance_df$Overall), ][1:10, ]
# # =====================================================
# # Step 2: Plot Top 15 Features
# # =====================================================
# library(ggplot2)
# 
# ggplot(top_features, aes(x = reorder(Feature, Overall), y = Overall)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(title = "Top 10 Important Features (RF)",
#        x = "Feature", y = "Importance (Impurity-based)") +
#   theme_minimal(base_size = 12)
# 
