# =====================================================
# Load Required Libraries
# =====================================================
library(readxl)
library(dplyr)
library(lubridate)
library(MASS)
library(nnet)
library(dplyr)
library(Matrix)
Sys.setenv(XGBOOST_VERBOSE = 0)
library(xgboost)
library(caret)
library(Metrics)  # For calculating RMSE
library(GGally)
library(readxl)
library(VGAM)
library(rstatix)
library(tidyr)
library(kernlab)   # caret pulls this in, but load explicitly for nSV()



# =====================================================
# Define Base Path and Custom Date
# =====================================================
data_input_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Data/06102025/"
output_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Output"

#custom_start_date <- as.Date("2025-06-01")  # We want to see all the projects which start date after this date to make the prediction of the expense. 

# =====================================================
# Load and Prepare Project Information Data (PI Count)
# =====================================================
project_details <- read_excel(paste0(data_input_path, "Project Information_Results.xlsx"))


project_details <- project_details %>%
  filter(`Award F&A Schedule` != "NONE") # We are using the project which have Indirect cost associated with that.

pi_project_summary <- project_details %>%
  group_by(`Project Principal Investigator`) %>%
  summarise(Number_of_Project_as_Principal_Investigator = n_distinct(`Project Number`), .groups = "drop") %>%
  na.omit()


project_details_selected <- project_details %>%
  left_join(pi_project_summary, by = "Project Principal Investigator") %>%
  dplyr::select(all_of(c(
    "Project Number", 
    "Number_of_Project_as_Principal_Investigator",
    "Project Designation", 
    "Project Type", 
    "Award Type",
    "Project Status", 
    "Project Start Date", 
    "Project Finish Date", 
    "Project Fund Source", 
    "Project Funding Type"
  )))


# =====================================================
# Load and Prepare Project Financial Summary Data
# =====================================================
project_finance <- read_excel(paste0(data_input_path, "Project Financial Summary_Results.xlsx"))

project_finance_selected <- project_finance %>%
  dplyr::select(all_of(c("Project Number", "Project Funding Amount", "Project Actual Expenses")))




details_indirect_cost <- merge(project_details_selected, project_finance_selected, by = "Project Number")

# =====================================================
# Load and Prepare Project Team Data
# =====================================================
project_team <- read_excel(paste0(data_input_path, "Project Team Members Details_Current Projects.xlsx"))

project_person_summary <- project_team %>%
  group_by(`Project Number`) %>%
  summarise(Total_project_person = n_distinct(`Project Person`), .groups = "drop")


details_indirect_cost <- merge(details_indirect_cost, project_person_summary, by = "Project Number")

# ========================================================
# Create Duration Variables and Extract the starting Month
# ========================================================
project_expenditure_data <- details_indirect_cost %>%
  mutate(
    `Project Start Date` = as.Date(`Project Start Date`),
    `Project Finish Date` = as.Date(`Project Finish Date`),
    Project_duration = as.numeric(`Project Finish Date` - `Project Start Date`),
    Project_Starting_Month = format(`Project Start Date`, "%B"),
    Academic_Semester = case_when(
      month(`Project Start Date`) %in% 1:5 ~ "Spring",
      month(`Project Start Date`) %in% 6:8 ~ "Summer",
      month(`Project Start Date`) %in% 9:12 ~ "Fall",
      TRUE ~ NA_character_
    )
  )




# =======================================================
# Load and Prepare Cost Details Data (Dependent Vairable)
# =======================================================

# Set the folder path
folder_path <- file.path(data_input_path, "Expenditure Details Data")


# List all .xlsx files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read and combine all Excel files into one data frame
combined_data <- file_list %>%
  lapply(read_excel) %>%
  bind_rows()
project_expenditure_start_date <- combined_data %>%
  filter(!is.na(`Project Number`) & !is.na(`Project Name`) & !is.na(`Expenditure Date`)) %>%
  group_by(`Project Number`, `Project Name`) %>%
  summarise(Date_First_Expenditure_Started = min(`Expenditure Date`, na.rm = TRUE), .groups = "drop")


full_data_for_modelling_all_columns <- left_join(project_expenditure_data, project_expenditure_start_date, by = "Project Number")

# library(dplyr)
# full_data_for_modelling_all_columns <- full_data_for_modelling_all_columns %>% 
#   # 1. Ensure the column is stored as a Date  
#   mutate(Date_First_Expenditure_Started = as.Date(Date_First_Expenditure_Started, format = "%Y-%m-%d")) %>% 
#   
#   # 2. Flag projects whose first expenditure is on or before the project start  
#   mutate(delayed_expenditure = ifelse(Date_First_Expenditure_Started <= `Project Start Date`, 1L, 0L))
# 
# table(full_data_for_modelling_all_columns$delayed_expenditure)
# View(full_data_for_modelling_all_columns)

full_data_for_modelling_all_columns <- full_data_for_modelling_all_columns %>%
  mutate(`Project Type` = case_when(
    grepl("UW Grant Cost Share", `Project Type`, fixed = TRUE) ~ "UW Grant Cost Share",
    grepl("UW Grant Internal", `Project Type`, fixed = TRUE)   ~ "UW Grant",
    TRUE ~ `Project Type`
  ))


full_data_for_modelling_all_columns <- full_data_for_modelling_all_columns %>%
  mutate(Fund_Source_Grouped = case_when(
    `Project Fund Source` == "Federal Direct Sponsored Funds" ~ "Federal Direct Sponsored Funds",
    `Project Fund Source` == "Industry" ~ "Industry",
    `Project Fund Source` %in% c("State of Wyoming", "Other States") ~ "State",
    `Project Fund Source` == "Non Profit/Foundation" ~ "Non Profit/Foundation",
    `Project Fund Source` == "Institutions of Higher Education" ~ "Institutions of Higher Education",
    `Project Fund Source` == "Unrestricted Operating" ~ "Unrestricted Operating",
    TRUE ~ "Others"
  ))


full_data_for_modelling_all_columns$`Project Fund Source` <- full_data_for_modelling_all_columns$Fund_Source_Grouped


full_data_for_modelling <- full_data_for_modelling_all_columns %>%
  mutate(
    `Date_First_Expenditure_Started` = as.Date(`Date_First_Expenditure_Started`),
    Expense_starting_days = as.numeric(`Date_First_Expenditure_Started` - `Project Start Date`)
  ) %>%
  filter(Expense_starting_days >= 0 & !is.na(`Project Number`) & !is.na(Number_of_Project_as_Principal_Investigator)) %>%
  dplyr::select(all_of(c(
    "Project Number", 
    "Project Fund Source", 
    "Project Funding Type", 
    "Project Designation", 
    "Project Type", 
    "Award Type",
    "Number_of_Project_as_Principal_Investigator", 
    "Total_project_person", 
    "Project_duration", 
    "Academic_Semester",
    "Project Funding Amount", 
    "Expense_starting_days"
  )))

full_data_for_modelling <- full_data_for_modelling %>%
  mutate(
    `Project Funding Type` = as.factor(ifelse(`Project Funding Type` %in% c("Federal Direct", "Federal Passthrough"), "Federal", "Non-Federal"))
  )





write.csv(full_data_for_modelling,
          file.path(output_path, "Data_Used_in_model_0616_25.csv"),
          row.names = FALSE)


# =====================================================
# Convert Variables to Proper Data Types
# =====================================================
full_data_for_modelling <- full_data_for_modelling %>%
  mutate(
    `Project Fund Source` = as.factor(`Project Fund Source`),
    `Project Funding Type` = as.factor(`Project Funding Type`),
    `Project Designation` = as.factor(`Project Designation`),
    `Project Type` = as.factor(`Project Type`),
    `Award Type` = as.factor(`Award Type`),
    Academic_Semester = as.factor(Academic_Semester),
    Project_duration = as.numeric(Project_duration),
    `Project Funding Amount` = as.numeric(`Project Funding Amount`),
    Expense_starting_days = as.numeric(Expense_starting_days)
  )


full_data_for_modelling <- full_data_for_modelling %>%
  distinct(`Project Number`, .keep_all = TRUE)


# =====================================================
# Final Dataset for Model Testing (No Expense Yet Projects)
# =====================================================


no_expense_yet <- full_data_for_modelling_all_columns %>%
  filter(`Project Status` == "ACTIVE", is.na(Date_First_Expenditure_Started)) %>%
  filter(!is.na(Number_of_Project_as_Principal_Investigator))



prediction_data <- no_expense_yet %>%
  dplyr::select(all_of(c(
    "Project Number",
    "Project Fund Source",
    "Project Funding Type",
    "Project Designation",
    "Project Type",
    "Award Type",
    "Number_of_Project_as_Principal_Investigator",
    "Total_project_person",
    "Project_duration",
    "Academic_Semester",
    "Project Funding Amount"
  )))





# For binary factors: t-tests
t_test <- full_data_for_modelling %>%
  select(`Expense_starting_days`, `Project Funding Type`, `Project Designation`,`Project Type`) %>%
  pivot_longer(cols = -Expense_starting_days, names_to = "Variable", values_to = "Group") %>%
  group_by(Variable) %>%
  t_test(Expense_starting_days ~ Group) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance()

t_test

# For multi-level factors: ANOVA
anova <- full_data_for_modelling %>%
  select(`Expense_starting_days`, `Project Fund Source`, `Academic_Semester`,`Award Type`) %>%
  pivot_longer(cols = -Expense_starting_days, names_to = "Variable", values_to = "Group") %>%
  group_by(Variable) %>%
  anova_test(Expense_starting_days ~ Group)
anova

# =============================
# Bivariate Relationships with Expense_starting_days
# =============================
ggpairs(full_data_for_modelling, 
        columns = c("Expense_starting_days","Number_of_Project_as_Principal_Investigator", 
                    "Total_project_person", "Project_duration", 
                    "Project Funding Amount"))

library(ggplot2)
library(patchwork)  # for side-by-side plots

# Create transformed versions
df <- full_data_for_modelling
df$log_trans  <- log1p(df$Expense_starting_days)
df$sqrt_trans <- sqrt(df$Expense_starting_days)
df$inv_trans  <- 1 / (df$Expense_starting_days + 1)  # Avoid division by zero

# Define plots
p1 <- ggplot(df, aes(x = Expense_starting_days)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Original Distribution")

p2 <- ggplot(df, aes(x = log_trans)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  ggtitle("Log1p Transform")

p3 <- ggplot(df, aes(x = sqrt_trans)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  ggtitle("Square Root Transform")

p4 <- ggplot(df, aes(x = inv_trans)) +
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  ggtitle("Reciprocal Transform")

# Combine with patchwork
p1 + p2 + p3 + p4 + plot_layout(ncol = 2)



# =============================
# Model Preparation
# =============================
set.seed(123)  # For reproducibility

# Define numeric columns
numeric_cols <- c("Number_of_Project_as_Principal_Investigator", 
                  "Total_project_person", 
                  "Project_duration", 
                  "Project Funding Amount")

# Create and apply preprocessing object for numeric variables
preproc <- preProcess(full_data_for_modelling[, numeric_cols], method = c("center", "scale"))
scaled_numeric <- predict(preproc, full_data_for_modelling[, numeric_cols])

# Generate dummy variables for categorical predictors
dummies <- dummyVars(" ~ .", data = full_data_for_modelling[, c("Project Funding Type", "Project Type", "Academic_Semester","Award Type")])
dummy_data <- as.data.frame(predict(dummies, newdata = full_data_for_modelling))

# Combine all predictors
scaled_data <- cbind(scaled_numeric, dummy_data)
scaled_data$Expense_starting_days <- full_data_for_modelling$Expense_starting_days

# Save preprocessing tools
saveRDS(preproc, file = file.path(output_path, "preproc_numeric_scaling.rds"))
saveRDS(dummies, file = file.path(output_path, "dummies.rds"))


# =====================================================
# 0.  Libraries & Setup -------------------------------
# =====================================================
library(caret)
library(ranger)     # Random-Forest backend
library(rpart)      # CART
library(xgboost)    # Gradient boosting
set.seed(123)

# -----------------------------------------------------
# 0A.  Make column names syntactically safe
# -----------------------------------------------------
scaled_data <- scaled_data[ , ]                       # drop tbl_df class if any
names(scaled_data) <- make.names(names(scaled_data), unique = TRUE)

# Target transformation
scaled_data$log_expense <- log1p(scaled_data$Expense_starting_days)

# Feature / response split
predictors <- setdiff(names(scaled_data),
                      c("Expense_starting_days", "log_expense"))
X_df  <- scaled_data[ , predictors]
y_true <- scaled_data$Expense_starting_days

# 10-fold CV
control <- trainControl(method = "cv", number = 10, verboseIter = FALSE)

# -----------------------------------------------------
# Helper functions
# -----------------------------------------------------
orig_rmse <- function(log_pred, y) sqrt(mean((expm1(log_pred) - y)^2))
orig_mae  <- function(log_pred, y) mean(abs(expm1(log_pred) - y))

add_row <- function(tbl, name, mins, rmse, mae) {
  rbind(tbl, data.frame(Model = name,
                        Train_minutes = round(mins, 2),
                        RMSE = round(rmse, 2),
                        MAE  = round(mae , 2)))
}
model_cmp <- data.frame(Model = character(),
                        Train_minutes = numeric(),
                        RMSE = numeric(),
                        MAE  = numeric())

# =====================================================
# 1.  Linear Regression -------------------------------
# =====================================================
t <- system.time({
  ols_model <- train(
    x = X_df,
    y = log1p(y_true),
    method = "lm",
    trControl = control
  )
})
ols_pred <- predict(ols_model, X_df)
model_cmp <- add_row(model_cmp, "Linear Regression",
                     t["elapsed"]/60,
                     orig_rmse(ols_pred, y_true),
                     orig_mae (ols_pred, y_true))

# =====================================================
# 2.  Decision Tree (CART) ----------------------------
# =====================================================
dt_grid <- expand.grid(cp = seq(0.001, 0.05, length.out = 10))
t <- system.time({
  dt_model <- train(
    x = X_df,
    y = log1p(y_true),
    method = "rpart",
    tuneGrid = dt_grid,
    trControl = control,
    metric = "RMSE"
  )
})
dt_pred <- predict(dt_model, X_df)
model_cmp <- add_row(model_cmp, "Decision Tree",
                     t["elapsed"]/60,
                     orig_rmse(dt_pred, y_true),
                     orig_mae (dt_pred, y_true))


# =====================================================
# 3.  Random Forest (ranger) --------------------------
# =====================================================
p <- ncol(X_df)
rf_grid <- expand.grid(
  mtry          = pmax(1, c(floor(sqrt(p)/2), floor(sqrt(p)), floor(1.5*sqrt(p)))),
  splitrule     = "variance",
  min.node.size = c(5, 10, 20)
)
t <- system.time({
  rf_model <- train(
    x = X_df,
    y = log1p(y_true),
    method = "ranger",
    importance = "impurity",
    tuneGrid = rf_grid,
    trControl = control,
    num.trees = 500
  )
})
rf_pred <- predict(rf_model, X_df)
model_cmp <- add_row(model_cmp, "Random Forest",
                     t["elapsed"]/60,
                     orig_rmse(rf_pred, y_true),
                     orig_mae (rf_pred, y_true))

# =====================================================
# 4.  XGBoost (xgbTree) -------------------------------
#       — no ntree_limit warning —
# =====================================================
# ✅ Disable XGBoost logging (R-safe way)
Sys.setenv(XGBOOST_VERBOSE = 0)

xgb_grid <- expand.grid(
  nrounds          = c(200, 400, 600),
  eta              = c(0.05, 0.10),
  max_depth        = c(3, 5),
  gamma            = 0,
  colsample_bytree = c(0.7, 0.9),
  min_child_weight = 1,
  subsample        = c(0.7, 0.9)
)

# Temporarily suppress XGBoost C++ warnings
Sys.setenv(XGBOOST_VERBOSE = 0)   # still fine to keep

t <- system.time({
  suppressWarnings({                    # silence R-level warnings
    tmp_txt <- capture.output(          # capture C++ “ntree_limit” lines
      xgb_model <- train(               #  ...but keep the model
        x = X_df,
        y = log1p(y_true),
        method   = "xgbTree",
        tuneGrid = xgb_grid,
        trControl = control,
        metric   = "RMSE"
      ),
      file = NULL
    )
  })
})



# Predict with modern API
best_booster <- xgb_model$finalModel
best_rounds  <- xgb_model$bestTune$nrounds

xgb_pred <- predict(
  object  = best_booster,
  newdata = as.matrix(X_df),
  iteration_range = c(0, best_rounds)
)

model_cmp <- add_row(model_cmp, "XGBoost",
                     t["elapsed"]/60,
                     orig_rmse(xgb_pred, y_true),
                     orig_mae (xgb_pred, y_true))

# =====================================================
# 5.  Results -----------------------------------------
# =====================================================
rownames(model_cmp) <- NULL
# Print Comparison Table
print(model_cmp[order(model_cmp$RMSE), ])


# -----------------------------------------------------
# Plot: Random Forest Feature Importance
# -----------------------------------------------------
rf_imp <- varImp(rf_model, scale = TRUE)
rf_imp_df <- rf_imp$importance
rf_imp_df$Feature <- rownames(rf_imp_df)

library(ggplot2)
library(dplyr)

# Convert to tibble and extract top 20 features
top_rf <- rf_imp_df %>%
  as_tibble() %>%
  arrange(desc(Overall)) %>%
  dplyr::slice(1:10)

# Plot
ggplot(top_rf, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Important Features (RF)",
    x = "Feature",
    y = "Importance (Scaled)"
  ) +
  theme_minimal(base_size = 10)


# Save the model to the full path
saveRDS(rf_model, file = file.path(output_path, "Random_forest_model.rds"))





# =====================================================
# 0. LOAD MODEL & PREPROCESSORS ------------------------
# =====================================================
rf_loaded <- readRDS(file.path(output_path, "Random_forest_model.rds"))
preproc   <- readRDS(file.path(output_path, "preproc_numeric_scaling.rds"))
dummies   <- readRDS(file.path(output_path, "dummies.rds"))

# =====================================================
# 1. HARMONISE FACTOR LEVELS ---------------------------
# =====================================================
prediction_data <- prediction_data %>%
  mutate(
    `Project Funding Type` = ifelse(
      `Project Funding Type` %in% c("Federal Direct", "Federal Passthrough"),
      "Federal",
      `Project Funding Type`
    )
  )

# =====================================================
# 2. BUILD TEST FRAME ---------------------------------
#    Ensure same structure as during training
# =====================================================
numeric_cols <- c(
  "Number_of_Project_as_Principal_Investigator",
  "Total_project_person",
  "Project_duration",
  "Project Funding Amount"
)

scaled_numeric_test <- predict(preproc, prediction_data[, numeric_cols])
dummy_test          <- as.data.frame(predict(dummies, newdata = prediction_data))

# Combine numeric and dummy predictors
x_test_df <- cbind(scaled_numeric_test, dummy_test)

# Fix column names to match training (caret made them syntactically valid)
names(x_test_df) <- make.names(names(x_test_df), unique = TRUE)

# Match column order expected by the trained model
model_cols <- setdiff(colnames(rf_loaded$trainingData), ".outcome")  # exclude response
missing    <- setdiff(model_cols, names(x_test_df))
if (length(missing) > 0) {
  stop("❌ Missing predictors required by the model: ",
       paste(missing, collapse = ", "))
}
x_test_df <- x_test_df[, model_cols]

# =====================================================
# 3. GENERATE PREDICTIONS ------------------------------
#    back-transform from log1p() scale
# =====================================================
log_pred_rf <- predict(rf_loaded, newdata = x_test_df)

prediction_data <- prediction_data %>%
  mutate(Predicted_days_RF = round(expm1(log_pred_rf)))

# =====================================================
# 4. ADD START DATE & CALCULATE PREDICTED DATE --------
# =====================================================
final_output <- prediction_data %>%
  left_join(
    project_expenditure_data %>%
      select(`Project Number`, `Project Start Date`) %>%
      mutate(`Project Start Date` = as.Date(`Project Start Date`)),
    by = "Project Number"
  ) %>%
  mutate(
    Predicted_date_RF = `Project Start Date` + Predicted_days_RF
  )

# =====================================================
# 5. EXPORT RESULTS -----------------------------------
# =====================================================
out_file <- file.path(output_path, "Predicted_data_random_forest_061625.csv")

write.csv(final_output, out_file, row.names = FALSE)

cat("✅ Random-forest prediction completed for",
    length(unique(final_output$`Project Number`)),
    "projects and saved to", basename(out_file), "\n")
