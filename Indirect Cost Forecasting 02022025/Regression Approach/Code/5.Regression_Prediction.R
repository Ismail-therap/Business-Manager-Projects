# =====================================================
# 0. LOAD MODEL & PREPROCESSORS ------------------------
# =====================================================
rf_loaded <- readRDS(file.path(output_path, "Random_forest_model_070125.rds"))
preproc   <- readRDS(file.path(output_path, "preproc_numeric_scaling.rds"))
dummies   <- readRDS(file.path(output_path, "dummies.rds"))



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
out_file <- file.path(output_path, "Predicted_data_random_forest_070125.csv")

write.csv(final_output, out_file, row.names = FALSE)

cat("✅ Random-forest prediction completed for",
    length(unique(final_output$`Project Number`)),
    "projects and saved to", basename(out_file), "\n")
