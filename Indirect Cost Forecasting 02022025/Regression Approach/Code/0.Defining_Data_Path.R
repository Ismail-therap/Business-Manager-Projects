# =====================================================
# Load Required Libraries
# =====================================================
library(readxl)
library(readr) 
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

library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(lubridate)  # For handling date-time conversions
library(stringr)    # For string operations
library(tidyr)      # For reshaping data

# =====================================================
# Define Base Path and Custom Date
# =====================================================
data_input_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Data/06102025/"
cayuse_input_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Data/Cayuse Data/"
ashlee_input_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Data/Ashlee/"

output_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Output"

#========================================================
# STEP 4: Required Functions
#========================================================

# To create the FY

library(lubridate)

get_fy <- function(date, fy_start_month = 7) {
  date <- as.Date(date)  # ensures POSIXct/t becomes Date
  ifelse(
    is.na(date),
    NA_character_,
    paste0("FY", year(date) + ifelse(month(date) >= fy_start_month, 1, 0))
  )
}


# Get the filtered data for selected month and year

# Function to filter by month name and year
filter_proposals_by_month_year <- function(data, month_name, year_val) {
  month_num <- match(tolower(month_name), tolower(month.name))  # Convert month name to number
  if (is.na(month_num)) {
    stop("Invalid month name. Please use full month name like 'March'.")
  }
  
  filtered_data <- data %>%
    filter(
      !is.na(Actual_Submission_Date),
      year(Actual_Submission_Date) == year_val,
      month(Actual_Submission_Date) == month_num
    )
  
  return(filtered_data)
}


# Get the proposal count by the correspondig status

create_fy_summary <- function(data) {
  library(dplyr)
  library(tidyr)
  
  # Step 1: Count per FY category
  fy_submission <- data %>%
    count(Actual_Submission_FY, name = "Submission")
  
  fy_consideration <- data %>%
    count(Actual_Udr_Consid_FY, name = "Consideration")
  
  fy_funding <- data %>%
    count(Actual_Funding_FY, name = "Funding")
  
  fy_not_funding <- data %>%
    count(Actual_Not_Funding_FY, name = "Not Funded")
  
  # Step 2: Merge on fiscal year
  fy_summary <- full_join(fy_submission, fy_consideration, 
                          by = c("Actual_Submission_FY" = "Actual_Udr_Consid_FY")) %>%
    full_join(fy_funding, by = c("Actual_Submission_FY" = "Actual_Funding_FY")) %>%
    full_join(fy_not_funding, by = c("Actual_Submission_FY" = "Actual_Not_Funding_FY")) %>%
    rename(FY = Actual_Submission_FY) %>%
    arrange(FY) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    filter(!is.na(FY))
  
  return(fy_summary)
}



# Define a function for state fiscal quarters
get_fiscal_quarter <- function(date) {
  if (is.na(date)) return(NA)
  m <- month(date)
  y <- year(date)
  
  if (m %in% 7:9) {
    return(paste("Q1", paste0("FY", y + 1)))
  } else if (m %in% 10:12) {
    return(paste("Q2", paste0("FY", y + 1)))
  } else if (m %in% 1:3) {
    return(paste("Q3", paste0("FY", y)))
  } else if (m %in% 4:6) {
    return(paste("Q4", paste0("FY", y)))
  } else {
    return(NA)
  }
}