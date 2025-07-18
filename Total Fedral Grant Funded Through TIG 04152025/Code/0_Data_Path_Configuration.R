# ==============================================
# SCRIPT 0: PACKAGES AND DATA PATH CONFIGURATION 
# ==============================================

# 📌 Load Required Libraries
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(lubridate)  # For handling date-time conversions
library(stringr)    # For string operations
library(tidyr)      # For reshaping data

# =====================================================
# STEP 1: Define Base Path for Data
# =====================================================

# 📌 Base Data Directory
base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Input/"



# 📌 Define File Paths Using Base Path
# Cayuse Data
proposal_data_path <- file.path(base_path, "Cayuse Data/All_proposal_04152025.csv")
award_data_path <- file.path(base_path, "Cayuse Data/All_award_04152025.csv")


wyocloud_path <- file.path(base_path, "WyoCloud Data/")
project_financial_summary_path <- file.path(wyocloud_path, "Project Financial Summary_Results_04152025.xlsx")
project_information_path <- file.path(wyocloud_path, "Project Information_Results_04182025.xlsx")



# =====================================================
# STEP 2: Define Output Paths
# =====================================================

# 📌 Define Base Output Directory
output_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Output/"

# 📌 Define Output File Names
# output_merged_fsu_award_path <- file.path(output_path, "filtered_award_non_award_with_FSU_031925.csv")
# output_no_submission_path <- file.path(output_path, "pi_without_any_submission_031925.csv")

# =====================================================
# STEP 3: Export Paths as Environment Variables
# =====================================================


Sys.setenv(PROJECT_FINANCIAL_SUMMARY_PATH = project_financial_summary_path)
Sys.setenv(PROJECT_INFORMATION_PATH = project_information_path)

Sys.setenv(PROPOSAL_DATA_PATH = proposal_data_path)
Sys.setenv(AWARD_DATA_PATH = award_data_path)

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


###########################################################

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











################################################################

# Create the visualization for the duration (Submission and Decision)


plot_proposal_durations <- function(proposal_data) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  # Step 1: Prepare long format dataset
  proposal_data_long <- proposal_data %>%
    select(
      Days_to_Submission,
      Days_to_Funding_From_Submission,
      Days_to_Non_Funding_From_Submission,
      `Proposal #`
    ) %>%
    pivot_longer(
      cols = starts_with("Days_to_"),
      names_to = "Duration_Type",
      values_to = "Days"
    ) %>%
    filter(!is.na(Days)) %>%
    mutate(Duration_Type = str_replace_all(Duration_Type, "_", " "),
           Duration_Type = str_to_title(Duration_Type))
  
  # Step 2: Count unique proposals and calculate medians
  proposal_counts <- proposal_data_long %>%
    group_by(Duration_Type) %>%
    summarise(Proposal_Count = n_distinct(`Proposal #`), .groups = "drop")
  
  medians <- proposal_data_long %>%
    group_by(Duration_Type) %>%
    summarise(
      median_days = median(Days),
      label_y = max(table(Days)) * 0.8,
      .groups = "drop"
    )
  
  # Step 3: Merge for plotting
  plot_data <- left_join(proposal_data_long, proposal_counts, by = "Duration_Type") %>%
    mutate(Facet_Label = paste0(Duration_Type, " (", Proposal_Count, " Proposals)"))
  
  medians <- left_join(medians, proposal_counts, by = "Duration_Type") %>%
    mutate(Facet_Label = paste0(Duration_Type, " (", Proposal_Count, " Proposals)"))
  
  # Step 4: Create the plot
  ggplot(plot_data, aes(x = Days)) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black") +
    geom_vline(data = medians, aes(xintercept = median_days), color = "darkorange", linetype = "dashed", size = 1) +
    geom_text(data = medians,
              aes(x = median_days, y = label_y,
                  label = paste0("Median: ", round(median_days), " days")),
              color = "darkorange",
              size = 4,
              fontface = "bold",
              hjust = -0.1) +
    facet_wrap(~ Facet_Label, scales = "free_y", ncol = 1) +
    theme_minimal() +
    labs(
      title = "Distribution of Key Proposal Durations",
      subtitle = "Days from Proposal Creation to Submission, Funding, and Non-Funding Decisions",
      x = "Days",
      y = "Frequency"
    ) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, face = "italic", hjust = 0.5)
    )
}

