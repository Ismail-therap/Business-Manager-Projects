


# Get the Prepeared Data
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Code/1.WyoCloud_Data_Preperation.R")
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Code/2.Cayuse_Proposal_Data_Preperation.R")


dim(full_data_for_modelling_all_columns)
dim(proposal_data_subsetted)




proposal_data_subsetted <- proposal_data_subsetted %>%
  mutate(
    `Project Number` = gsub("^(\\d{2})-(\\d{4})-P", "\\1\\2A", `Proposal #`)
  ) %>%
  select(-`Proposal #`)

proposal_data_subsetted <- proposal_data_subsetted %>%
  distinct(`Project Number`, .keep_all = TRUE)
full_data_for_modelling_all_columns <- full_data_for_modelling_all_columns %>%
  distinct(`Project Number`, .keep_all = TRUE)

modelling_data <- merge(
  full_data_for_modelling_all_columns,
  proposal_data_subsetted,
  by = "Project Number"
)


# Checking data issue and fix:
# 231856A0001 == Funded. 7/25/2023
# 231860A0001 ===  Funded. 8/2/2023
# 230856A0002 == Funded 6/26/2023  # Date when the contract signed, no clear indication of funding date
# 240430A0001 == Funded 01/01/2024 # Contract finalized and PO issued in January 2024
# 231951A0001 == 6/14/2022 # Signature date
# 240645A0001 ==  03/20/2024 # Signature date
# 231862A0001 == 7/26/2023 # Note Details
# 240951A0002 ==  8/28/2024 # From Note (but not clear from anywhere it's an estimate)
# 240482A0002 == 10/24/2024

# Create the Set up time variable: We cannot use the Setup time because many funidng date have issues and in some case funding date isn't available,
# althought he proposal already funded. 

# modelling_data <- modelling_data %>%
#   mutate(
#     # Convert "Project Creation Date" to Date
#     `Project Creation Date` = as.Date(`Project Creation Date`),
#     
#     # Calculate setup time in days
#     Set_Up_Time = as.numeric(difftime(`Project Creation Date`,Actual_Funding_Date , units = "days"))
#   )
# View(modelling_data)
# summary(modelling_data$Set_Up_Time)
# 


# =====================================================
# Subsetting for final model
# =====================================================

modelling_data$`Award Type` <- ifelse(modelling_data$`Award Type` == "Research - Sponsored","Research - Sponsored","Others")
modelling_data$`Project Fund Source` <- ifelse(modelling_data$`Project Fund Source` == "Federal Direct Sponsored Funds","Federal",
                                               ifelse(modelling_data$`Project Fund Source` == "State","State","Others"))


# table(modelling_data$`Project Designation`) # Remove
# table(modelling_data$`Project Type`) # Remove
# table(modelling_data$`Award Type`)
# table(modelling_data$`Project Status`) # Remove
# # "Project Start Date" : Remove
# # "Project Finish Date":  Remove
# # "Project_Starting_Month": Remove
# # "Project Name": Remove
# # "Fund_Source_Grouped": Remove
# 
# # I could use any one of this variable:
# table(modelling_data$`Project Fund Source`)
# table(modelling_data$`Project Funding Type`) # Remove

# "Project Actual Expenses" : Remove


modelling_data <- modelling_data %>%
  mutate(
    `Project Fund Source` = as.factor(`Project Fund Source`),
    `Award Type` = as.factor(`Award Type`),
    Academic_Semester = as.factor(Academic_Semester),
    Reduced_FA = as.factor(Reduced_FA),
    `subawards subcontracts` = as.factor(`subawards subcontracts`),
    Approval_Required = as.factor(Approval_Required),
    Chemical_or_Hazard = as.factor(Chemical_or_Hazard),
    Foreign_Involvement = as.factor(Foreign_Involvement),
    Technology_or_IP_Involved = as.factor(Technology_or_IP_Involved),
    Project_duration = as.numeric(Project_duration),
    `Project Funding Amount` = as.numeric(`Project Funding Amount`)
    )


# Calculate missing value percentage by column
missing_pct <- sapply(modelling_data, function(x) sum(is.na(x)) / length(x)) * 100

# Convert to data frame and sort in descending order
missing_df <- data.frame(Column = names(missing_pct), Missing_Percentage = missing_pct)
missing_df <- missing_df[order(-missing_df$Missing_Percentage), ]



# Impute Reduced_FA (factor) with most frequent value
mode_value <- names(which.max(table(modelling_data$Reduced_FA)))
modelling_data$Reduced_FA[is.na(modelling_data$Reduced_FA)] <- mode_value

# Impute Number_of_Project_as_Principal_Investigator (numeric) with median
median_value <- median(modelling_data$Number_of_Project_as_Principal_Investigator, na.rm = TRUE)
modelling_data$Number_of_Project_as_Principal_Investigator[is.na(modelling_data$Number_of_Project_as_Principal_Investigator)] <- median_value

# Optional: Verify that there are no missing values left
colSums(is.na(modelling_data)[, c("Reduced_FA", "Number_of_Project_as_Principal_Investigator")])


########################################################################



full_data_for_modelling <- modelling_data %>%
  mutate(
    `Date_First_Expenditure_Started` = as.Date(`Date_First_Expenditure_Started`),
    Expense_starting_days = as.numeric(`Date_First_Expenditure_Started` - `Project Start Date`)
  ) %>%
  filter(Expense_starting_days >= 0 & !is.na(`Project Number`) & !is.na(Number_of_Project_as_Principal_Investigator)) %>%
  dplyr::select(all_of(c(
    "Project Number", 
    "Project Fund Source", 
    "Award Type",
    "Number_of_Project_as_Principal_Investigator", 
    "Total_project_person", 
    "Project_duration", 
    "Academic_Semester",
    "Project Funding Amount", 
    "Expense_starting_days",
    "Reduced_FA",
    "subawards subcontracts",                 
    "Approval_Required",
    "Chemical_or_Hazard",
    "Foreign_Involvement",                      
   "Technology_or_IP_Involved"
  )))




write.csv(full_data_for_modelling,
          file.path(output_path, "Data_Used_in_model_06_23_25.csv"),
          row.names = FALSE)

# =====================================================
# Final Dataset for Model Testing (No Expense Yet Projects)
# =====================================================


no_expense_yet <- modelling_data %>%
  filter(`Project Status` == "ACTIVE", is.na(Date_First_Expenditure_Started)) 


prediction_data <- no_expense_yet %>%
  dplyr::select(all_of(c(
    "Project Number", 
    "Project Fund Source", 
    "Award Type",
    "Number_of_Project_as_Principal_Investigator", 
    "Total_project_person", 
    "Project_duration", 
    "Academic_Semester",
    "Project Funding Amount", 
    "Reduced_FA",
    "subawards subcontracts",                 
    "Approval_Required",
    "Chemical_or_Hazard",
    "Foreign_Involvement",                      
    "Technology_or_IP_Involved"
  )))


