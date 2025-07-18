# Get the Data Directory and other input
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Indirect Cost Forecasting 02022025/Regression Approach/Code/0.Defining_Data_Path.R")


# =====================================================
# Load and Prepare Project Information Data (PI Count)
# =====================================================
project_details <- read_excel(paste0(data_input_path, "Project Information_Results.xlsx"))




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




#############################################################
###### Get the project Details data: Project Created Date ###
#############################################################

# project_details <- read_excel(paste0(data_input_path, "Project Details_Results.xlsx"))
# 
# 
# project_details_selected <- project_details %>%
#   dplyr::select(all_of(c("Project Number", "Project Creation Date")))
# 
# 
# 
# full_data_for_modelling_all_columns <- merge(full_data_for_modelling_all_columns, project_details_selected, by = "Project Number")














