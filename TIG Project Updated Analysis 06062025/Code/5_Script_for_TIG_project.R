# ================================================
# SCRIPT 2: PROJECT FINANCIAL DATA CLEANING & MERGING
# ================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Business Manager Jami Work Requests/TIG Project Updated Analysis 06062025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))
source(file.path(code_base_path, "2_Award_Data_Preperation_from_Cayuse.R"))

# 1. The number of submitted federal grant proposals and how many are funded.

############################# Data request: Jami 04182025 ##

proposal_data_subset <- proposal_data %>%
  filter(!is.na(PI) & PI != "") %>%
  select(
    `Proposal #`,
    `Admin Unit`,
    `Instrument Type`,
    `PI`,
    `sponsor type`,
    `Prime Sponsor`,
    `Project End Date`,
    `Project Start Date`,
    `Project Title`,
    `Proposal Type`,
    `Sponsor`,
    `Status`,
    `Actual_Submission_Date`,
    `Actual_Udr_Consid_Date`,
    `Actual_Funding_Date`,
    `Actual_Not_Funding_Date`,
    `Actual_Submission_FY`,
    `Actual_Udr_Consid_FY`,
    `Actual_Funding_FY`,
    `Actual_Not_Funding_FY`,
    `Created Date.y`,
    `Proposal_Creation_FY`,
    `Days_to_Submission`,
    `Days_to_Funding_From_Submission`,
    `Days_to_Non_Funding_From_Submission`,
    `College/Division`,
    `Total Sponsor Costs`
  )


Clean_TIG_data <- read_excel(TIG_Data_From_Nick)

# Clean_TIG_data_subsetted <- Clean_TIG_data %>%
#   select("Proposal #","Awarded_Status_By_TIG","PI","Title")
merged_data <- left_join(proposal_data_subset, Clean_TIG_data, by = "Proposal #") %>%
  distinct(`Proposal #`, .keep_all = TRUE)


output_path_raw_processed <- file.path(output_path, "/07182025/Cayuse_TIG_Merged_07182025.csv")
TIG_data_path <- file.path(output_path, "/07182025/Clean_TIG_data_07182025.csv")


write.csv(Clean_TIG_data,TIG_data_path,row.names=F,na="")
write.csv(merged_data,output_path_raw_processed,row.names=F,na="")
