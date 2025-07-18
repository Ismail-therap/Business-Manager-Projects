# ================================================
# SCRIPT 2: PROJECT FINANCIAL DATA CLEANING & MERGING
# ================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))


# =====================================================
# STEP 1: Load Financial Summary  and Information Data 
# =====================================================

# Load Project Financial Summary Data
project_financial_summary <- read_excel(Sys.getenv("PROJECT_FINANCIAL_SUMMARY_PATH"))

# Load project information and merge with financial summary

project_information <- read_excel(Sys.getenv("PROJECT_INFORMATION_PATH"))
names(project_information)[5] <- "Project_Status"

# =====================================================
# STEP 2: Load Additional Financial Data for Indirect Costs
# =====================================================


project_financial_summary <- merge(project_financial_summary, 
                                    project_information, 
                                    by = "Project Number") %>%
   select(`Project Number`,`Project Status`,`Project Funding Type`, `Project Funding Amount`, `Award F&A Schedule`)


# =====================================================
# STEP 3: Clean 'Project Number' for Merging
# =====================================================

proposal_data <- proposal_data %>%
  mutate(`Project Number` = gsub("-", "", `Proposal #`),   # Remove hyphens
         `Project Number` = gsub("P", "A", `Project Number`)) # Replace 'P' with 'A'


# =====================================================
# STEP 4: Merge Financial Data with Proposal Data
# =====================================================

award_non_award <- left_join(proposal_data, 
                             project_financial_summary, 
                             by = "Project Number") %>%
  distinct()   # Ensure unique rows


# =====================================================
# STEP 5: Load and Merge College Name Data
# =====================================================

# Load College Name Data
college_name_data <- read_excel(Sys.getenv("COLLEGE_NAME_DATA_PATH"))

# Clean and Prepare College Data
college_name_data <- college_name_data %>%
  select(`College (Subdivision)`, `Department (Organization)`) %>%
  na.omit() %>%
  rename(`PI Unit` = `Department (Organization)`)

# Merge College Data with Award Data
award_non_award <- left_join(award_non_award, 
                             college_name_data, 
                             by = "PI Unit") %>%
  distinct()




# =====================================================
# STEP 6: Create Status Summary Columns
# =====================================================

# Convert 'Status' to Character for Pivoting

# award_status_wide <- award_non_award %>%
#   mutate(Status = as.character(Status)) %>%
#   filter(!is.na(Status)) %>%           # Remove rows where Status is NA
#   select(`Proposal #`, Status) %>%
#   distinct() %>%
#   mutate(Value = 1) %>%
#   pivot_wider(
#     names_from = Status,
#     values_from = Value,
#     values_fill = list(Value = 0)
#   )
# 



# # Combine Status Summary Back with Proposal Data
# award_non_award <- left_join(award_non_award, 
#                              award_status_wide, 
#                              by = "Proposal #") %>%
#   mutate(Total_submitted_to_sponsor = `Submitted to Sponsor` + 
#            `Under Consideration` + 
#            `Not Funded` + 
#            Funded) %>%
#   arrange(`full proposal title`, desc(`Created Date.x`)) %>%
#   distinct(PI, `full proposal title`, .keep_all = TRUE)  # Removing the same proposal title or removing duplicated proposal!



# =====================================================
# STEP 7: Data Cleaning - Remove Unnecessary Entries
# =====================================================

# Remove rows with 'test' or 'testing' in 'History Comment'
award_non_award <- award_non_award[!grepl("test|testing", 
                                          award_non_award$`History Comment`, 
                                          ignore.case = TRUE), ]



# =====================================================
# STEP 8: Date Formatting and Final Touch
# =====================================================

# Convert 'Actual_Submission_Date' to Date Format
award_non_award <- award_non_award %>%
  mutate(Actual_Submission_Date = as.POSIXct(Actual_Submission_Date, 
                                             format = "%Y-%m-%d %H:%M:%S", 
                                             tz = "UTC"))

# First Proposal Submission Date for Each PI
award_non_award <- award_non_award %>%
  group_by(PI) %>%
  mutate(First_Proposal_Submission_Date = min(Actual_Submission_Date, na.rm = TRUE)) %>%
  ungroup()




# First Federal Grant Submission Date — FIXED VERSION
first_federal_submission <- award_non_award %>%
  filter(`sponsor type` == "U.S. Federal Government") %>%
  group_by(PI) %>%
  summarize(First_Federal_Grant_Submission_Date = min(Actual_Submission_Date, na.rm = TRUE), .groups = "drop")

# Merge back to award_non_award
award_non_award <- award_non_award %>%
  left_join(first_federal_submission, by = "PI")


# Replace Infinite Values with NA for Empty Date Entries
award_non_award <- award_non_award %>%
  mutate(
    First_Proposal_Submission_Date = ifelse(is.infinite(First_Proposal_Submission_Date), NA, First_Proposal_Submission_Date),
    First_Federal_Grant_Submission_Date = ifelse(is.infinite(First_Federal_Grant_Submission_Date), NA, First_Federal_Grant_Submission_Date)
  )

# Format Dates to Readable Format
award_non_award <- award_non_award %>%
  mutate(
    First_Proposal_Submission_Date = format(as.POSIXct(First_Proposal_Submission_Date, origin = "1970-01-01", tz = "UTC"), "%m/%d/%Y"),
    First_Federal_Grant_Submission_Date = format(as.POSIXct(First_Federal_Grant_Submission_Date, origin = "1970-01-01", tz = "UTC"), "%m/%d/%Y")
  )



# If there is no submission date we will not consider that proposal here.
award_non_award <- award_non_award %>%
  filter(!(Status == "Not Funded" & is.na(Actual_Submission_FY)))




award_non_award <- award_non_award %>%
  mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))


