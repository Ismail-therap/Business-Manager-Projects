# ================================================
# SCRIPT 1: PROPOSAL DATA CLEANING & DATE CREATION
# ================================================
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
# =====================================================
# STEP 1: Load and Clean Proposal Data
# =====================================================

# Load Proposal Data
award_data <- read_csv("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Input/Cayuse Data/All_award_04152025.csv")
# Step 3: View the two columns
View(award_data[, c("Award #","Award Notice Received")])


# Creating a general column to select Prime Sponsor:
award_data <- award_data %>%
  mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))


# Step 1: Parse the date and remove rows with missing Award Notice Received
award_data <- award_data %>%
  mutate(`Award Notice Received` = mdy(`Award Notice Received`)) %>%  # Parse dates like "3/17/2025"
  filter(!is.na(`Award Notice Received`))  # Remove rows where date is missing

# Step 2: Create Award_FY based on U.S. Federal Fiscal Year (Oct–Sep)
award_data <- award_data %>%
  mutate(Award_FY = case_when(
    month(`Award Notice Received`) >= 10 ~ paste0("FY", year(`Award Notice Received`) + 1),
    TRUE ~ paste0("FY", year(`Award Notice Received`))
  ))

# Step 3: View the two columns
View(award_data[, c("Award #","Award Notice Received", "Award_FY")])





award_data <- award_data %>%
  mutate(`Prime Sponsor` = ifelse(is.na(`Prime Sponsor`) | `Prime Sponsor` == "", `Sponsor`, `Prime Sponsor`))

fed_award_non_award <- award_data %>%
  filter(`sponsor type` == "U.S. Federal Government") %>%
  select(`Award #`,`Project Title`,Award_FY)%>%
  distinct()

table(fed_award_non_award$Award_FY)

#grep("date", names(award_data), value = TRUE, ignore.case = TRUE)

# 
# award_data
# All_Award_data_NASA <- All_Award_data_filtered %>%
#   filter(grepl("National Science Foundation", `Prime Sponsor`, ignore.case = TRUE))