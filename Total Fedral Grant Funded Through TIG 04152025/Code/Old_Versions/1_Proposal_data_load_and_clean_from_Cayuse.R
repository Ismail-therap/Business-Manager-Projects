# ================================================
# SCRIPT 1: PROPOSAL DATA CLEANING & DATE CREATION
# ================================================
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
# =====================================================
# STEP 1: Load and Clean Proposal Data
# =====================================================

# Load Proposal Data
proposal_data <- read_csv(Sys.getenv("PROPOSAL_DATA_PATH"))

names(proposal_data)





# Update 'Status' Based on 'Resolution' Information
proposal_data <- proposal_data %>%
  mutate(Status = case_when(
    Resolution == "PI Abandoned" & Status == "Closed" ~ "Not Submitted to Sponsor",
    Resolution == "Not Funded" & Status == "Closed" ~ "Not Funded",
    `Proposal Type` == "Transfer" ~ "Transfer",
    TRUE ~ Status
  ))



# =====================================================
# STEP 2: Extract Proposal History Data
# =====================================================

proposal_history_data <- proposal_data %>%
  select(`Proposal #`, Status, `History Action`, `History Action Date`) %>%
  group_by(`Proposal #`) %>%
  fill(Status, .direction = "downup") %>%  # Fill missing values both down and up
  ungroup() %>%
  filter(Status %in% c("Submitted to Sponsor", "Under Consideration", "Not Funded", "Funded"))



# =====================================================
# STEP 3: Clean and Pivot 'History Action' Data
# =====================================================

# Clean 'History Action' Column
proposal_history_data <- proposal_history_data %>%
  mutate(History_Action_Cleaned = gsub(".*: ", "", `History Action`)) %>%
  group_by(`Proposal #`, History_Action_Cleaned) %>%
  mutate(Action_Count = row_number(),
         History_Action_Final = ifelse(Action_Count > 1, 
                                       paste0(History_Action_Cleaned, "_", Action_Count), 
                                       History_Action_Cleaned)) %>%
  ungroup()

# Pivot History Data to Wide Format
proposal_wide <- proposal_history_data %>%
  select(`Proposal #`, History_Action_Final, `History Action Date`) %>%
  pivot_wider(names_from = History_Action_Final, values_from = `History Action Date`)

# =====================================================
# STEP 4: Add Submission and Funding Dates
# =====================================================

# Add 'Status' Information
proposal_wide <- proposal_history_data %>%
  select(`Proposal #`, Status) %>%
  distinct() %>%
  left_join(proposal_wide, by = "Proposal #")

# Define Columns for Submission and Funding Dates
date_cols <- c(
  "Approved to Submitted to Sponsor", "In Development to Submitted to Sponsor",
  "Form was edited in 'Submitted to Sponsor' status", "In Development to Submitted to Sponsor_2",
  "Approved to Submitted to Sponsor_2", "Under Review to Submitted to Sponsor",
  "Under Review to Submitted to Sponsor_2", "Under Consideration to Submitted to Sponsor",
  "Form was edited in 'Submitted to Sponsor' status_2"
)

funding_cols <- c(
  "Submitted to Sponsor to Funded", "Form was edited in 'Funded' status",
  "Under Consideration to Funded", "Funded (Project Complete)",
  "Submitted to Sponsor to Funded_2"
)

# Convert Dates to POSIXct Format
existing_date_cols <- intersect(date_cols, names(proposal_wide))
existing_funding_cols <- intersect(funding_cols, names(proposal_wide))

proposal_wide[existing_date_cols] <- lapply(proposal_wide[existing_date_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_funding_cols] <- lapply(proposal_wide[existing_funding_cols], function(x) mdy_hms(x, tz = "America/Denver"))

# Create Actual Submission and Funding Dates
proposal_wide <- proposal_wide %>%
  mutate(
    Actual_Submission_Date = pmax(!!!syms(existing_date_cols), na.rm = TRUE),
    Actual_Submission_Date = ifelse(is.infinite(Actual_Submission_Date), NA, Actual_Submission_Date),
    Actual_Funding_Date = pmax(!!!syms(existing_funding_cols), na.rm = TRUE),
    Actual_Funding_Date = ifelse(is.infinite(Actual_Funding_Date), NA, Actual_Funding_Date)
  )

# Format Dates for Final Output
proposal_wide <- proposal_wide %>%
  mutate(
    Actual_Submission_Date = as.POSIXct(Actual_Submission_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Funding_Date = as.POSIXct(Actual_Funding_Date, origin = "1970-01-01", tz = "America/Denver")
  )

proposal_wide <- proposal_wide %>%
  filter(!is.na(Actual_Submission_Date))


# =====================================================
# STEP 5: Finalizing Cleaned Proposal Data
# =====================================================

# Select Submission and Funding Dates
proposal_data_submission_funding_date <- proposal_wide %>%
  select(`Proposal #`, Actual_Submission_Date, Actual_Funding_Date)

# Merge Submission/Funding Dates with Main Proposal Data
proposal_data <- merge(proposal_data, proposal_data_submission_funding_date, by = "Proposal #")


dim(proposal_data)

# Add Proposal Submission Fiscal Year (FY)
proposal_data <- proposal_data %>%
  mutate(
    Actual_Submission_Date = as.POSIXct(Actual_Submission_Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    FY = ifelse(!is.na(Actual_Submission_Date),
                paste0("FY", year(Actual_Submission_Date) + ifelse(month(Actual_Submission_Date) >= 7, 1, 0)),
                NA)
  )





