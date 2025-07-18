# ================================================
# SCRIPT 1: PROPOSAL DATA CLEANING & DATE CREATION
# ================================================
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Proposal health Project by PI 01312025/Code/0_Data_Path_Configuration.R")
# =====================================================
# STEP 1: Load and Clean Proposal Data
# =====================================================

# Load Proposal Data
proposal_data <- read_csv(Sys.getenv("PROPOSAL_DATA_PATH"))



# Subset relevant columns
Created_date <- proposal_data[, c("Proposal #", "Created Date")]

# Clean and convert Created Date to fiscal year
cleaned_Created_date <- Created_date %>%
  filter(!is.na(`Created Date`)) %>%               # Remove NA Created Dates
  distinct(`Proposal #`, .keep_all = TRUE) %>%     # Keep first per Proposal #
  mutate(`Created Date` = mdy_hms(`Created Date`), # Convert string to datetime
         Proposal_Creation_FY = get_fy(`Created Date`))              # Apply FY conversion


# Update 'Status' Based on 'Resolution' Information

proposal_data <- proposal_data %>%
  mutate(Status = case_when(
    Resolution == "PI Abandoned" & Status == "Closed" ~ "Not Submitted to Sponsor",
    Resolution == "Not Funded" & Status == "Closed" ~ "Not Funded",
    `Proposal Type` == "Transfer" ~ "Transfer",
    TRUE ~ Status
  ))
# 



# =====================================================
# STEP 2: Extract Proposal History Data
# =====================================================

proposal_history_data <- proposal_data %>%
  select(`Proposal #`, Status,`sponsor type`, `History Action`, `History Action Date`) %>%
  group_by(`Proposal #`) %>%
  fill(Status, .direction = "downup") %>%  # Fill missing values both down and up
  ungroup() %>%
  filter(Status %in% c("Submitted to Sponsor", "Under Consideration", "Not Funded", "Funded"))



# We are filtering around 50% of the proposal in this step. Originally there were 4030 proposal now it have 1930 which have status
# "Submitted to Sponsor", "Under Consideration", "Not Funded", "Funded"



# =====================================================
# STEP 3: Clean and Pivot 'History Action' Data
# =====================================================

# Clean 'History Action' Column
clean_proposal_history_data <- proposal_history_data %>%
  mutate(History_Action_Cleaned = gsub(".*: ", "", `History Action`)) %>%
  group_by(`Proposal #`, History_Action_Cleaned) %>%
  mutate(Action_Count = row_number(),
         History_Action_Final = ifelse(Action_Count > 1, 
                                       paste0(History_Action_Cleaned, "_", Action_Count), 
                                       History_Action_Cleaned)) %>%
  ungroup()


# Pivot History Data to Wide Format
proposal_wide <- clean_proposal_history_data %>%
  select(`Proposal #`, History_Action_Final, `History Action Date`) %>%
  pivot_wider(names_from = History_Action_Final, values_from = `History Action Date`)






# =====================================================
# STEP 4: Add Submission and Funding Dates
# =====================================================

# Add 'Status' Information
proposal_wide <- clean_proposal_history_data %>%
  select(`Proposal #`, Status,`sponsor type`) %>%
  distinct() %>%
  left_join(proposal_wide, by = "Proposal #")




# Selecting columns based on the Status:



# All Under Consideration Columns
Under_Consideration_cols <- c("Submitted to Sponsor to Under Consideration")

# All Not Funded Columns
Not_Funded_cols <- c("Not Funded","Not Funded_2","Not Funded_3","In Development to Closed. Resolution set to: Not Funded")

# All Submission Columns
Submission_cols <- c(
  "Approved to Submitted to Sponsor", "In Development to Submitted to Sponsor",
  "Form was edited in 'Submitted to Sponsor' status", "In Development to Submitted to Sponsor_2",
  "Approved to Submitted to Sponsor_2", "Under Review to Submitted to Sponsor",
  "Under Review to Submitted to Sponsor_2", "Under Consideration to Submitted to Sponsor",
  "Form was edited in 'Submitted to Sponsor' status_2","Status was changed from: Submitted to Sponsor to Under Consideration"
)

# All Funding Columns
funding_cols <- c(
  "Submitted to Sponsor to Funded", "Form was edited in 'Funded' status",
  "Under Consideration to Funded", "Funded (Project Complete)",
  "Submitted to Sponsor to Funded_2"
)



# Convert Dates to POSIXct Format
existing_submission_cols <- intersect(Submission_cols, names(proposal_wide))
existing_Udr_Consid_cols <- intersect(Under_Consideration_cols, names(proposal_wide))
existing_funding_cols <- intersect(funding_cols, names(proposal_wide))
existing_not_funding_cols <- intersect(Not_Funded_cols, names(proposal_wide))




proposal_wide[existing_submission_cols] <- lapply(proposal_wide[existing_submission_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_funding_cols] <- lapply(proposal_wide[existing_funding_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_Udr_Consid_cols] <- lapply(proposal_wide[existing_Udr_Consid_cols], function(x) mdy_hms(x, tz = "America/Denver"))
proposal_wide[existing_not_funding_cols] <- lapply(proposal_wide[existing_not_funding_cols], function(x) mdy_hms(x, tz = "America/Denver"))


# Create Actual Submission and Funding Dates
proposal_wide <- proposal_wide %>%
  mutate(
    Actual_Submission_Date = pmax(!!!syms(existing_submission_cols), na.rm = TRUE),
    Actual_Submission_Date = ifelse(is.infinite(Actual_Submission_Date), NA, Actual_Submission_Date),
    
    Actual_Funding_Date = pmax(!!!syms(existing_funding_cols), na.rm = TRUE),
    Actual_Funding_Date = ifelse(is.infinite(Actual_Funding_Date), NA, Actual_Funding_Date),
    
    Actual_Udr_Consid_Date = pmax(!!!syms(existing_Udr_Consid_cols), na.rm = TRUE),
    Actual_Udr_Consid_Date = ifelse(is.infinite(Actual_Udr_Consid_Date), NA, Actual_Udr_Consid_Date),
    
    Actual_Not_Funding_Date = pmax(!!!syms(existing_not_funding_cols), na.rm = TRUE),
    Actual_Not_Funding_Date = ifelse(is.infinite(Actual_Not_Funding_Date), NA, Actual_Not_Funding_Date)
    
  )

# Format Dates for Final Output
proposal_wide <- proposal_wide %>%
  mutate(
    Actual_Submission_Date = as.POSIXct(Actual_Submission_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Funding_Date = as.POSIXct(Actual_Funding_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Udr_Consid_Date = as.POSIXct(Actual_Udr_Consid_Date, origin = "1970-01-01", tz = "America/Denver"),
    Actual_Not_Funding_Date = as.POSIXct(Actual_Not_Funding_Date, origin = "1970-01-01", tz = "America/Denver")
  )



# Making Sure at least the Proposal Submitted
proposal_wide <- proposal_wide %>%
  filter(!is.na(Actual_Submission_Date))




# =====================================================
# STEP 5: Finalizing Cleaned Proposal Data
# =====================================================

# Select Submission and Funding Dates
proposal_data_submission_funding_date <- proposal_wide %>%
  select(`Proposal #`, Actual_Submission_Date,Actual_Udr_Consid_Date, Actual_Funding_Date,Actual_Not_Funding_Date)

# Merge Submission/Funding Dates with Main Proposal Data
proposal_data <- merge(proposal_data, proposal_data_submission_funding_date, by = "Proposal #")


#========================================================================
# STEP 6: Creating the FY columns from the Dates by the Status
#========================================================================

proposal_data <- proposal_data %>%
  mutate(
    Actual_Submission_Date = ymd_hms(Actual_Submission_Date, tz = "America/Denver"),
    Actual_Udr_Consid_Date = ymd_hms(Actual_Udr_Consid_Date, tz = "America/Denver"),
    Actual_Funding_Date = ymd_hms(Actual_Funding_Date, tz = "America/Denver"),
    Actual_Not_Funding_Date = ymd_hms(Actual_Not_Funding_Date, tz = "America/Denver"),
    
    Actual_Submission_FY = get_fy(Actual_Submission_Date),
    Actual_Udr_Consid_FY = get_fy(Actual_Udr_Consid_Date),
    Actual_Funding_FY = get_fy(Actual_Funding_Date),
    Actual_Not_Funding_FY = get_fy(Actual_Not_Funding_Date)
  )
# Adding the Proposal Creation date and creation FY
proposal_data <- merge(proposal_data,cleaned_Created_date,by = "Proposal #")


proposal_data_N <- proposal_data %>%
  filter(!is.na(Status)) %>%            # Remove rows where Status is NA
  distinct()                            # Then keep only distinct rows



# Calculate durations (in days)
proposal_data <- proposal_data %>%
  mutate(
    Days_to_Submission = ifelse(!is.na(Actual_Submission_Date) & !is.na(`Created Date.y`),
                                round(as.numeric(difftime(Actual_Submission_Date, `Created Date.y`, units = "days"))),
                                NA),
    
    Days_to_Funding_From_Submission = ifelse(!is.na(Actual_Funding_Date) & !is.na(Actual_Submission_Date),
                                             round(as.numeric(difftime(Actual_Funding_Date, Actual_Submission_Date, units = "days"))),
                                             NA),
    
    Days_to_Non_Funding_From_Submission = ifelse(!is.na(Actual_Not_Funding_Date) & !is.na(Actual_Submission_Date),
                                                 round(as.numeric(difftime(Actual_Not_Funding_Date, Actual_Submission_Date, units = "days"))),
                                                 NA)
  )





