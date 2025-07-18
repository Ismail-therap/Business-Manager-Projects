# =============================================================
# SCRIPT 3: SUBMISSION, AWARD, NON-AWARD DURATION VISUALIZATION
# =============================================================

code_base_path <- "C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Total Fedral Grant Funded Through TIG 04152025/Code"
source(file.path(code_base_path, "0_Data_Path_Configuration.R"))
source(file.path(code_base_path, "1_Updated_Proposal_data_load_and_clean_from_Cayuse.R"))
source(file.path(code_base_path, "2_Financial_data_load_and_clean_from_Wyocloud.R"))

# Overall:

fed_award_non_award <- award_non_award %>%
  filter(`sponsor type` == "U.S. Federal Government")

plot_proposal_durations(fed_award_non_award)


# NSF , 2025

# NSF_award_non_award <- award_non_award %>%
#   filter(`Prime Sponsor` == "National Science Foundation" & Actual_Funding_FY =="FY2024")
# 
# 
# plot_proposal_durations(NSF_award_non_award)

