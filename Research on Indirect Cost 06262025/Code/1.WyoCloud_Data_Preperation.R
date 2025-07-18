# Get the Data Directory and other input
source("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Research on Indirect Cost 06262025/Code/0.Defining_Data_Path.R")



# =======================================================
# Load and Prepare Cost Details Data (Dependent Vairable)
# =======================================================

# Set the folder path
folder_path <- file.path(wyocloud_data_input_path, "Expenditure Details Data")


# List all .xlsx files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read and combine all Excel files into one data frame
combined_data <- file_list %>%
  lapply(read_excel) %>%
  bind_rows()



All_IC_data <- combined_data %>%
  filter( `Expenditure Category` == "F&A" & 
           `Expenditure Type` == "Indirect Cost Expense")



# Step 1: Filter before aggregating
aggregated_IC_data <- All_IC_data %>%
  # Convert Expenditure Date to a proper Date type
  mutate(`Expenditure Date` = ymd(`Expenditure Date`)) %>%
  # Filter only from July 1, 2021 onward
  filter(`Expenditure Date` >= ymd("2021-07-01")) %>%
  # Create Expenditure Month Year from the Date
  mutate(`Expenditure Month Year` = format(`Expenditure Date`, "%Y-%m")) %>%
  # Aggregate
  group_by(`Project Organization`, `Expenditure Month Year`) %>%
  summarise(
    `Total Expenditure Amount` = sum(`Expenditure Amount`, na.rm = TRUE),
    .groups = 'drop'
  )



write.csv(aggregated_IC_data,"Output/aggregated_IC_data_filtered.csv",row.names = F)


# library(dplyr)
# library(lubridate)
# library(plotly)
# 
# library(dplyr)
# library(lubridate)
# library(plotly)
# library(scales)  # for comma formatting
# 
# plot_monthly_expenditure <- function(data, project_org_name) {
#   # Ensure Expenditure Month Year is a proper Date object
#   data <- data %>%
#     mutate(`Expenditure Month Year` = ymd(paste0(`Expenditure Month Year`, "-01")))
#   
#   # Filter for the selected Project Organization
#   filtered_data <- data %>%
#     filter(`Project Organization` == project_org_name)
#   
#   # Create interactive Plotly plot
#   p <- plot_ly(
#     data = filtered_data,
#     x = ~`Expenditure Month Year`,
#     y = ~`Total Expenditure Amount`,
#     type = 'scatter',
#     mode = 'lines+markers',
#     line = list(color = 'steelblue'),
#     marker = list(color = 'darkblue'),
#     text = ~paste(
#       "Month: ", format(`Expenditure Month Year`, "%b %Y"), "<br>",
#       "Total Expenditure: $", comma(`Total Expenditure Amount`)
#     ),
#     hoverinfo = "text"
#   ) %>%
#     layout(
#       title = list(text = paste("Monthly IC Trend for:", project_org_name)),
#       xaxis = list(title = "Month"),
#       yaxis = list(title = "Total IC Amount", tickformat = ","),
#       hovermode = "closest"
#     )
#   
#   return(p)
# }
# 
# 
# 
# plot_monthly_expenditure(aggregated_IC_data_filtered, "Zoology & Physiology")
# 
