# app.R

library(shiny)
library(dplyr)
library(lubridate)
library(plotly)
library(prophet)
library(scales)

# --- Load and Prepare Data ---
aggregated_IC_data <- read.csv("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Research on Indirect Cost 06262025/Output/aggregated_IC_data_filtered.csv", stringsAsFactors = FALSE)

aggregated_IC_data <- aggregated_IC_data %>%
  mutate(Expenditure.Date = ymd(paste0(Expenditure.Month.Year, "-01"))) %>%
  filter(Expenditure.Date >= ymd("2021-07-01") & Expenditure.Date <= ymd("2025-05-01")) %>%
  group_by(Project.Organization) %>%
  filter(n() >= 36) %>%
  ungroup()

# UI ----
ui <- fluidPage(
  titlePanel("Project Organization IC Trend & Forecast"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("org", "Select Project Organization:",
                  choices = sort(unique(aggregated_IC_data$Project.Organization)),
                  selected = unique(aggregated_IC_data$Project.Organization)[1])
    ),
    
    mainPanel(
      plotlyOutput("trendPlot"),
      br(),
      plotlyOutput("forecastPlot")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  org_data <- reactive({
    aggregated_IC_data %>%
      filter(Project.Organization == input$org) %>%
      arrange(Expenditure.Date)
  })
  
  output$trendPlot <- renderPlotly({
    df <- org_data()
    
    plot_ly(
      data = df,
      x = ~Expenditure.Date,
      y = ~Total.Expenditure.Amount,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'steelblue'),
      marker = list(color = 'darkblue'),
      text = ~paste("Month: ", format(Expenditure.Date, "%b %Y"),
                    "<br>Total: $", comma(Total.Expenditure.Amount)),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Monthly IC Trend for:", input$org),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total IC Amount", tickformat = ","),
        hovermode = "closest"
      )
  })
  
  output$forecastPlot <- renderPlotly({
    df <- org_data()
    req(nrow(df) >= 12)
    
    prophet_df <- df %>%
      select(ds = Expenditure.Date, y = Total.Expenditure.Amount)
    m <- prophet(prophet_df, interval.width = 0.95, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
    future <- make_future_dataframe(m, periods = 6, freq = "month")
    forecast_prophet <- predict(m, future)
    
    forecast_df <- forecast_prophet %>%
      select(Date = ds, Forecast = yhat, Lo95 = yhat_lower, Hi95 = yhat_upper) %>%
      tail(6)
    
    plot_ly() %>%
      add_lines(data = df, x = ~Expenditure.Date, y = ~Total.Expenditure.Amount,
                name = "Historical", line = list(color = 'steelblue')) %>%
      add_lines(data = forecast_df, x = ~Date, y = ~Forecast,
                name = "Forecast (Prophet)", line = list(color = 'darkgreen', dash = "dash")) %>%
      add_ribbons(data = forecast_df, x = ~Date, ymin = ~Lo95, ymax = ~Hi95,
                  name = "95% CI", fillcolor = 'rgba(0,100,80,0.2)', line = list(width = 0), showlegend = TRUE) %>%
      layout(
        title = paste("6-Month Forecast for:", input$org),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total IC Amount"),
        hovermode = "x unified"
      )
  })
}

# RUN ----
shinyApp(ui, server)
