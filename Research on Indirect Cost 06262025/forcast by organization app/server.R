# SERVER
server <- function(input, output, session) {
  
  # Load and preprocess data (REPLACE this with your actual data loading logic)
  All_IC_data <- read_csv("C:/Users/mhossa11/OneDrive - University of Wyoming/Projects/Research on Indirect Cost 06262025/Output/aggregated_IC_data_filtered.csv")
  
  aggregated_IC_data <- All_IC_data %>%
    mutate(`Expenditure Month Year` = format(ymd(`Expenditure Date`), "%Y-%m")) %>%
    group_by(`Project Organization`, `Expenditure Month Year`) %>%
    summarise(`Total Expenditure Amount` = sum(`Expenditure Amount`, na.rm = TRUE), .groups = 'drop') %>%
    mutate(`Expenditure Date` = ymd(paste0(`Expenditure Month Year`, "-01"))) %>%
    filter(`Expenditure Date` >= ymd("2021-07-01"))
  
  updateSelectInput(session, "project_org", choices = sort(unique(aggregated_IC_data$`Project Organization`)))
  
  filtered_data <- reactive({
    req(input$project_org)
    aggregated_IC_data %>% filter(`Project Organization` == input$project_org)
  })
  
  output$trendPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    plot_ly(
      data = df,
      x = ~`Expenditure Date`,
      y = ~`Total Expenditure Amount`,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'steelblue'),
      marker = list(color = 'darkblue'),
      text = ~paste("Month:", format(`Expenditure Date`, "%b %Y"),
                    "<br>Total Expenditure: $", scales::comma(`Total Expenditure Amount`)),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste("Monthly IC Trend for:", input$project_org),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total IC Amount"),
        hovermode = "closest"
      )
  })
  
  output$forecastPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 6)  # require at least 6 points for forecasting
    
    ts_data <- ts(df$`Total Expenditure Amount`, start = c(year(min(df$`Expenditure Date`)), month(min(df$`Expenditure Date`))), frequency = 12)
    
    # Forecast using ETS (can be replaced with auto.arima or prophet)
    fit <- ets(ts_data)
    fcast <- forecast(fit, h = 6)
    
    fcast_df <- data.frame(
      Date = seq(max(df$`Expenditure Date`) + months(1), by = "month", length.out = 6),
      Forecast = as.numeric(fcast$mean)
    )
    
    plot_ly() %>%
      add_trace(x = df$`Expenditure Date`, y = df$`Total Expenditure Amount`, type = 'scatter', mode = 'lines+markers', name = "Actual") %>%
      add_trace(x = fcast_df$Date, y = fcast_df$Forecast, type = 'scatter', mode = 'lines+markers', name = "Forecast", line = list(dash = 'dash')) %>%
      layout(
        title = paste("6-Month Forecast for:", input$project_org),
        xaxis = list(title = "Month"),
        yaxis = list(title = "Forecasted IC Amount"),
        hovermode = "closest"
      )
  })
}
