library(data.table)
library(dplyr)
library(forecast)

data = fread("E:\\BI_and_Analytics\\1_UC\\Capstone_Project\\Final_Files\\Dataset\\data.csv")
state_name <- c(unique(data$State))

##Forecast using Auto ARIMA
for (j in 1:length(state_name)) {
  state_data_filtered <- data %>% filter(State %in% state_name[j])
  state_data_ts1 <- ts(state_data_filtered$NominalGSDP, frequency = 1)
  
  ts_log = log(state_data_ts1)
  fit_auto <- auto.arima(ts_log,trace = FALSE)
  pred_auto <- forecast(fit_auto,h=10) #'h' -> number of years to be forecasted
  pred_auto_df <- as.data.frame(pred_auto)
  arima_forecast_df = exp(pred_auto$mean)
  
  #Getting a data-frame with state name and forecast for each state
  state_forecast_arima <- data.frame(state_name = rep(state_name[j], 10),
                                     year = c(2015:2024),
                                      forecast = exp(pred_auto$mean),
                                      model = pred_auto$method)
  
  state_forecast_arima$forecast = as.numeric(state_forecast_arima$forecast)
  
  if (j == 1) {
    state_forecast_arima_final <- state_forecast_arima
  } else {
    state_forecast_arima_final <- rbind(state_forecast_arima_final, state_forecast_arima)
  }
}

fwrite(state_forecast_arima_final,
       "E:\\BI_and_Analytics\\1_UC\\Capstone_Project\\Final_Files\\forecasting_all_states_arima.csv")


##Forecast using Holt's Linear Trend Model
data = fread("E:\\BI_and_Analytics\\1_UC\\Capstone_Project\\Final_Files\\Dataset\\data.csv")

state_name <- c(unique(data$State))

for (j in 1:length(state_name)) {
  state_data_filtered <- data %>% filter(State %in% state_name[j])
  state_data_ts1 <- ts(state_data_filtered$NominalGSDP, frequency = 1)

  
  ts_log = log(state_data_ts1)
  fit_auto <- holt(ts_log,h=10)
  pred_auto <- forecast(fit_auto,h=10)
  pred_auto_df <- as.data.frame(pred_auto)
  holt_forecast_df = exp(pred_auto$mean)
  
  #Get a data-frame with branch name and forecast for each branch
  state_forecast_holt <- data.frame(state_name = rep(state_name[j], 10),
                                     year = c(2015:2024),
                                     forecast = exp(pred_auto$mean),
                                     model = pred_auto$method)
  
  state_forecast_holt$forecast = as.numeric(state_forecast_holt$forecast)
  
  if (j == 1) {
    state_forecast_holt_final <- state_forecast_holt
  } else {
    state_forecast_holt_final <- rbind(state_forecast_holt_final, state_forecast_holt)
  }
}

fwrite(state_forecast_holt_final,
       "E:\\BI_and_Analytics\\1_UC\\Capstone_Project\\Final_Files\\forecasting_all_states_holt.csv")

#state_data_filtered = data %>% filter(State %in% "Andhra Pradesh")
#state_data_ts1 <- ts(state_data_filtered$NominalGSDP, frequency = 1)
#ts_log = log(state_data_ts1)

#hol = holt(log(state_data_ts1),h=10)
#plot(hol)
#summary(hol)
#accuracy(hol)
#checkresiduals(hol) #if p<0.05, data exhibits serial autocorrelation and has seasonality
