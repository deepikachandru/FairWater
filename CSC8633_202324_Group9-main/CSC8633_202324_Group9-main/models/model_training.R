# Function to train and save the models
train_and_save_models <- function(dataset_name) {
  train_set <- readRDS(paste0("data/train_set_", dataset_name, ".rds"))
  
  # Train Linear Regression Model
  lm_model <- lm(Flow ~ Hour_of_Day + Day_of_Week + Month + as.factor(Season), data = train_set)
  saveRDS(lm_model, paste0("models/lm_model_", dataset_name, ".rds"))
  
  # Train ARIMA Model
  ts_data <- ts(train_set$Flow, frequency = 24) 
  arima_model <- auto.arima(ts_data)
  saveRDS(arima_model, paste0("models/arima_model_", dataset_name, ".rds"))
}

train_and_save_models("Kitchenfaucet")
train_and_save_models("Washbasin")
train_and_save_models("Shower")
train_and_save_models("AggregatedWholeHouse")
