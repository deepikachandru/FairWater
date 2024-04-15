# Function to split and save the train and test datasets for models
split_and_save_data <- function(df, dataset_name) {
  cutoff_date <- max(df$DateTime) %m-% months(2)
  
  train_set <- df %>% filter(DateTime <= cutoff_date)
  test_set <- df %>% filter(DateTime > cutoff_date)
  
  # Save the splits
  saveRDS(train_set, paste0("data/train_set_", dataset_name, ".rds"))
  saveRDS(test_set, paste0("data/test_set_", dataset_name, ".rds"))
}

split_and_save_data(final_df_Kitchenfaucet, "Kitchenfaucet")
split_and_save_data(final_df_Shower, "Shower")
split_and_save_data(final_df_Washbasin, "Washbasin")
split_and_save_data(final_df_AggregatedWholeHouse, "AggregatedWholeHouse")