########################
##### Data Loading #####
########################

df_Bidet <- read.csv("data/feedBidet.csv")
df_Kitchenfaucet <- read.csv("data/feedKitchenfaucet.csv")
df_Shower <- read.csv("data/feedShower.csv")
df_Washbasin <- read.csv("data/feedWashbasin.csv")
df_Washingmachine <- read.csv("data/feedWashingmachine.csv")
df_AggregatedWholeHouse <- read.csv("data/aggregatedWholeHouse.csv")
df_Dishwasher <- read.csv("data/feedDishwasher.csv")
df_Toilet <- read.csv("data/feedToilet.csv")


########################
### Data Exploration ###
########################

# Check for NA values in df_Bidet
na_values_df_Bidet <- sum(is.na(df_Bidet))

# Check for NA values in df_Kitchenfaucet
na_values_df_Kitchenfaucet <- sum(is.na(df_Kitchenfaucet))

# Check for NA values in df_Shower
na_values_df_Shower <- sum(is.na(df_Shower))

# Check for NA values in df_Washbasin
na_values_df_Washbasin <- sum(is.na(df_Washbasin))

# Check for NA values in df_Washingmachine
na_values_df_Washingmachine <- sum(is.na(df_Washingmachine))

# Check for NA values in df_AggregatedWholeHouse
na_values_df_AggregatedWholeHouse <- sum(is.na(df_AggregatedWholeHouse))

# Check for NA values in df_Dishwasher
na_values_df_Dishwasher <- sum(is.na(df_Dishwasher))

# Check for NA values in df_Toilet
na_values_df_Toilet <- sum(is.na(df_Toilet))

################################################################################


##########################
### Data Preprocessing ###
##########################

# Linear Interpolation
fill_missing_val_pipeline <- function(df, time_col_name = "Time", flow_col_name = "Flow") {
  # Assuming 'Time' is already sorted
  time_diff <- c(NA, diff(df[[time_col_name]]))
  missing_indices <- which(time_diff == 2)
  
  if (length(missing_indices) > 0) {
    new_rows <- data.frame(
      setNames(data.frame(df[[time_col_name]][missing_indices] - 1), time_col_name),
      setNames(data.frame((df[[flow_col_name]][missing_indices] + df[[flow_col_name]][missing_indices - 1]) / 2), flow_col_name)
    )
    
    # Combine original df with new_rows and sort
    df <- rbind(df, new_rows) %>%
      arrange(!!sym(time_col_name))
  }
  
  return(df)
}

# Apply the function to each dataset, specifying column names for df_AggregatedWholeHouse
df_Bidet <- fill_missing_val_pipeline(df_Bidet)
message("Bidet preprocessed")

df_Kitchenfaucet <- fill_missing_val_pipeline(df_Kitchenfaucet)
message("Kitchen Faucet preprocessed")

df_Shower <- fill_missing_val_pipeline(df_Shower)
message("Shower preprocessed")

df_Washbasin <- fill_missing_val_pipeline(df_Washbasin)
message("Washbasin preprocessed")

df_Dishwasher <- fill_missing_val_pipeline(df_Dishwasher)
message("Dishwasher preprocessed")

# For df_AggregatedWholeHouse with 'unix' and 'flow' columns
df_AggregatedWholeHouse <- fill_missing_val_pipeline(df_AggregatedWholeHouse, "unix", "flow")
message("Whole House preprocessed")

################################################################################

# Function to prepare datasets, convert Unix time, extract features, and reorder columns
prepare_dataset <- function(df, has_end_column = FALSE) {
  # Conditionally remove EndTime or EndFlow if present
  if (has_end_column) {
    df <- select(df, -matches("End(Time|Flow)$"))
  }
  
  # Perform mutations
  df <- df %>%
    mutate(
      DateTime = as.POSIXct(Time, origin = "1970-01-01", tz = "UTC"),
      Hour_of_Day = hour(DateTime),
      Day_of_Week = wday(DateTime), 
      Month = month(DateTime), 
      Season = case_when(
        Month %in% 3:5 ~ "Spring",
        Month %in% 6:8 ~ "Summer",
        Month %in% 9:11 ~ "Autumn",
        TRUE ~ "Winter"
      )
    ) %>%
    select(-Time) # Remove original Time column here
  
  # Ensure Flow is the last column by rearranging column names
  cols_except_flow <- setdiff(names(df), "Flow")
  df <- df %>%
    select(all_of(cols_except_flow), Flow)
  
  return(df)
}

# List of datasets without EndTime or EndFlow columns
initial_datasets <- list(
  df_Bidet = df_Bidet,
  df_Kitchenfaucet = df_Kitchenfaucet,
  df_Shower = df_Shower,
  df_Washbasin = df_Washbasin,
  df_Washingmachine = df_Washingmachine
)

# Process datasets
processed_initial_datasets <- lapply(initial_datasets, prepare_dataset)


# List of datasets with EndTime or EndFlow columns
new_datasets <- list(
  df_Dishwasher = df_Dishwasher,
  df_Toilet = df_Toilet
)

# Process datasets indicating they have EndTime/EndFlow columns
processed_new_datasets <- lapply(new_datasets, function(x) prepare_dataset(x, TRUE))

# Extracting processed datasets into new variables
final_df_Bidet <- processed_initial_datasets$df_Bidet
final_df_Kitchenfaucet <- processed_initial_datasets$df_Kitchenfaucet
final_df_Shower <- processed_initial_datasets$df_Shower
final_df_Washbasin <- processed_initial_datasets$df_Washbasin
final_df_Washingmachine <- processed_initial_datasets$df_Washingmachine
final_df_Dishwasher <- processed_new_datasets$df_Dishwasher
final_df_Toilet <- processed_new_datasets$df_Toilet


# Adjusted function for df_aggregatedWholeHouse with 'unix' and 'flow' columns, renaming 'flow' to 'Flow'
prepare_aggregatedWholeHouse <- function(df) {
  df %>%
    rename(Flow = flow) %>%
    mutate(
      DateTime = as.POSIXct(unix, origin = "1970-01-01", tz = "UTC"),
      Hour_of_Day = hour(DateTime),
      Day_of_Week = wday(DateTime), 
      Month = month(DateTime), 
      Season = case_when(
        Month %in% 3:5 ~ "Spring",
        Month %in% 6:8 ~ "Summer",
        Month %in% 9:11 ~ "Autumn",
        TRUE ~ "Winter"
      )
    ) %>%
    select(-unix, DateTime, Hour_of_Day, Day_of_Week, Month, Season) %>%
    mutate(Flow_at_End = Flow) %>%
    select(-Flow) %>%
    rename(Flow = Flow_at_End)
}

# Apply the adjusted function to df_aggregatedWholeHouse
final_df_AggregatedWholeHouse <- prepare_aggregatedWholeHouse(df_AggregatedWholeHouse)

################################################################################

# Define start date
start_date <- as.Date('2019-02-12')

# Filter Washbasin data
final_df_Washbasin <- final_df_Washbasin %>% 
  filter(DateTime >= start_date)

################################################################################

# Boxplot Preparation
# Clone and prepare each dataset
df_1Bidet <- final_df_Bidet %>% select(Flow) %>% mutate(Dataset = 'Bidet')
df_1KitchenFaucet <- final_df_Kitchenfaucet %>% select(Flow) %>% mutate(Dataset = 'KitchenFaucet')
df_1Shower <- final_df_Shower %>% select(Flow) %>% mutate(Dataset = 'Shower')
df_1Washbasin <- final_df_Washbasin %>% select(Flow) %>% mutate(Dataset = 'Washbasin')
df_1WashingMachine <- final_df_Washingmachine %>% select(Flow) %>% mutate(Dataset = 'WashingMachine')
df_1Dishwasher <- final_df_Dishwasher %>% select(Flow) %>% mutate(Dataset = 'Dishwasher')
df_1Toilet <- final_df_Toilet %>% select(Flow) %>% mutate(Dataset = 'Toilet')

# Combine the prepared dataframes
combined_df <- rbind(df_1Bidet, df_1KitchenFaucet, df_1Shower, 
                     df_1Washbasin, df_1WashingMachine, df_1Dishwasher, 
                     df_1Toilet)

# Visualize with a boxplot
boxplot_outliers <- ggplot(combined_df, aes(x = Dataset, y = Flow, fill = Dataset)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Dataset", y = "Flow")


################################################################################


#################################
### Exploratory Data Analysis ###
#################################

####### Total flow in each dataset  #####
bar_df <- combined_df %>%
  group_by(Dataset) %>%
  summarise(total_flow = sum(Flow, na.rm = TRUE)) 

barplot_appliances <- 
  ggplot(bar_df, aes(x = Dataset, y = total_flow, fill = Dataset)) +
  geom_bar(stat = "identity") +
  labs(title = "Total flow", x = "Dataset", y = "Flow") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(face="bold", size = 18))

####### Mean flow in each dataset  #####

bar_df1 <- combined_df %>%
  group_by(Dataset) %>%
  summarise(mean_flow = mean(Flow, na.rm = TRUE))

barplot_appliances1 <- 
  ggplot(bar_df1, aes(x = Dataset, y = mean_flow, fill = Dataset)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean flow", x = "Dataset", y = "Flow") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 20), 
        axis.text = element_text(size = 18), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title = element_text(size = 18), 
        legend.text = element_text(size = 18), 
        legend.title = element_text(face="bold", size = 18))


# Define the datasets
datasets <- list(
  AggregatedWholeHouse = final_df_AggregatedWholeHouse,
  Bidet = final_df_Bidet,
  Kitchenfaucet = final_df_Kitchenfaucet,
  Shower = final_df_Shower,
  Washbasin = final_df_Washbasin,
  Washingmachine = final_df_Washingmachine,
  Dishwasher = final_df_Dishwasher,
  Toilet = final_df_Toilet
)

# Function to summarize data
summarize_flow <- function(df, by_vars) {
  df %>%
    group_by(across(all_of(by_vars))) %>%
    summarise(Mean_Flow = mean(Flow, na.rm = TRUE), .groups = 'drop')
}

#################### Flow for Day of week###############
# Loop through datasets to create summaries by day of the week and season
for (name in names(datasets)) {
  summary_name_day_season <- paste0("df_summary_", name)
  assign(summary_name_day_season, summarize_flow(datasets[[name]], c("Day_of_Week", "Season")))
}

# Flow_Week Plot
combined_summary <- bind_rows(
  df_summary_AggregatedWholeHouse %>% mutate(Category = "Aggregated Whole House"),
  df_summary_Bidet %>% mutate(Category = "Bidet"),
  df_summary_Kitchenfaucet %>% mutate(Category = "Kitchen Faucet"),
  df_summary_Shower %>% mutate(Category = "Shower"),
  df_summary_Washbasin %>% mutate(Category = "Washbasin"),
  df_summary_Washingmachine %>% mutate(Category = "Washing Machine"),
  df_summary_Dishwasher %>% mutate(Category = "Dishwasher"),
  df_summary_Toilet %>% mutate(Category = "Toilet")
)

flow_week <- ggplot(combined_summary, aes(x = Day_of_Week, y = Mean_Flow, fill = Season)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  facet_wrap(~ Category, scales = "free_y") +
  labs(x = "Day of the Week",
       y = "Mean Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines")) +
  scale_fill_brewer(palette = "Paired")

flow_week1 <- ggplot(combined_summary, aes(x = Day_of_Week, y = Mean_Flow, fill = Day_of_Week)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Category, scales = "free_y") +
  labs(x = "Day of the Week",
       y = "Mean Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines"))


#################### Flow for Hour of day###############
# Loop through datasets to create summaries by hour of the day and season
for (name in names(datasets)) {
  summary_name_hour_season <- paste0("df_summary_", name, "1")
  assign(summary_name_hour_season, summarize_flow(datasets[[name]], c("Hour_of_Day", "Season")))
}

# Flow Hour Plot
combined_hourly_summary <- bind_rows(
  df_summary_AggregatedWholeHouse1 %>% mutate(Category = "Aggregated Whole House"),
  df_summary_Bidet1 %>% mutate(Category = "Bidet"),
  df_summary_Kitchenfaucet1 %>% mutate(Category = "Kitchen Faucet"),
  df_summary_Shower1 %>% mutate(Category = "Shower"),
  df_summary_Washbasin1 %>% mutate(Category = "Washbasin"),
  df_summary_Washingmachine1 %>% mutate(Category = "Washing Machine"),
  df_summary_Dishwasher1 %>% mutate(Category = "Dishwasher"),
  df_summary_Toilet1 %>% mutate(Category = "Toilet")
)

flow_hour <- ggplot(combined_hourly_summary, aes(x = Hour_of_Day, y = Mean_Flow, fill = Season)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1.2)) +
  facet_wrap(~ Category, scales = "free_y", nrow = 4) +
  labs(x = "Hour of the Day",
       y = "Mean Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines")) +
  scale_fill_brewer(palette = "Paired")

flow_hour1 <- ggplot(combined_hourly_summary, aes(x = Hour_of_Day, y = Mean_Flow, fill = Hour_of_Day)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Category, scales = "free_y", nrow = 4) +
  labs(x = "Hour of the Day",
       y = "Mean Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines"))


#################### Flow for each month###############
# Loop through datasets to create summaries by hour of the day and season
for (name in names(datasets)) {
  summary_month <- paste0("df_summary_", name, "2")
  assign(summary_month, summarize_flow(datasets[[name]], c("Month", "Season")))
}

# Flow_Month Plot
combined_summary_month <- bind_rows(
  df_summary_AggregatedWholeHouse2 %>% mutate(Category = "Aggregated Whole House"),
  df_summary_Bidet2 %>% mutate(Category = "Bidet"),
  df_summary_Kitchenfaucet2 %>% mutate(Category = "Kitchen Faucet"),
  df_summary_Shower2 %>% mutate(Category = "Shower"),
  df_summary_Washbasin2 %>% mutate(Category = "Washbasin"),
  df_summary_Washingmachine2 %>% mutate(Category = "Washing Machine"),
  df_summary_Dishwasher2 %>% mutate(Category = "Dishwasher"),
  df_summary_Toilet2 %>% mutate(Category = "Toilet")
)

flow_month <- ggplot(combined_summary_month, aes(x = Month, y = Mean_Flow, fill = Season)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  facet_wrap(~ Category, scales = "free_y") +
  labs(x = "Month",
       y = "Mean Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines")) +
  scale_fill_brewer(palette = "Set1")


###################### Top 100 flows for each dataset###########
rank_flow <- function(df) {
  df <- transform(df, rank = rank(-Flow))
}

# for (name in names(datasets)) {
#   summary_month <- paste0("df_topn_", name)
#   assign(summary_month, rank_flow(datasets[[name]]))
# }

top_n <- function(df, col_name, top_n) {
  top_n_rows <- df %>%
    arrange(desc(df[[col_name]])) %>%
    slice_head(n = top_n)
  
  return(top_n_rows)
}

# for (name in names(datasets)) {
#   summary_month <- paste0("df_topn_", name)
#   assign(summary_month, rank_flow(datasets[[name]]))
#   assign(summary_month, top_n(datasets[[name]], c("Flow"), 500))
# }

for (name in names(datasets)) {
  topn_df <- top_n(datasets[[name]], "Flow", 500)
  rank_df <- rank_flow(datasets[[name]])
  
  combined_df <- merge(rank_df, topn_df, by = c("DateTime", "Hour_of_Day", "Day_of_Week", "Month", "Season", "Flow"))
  assign(paste0("df_topn_", name), combined_df)
}


combined_summary_topn <- bind_rows(
  df_topn_AggregatedWholeHouse %>% mutate(Category = "Aggregated Whole House"),
  df_topn_Bidet %>% mutate(Category = "Bidet"),
  df_topn_Kitchenfaucet %>% mutate(Category = "Kitchen Faucet"),
  df_topn_Shower %>% mutate(Category = "Shower"),
  df_topn_Washbasin %>% mutate(Category = "Washbasin"),
  df_topn_Washingmachine %>% mutate(Category = "Washing Machine"),
  df_topn_Dishwasher %>% mutate(Category = "Dishwasher"),
  df_topn_Toilet %>% mutate(Category = "Toilet")
)


top_n_plot <- ggplot(combined_summary_topn, aes(x = DateTime, y = Flow, color = rank)) +
  geom_point() +
  facet_wrap(~ Category, scales = "free_y") +
  labs(x = "Date",
       y = "Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines")) +
  scale_color_gradient(low = "red", high = "yellow")


top_n_plot_month <- ggplot(combined_summary_topn, aes(x = Month, y = Flow, color = rank)) +
  geom_point() +
  facet_wrap(~ Category, scales = "free_y") +
  labs(x = "Date",
       y = "Flow (L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(face="bold", size = 14, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "lightblue"),
        panel.spacing = unit(1, "lines")) +
  scale_color_gradient(low = "red", high = "yellow")