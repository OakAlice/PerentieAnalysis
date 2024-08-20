# Plotting the features

# plot the training_data
data <- other_data %>%
  group_by(Activity) %>%
  slice_head(n = 10000) %>%
  ungroup()
data_processed <- process_data(na.omit(data), features_list, window_length = 3, 
                                        overlap_percent = 0, freq_Hz = 20, 
                                        feature_normalisation = "Standardised")

# Reshape data from wide to long format
data_long <- data_processed %>%
  pivot_longer(
    cols = starts_with("mean_") | starts_with("max_") | starts_with("min_") | starts_with("sd_") | starts_with("entropy_") | starts_with("auto_") | starts_with("SMA") | starts_with("minODBA") | starts_with("maxODBA") | starts_with("minVDBA") | starts_with("maxVDBA") | starts_with("cor_"),
    names_to = "Feature",
    values_to = "Value"
  )

# Create the plot
ggplot(data_long, aes(x = Value, fill = Activity)) +
  geom_histogram(position = "identity", alpha = 0.6, binwidth = 0.1) +
  facet_wrap(~ Feature, scales = "free_x", ncol = 4) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.title.x = element_blank(), # Remove x-axis title
    axis.title.y = element_blank(), # Remove y-axis title
    axis.ticks = element_blank()    # Remove axis ticks
  ) +
  labs(
    fill = "Activity"
  )



# plot how the behaviours change over time
data2 <- other_data %>% arrange(ID, time) %>% slice_head(n = 10000) %>% ungroup() %>%
  mutate(numeric_activity = as.numeric(factor(Activity)), 
         Activity = factor(Activity),
         relative_seconds = row_number())

ggplot(data2, aes(x = (relative_seconds/20), y = as.numeric(numeric_activity))) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Time (seconds)",
    y = "Activity",
    color = "Activity"
  ) +
  scale_y_continuous(
    breaks = unique(data2$numeric_activity),
    labels = levels(data2$Activity)
  )

# average duration of each behaviour before it changes
# will create a negative number when the ID changes. Therefore I just remove all of those rows
sample_rate <- 20
summary <- other_data %>%
  arrange(ID, time) %>%
  group_by(ID) %>%
  mutate(
    # Detect behavior change
    behavior_change = lag(Activity) != Activity,
    behavior_change = ifelse(is.na(behavior_change), TRUE, behavior_change),  # Set NA for the first row of each ID
    row = row_number()
  ) %>%
  ungroup() %>%
  filter(behavior_change) %>%
  mutate(
    duration_samples = row - lag(row, default = 0),
    duration_seconds = duration_samples / sample_rate
  ) %>%
  filter(duration_seconds >= 0) # %>%
#  group_by(Activity) %>%
#  summarise(
#    average_duration = mean(duration_seconds, na.rm = TRUE),
#    variation_duration = sd(duration_seconds, na.rm = TRUE),
#    count = n()
#  )

# plot that
ggplot(summary, aes(x = Activity, y = duration_seconds)) +
  geom_boxplot(aes(color = Activity), outlier.shape = NA) +  # Use color to distinguish activities
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Activity",
    y = "Duration (seconds)"
  ) +
  scale_y_continuous(
    limits = c(min(summary$duration_seconds, na.rm = TRUE), max(summary$duration_seconds, na.rm = TRUE)),  # Set y-axis limits
    breaks = seq(0, max(summary$duration_seconds, na.rm = TRUE), by = 160)  # Adjust the step size as needed
  )



# look at the trace shapes
beh_trace_plot <- plot_behaviours(behaviours = unique(other_data$Activity), data = other_data, n_samples = 200, n_col = 2)
