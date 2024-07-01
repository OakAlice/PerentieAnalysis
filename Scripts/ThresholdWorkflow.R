# Threshold method

library(ggplot2)
library(gridExtra)

# define varaibles
window_length <- 3
overlap_percent <- 0
down_Hz <- 50
feature_normalisation <- FALSE
features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", 
"entropy", "zero", "auto")

example_data <- fread(file.path(base_path, "DiCicco_Perentie_Labelled.csv")) %>% 
  filter(!Activity == "NaN") %>%
  filter(ID == "Eric")

example_data_processed <- example_data %>%
  process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
  na.omit() %>%
  select(-Timestamp)

# plot the VDBA for each of the behaviours and Individuals
# Create a list to store individual plots
plot_list <- list()

# Iterate over each ID and create a jitter plot
for (id in unique(example_data_processed$ID)) {
  # Subset data for the current ID
  plot_data <- example_data_processed %>%
    filter(ID == id)
  
  # Create jitter plot for each variable
  jitterplots <- lapply(names(plot_data)[1:24], function(var) {
    ggplot(plot_data, aes(x = Activity, y = !!sym(var), color = Activity)) +
      geom_jitter(position = position_jitter(width = 0.1)) +
      labs(title = paste(id, "-", var)) +
      theme_minimal()
  })
  
  # Combine jitter plots for the current ID into a single plot
  combined_plot <- grid.arrange(grobs = jitterplots, ncol = 4)
  
  # Add the combined plot to the list
  plot_list[[id]] <- combined_plot
}

# Arrange all plots in a grid
final_plot <- grid.arrange(grobs = plot_list, ncol = 1)

# Display the final plot
final_plot
