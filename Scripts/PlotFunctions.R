# all the plots I need for data exploration

# set colours
generate_random_colors <- function(n) {
  colors <- rgb(runif(n), runif(n), runif(n))
  return(colors)
}
set.seed(123)



# total volume by Activity and ID
explore_data <- function(data, frequency, colours) {
  my_colours <- generate_random_colors(colours)
  # summarise into a table
  labelledDataSummary <- data %>%
    #filter(!Activity %in% ignore_behaviours) %>%
    count(ID, Activity)
  
  # account for the HZ, convert to minutes
  labelledDataSummaryplot <- labelledDataSummary %>%
    mutate(minutes = (n/frequency)/60)
  
  # Plot the stacked bar graph
  behaviourIndividualDistribution <- ggplot(labelledDataSummaryplot, aes(x = Activity, y = minutes, fill = as.factor(ID))) +
    geom_bar(stat = "identity") +
    labs(x = "Activity",
         y = "minutes") +
    theme_minimal() +
    scale_fill_manual(values = my_colours) +
    theme(axis.line = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(color = "black", fill = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(behaviourIndividualDistribution)
}



# PART TWO: DISPLAYING SAMPLES OF EACH TRACE TYPE ####
plot_behaviours <- function(behaviours, data, n_samples, n_col) {
  # Function to create the plot for each behavior
  plot_behaviour <- function(behaviour, n_samples) {
    df <- data %>%
      filter(Activity == behaviour) %>%
      group_by(ID, Activity) %>%
      slice(1:n_samples) %>%
      mutate(relative_time = row_number())
    
    
    ggplot(df, aes(x = relative_time)) +
      geom_line(aes(y = Accelerometer.X, color = "X"), show.legend = FALSE) +
      geom_line(aes(y = Accelerometer.Y, color = "Y"), show.legend = FALSE) +
      geom_line(aes(y = Accelerometer.Z, color = "Z"), show.legend = FALSE) +
      labs(title = paste(behaviour),
           x = NULL, y = NULL) +
      scale_color_manual(values = c(X = "salmon", Y = "turquoise", Z = "darkblue"), guide = "none") +
      facet_wrap(~ ID, nrow = 1, scales = "free_x") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank())
  }
  
  # Create plots for each behavior (with error catching)
  plots <- purrr::map(behaviours, function(behaviour) {
    tryCatch(
      {
        plot_behaviour(behaviour, n_samples)
      },
      error = function(e) {
        message("Skipping plot for ", behaviour, ": ", e$message)
        NULL  # Return NULL to indicate skipping
      }
    )
  })
  
  # Combine plots into a single grid
  # plots <- plots[1:13]
  grid_plot <- cowplot::plot_grid(plotlist = plots, ncol = n_col)
  
  return(grid_plot)
}
