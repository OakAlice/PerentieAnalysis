# selecting the minimum number of individuals to fulfill the threshold per behaviour
identify_minimum_individuals <- function(dat, threshold) {
  
  Inds_per_act <- data.frame()
  
  for (activity in unique(dat$Activity)){
    activitydat <- dat %>% filter(Activity == activity)
    
    # Count the number of samples per individual per activity
    individual_counts <- activitydat %>%
      group_by(Activity, ID) %>%
      tally() %>%
      arrange(desc(n)) %>%
      mutate(Size = row_number()) %>%
      mutate(cumulative_sum = cumsum(n)) %>%
      filter(cumulative_sum >= threshold) %>%
      head(n=1)
    
    if (nrow(individual_counts) == 0) {
      print(paste0("insufficient data for ", activity))
      specific_individuals <- activitydat$ID
    } else {
      specific_individuals <- activitydat %>%
        group_by(Activity, ID) %>%
        tally() %>%
        arrange(desc(n)) %>%
        mutate(Size = row_number()) %>%
        filter(Size <= individual_counts$Size) %>%
        pull(ID)
    }
    
    specific_individuals <- toString(specific_individuals)
    
    inds_for_beh <- data.frame(activity = activity, individuals = specific_individuals)
    Inds_per_act <- rbind(Inds_per_act, inds_for_beh)
  }
  return(Inds_per_act)
}



# version 2
minimize_total_individuals <- function(dat, threshold) {
  # Count the number of samples per individual per behavior
  individual_counts <- dat %>%
    group_by(Activity, ID) %>%
    tally() %>%
    arrange(Activity, desc(n))
  
  selected_individuals <- data.frame()
  
  # Loop through each behavior to select individuals
  for(activity in unique(dat$Activity)) {
    activity_data <- individual_counts %>%
      filter(Activity == activity)
    
    cumulative_sum <- 0
    ids <- c()
    
    for(i in 1:nrow(activity_data)) {
      if (cumulative_sum >= threshold) break
      cumulative_sum <- cumulative_sum + activity_data$n[i]
      ids <- c(ids, activity_data$ID[i])
    }
    
    selected_individuals <- bind_rows(selected_individuals, data.frame(Activity = activity, ID = ids, cumulative_sum = cumulative_sum))
  }
  
  # Filter out duplicates and minimize the total number of individuals across behaviors
  minimized_individuals <- selected_individuals %>%
    distinct(ID, .keep_all = TRUE) %>%
    arrange(Activity, ID)
  
  return(minimized_individuals)
}







# Balancing within the individuals
balance_ID_data <- function(dat, threshold) {
  # Determine counts of each 'activity' and identify over-represented behaviors
  activity_counts <- dat %>%
    group_by(Activity) %>%
    tally() %>%
    mutate(max_samples = pmin(n, threshold))
  
  balanced_data <- data.frame()
  
  # Loop through each activity to sample data
  for(activity in unique(dat$Activity)) {
    activity_data <- dat %>% filter(Activity == activity)
    max_samples <- activity_counts %>% filter(Activity == activity) %>% pull(max_samples)
    num_individuals <- length(unique(activity_data$ID))
    samples_per_individual <- floor(max_samples / num_individuals)
    
    # Sample initial data
    sampled_activity_data <- activity_data %>%
      group_by(ID) %>%
      mutate(num_samples = n()) %>%
      ungroup() %>%
      mutate(samples_needed = pmin(samples_per_individual, num_samples)) %>%
      arrange(ID, time) %>%
      group_by(ID) %>%
      slice(1:first(samples_needed)) %>%
      ungroup()
    
    # Calculate the total samples collected and the deficit
    total_samples_collected <- nrow(sampled_activity_data)
    deficit <- max_samples - total_samples_collected
    
    # Distribute the deficit among individuals with sufficient samples
    if(deficit > 0) {
      remaining_samples <- activity_data %>%
        anti_join(sampled_activity_data, by = c("ID", "time")) %>%
        sample_n(deficit, replace = TRUE)
      sampled_activity_data <- bind_rows(sampled_activity_data, remaining_samples)
    }
    
    balanced_data <- bind_rows(balanced_data, sampled_activity_data)
  }
  
  return(balanced_data)
}
