# Anomaly Detection Model Tuning
# run the algorithm

# Function for iterating through parameters, saving metrics for each iteration
model_tuning <- function(options_df, base_path, training_data, evaluation_data, targetActivity) {
  
  # Initialize an empty list to store the results
  all_results <- list()
  
  # Get the start time
  start_time <- Sys.time()
  
  lapply(1:nrow(options_df), function(i) {
    try({
      # Start timing this iteration
      iteration_start_time <- Sys.time()
      
      # Define the variables for this loop
      window_length <- as.numeric(options_df[i, "window_length"])
      overlap_percent <- as.numeric(options_df[i, "overlap_percent"])
      down_Hz <- as.numeric(options_df[i, "frequency_Hz"])
      feature_normalisation <- as.character(options_df[i, "feature_normalisation"])
      nu <- as.numeric(options_df[i, "nu"])
      kernel_shape <- as.character(options_df[i, "kernel"])
      
      print(paste("beginning model training at:", Sys.time()))
      
          # Build the SVM
          single_class_SVM <- build_1_class_SVM(
            training_data,
            features_list = features_list,
            window_length,
            overlap_percent,
            down_Hz,
            feature_normalisation,
            nu,
            kernel_shape
          )
          
          # Process and evaluate the data
          evaluation_data_processed <- evaluation_data %>%
            process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
            na.omit()
          
          print(paste("evaluation data processed at:", Sys.time()))
          
          model_evaluation <- evaluate_model_performance(evaluation_data_processed, single_class_SVM, validation_type = "Validation", targetActivity)
          metrics <- model_evaluation$metrics
        
      print(paste("metrics calculated at:", Sys.time()))
      
      # Combine the results into a dataframe
      results_sheet <- data.frame(
        targetActivity = targetActivity,
        window_length = window_length,
        overlap_percent = overlap_percent,
        down_Hz = down_Hz,
        feature_normalisation = feature_normalisation,
        nu = nu,
        kernel = kernel_shape,
        t(metrics)
      )
      
      # Write the results to a CSV file iteratively
      fwrite(results_sheet, file.path(base_path, paste(targetActivity, "tuning_metrics.csv", sep = "_")), append = TRUE)
      
      print(paste("results written at:", Sys.time()))
      
      # Store the results in the list
      all_results[[i]] <- results_sheet
      
      # Print progress
      iteration_end_time <- Sys.time()
      iteration_time <- round(difftime(iteration_end_time, iteration_start_time, units = "secs"), 2)
      total_time <- round(difftime(iteration_end_time, start_time, units = "mins"), 2)
      cat(sprintf("Completed iteration %d of %d in %s seconds. Total time elapsed: %s minutes.\n", 
                  i, nrow(options_df), iteration_time, total_time))
      
      
    }, silent = TRUE) # Skip any iterations that cause errors
  })
  
  # Combine all results into a single dataframe
  model_tuning_metrics <- do.call(rbind, all_results)
  
  return(as.data.frame(model_tuning_metrics))
}

# applying optimal metrics to final hold-out test data
model_testing <- function(optimal_df_row, base_path, training_data, evaluation_data, targetActivity){
  
  # define the variables
  window_length = as.numeric(optimal_df_row["window_length"])
  overlap_percent = as.numeric(optimal_df_row["overlap_percent"])
  down_Hz = as.numeric(optimal_df_row["down_Hz"])
  feature_normalisation = as.character(optimal_df_row["feature_normalisation"])
  nu = as.numeric(optimal_df_row["nu"])
  kernel = as.character(optimal_df_row["kernel"])
  
  # build the SVM
  single_class_SVM <- build_1_class_SVM(
    training_data,
    features_list,
    window_length,
    overlap_percent,
    down_Hz,
    feature_normalisation,
    nu,
    kernel
  )
  
  # save this svm
  saveRDS(single_class_SVM, file.path(base_path, paste0(targetActivity, "_2.rds")))
  
  # evaluate performance
  evaluation_data <- evaluation_data %>%
    process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
    na.omit()
  
  # downsample the data to balance classes
  activity_counts <- evaluation_data %>%
    count(Activity)
  
  # Find the minimum count (to determine the size of the smallest group after downsampling)
  min_count <- min(activity_counts$n)
  
  # Downsample each 'Activity' group to have the same number of observations as 'min_count'
  downsampled_data <- evaluation_data %>%
    group_by(Activity) %>%
    sample_n(min_count) %>%  # Sample 'min_count' observations from each group
    ungroup()
  
  model_evaluation <- evaluate_model_performance(downsampled_data, single_class_SVM, "Test", targetActivity)
  
  model_evaluation_metrics <- cbind(
    targetActivity,
    window_length,
    overlap_percent,
    down_Hz,
    feature_normalisation,
    nu,
    kernel,
    t(model_evaluation$metrics)
  )
  
  return(model_evaluation_metrics)
}

# function for processing training data and building a single-class SVM
build_1_class_SVM <- function(
    training_data,
    features_list,
    window_length,
    overlap_percent,
    down_Hz,
    feature_normalisation,
    nu,
    kernel_shape
){
  
  # process the training and validation data
  training_data_processed <- process_data(na.omit(training_data), features_list, window_length = window_length, 
                                          overlap_percent = overlap_percent, down_Hz = down_Hz, 
                                          feature_normalisation = feature_normalisation)
  
  print(paste("training data processed at:", Sys.time()))
  
  # separate the training data into predictors and labels 
  training_data_predictors <- training_data_processed %>% select(-Activity)
  training_data_labels <- training_data_processed %>% select(Activity)

  # train a model with the training predictors (no labels)
  single_class_SVM <- svm(training_data_predictors, 
                          y = NULL,  # No response variable for one-class SVM
                          type = 'one-classification',
                          nu = nu,
                          scale = TRUE,
                          kernel = kernel_shape)
  
  print(paste("model trained at:", Sys.time()))
  
  return (single_class_SVM)
}

# stand alone function to evlauate SVM model performance
evaluate_model_performance <- function(
    processed_data, 
    single_class_SVM,
    validation_type,
    targetActivity
){
  
  # Separate predictors and labels
  data_predictors <- processed_data %>% select(-Activity)
  data_labels <- processed_data$Activity
  
  # Make predictions on the test data
  predicted <- predict(single_class_SVM, newdata = data_predictors)
  
  predicted <- ifelse(predicted == "TRUE", "Normal", "Outlier")
  reference <- ifelse(data_labels == targetActivity, "Normal", "Outlier")
  
  # Compute performance metrics
  #compare_performance <- as.data.frame(cbind(predicted, reference)) %>%
  #  mutate(result = ifelse(predicted == reference, "Correct", "Incorrect"))
  
  performance <- table(Predicted = predicted, Reference = reference)
  metrics <- flatten_confusion_matrix(performance, validation_type)
  
  return(list(predicted = predicted, 
              reference = reference,
              metrics = metrics))
}

# function to turn matrices into rows
flatten_confusion_matrix <- function(conf_matrix, prefix) {
  if (nrow(conf_matrix) == 2 && ncol(conf_matrix) == 2) { # validation
    c(TP = conf_matrix[1, 1],
      FP = conf_matrix[1, 2],
      FN = conf_matrix[2, 1],
      TN = conf_matrix[2, 2],
      accuracy = round((conf_matrix[1, 1]+conf_matrix[2, 2])/(conf_matrix[1, 1]+conf_matrix[2, 2]+conf_matrix[1, 2]+conf_matrix[2, 1]),2)) %>%
      setNames(paste(prefix, c("TP", "FP", "FN", "TN", "accuracy"), sep = "_"))
  } else if (nrow(conf_matrix) == 2 && ncol(conf_matrix) == 1) { # training
    c(TP = conf_matrix[1, 1],
      FN = conf_matrix[2, 1],
      FP = NA,
      TN = NA,
      accuracy = round((conf_matrix[1, 1]/(conf_matrix[1, 1]+conf_matrix[2, 1])),2)) %>%
      setNames(paste(prefix, c("TP", "FP", "FN", "TN", "accuracy"), sep = "_"))
  } else if (nrow(conf_matrix) == 1 && ncol(conf_matrix) == 2){ # validation extra
    if (row.names(conf_matrix) == "Outlier"){ # only outliers predicted
      c(FN = conf_matrix[1, 1],
        TN = conf_matrix[1, 2],
        FP = NA,
        TP = NA,
        accuracy = round((conf_matrix[1, 2]/(conf_matrix[1, 1]+conf_matrix[1, 2])),2)) %>%
        setNames(paste(prefix, c("TP", "FP", "FN", "TN", "accuracy"), sep = "_"))
    } else if (row.names(conf_matrix) == "Normal"){ # only normal predicted
      c(TP = conf_matrix[1, 1],
        FP = conf_matrix[1, 2],
        TN = NA,
        FN = NA,
        accuracy = round((conf_matrix[1, 1]/(conf_matrix[1, 1]+conf_matrix[1, 2])),2)) %>%
        setNames(paste(prefix, c("TP", "FP", "FN", "TN", "accuracy"), sep = "_"))
      
    } else {
      stop("Confusion matrix is in unexpected format.")
    }
  } else {
    stop("Confusion matrix dimensions are not supported.")
  }
}

# function for creating the specific training, validation, and testing datastes
create_datasets <- function(data, targetActivity, validation_individuals) {
  
  # individuals
  training_individuals <- length(unique(data$ID)) - validation_individuals
  
  # training data from the 10 individuals
  data_training <- data[data$ID %in% unique(data$ID)[1:training_individuals], ]
  
  # data validation should be everything else
  data_validation <- anti_join(data, data_training)
  
  # now remove the non-target behaviours from the training data
  data_training <- data_training %>%
    filter(Activity == targetActivity) # Filter for target activity
  
  return(list(data_training = data_training,
              data_validation = data_validation))
}
