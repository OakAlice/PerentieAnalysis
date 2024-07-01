# Functions for anomaly detection

# Function for iterating through parameters, saving metrics for each
model_tuning <- function(options_df, base_path, training_data, evaluation_data, targetActivity){
  
  results_sheet <- lapply(1:nrow(options_df), function(i) {
    
    # define the variables for this loop
    window_length <- as.numeric(options_df[i, "window_length"])
    overlap_percent <- as.numeric(options_df[i, "overlap_percent"])
    down_Hz <- as.numeric(options_df[i, "down_Hz"])
    feature_normalisation <- as.character(options_df[i, "feature_normalisation"])
    nu <- as.numeric(options_df[i, "nu"])
    kernel <- as.character(options_df[i, "kernel"])
    
    # build the SVM
    single_class_SVM <- build_1_class_SVM(
      training_data,
      features_list = features_list,
      window_length,
      overlap_percent,
      down_Hz,
      feature_normalisation,
      nu,
      kernel
    )
    
    # evaluate performance
    evaluation_data <- evaluation_data %>%
      process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
      select(-c(zero_Accel_Y, zero_Accel_Z)) %>% 
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
    
    model_evaluation <- evaluate_model_performance(downsampled_data, single_class_SVM, "Validation", targetActivity)
    
    metrics <- model_evaluation$metrics
    
    results_sheet <- cbind(
      targetActivity,
      window_length,
      overlap_percent,
      down_Hz,
      feature_normalisation,
      nu,
      kernel,
      t(metrics)
    )
  })
  
  unsup_1_SVM_options <- do.call(rbind, results_sheet)
  model_tuning_metrics <- as.data.frame(unsup_1_SVM_options)
  
  return(model_tuning_metrics)
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
  saveRDS(single_class_SVM, file.path(base_path, paste0(targetActivity, ".rds")))
  
  # evaluate performance
  evaluation_data <- evaluation_data %>%
    process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
    select(-c(zero_Accel_Y, zero_Accel_Z))%>% 
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
    kernel
){
  
  # process the training and validation data
  training_data_processed <- process_data(na.omit(training_data), features_list, window_length = window_length, 
                                overlap_percent = overlap_percent, down_Hz = down_Hz, 
                                feature_normalisation = feature_normalisation)
  training_data_processed <- training_data_processed %>% 
            select(-c(zero_Accel_Y, zero_Accel_Z))
  
  # separate the training data into predictors and labels 
  training_data_predictors <- training_data_processed %>% select(-Activity)
  training_data_labels <- training_data_processed %>% select(Activity)
  
  # train a model with the training predictors (no labels)
  single_class_SVM <- svm(training_data_predictors, 
                          y = NULL,  # No response variable for one-class SVM
                          type = 'one-classification',
                          nu = nu,
                          scale = TRUE,
                          kernel = kernel)
  
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
create_datasets <- function(data, targetActivity) {
  
  # training data,  
  training_data <- data %>%
    filter(Activity == targetActivity) %>%  # Filter for target activity
    group_by(ID) %>%  # Group by individual ID
    arrange(Timestamp) %>%  # Ensure data is ordered chronologically
    mutate(partition = ntile(row_number(), 10)) %>%  # Create 10 partitions using cut
    filter(partition %in% sample(unique(partition), 6)) %>%  # Randomly select 6 partitions per individual
    ungroup() %>%
    select(-partition)
  
  validation_data <- anti_join(data, training_data)
  
  return(list(training_data = training_data,
              validation_data = validation_data))
}
