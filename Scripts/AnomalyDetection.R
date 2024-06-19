# Anomaly Detection

# functions ####
# function for splitting the data
split_data <- function(data, targetActivity) {
  target_data <- data %>%
    filter(Activity == targetActivity) %>%
    arrange(ID, Timestamp) %>%
    group_by(ID) %>%
    mutate(partition = ntile(row_number(), 5)) %>%
    ungroup()
  
  # Use a random selection of partitions for training and validation
  training_partitions <- sample(1:5, size = 4, replace = FALSE)
  training_data <- target_data %>%
    filter(partition %in% training_partitions) %>%
    select(-partition)
  
  # Get the validation data as the remaining data not in training
  validation_data <- target_data %>%
    filter(!partition %in% training_partitions) %>%
    select(-partition)
  
  # Combine with other activities to form the full validation dataset
  validation_data <- bind_rows(validation_data, data %>% filter(Activity != targetActivity))
  
  list(training = training_data, validation = validation_data)
}

# function for performing the test
unsup_1_class_SVM_results <- function(
                              fullData,
                              targetActivity, 
                              features_list,
                              window_length,
                              overlap_percent,
                              down_Hz,
                              feature_normalisation,
                              nu,
                              kernel
                            ){
  
  # chronologically splits it within ID and Activity, then randomly combines into training and Val data 
  split <- split_data(fullData, targetActivity) 
  
  # process the training and validation data
  training_data <- process_data(na.omit(split$training), features_list, window_length = window_length, 
                                overlap_percent = overlap_percent, down_Hz = down_Hz, 
                                feature_normalisation = feature_normalisation)
  training_data <- training_data %>% select(-c(ID))
  
  validation_data <- process_data(na.omit(split$validation), features_list, window_length = window_length, 
                                  overlap_percent = overlap_percent, down_Hz = down_Hz, 
                                  feature_normalisation = feature_normalisation)
  validation_data <- validation_data %>% select(-c(ID))
  
  # separate the training data into predictors and labels 
  training_data_predictors <- training_data %>% select(-c(Activity))
  training_data_labels <- training_data %>% select(Activity)
  
  validation_data_predictors <- validation_data %>% select(-c(Activity))
  validation_data_labels <- validation_data %>% select(Activity)
  
  # train a model with the training predictors (no labels)
  unsup_1_class_SVM <- svm(training_data_predictors, y = NULL,
                             type = 'one-classification',
                             nu = nu,
                             scale = TRUE,
                             kernel = kernel)
  
  # make predictions with the trained SVM onto predictor only data for training
  predicted_training <- predict(unsup_1_class_SVM, training_data_predictors) # predict on training data
  predicted_training <- as.factor(ifelse(predicted_training == "TRUE", "Normal", "Outlier"))
  reference_training <- as.factor(ifelse(training_data_labels$Activity == targetActivity, "Normal", "Outlier"))
  training_performance <- table(Predicted = predicted_training, Reference = reference_training)
  
  # do the same for validation
  predicted_validation <- predict(unsup_1_class_SVM, validation_data_predictors) # predict on training data
  
  # lengths
  length(validation_data_predictors$mean_Accel_X)
  length(predicted_validation)
  
  predicted_validation <- predict(unsup_1_class_SVM, validation_data_predictors) # predict on validation data
  predicted_validation <- as.factor(ifelse(predicted_validation == "TRUE", "Normal", "Outlier"))
  reference_validation <- as.factor(ifelse(validation_data_labels$Activity == targetActivity, "Normal", "Outlier")) 
  validation_performance <- table(Predicted = predicted_validation, Reference = reference_validation)
  
  return(list(training_performance = training_performance,
              validation_performance = validation_performance))
}

# function to turn matrices into rows
flatten_confusion_matrix <- function(conf_matrix, prefix) {
  if (nrow(conf_matrix) == 2 && ncol(conf_matrix) == 2) {
    c(TP = conf_matrix[1, 1],
      FP = conf_matrix[1, 2],
      FN = conf_matrix[2, 1],
      TN = conf_matrix[2, 2],
      accuracy = round((conf_matrix[1, 1]+conf_matrix[2, 2])/(conf_matrix[1, 1]+conf_matrix[2, 2]+conf_matrix[1, 2]+conf_matrix[2, 1]),2)) %>%
      setNames(paste(prefix, c("TP", "FP", "FN", "TN", "accuracy"), sep = "_"))
  } else if (nrow(conf_matrix) == 2 && ncol(conf_matrix) == 1) {
    c(TP = conf_matrix[1, 1],
      FN = conf_matrix[2, 1],
      accuracy = round((conf_matrix[1, 1]/(conf_matrix[1, 1]+conf_matrix[2, 1])),2)) %>%
      setNames(paste(prefix, c("TP", "FN", "accuracy"), sep = "_"))
  } else {
    stop("Confusion matrix dimensions are not supported.")
  }
}



# Run Script ####
library(pacman)
p_load(tidyverse, data.table, stringr, tools, e1071)

base_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis"
  
labelled_data <- fread(file.path(base_path, "Controlled_DiCicco_Perentie_Labelled.csv"))
tstDat <- labelled_data %>% filter(ID == "Abigail") # individual not included in training
otherDat <- labelled_data %>% filter(!ID == "Abigail") # other data

# define variables
  targetActivity_options <- c("Locomotion")
  window_length_options <- c(0.5, 1, 2)
  overlap_percent_options <- c(0, 10, 50)
  down_Hz_options <- 50
  feature_normalisation_options <- c("MinMaxScaling", "Standardisation")
  nu_options <- c(0.01, 0.05, 0.10, 0.15, 0.20)
  kernel_options <- c("polynomial", "radial", "sigmoid")
  features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", 
                     "RMS", "FFT", "entropy", "zero_crossing")
  
# generate all possible combinations
  options_df <- expand.grid(targetActivity_options, window_length_options, overlap_percent_options, down_Hz_options, 
                            feature_normalisation_options, nu_options, kernel_options)
  colnames(options_df) <- c("targetActivity", "window_length", "overlap_percent", "down_Hz", 
                            "feature_normalisation", "nu", "kernel")
  
# run the algorithm
results_sheet <- lapply(1:nrow(options_df), function(i) {
    
  tryCatch({
    results <- unsup_1_class_SVM_results(
              fullData = otherDat, 
              targetActivity = options_df[i, "targetActivity"],
              features_list = features_list,
              window_length = options_df[i, "window_length"],
              overlap_percent = options_df[i, "overlap_percent"],
              down_Hz = options_df[i, "down_Hz"],
              feature_normalisation = options_df[i, "feature_normalisation"],
              nu = options_df[i, "nu"],
              kernel = options_df[i, "kernel"]
      )
  }, error = function(e) {
    # Print error message and skip iteration
    message(paste("Error in iteration", i, ":", conditionMessage(e)))
  })
    
    performance_on_training <- results$training_performance
    performance_on_validation <- results$validation_performance
    
    # Flatten the training and validation confusion matrices
    training_metrics <- flatten_confusion_matrix(performance_on_training, "Train")
    validation_metrics <- flatten_confusion_matrix(performance_on_validation, "Validation")
    
    results_sheet <- cbind(
      targetActivity = as.character(options_df[i, "targetActivity"]),
      window_length = options_df[i, "window_length"],
      overlap_percent = options_df[i, "overlap_percent"],
      down_Hz = options_df[i, "down_Hz"],
      feature_normalisation = as.character(options_df[i, "feature_normalisation"]),
      nu = options_df[i, "nu"],
      kernel = as.character(options_df[i, "kernel"]),
      t(training_metrics), 
      t(validation_metrics)
    )
    })

unsup_1_SVM_options <- do.call(rbind, results_sheet)
