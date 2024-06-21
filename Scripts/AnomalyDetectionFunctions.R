# Functions for anomaly detection

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
    name,
    targetActivity, 
    features_list,
    window_length,
    overlap_percent,
    down_Hz,
    feature_normalisation,
    nu,
    kernel,
    save_path
){
  
  # read in the training and validation data
  training_data <- fread(file.path(save_path, paste(name, targetActivity, "training_data.csv", sep = "_")))
  validation_data <- fread(file.path(save_path, paste(name, targetActivity, "validation_data.csv", sep = "_")))
  
  # process the training and validation data
  training_data <- process_data(na.omit(training_data), features_list, window_length = window_length, 
                                overlap_percent = overlap_percent, down_Hz = down_Hz, 
                                feature_normalisation = feature_normalisation)
  training_data <- training_data %>% na.omit() %>% select(-c(ID)) 
  
  validation_data <- process_data(validation_data, features_list, window_length = window_length, 
                                  overlap_percent = overlap_percent, down_Hz = down_Hz, 
                                  feature_normalisation = feature_normalisation)
  validation_data <- validation_data %>% na.omit() %>% select(-c(ID)) 
  
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
  predicted_validation <- predict(unsup_1_class_SVM, newdata = validation_data_predictors) # predict on training data
  predicted_validation <- as.factor(ifelse(predicted_validation == "TRUE", "Normal", "Outlier"))
  reference_validation <- as.factor(ifelse(validation_data_labels$Activity == targetActivity, "Normal", "Outlier")) 
  validation_performance <- table(Predicted = predicted_validation, Reference = reference_validation)
  
  return(list(training_performance = training_performance,
              validation_performance = validation_performance,
              SVM_model = unsup_1_class_SVM))
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

# function for creating the specific datastes
create_datasets <- function(names, targetActivities, save_path){
  for (name in names) {
    for (activity in targetActivities) {
      
      # Pull out the non-target activity from the target individual
      nameNonTargetActivity <- labelled_data %>% 
        filter(ID == name) %>% 
        filter(Activity != activity) %>%
        arrange(Timestamp) %>%
        mutate(partition = ntile(row_number(), 10)) %>%
        ungroup()
      
      # Pull out the target behaviour from the target individual
      nameTargetActivity <- labelled_data %>% 
        filter(ID == name) %>% 
        filter(Activity == activity) %>%
        arrange(Timestamp) %>%
        mutate(partition = ntile(row_number(), 10)) %>%
        ungroup()
      
      # Pull out the target behaviour from the non-target individuals
      otherIndsTargetActivity <- labelled_data %>% 
        filter(ID != name) %>% 
        filter(Activity == activity)
      
      # Create the training set from 5 randomly selected partitions of nameTargetActivity, and all otherIndsTargetActivity 
      training_partitions <- sample(1:10, 5)
      training_data <- rbind(
        otherIndsTargetActivity,
        nameTargetActivity %>% filter(partition %in% training_partitions) %>% select(-partition)
      )
      
      # Create the validation set with 2 nameTargetActivity and 3 nameNonTargetActivity
      remaining_partitions <- setdiff(1:10, training_partitions)
      validation_partitions <- sample(remaining_partitions, 3)
      validation_data <- rbind(
        nameTargetActivity %>% filter(partition %in% validation_partitions) %>% select(-partition),
        nameNonTargetActivity %>% filter(partition %in% remaining_partitions & !(partition %in% validation_partitions)) 
        %>% select(-partition)
      )
      
      # Create the test set with 1 nameTargetActivity and 2 nameNonTargetActivity
      test_partition <- setdiff(remaining_partitions, validation_partitions)
      test_data <- rbind(
        nameTargetActivity %>% filter(partition %in% test_partition) %>% select(-partition),
        nameNonTargetActivity %>% filter(partition %in% test_partition) %>% select(-partition)
      )
      
      # Save the datasets to CSV files
      write.csv(training_data, file.path(save_path, paste(name, activity, "training_data.csv", sep = "_")), row.names = FALSE)
      write.csv(validation_data, file.path(save_path, paste(name, activity, "validation_data.csv", sep = "_")), row.names = FALSE)
      write.csv(test_data, file.path(save_path, paste(name, activity, "test_data.csv", sep = "_")), row.names = FALSE)
      
      print(paste(name, activity, "complete", sep = " "))  
    }
  }
}

# function to evaluate optimal model on test data
test_evaluate_SVM <- function(top_options, i, name, targetActivity, save_path, base_path, features_list) {
  # Extract and convert parameters from top_options
  name <- as.character(top_options[i, "name"])
  targetActivity <- as.character(top_options[i, "targetActivity"])
  window_length <- as.numeric(top_options[i, "window_length"])
  overlap_percent <- as.numeric(top_options[i, "overlap_percent"])
  down_Hz <- as.numeric(top_options[i, "down_Hz"])
  feature_normalisation <- as.character(top_options[i, "feature_normalisation"])
  nu <- as.numeric(top_options[i, "nu"])
  kernel <- as.character(top_options[i, "kernel"])
  
  # Run unsupervised 1-class SVM
  test_results <- unsup_1_class_SVM_results(
    name = name,
    targetActivity = targetActivity,
    features_list = features_list,
    window_length = window_length,
    overlap_percent = overlap_percent,
    down_Hz = down_Hz,
    feature_normalisation = feature_normalisation,
    nu = nu,
    kernel = kernel,
    save_path = save_path
  )
  
  # Extract the SVM model
  SVM_model <- test_results$SVM_model
  saveRDS(SVM_model, file.path(base_path, "Output/SVM_Models", paste(input_name, input_targetActivity, "SVM.rds", sep = "_")))
  
  # Load and preprocess the test data
  test_data_path <- file.path(base_path, "SVM_datasets", paste(name, targetActivity, "test_data.csv", sep = "_"))
  test_data <- fread(test_data_path) %>%
    process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
    na.omit() %>%
    select(-ID)
  
  # Separate predictors and labels
  test_data_predictors <- test_data %>% select(-Activity)
  test_data_labels <- test_data$Activity
  
  # Make predictions on the test data
  predicted_test <- predict(SVM_model, newdata = test_data_predictors)
  predicted_test <- as.factor(ifelse(predicted_test == "TRUE", "Normal", "Outlier"))
  reference_test <- as.factor(ifelse(test_data_labels == targetActivity, "Normal", "Outlier"))
  
  # Compute performance metrics
  test_performance <- table(Predicted = predicted_test, Reference = reference_test)
  
  return(test_performance)
}

