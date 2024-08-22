# Anomaly detection test

# test on hold-out test data and save parameters and performance in table 
optimal_information <- data.frame()
for (name_input in names){
  for (targetActivity_input in targetActivities){
    
    # load in the results and review
    model_options <- fread(file.path(base_path, "Output/Model_Options/", paste0("ModelOptions2_", name, ".csv")))
    top_options <- model_options %>%
      filter(name == name_input, targetActivity == targetActivity_input) %>%
      arrange(desc(as.numeric(Validation_accuracy))) %>%
      head(n = 10)
    
    # select the row you want
    i <- 2
    
    # test and save the model (saved in the function)
    test_results <- test_evaluate_SVM(top_options, i, name_input, targetActivity_input, save_path, base_path, features_list)
    test_metrics <- flatten_confusion_matrix(test_results, "Test")
    
    optimal_model_information <- cbind(top_options[i,], t(test_metrics))
    colnames(optimal_model_information) <- c(colnames(top_options), colnames(t(test_metrics)))
    
    optimal_information <- rbind(optimal_information, optimal_model_information)
  }
}

optimal_information

# use that information to 


# layering the two models ####
# set the parameters

name_input <- c("Eric")
targetActivity_input <- c("Inactive", "Locomotion")

# load in the unlabelled data 
unlabelled_performance <- data.frame()  # Initialize an empty data frame to store results

for (name_input in names) {
  for (targetActivity_input in targetActivities) {
    
    info <- optimal_information %>%
      filter(name == name_input, targetActivity == targetActivity_input)
    
    unlabelled_features <- process_data(unlabelled_data, features_list, 
                                        window_length = as.numeric(info[1, "window_length"]), 
                                        overlap_percent = as.numeric(info[1, "overlap_percent"]), 
                                        down_Hz = as.numeric(info[1, "down_Hz"]), 
                                        feature_normalisation = as.character(info[1, "feature_normalisation"]))
    
    labels <-  unlabelled_features %>% na.omit() %>% select(Activity) 
    unlabelled_features <- unlabelled_features %>% na.omit() %>% select(-c(ID, Activity)) 
    
    # Load in the appropriate SVM model
    svm_model_path <- file.path(base_path, "Output/SVM_Models", paste(name_input, targetActivity_input, "SVM.rds", sep = "_"))
    SVM_model <- readRDS(svm_model_path)
    
    # Predict onto the new data
    predicted_unlabelled <- predict(SVM_model, newdata = unlabelled_features)
    predicted_unlabelled <- as.factor(ifelse(predicted_unlabelled == "TRUE", targetActivity_input, "Outlier"))
    
    reference_unlabelled <- as.factor(ifelse(labels$Activity == targetActivity_input, targetActivity_input, "Outlier"))
    
    # Combine predictions and references into a data frame
    current_performance <- data.frame(
      Predicted = predicted_unlabelled,
      Reference = reference_unlabelled
    )
    
    # Append current results to the overall performance data frame
    unlabelled_performance <- c(unlabelled_performance, current_performance)
  }
}

unlabelled_performance_df <- data.frame(unlabelled_performance)