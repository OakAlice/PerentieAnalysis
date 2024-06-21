# Anomaly Detection Model Tuning
# run the algorithm

model_tuning <- function(options_df, labelled_path){

    results_sheet <- lapply(1:nrow(options_df), function(i) {
        
      tryCatch({
        results <- unsup_1_class_SVM_results(
                      name = options_df[i, "ID"],
                      targetActivity = options_df[i, "targetActivity"],
                      labelled_path,
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
        message(paste("Error in model iteration", i, ":", conditionMessage(e)))
      })
        
        performance_on_training <- results$training_performance
        performance_on_validation <- results$validation_performance
        
        # Flatten the training and validation confusion matrices
        tryCatch({
          training_metrics <- flatten_confusion_matrix(performance_on_training, "Train")
          validation_metrics <- flatten_confusion_matrix(performance_on_validation, "Validation")
        }, error = function(e) {
          # Print error message and skip iteration
          message(paste("Error in performance metrics iteration", i, ":", conditionMessage(e)))
          print(performance_on_validation)
          print(performance_on_training)
        })
        
        results_sheet <- cbind(
          name = as.character(options_df[i, "ID"]),
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
  unsup_1_SVM_options_table <- as.data.frame(unsup_1_SVM_options)
  
  return(unsup_1_SVM_options_table)
}
