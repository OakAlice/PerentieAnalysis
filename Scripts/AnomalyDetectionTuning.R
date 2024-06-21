# Anomaly Detection
# Run Script ####
install.packages("pacman")
library(pacman)
p_load(tidyverse, data.table, stringr, tools, e1071, doParallel, parallel)

base_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis"

source(file.path(base_path, "Scripts/DataProcessing.R")) 
source(file.path(base_path, "Scripts/AnomalyDetectionFunctions.R")) 

# creating datasets
labelled_data <- fread(file.path(base_path, "Controlled_DiCicco_Perentie_Labelled.csv"))

#names <- c("Eric", "Bubbles", "Abigail")
#targetActivities <- c("Locomotion", "Inactive")
#save_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis/SVM_datasets"
#create_datasets(names, targetActivities, save_path)

# define variables
  name_options <- "Eric"
  targetActivity_options <- targetActivities
  save_path <- save_path
  window_length_options <- c(0.5, 1, 2, 5)
  overlap_percent_options <- c(0, 25, 50)
  down_Hz_options <- c(25, 50)
  feature_normalisation_options <- c("MinMaxScaling", "Standardisation")
  nu_options <- c(0.01, 0.1, 0.2)
  kernel_options <- c("radial", "sigmoid", "polynomial")
  features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA") #, 
                     #"RMS", "FFT", "entropy", "zero_crossing")
  
# generate all possible combinations
  options_df <- expand.grid(name_options, targetActivity_options, window_length_options, overlap_percent_options, down_Hz_options, 
                            feature_normalisation_options, nu_options, kernel_options)
  colnames(options_df) <- c("ID", "targetActivity", "window_length", "overlap_percent", "down_Hz", 
                            "feature_normalisation", "nu", "kernel")
  
# run the algorithm
results_sheet <- lapply(1:nrow(options_df), function(i) {
    
  tryCatch({
    results <- unsup_1_class_SVM_results(
              name = options_df[i, "ID"],
              targetActivity = options_df[i, "targetActivity"],
              features_list = features_list,
              window_length = options_df[i, "window_length"],
              overlap_percent = options_df[i, "overlap_percent"],
              down_Hz = options_df[i, "down_Hz"],
              feature_normalisation = options_df[i, "feature_normalisation"],
              nu = options_df[i, "nu"],
              kernel = options_df[i, "kernel"],
              save_path
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
name <- "Eric"
unsup_1_SVM_options_table <- as.data.frame(unsup_1_SVM_options)
fwrite(unsup_1_SVM_options_table, file.path(save_path, paste0("ModelOptions2_", name, ".csv")))


# test it ####
# specify the test
names <- c("Eric")
targetActivities <- c("Inactive", "Locomotion")

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

# layering the two models ####
# set the parameters

name_input <- c("Eric")
targetActivity_input <- c("Inactive")

# load in the unlabelled data 
# will use labelled data for now
unlabelled_data <- fread(file.path(base_path, "SVM_datasets", paste(name, targetActivity, "test_data.csv", sep = "_")))

# process it according to the best parameters
info <- optimal_information %>%
  filter(name == name_input, targetActivity == targetActivity_input)

unlabelled_features <- process_data(unlabelled_data, features_list, window_length = as.numeric(info[1, "window_length"]), 
             overlap_percent = as.numeric(info[1, "overlap_percent"]), down_Hz = as.numeric(info[1, "down_Hz"]), 
             feature_normalisation = as.character(info[1, "feature_normalisation"]))
labels <-  unlabelled_features %>% na.omit() %>% select(-c(!Activity)) 
unlabelled_features <- unlabelled_features %>% na.omit() %>% select(-c(ID, Activity)) 

# load in the right svm
readRDS(file.path(base_path, "Output/SVM_Models", paste(name, targetActivity_input, "SVM.rds", sep = "_")))

# predict onto the new one
predicted_unlabelled <- predict(Eric_Inactive_SVM, newdata = unlabelled_features)
predicted_unlabelled <- as.factor(ifelse(predicted_unlabelled == "TRUE", "Inactive", "Outlier"))


reference_unlabelled <- as.factor(ifelse(labels == targetActivity_input, "Normal", "Outlier"))
unlabelled_performance <- cbind(Predicted = data.frame(predicted_unlabelled), Reference = data.frame(reference_unlabelled))


