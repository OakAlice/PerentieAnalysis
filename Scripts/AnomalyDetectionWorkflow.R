# Anomaly Dectection Worklfow
# Animal accelerometer behaviour classification using an ensemble of single class SVM
# runs the entire processing for one individual at a time

# Set up
install.packages("pacman")
library(pacman)
p_load(tidyverse, data.table, stringr, tools, e1071, doParallel, parallel)

base_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis"

source(file.path(base_path, "Scripts/DataProcessing.R")) 
source(file.path(base_path, "Scripts/AnomalyDetectionFunctions.R"))
source(file.path(base_path, "Scripts/AnomalyDetection.R"))

# creating datasets ####
#labelled_data <- fread(file.path(base_path, "Controlled_DiCicco_Perentie_Labelled.csv"))
#names <- c("Eric", "Bubbles", "Abigail")
#targetActivities <- c("Locomotion", "Inactive")
#save_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis/SVM_datasets"
#create_datasets(names, targetActivities, save_path)

# define variables ####
name_input <- "Eric"
targetActivity_options <- c("Inactive", "Locomotion")

window_length_options <- c(0.5)
overlap_percent_options <- c(0)
down_Hz_options <- c(50)
feature_normalisation_options <- c("MinMaxScaling") #, "Standardisation")
nu_options <- c(0.01)
kernel_options <- c("radial") #, "sigmoid", "polynomial")
features_list <- c("mean", "max", "min", "sd", "cor") #, "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA") #, 
#"RMS", "FFT", "entropy", "zero_crossing")

# Tuning ####
# generate all possible combinations
options_df <- expand.grid(name_input, targetActivity_options, window_length_options, overlap_percent_options, down_Hz_options, 
                          feature_normalisation_options, nu_options, kernel_options)
colnames(options_df) <- c("ID", "targetActivity", "window_length", "overlap_percent", "down_Hz", 
                          "feature_normalisation", "nu", "kernel")
# model tuning function
unsup_1_SVM_options_table <- model_tuning(options_df, labelled_path = file.path(base_path, "SVM_datasets"))

# Extract the optimal ####
optimal <- unsup_1_SVM_options_table %>%
  group_by(name, targetActivity) %>%
  arrange(desc(as.numeric(Validation_accuracy))) %>%
  slice(1) %>%
  ungroup()

# Test the optimal model design ####
i <- 1
while (i <= nrow(optimal)) {
  # Test each of the independent SVMs and save the SVM as an RDS
  unsup_1_SVM_test <- model_testing(optimal, i, labelled_path = file.path(base_path, "SVM_datasets"))
  
  # Flatten the confusion matrix
  test_metrics <- flatten_confusion_matrix(unsup_1_SVM_test, "Test")
  
  # Combine the optimal options with the test metrics
  optimal_model_information <- cbind(optimal[i, ], t(test_metrics))
  colnames(optimal_model_information) <- c(colnames(optimal), colnames(t(test_metrics)))
  
  # Combine the results into a single data frame
  if (i == 1) {
    optimal_information <- optimal_model_information
  } else {
    optimal_information <- rbind(optimal_information, optimal_model_information)
  }
  
  i <- i + 1
}

# Apply both the SVMs to the same data as an example ####
example_data <- fread(file.path(base_path, "Controlled_DiCicco_Perentie_Labelled.csv")) %>% 
  filter(ID == "Abigail")

processed_data_list <- list()

for (target in targetActivity_options) {
  # Process the data for the current target activity
  example_processed <- example_data %>%
    process_data(features_list, 
                 window_length = as.numeric(optimal$window_length[optimal$targetActivity == target]), 
                 overlap_percent = as.numeric(optimal$overlap_percent[optimal$targetActivity == target]),
                 down_Hz = as.numeric(optimal$down_Hz[optimal$targetActivity == target]), 
                 feature_normalisation = as.character(optimal$feature_normalisation[optimal$targetActivity == target])) %>%
    na.omit() %>%
    select(-ID)
  
  # Separate features and labels
  example_features <- example_processed %>% select(-Activity)
  example_labels <- example_processed %>% select(Activity)
  
  # predict
  # load the SVM
  SVM_model <- readRDS(file.path(base_path, "Output/SVM_Models", paste(name, target, "SVM.rds", sep = "_")))
  
  predicted_labels <- predict(SVM_model, newdata = example_features)
  predicted_labels <- as.factor(ifelse(predicted_labels == "TRUE", target, "Outlier"))
  
  # Store the processed features and labels in the list
  processed_data_list[[paste("processed_data_", target, sep = "")]] <- list(predicted_labels = predicted_example, example_labels = example_labels)
}

######## THIS IS NOT WORKING ##############

# Access each of the processed data and line them up next to each other
example_Inactive_labels <- processed_data_list[["processed_data_Inactive"]]$predicted_labels
example_Locomotion_labels <- processed_data_list[["processed_data_Locomotion"]]$predicted_labels

predicted_labels <- cbind(example_Inactive_labels, example_Locomotion_labels)

