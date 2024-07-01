# Anomaly Dectection Worklfow
# Animal accelerometer behaviour classification using an ensemble of single class SVM
# runs the entire processing for one individual at a time

# Set up ####
#install.packages("pacman")
library(pacman)
p_load(tidyverse, data.table, stringr, tools, e1071, doParallel, parallel)

base_path <- "C:/Users/oaw001/Documents/Perentie"

source(file.path(base_path, "Scripts/DataProcessing.R")) 
source(file.path(base_path, "Scripts/AnomalyDetectionFunctions.R"))

# Creating datasets ####
#(base_path, file.path(base_path, "Activity_Key.csv"))
labelled_data <- fread(file.path(base_path, "DiCicco_Perentie_Labelled.csv"))
#labelled_data <- labelled_data %>%
#  na.omit() %>%
#  mutate(Activity = ifelse(Activity == "Climbing" | Activity == "Eating", "Other", Activity)) %>%
#  mutate(Activity = ifelse(Activity == "Run" | Activity == "Walk", "Locomotion", Activity))

# Check data quality ####
#data <- fread(file.path(base_path, "SVM_datasets/Abigail_Locomotion_training_data.csv"))
#behaviour_samples <- plot_behaviours(c("Inactive", "Walk", "Run", "Other"), labelled_data, 1000, 1)


labelled_data <- labelled_data %>%
  filter(Activity != "NaN")

# Create a test dataframe containing the final third of each activity for each individual
test_data <- labelled_data %>%
  group_by(ID, Activity) %>%
  arrange(Timestamp) %>%  # Ensure data is arranged chronologically
  mutate(row_num = row_number(), 
         total_rows = n()) %>%
  filter(row_num > 2 * total_rows / 3) %>%
  ungroup()

targetActivity_options <- c("Locomotion", "Inactive")
window_length_options <- c(1)
overlap_percent_options <- c(0)
down_Hz_options <- c(25)
feature_normalisation_options <- c("MinMaxScaling") #, "Standardisation")
nu_options <- c(0.01, 0.1)
kernel_options <- c("radial", "sigmoid", "polynomial")
features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", "entropy", "zero", "auto")

# From here on, loop per individual and activity
optimal_model_designs <- data.frame()

for (targetActivity in targetActivity_options){
  # Tuning ####
  # generate all possible combinations 
  options_df <- expand.grid(targetActivity, window_length_options, overlap_percent_options, down_Hz_options, 
                            feature_normalisation_options, nu_options, kernel_options)
  colnames(options_df) <- c("targetActivity", "window_length", "overlap_percent", "down_Hz", 
                            "feature_normalisation", "nu", "kernel")

  # remove the test data
  other_data <- anti_join(labelled_data, test_data)
  
  # randomly create training and validation dataset
  datasets <- create_datasets(other_data, targetActivity)
  training_data <- datasets$training_data %>% select(-Timestamp, -ID)
  validation_data <- datasets$validation_data %>% select(-Timestamp, -ID)
  print("datasets created")
    
  model_tuning_metrics <- model_tuning(options_df, base_path, training_data, validation_data, targetActivity)
    
  print(paste("Model tuning for", targetActivity, "complete"))
  
  # write out the tuning csv
  fwrite(model_tuning_metrics, file.path(base_path, paste(targetActivity, "tuning_metrics.csv", sep = "_")))
}
  
  # CALCULATE THE AVERAGE ##
  # Extract the optimal ####
  optimal_df <- model_tuning_metrics %>%
    group_by(name, targetActivity) %>%
    arrange(desc(as.numeric(Validation_accuracy))) %>%
    slice(1) %>%
    ungroup()

  # or upload the optimal
  Loco <- as.data.frame(fread(file.path(base_path, "Locomotion_tuning_metrics.csv"))) %>% 
    arrange(desc(Validation_accuracy)) %>% head(n=1)
  Inactive <- as.data.frame(fread(file.path(base_path, "Inactive_tuning_metrics.csv"))) %>%
    arrange(desc(Validation_accuracy)) %>% head(n=1)
  optimal_df <- rbind(Loco, Inactive)

optimal_model_tests <- data.frame()

activity <- "Inactive"

for (activity in targetActivity_options){

  # Extract the training and test data
  evaluation_data <- test_data %>% select(-Timestamp, -ID) # generated earlier
  other_data <- anti_join(labelled_data, evaluation_data)
  training_data <- other_data %>% filter(Activity == activity) %>% select(-Timestamp, -ID)
  
  # Extract the optimal parameters
  optimal_df_row <- optimal_df %>% filter(targetActivity == activity)
  
  model_evaluation_metrics <- model_testing(optimal_df_row, base_path, training_data, evaluation_data, activity)
  print(paste("Optimal model testing for", activity, "complete"))
  
  optimal_model_tests <- rbind(optimal_model_tests, model_evaluation_metrics)

}
  
fwrite(optimal_model_tests, file.path(base_path, "Optimal_Pooled_Model_Test_Metrics_Balanced.csv"))








# Apply an optimal SVMs to some sample data as an example ####
# load in the example individual
example_data <- fread(file.path(base_path, "DiCicco_Perentie_Labelled.csv")) %>% 
    filter(ID == "Abigail") %>%
    filter(!Activity == "NaN")
fwrite(example_data, file.path(base_path, "Abigail_data.csv"))

# load in the optimal variables
optimal_df <- as.data.frame(fread(file.path(base_path, "optimal.csv")))

# behaviours to analyse
behaviours<- c("Inactive", "Locomotion")

for (behaviour in behaviours){
  # behaviour <- behaviours[2]
  # extract the right row
  optimal_design <- optimal_df %>% filter(targetActivity == behaviour)
  
  # define the variables
  window_length <- as.numeric(optimal_design["window_length"])
  overlap_percent = as.numeric(optimal_design["overlap_percent"])
  down_Hz = as.numeric(optimal_design["down_Hz"])
  feature_normalisation = as.character(optimal_design["feature_normalisation"])
  nu = as.numeric(optimal_design["nu"])
  kernel = as.character(optimal_design["kernel"])

  # process data
  example_data_processed <- example_data %>%
    process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
    na.omit() %>%
    select(-ID) %>% 
    select(-c(zero_Accel_Y, zero_Accel_Z))
  
  # extract components
  example_times <- example_data_processed %>% select(Timestamp)
  example_reference <- example_data_processed %>% select(Activity)
  example_data_processed <- example_data_processed %>% select(-Timestamp)
  
  ## read in the SVM
  optimal_SVM<- readRDS(file.path(base_path, paste0(behaviour, ".rds")))

  # run the predictions
  model_evaluation <- evaluate_model_performance(example_data_processed, optimal_SVM, "Example", behaviour)
  
  metrics <- model_evaluation$metrics
  predicted <- model_evaluation$predicted
  reference <- model_evaluation$reference
  
  # combine these
  example_results <- example_times %>%
    mutate(Reference = reference) %>%
    mutate(Predicted = predicted)
  
  fwrite(example_results, file.path(base_path, paste0(behaviour, "_Abigail.csv")))
}


# combine the dataframes
options(digits=20)
Inactive<- fread(file.path(base_path, "Inactive_Abigail.csv")) %>% 
  mutate(InactivePredictions = ifelse(Predicted == "Normal", "Inactive", "Other")) %>%
  select(Timestamp, InactivePredictions)
Locomotion<- fread(file.path(base_path, "Locomotion_Abigail.csv")) %>%
  mutate(LocomotionePredictions = ifelse(Predicted == "Normal", "Locomotion", "Other")) %>%
  select(Timestamp, LocomotionePredictions)

Example<- fread(file.path(base_path, "Abigail_data.csv")) %>% select(Timestamp, Activity)

Total<- merge(Inactive, Locomotion, by = "Timestamp", all = TRUE)
Total <- merge(Total, Example, by = "Timestamp", all = TRUE)

# convert the time to an averaged minute
Total <- Total %>% 
  mutate(Time = as.POSIXct((Timestamp - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(Minute = round_date(Time, unit = "minute"))

# find the most frequent non-NA value
most_frequent <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(names(sort(table(x), decreasing = TRUE))[1])
  }
}

Total_grouped <- Total %>%
  select(-Timestamp, -Time) %>%
  group_by(Minute) %>%
  summarise(across(everything(), most_frequent, .names = "{col}"), .groups = 'drop')

Total_grouped <- Total_grouped %>%
  mutate(Outcome = ifelse(
    InactivePredictions == "Inactive" & LocomotionePredictions == "Other", "Inactive",
    ifelse(
      InactivePredictions == "Other" & LocomotionePredictions == "Other", "Other", "Locomotion"
    )
  ))


summary <- Total_grouped %>%
  group_by(Activity, Outcome) %>%
  count() %>%
  na.omit()


# now caluclate the accuracy

