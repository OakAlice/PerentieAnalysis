# Main Script for Jordan Perentie Data Analysis

# Contains formatting and inspecting training data, building and validating a ML model, 
# predicting behaviours from unlabelled data.
# Expected Directory Format:
# Scripts <- all the scripts called in this main executor
# Raw_Data <- contains all raw data
# Labelled_Data <- folders labelled by individual, csv or txt files containing annotated training data"

# Install Packages ####
#install.packages("pacman")
library(pacman)
p_load(tidyverse, data.table, stringr, tools, doParallel, parallel, randomForest, e1071)

# Set Variables ####
base_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis"
activity_key_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis/Activity_Key.csv"

frequency <- 50

# load in the functions from other scripts
source(file.path(base_path, "Scripts/CreateTrainingData.R"))
source(file.path(base_path, "Scripts/Plots.R"))
source(file.path(base_path, "Scripts/Balancing.R"))
source(file.path(base_path, "Scripts/OptimalModel.R"))
source(file.path(base_path, "Scripts/DataProcessing.R"))

# TRAINING DATA ####
## Create the training dataframe from individual files ####
training_data <- data.frame()
training_data <- matlab_to_training(base_path, activity_key_path)

## Clean up the data ####
clean_training_data <- training_data %>%
  filter(!is.na(Activity)) %>% # Remove rows where Activity is NA
  mutate(ID = ifelse(ID == "tail_down", "Tiny", ID)) %>% # Update ID
  select(ID, Timestamp, Accel_X, Accel_Y, Accel_Z, Activity) # select good cols

# Adjust behavioural categories
clean_training_data <- clean_training_data %>%
  mutate(Activity = ifelse(Activity == "Climbing"|Activity == "Eating", "Other", Activity)) %>%
  mutate(Activity = ifelse(Activity == "Walk"|Activity == "Run", "Locomotion", Activity))

## Explore Data: Quantity ####
behaviourPerIndPlot <- explore_data(data = clean_training_data, frequency = frequency, colours = length(unique(clean_training_data$ID)))
behaviourPerIndChart <- clean_training_data %>%
  group_by(Activity) %>%
  count() %>%
  mutate(minutes = (n/frequency)/60)%>%
  arrange(desc(minutes))

# Select the behaviours to continue with # in this case, all of them
clean_training_data <- clean_training_data %>%
  filter(Activity %in% c("Locomotion", "Inactive"))

## Explore Data: Quantity ####
plotOfBehaviours <- plot_behaviours(behaviours = unique(clean_training_data$Activity), 
                                    data = clean_training_data, 
                                    n_samples = 1000, 
                                    n_col = 1)
plotOfBehaviours

# save the dataframe
fwrite(clean_training_data, file.path(base_path, "DiCicco_Perentie_Labelled.csv"))

# BUILDING THE MODEL ####
# Export the training data into the AutoML pipeline I built on the HPC
# Run experiments
# When you have determined the ideal ML architecture, enter the parameters below

## Load in Data ####
training_data <- fread(file.path(base_path, "Controlled_DiCicco_Perentie_Labelled.csv"))
tstDat <- training_data %>% filter(ID == "Abigail") # individual not included in training
otherDat <- training_data %>% filter(!ID == "Abigail") # remaining data

## Generate Optimal Model ####
featuresList <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", 
"RMS", "FFT", "entropy", "zero_crossing")

optimal_trained_model <- generate_optimal_model(otherDat, 
                                                down_Hz = 50, 
                                                window_length = 2, 
                                                overlap_percent = 20, 
                                                featuresList, 
                                                threshold = 250000, 
                                                stratification, # set to FALSE for now, not coded
                                                feature_normalisation = FALSE,
                                                folds = 1, 
                                                training_percentage = 0.9, 
                                                model_architecture = "RF",
                                                trees_number = 50)
# save for later
model_file_path <- file.path(base_path, 'Output', "OptimalTrainedModel.rda")
save(optimal_trained_model, file = model_file_path)

# TEST MODEL ####
## Process tstDat to match ####
processed_data <- process_data(tstDat, features_list, window_length = 2, 
                               overlap_percent = 20, 50, feature_normalisation = FALSE) # second last one is down_Hz
tstDat2 <- processed_data %>% select(-ID)

optimal_results <- verify_optimal_results(tstDat2, optimal_trained_model, 
                                          test_type = "test", probability_report = FALSE,  
                                          probability_threshold = NULL)

print(optimal_results$confusion_matrix)
print(optimal_results$confusion_plot)
print(optimal_results$stacked_plot)
#print(optimal_results$NA_loss_plot)
print(optimal_results$metrics)


# Predict behaviours onto unlabelled data
# load in the model
load(file.path(base_path, "Output/OptimalTrainedModel.rda"))

# load in the raw files 
unlabelled_data <- list.files(file.path(base_path, "Raw_data"), full.names = TRUE)

behaviours <- predict_behaviours(optimal_trained_model, 
                                 unlabelled_data, 
                                 down_Hz = 50, 
                                 window_length = 1, 
                                 overlap_percent = 0, 
                                 features_list, 
                                 feature_normalisation = FALSE
                                 )



