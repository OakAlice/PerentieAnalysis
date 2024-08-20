# Initialisation Script

# Set up ####
# load packages
library(data.table)
library(tidyverse)
library(future)
library(future.apply)
library(e1071)
library(h2o)
library(zoo)


# set base path
base_path <- ("C:/Users/oaw001/Documents/AnomalyDetection")

# load in the files
source(file.path(base_path, "Scripts", "PlotFunctions.R"))
source(file.path(base_path, "Scripts", "1classSVMFunctions.R"))
source(file.path(base_path, "Scripts", "FeatureGeneration.R"))

# jordan data
data_original <- fread("C:/Users/oaw001/Documents/Perentie/DiCicco_Perentie_Labelled.csv")
data_original <- data_original %>%
  rename(Accelerometer.X = Accel_X,
         Accelerometer.Y = Accel_Y,
         Accelerometer.Z = Accel_Z) %>%
  filter(!Activity == "NaN")
base_path <- "C:/Users/oaw001/Documents/Perentie/Round2"


# Split Data ####
# randomly allocate each individual to training, validating, or testing datasets
data_test <- data_original[data_original$ID %in% "Eric", ] # select test individual
other_data <- anti_join(data_original, data_test) # remainder


# Visualising data ####
# plot shows sample of each behaviour for each individual
# select a subset of individuals to use
data_subset <- data_original[data_original$ID %in% unique(data_original$ID)[1:3], ]
beh_trace_plot <- plot_behaviours(behaviours = unique(data_subset$Activity), data = data_subset, n_samples = 200, n_col = 4)
# plot shows the total samples per behaviour and individual
beh_volume_plot <- explore_data(data = data_original, frequency = 25, colours = unique(data_original$ID))


# 1class-SVM model design tuning ####
# list variables to test
targetActivity_options <- c("Inactive", "Locomotion")


window_length_options <- c(1, 5) ##
overlap_percent_options <- c(0, 50) ##
freq_Hz <- 50 ##
feature_normalisation_options <- c("Standardisation") # "MinMaxScaling" ##
nu_options <- c(0.4, 0.5, 0.6, 0.7) ##
kernel_options <- c("radial", "polynomial", "sigmoid", "linear")
gamma_options <- c(0.001, 0.01, 0.1, 1, "auto")
degree_options <- c(3)

model_hyperparameters_list <- list(
  radial = list(
    gamma = gamma_options
  ),
  polynomial = list(
    gamma = gamma_options,
    degree = degree_options
  ),
  sigmoid = list(
    gamma = gamma_options
  )
)

#features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", "entropy", "auto") # zero
features_list <- c("auto", "entropy", "max", "min", "maxVDBA", "sd")


#validation_individuals <- 1
all_axes <- c("Accelerometer.X", "Accelerometer.Y", "Accelerometer.Z")
# need to add this into the rest of the code 

# from here on, it will loop
optimal_model_designs <- data.frame()

#for (targetActivity in targetActivity_options){
targetActivity <- "Locomotion"
# Tuning ##
# generate all possible combinations
options_df <- expand.grid(targetActivity, window_length_options, overlap_percent_options, freq_Hz, 
                          feature_normalisation_options, nu_options, kernel_options)
colnames(options_df) <- c("targetActivity", "window_length", "overlap_percent", "frequency_Hz", 
                          "feature_normalisation", "nu", "kernel")

# add the additional parameters
extended_options_df <- create_extended_options(model_hyperparameters_list, options_df)

# create training and validation datasets by selecting chronological segments
training_data <- other_data %>%
  filter(ID %in% unique(ID)[-length(unique(ID))]) %>%
  filter(Activity == targetActivity) %>% 
  select(-Timestamp, -ID)
validation_data <- anti_join(other_data, training_data) %>% 
  select(-Timestamp, -ID)

print("datasets created")

model_tuning_metrics <- model_tuning(extended_options_df, base_path, training_data, validation_data, targetActivity)

print(paste("Model tuning for", targetActivity, "complete"))

# write out the tuning csv
fwrite(model_tuning_metrics, file.path(base_path, paste(targetActivity, "tuning_metrics.csv", sep = "_")))
#}








# Test optimal model ####
# upload csv with the best model designs
optimal_df <- fread(file.path(base_path, "Optimal_metrics_test.csv"))

optimal_model_tests <- data.frame()

targetActivity_options <- c("Inactive", "Locomotion")
features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", "entropy", "auto") # zero
all_axes <- c("Accelerometer.X", "Accelerometer.Y", "Accelerometer.Z")

for (activity in targetActivity_options){
  
  # Extract the training and test data
  evaluation_data <- data_test %>% select(-Timestamp, -ID) # generated earlier
  training_data <- other_data %>%
    filter(ID %in% unique(ID)[-length(unique(ID))]) %>%
    filter(Activity == activity) %>% 
    select(-Timestamp, -ID)
  
  # Extract the optimal parameters
  optimal_df_row <- optimal_df %>% as.data.frame() %>% filter(targetActivity == activity)
  
  model_evaluation_metrics <- model_testing(optimal_df_row, base_path, training_data, evaluation_data, activity)
  
  print(paste("Optimal model testing for", activity, "complete"))
  
  optimal_model_tests <- rbind(optimal_model_tests, model_evaluation_metrics)
  
}

fwrite(optimal_model_tests, file.path(base_path, "Optimal_Model_Test_Balanced_2.csv"))

