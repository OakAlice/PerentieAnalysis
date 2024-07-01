# Apply to unlabelled data

# load in the SVMS
Inactive_SVM <- readRDS(file.path(base_path, "Output/SVM_Models/Inactive.rds"))
Locomotion_SVM <- readRDS(file.path(base_path, "Output/SVM_Models/Locomotion.rds"))

# Load in the optimal model designs that match the SVMs
optimal <- fread(file.path(base_path, "optimal.csv"))

# Behaviours to analyse
behaviours <- c("Inactive", "Locomotion")

# List the unlabelled data
chunked_raw <- list.files("D:/Jordan_Perentie/Chunked_accel", full.names = TRUE)
write_path <- "D:/Jordan_Perentie/Labelled_Chunked_accel"

for (file in chunked_raw) {
  raw <- fread(file)
  colnames(raw) <- c("Timestamp", "Accel_X", "Accel_Y", "Accel_Z")
  
  # Get the name from the filename
  file_name <- basename(file)
  name <- ifelse(grepl("abigail", file_name, ignore.case = TRUE), "Abigail",
                 ifelse(grepl("bubbles", file_name, ignore.case = TRUE), "Bubbles",
                        ifelse(grepl("eric", file_name, ignore.case = TRUE), "Eric", NA)))
  
  # Initialise empty dataframe for storing predictions
  file_predictions <- data.frame()
  
  for (behaviour in behaviours) {
    # Extract the right row for the optimal design
    optimal_design <- optimal %>% filter(targetActivity == behaviour)
    
    # Define the variables
    window_length <- as.numeric(optimal_design$window_length)
    overlap_percent <- as.numeric(optimal_design$overlap_percent)
    down_Hz <- as.numeric(optimal_design$down_Hz)
    feature_normalisation <- as.character(optimal_design$feature_normalisation)
    nu <- as.numeric(optimal_design$nu)
    kernel <- as.character(optimal_design$kernel)
    features_list <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA", "entropy", "zero", "auto")
    
    # Process data
    raw_processed <- raw %>%
      process_data(features_list, window_length, overlap_percent, down_Hz, feature_normalisation) %>%
      na.omit() %>%
      select(-c(zero_Accel_Y, zero_Accel_Z))
    
    # Extract components
    raw_times <- raw_processed %>% select(Timestamp)
    raw_predictors <- raw_processed %>% select(-Timestamp)
    
    # Apply SVMs
    if (behaviour == "Locomotion") {
      predictions <- apply_model(raw_predictors, Locomotion_SVM, "Locomotion")
      predictions <- as.data.frame(predictions)
      colnames(predictions) <- c("Locomotion_prediction")
    } else {
      predictions <- apply_model(raw_predictors, Inactive_SVM, "Inactive")
      predictions <- as.data.frame(predictions)
      colnames(predictions) <- c("Inactive_prediction")
    }
    
    energy <- raw_predictors %>% select(21:24)
    
    # Combine these
    if (nrow(file_predictions) == 0) {
      file_predictions <- cbind(raw_times, name, energy, predictions)
    } else {
      file_predictions <- merge(file_predictions, cbind(raw_times, predictions), by = "Timestamp", all = TRUE)
    }
  }
  
  # Write this to the folder
  output_file <- file.path(write_path, paste0("Labelled_", file_name))
  fwrite(file_predictions, output_file)
}