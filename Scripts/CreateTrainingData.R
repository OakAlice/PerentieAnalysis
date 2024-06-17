# Function for stitching individual files into a single formatted dataframe
matlab_to_training <- function(base_path, activity_key_path) {
  files <- list.files(file.path(base_path, "Labelled_Data"), recursive = TRUE, full.names = TRUE)
  
  training_data <- do.call(rbind, lapply(files, function(file) {
    ID <- basename(dirname(file)) # Extract the folder name
    ID <- gsub("_tagged$", "", ID)
    file_extension <- file_ext(file)
    
    if (file_extension == "csv") { # Newer version of Sync Station
      data <- read.csv(file, header = TRUE)
      data <- data %>%
        rename(Timestamp = time,
               Accel_X = x,
               Accel_Y = y,
               Accel_Z = z,
               number = behnum)
    } else { # For txt files, earlier version
      data <- read.table(file, header = FALSE)
      data <- data %>%
        rename(Timestamp = V1,
               Accel_X = V2,
               Accel_Y = V3,
               Accel_Z = V4,
               number = V5)
    }
    
    data$ID <- ID
    
    return(data)  # Return data at each iteration
  }))
  
  activity_key <- read.csv(activity_key_path)
  training_data <- left_join(training_data, activity_key, by = "number")
  training_data <- select(training_data, -number)
  
  return(training_data)
}
