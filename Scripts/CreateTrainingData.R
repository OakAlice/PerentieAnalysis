# Function for stitching individual files into a single formatted dataframe
matlab_to_training <- function(base_path, activity_key_path) {
  files <- list.files(file.path(base_path, "Labelled_Data/Cleaned"), recursive = TRUE, full.names = TRUE)
  
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




# Split these back out into constituant files
labelled_data %>%
  group_by(ID, Activity) %>%
  group_split() %>%
  walk(~ {
    # Create a filename using ID and Activity
    file_name <- paste0(base_path, "/", unique(.x$ID), "_", unique(.x$Activity), ".csv")
    # Remove columns 'activity', 'Activity', and 'ID'
    data_to_save <- .x %>% select(-activity, -Activity, -ID)
    # Write the data to a CSV file without column headers
    write.table(data_to_save, file = file_name, sep = ",", col.names = FALSE, row.names = FALSE)
  })

# and then load these files back in
data_list <- list.files(file.path(base_path, "Labelled_Data/Cleaned"), full.names = TRUE, recursive = TRUE)
relabelled_data <- process_files(data_list, base_path)
relabelled_data <- relabelled_data %>%
  rename(Timestamp = time,
         Accel_X = x,
         Accel_Y = y,
         Accel_Z = z,
         Activity = activity)
fwrite(relabelled_data, file.path(base_path, "DiCicco_Perentie_Labelled.csv"))

# stitch them back together with various logic
# Function to process files
relabelled_data <- data.frame()
process_files <- function(data_list, base_path) {
  for (file in data_list) {
    # Get the base filename
    base_filename <- basename(file)
    ID <- basename(dirname(file))
    
    # Read the file
    if (grepl("_retagged", base_filename)) {
      # Read the CSV file with headers and select specific columns
      data <- read.csv(file, header = TRUE) %>%
        select(time, x, y, z, activity) %>%
        mutate(ID = ID)
    } else {
      # Read the CSV file without headers and assign column names
      data <- read.csv(file, header = FALSE)
      colnames(data) <- c("time", "x", "y", "z")
      
      # Determine the Activity based on the base filename
      if (grepl("Locomotion", base_filename, ignore.case = TRUE)) {
        activity_label <- "Locomotion"
      } else if (grepl("Other", base_filename, ignore.case = TRUE)) {
        activity_label <- "Other"
      } else if (grepl("Inactive", base_filename, ignore.case = TRUE)) {
        activity_label <- "Inactive"
      } else {
        activity_label <- NA
      }
      
      # Add the Activity column
      data <- data %>% mutate(activity = activity_label) %>%
        mutate(ID = ID)
    }

    relabelled_data <- rbind(relabelled_data, data)
  }
  return(relabelled_data)
}









