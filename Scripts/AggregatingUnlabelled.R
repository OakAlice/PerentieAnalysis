# Aggregating predictions on the unseen data

base_path <- "R:/FSHEE/Science/Unsupervised-Accel/Other_Projects/Jordan_Perentie/PerentieAnalysis"

prediction_files <- list.files(base_path, "Labelled_Chunked_accel")

# find the most frequent non-NA value
most_frequent <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(names(sort(table(x), decreasing = TRUE))[1])
  }
}

for (file in prediction_files){
  # convert the time to an averaged minute
  data <- fread(file) %>% 
    mutate(Time = as.POSIXct((Timestamp - 719529)*86400, origin = "1970-01-01", tz = "UTC")) %>%
    mutate(Minute = round_date(Time, unit = "minute"))
  
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
}