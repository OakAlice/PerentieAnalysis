## OPTIMAL MODEL, CREATE AND TEST ON HOLD-OUT DATA ####
# train the model
train_model <- function(trDat, 
                        model_architecture,
                        num_trees = NULL, # RF
                        kernel = NULL, cost = NULL, # SVM
                        hidden = NULL # MLP
) {
  
  predictors <- trDat %>% 
    ungroup() %>%
    select(-any_of(c("Activity", "ID"))) %>%
    mutate(across(everything(), as.numeric))
  
  target <- factor(trDat$Activity)
  numberOfClasses <- length(unique(target))
  
  if (model_architecture == "RF"){
    model <- randomForest::randomForest(x = predictors, y = target, ntree = num_trees, importance = TRUE)
    
  } else if (model_architecture == "SVM"){
    model <- e1071::svm(x = predictors, y = target, kernel = kernel, cost = cost)
    
  } else if (model_architecture == "kNN"){
    skip() # does it all in one step in the test section
    
  } else if (model_architecture == "DT") {
    model <- rpart::rpart(formula = target ~ ., data = predictors)
    
  } else if (model_architecture == "XGB") {
    skip()
    
  } else if (model_architecture == "MLP") {
    #model <- neuralnet::neuralnet(target ~ ., data = predictors, hidden = hidden, linear.output = FALSE)
    # taking too long, haven't tested
    skip()
  }
  
  return(model)
}



# partition into folds
partition_data <- function(balanced_data, folds, training_percentage, stratification){
  
  Partition_data <- balanced_data %>%
    arrange(ID) %>%
    group_by(ID) %>%
    mutate(partition = ceiling(row_number() / n() * folds),
           unique_code = paste0(ID, "_", partition)) # Create unique code
  
  
  training_folds <- floor(training_percentage * folds)
  
  training <- Partition_data %>% 
    group_by(ID) %>%
    filter(partition %in% sample(1:folds, training_folds)) %>%  
    ungroup()
  
  validation <- anti_join(Partition_data, training, by = "unique_code") %>% select(-c("unique_code"))
  
  training <- training %>% select(-c("unique_code"))
  
  return(list(training = training,
              validation = validation))
}


# generate the optimal model
generate_optimal_model <- function(otherDat, 
                                   down_Hz, 
                                   window_length, 
                                   overlap_percent, 
                                   featuresList, 
                                   threshold, 
                                   stratification,
                                   feature_normalisation,
                                   folds, 
                                   training_percentage, 
                                   model_architecture, 
                                   trees_number) {
  
  processed_data <- process_data(otherDat, featuresList, window_length, overlap_percent, down_Hz, feature_normalisation)
  
  balanced_data <- balance_data(processed_data, threshold)
  
  partitioned_data <- partition_data(balanced_data, folds, training_percentage, stratification)
  trDat <- na.omit(partitioned_data$training)
  trDat <- trDat %>% select(-partition, -ID)
  
  if (model_architecture == "RF"){
    trained_model <- train_model(trDat, trees_number)
  } else if (model_architecture == "SVM"){
    print("Still have to do that lol")
  }
  
  return(trained_model)
}

## Plots and stuff for the final version 
plot_confusion <- function(confusion_matrix) {
  # Convert confusion matrix to data frame
  conf_df <- as.data.frame(as.table(confusion_matrix))
  colnames(conf_df) <- c("Actual", "Predicted", "Count")
  conf_df$Count[conf_df$Count == 0] <- NA
  conf_df$Result <- conf_df$Actual == conf_df$Predicted
  conf_df$Result[is.na(conf_df$Count)] <- NA
  
  # Normalize counts within each actual behavior
  conf_df <- conf_df %>%
    group_by(Actual) %>%
    mutate(total_count = sum(Count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Count_norm = Count / total_count)
  
  # Create confusion matrix plot
  confusion_plot <- ggplot(conf_df, aes(y = Predicted, x = Actual, fill = Result, alpha = Count_norm)) +
    geom_tile(color = "white", linewidth = 1) +
    scale_fill_manual(values = c("FALSE" = "salmon", "TRUE" = "steelblue", "NA" = "white"), na.value = "white") +
    scale_alpha_continuous(range = c(0.1, 1)) +  
    theme_minimal() +
    labs(x = "Actual",
         y = "Predicted",
         fill = "Result",
         alpha = "Count") +
    guides(alpha = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(confusion_plot)
}


plot_stacked <- function(confusion_matrix, test_predictions, test_actual) {
  # Convert confusion matrix to data frame
  confusion_df <- as.data.frame(as.table(confusion_matrix))
  names(confusion_df) <- c("Actual", "Predicted", "Count")
  
  # Plot
  custom_palette <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", 
                      "#ffd92f", "#e5c494", "#b3b3b3", "#ff69b4", "#ba55d3", 
                      "#cd5c5c", "#ffa07a", "#f08080", "#4682b4","#FF6347",
                      "#3A7C75", "#00ff00", "#87CEEB", "#F4A460")
  alternative_plot <- ggplot(confusion_df, aes(x = Predicted, y = Count, fill = Actual)) +
    geom_bar(stat = "identity") +
    labs(x = "Predicted Activity",
         y = "Count") +
    scale_fill_manual(values = custom_palette) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  # and then a visualisation of what was lost into the NAs
  dataframe <- data.frame(Predicted = as.character(test_predictions), Actual = as.character(test_actual))
  NAdataframe_counts <- dataframe %>%
    filter(is.na(Predicted)) %>%
    count(Actual) %>%
    rename(Frequency = n)
  
  # Plot the bar graph
  NAplot <- ggplot(NAdataframe_counts, aes(x = Actual, y = Frequency, fill = Actual)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = custom_palette) +
    labs(x = "Actual behaviour classified as NA", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 1))
  
  return(list(stacked_plot = alternative_plot, 
              Na_leftovers = NAplot))
}


display_metrics <- function(confusion_matrix) {
  TP <- diag(confusion_matrix)
  TN <- sum(confusion_matrix) - rowSums(confusion_matrix) - colSums(confusion_matrix) + 2 * diag(confusion_matrix)
  FP <- colSums(confusion_matrix) - TP
  FN <- rowSums(confusion_matrix) - TP
  
  accuracy <- sum(TP) / sum(confusion_matrix)
  recall <- TP / (TP + FN)
  precision <- TP / (TP + FP)
  F1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  
  # Calculate specificity for all classes
  num_classes <- ncol(confusion_matrix)
  specificity_all <- numeric(num_classes)
  for (i in 1:num_classes) {
    TN_class <- sum(confusion_matrix) - sum(confusion_matrix[i, ]) - sum(confusion_matrix[, i]) + confusion_matrix[i, i]
    FP_class <- sum(confusion_matrix[, i]) - confusion_matrix[i, i]
    specificity_all[i] <- TN_class / (TN_class + FP_class)
  }
  specificity <- mean(specificity_all)
  
  metrics <- data.frame(General_accuracy = accuracy, Mean_recall = mean(recall), Mean_precision = mean(precision), General_specificity = specificity, Mean_F1 = mean(F1), MCC = mean(MCC))
  
  return(metrics)
}


test_optimal_model <- function(model, tstDat2, probability_report, probability_threshold){
  # predict onto the testing data
  test_actual <- factor(tstDat2$Activity) # extract the actual names 
  test_predictors <- tstDat2 %>% # extract the predictors
    ungroup() %>%
    select(-any_of(c("Activity", "ID")))
  
  if (probability_report == FALSE){
    
    test_predictions <- predict(model, test_predictors) # predict
    
    # convert to chanracter and change formatting
    test_predictions <- as.character(test_predictions)
    test_predictions <- gsub("[/\\.]", " ", test_predictions)
    
    test_predictions <- as.factor(test_predictions)
    
  }else{ # probability reports then get handled differently
    
    test_predictions <- predict(model, newdata = test_predictors, type = "prob")
    # this gives me the probability of all classes
    
    test_predictions <- data.frame(test_predictions)
    test_predictions <- test_predictions %>%
      mutate(likelyActivity = apply(test_predictions[, -1], 1, function(x) {
        if (all(is.na(x))) {
          NA
        } else {
          max_index <- which.max(x)
          if (!is.na(max_index) && x[max_index] > probability_threshold) {
            names(x)[max_index]
          } else {
            NA
          }
        }
      }))
    
    # Merge predicted probabilities with actual behaviors
    test_predictions <- test_predictions %>%
      mutate(likelyActivity = gsub("\\.", " ", likelyActivity))
    test_predictions <- as.factor(test_predictions$likelyActivity)
  }
  
  # extract the actual classes and change their formatting to match
  test_actual <- as.character(tstDat2$Activity)
  #test_actual <- gsub("[/\\.]", " ", test_actual)
  test_actual <- as.factor(test_actual)
  
  unique_classes <- union(levels(test_actual), levels(test_predictions))
  
  confusion_matrix <- table(factor(test_actual, levels = unique_classes), 
                            factor(test_predictions, levels = unique_classes))
  
  return(test_outputs = list(confusion_matrix = confusion_matrix,
                             test_predictions = test_predictions,
                             test_actual = test_actual))
}


## apply the optimal settings and verify with graphs, etc.
verify_optimal_results <- function(tstDat2, optimal_trained_model, test_type, probability_report, probability_threshold) {
  
  # extract the test data type (for controlled comparisons)
  if(test_type == "random") {
    tstDat2$Activity <- tstDat2$Activity[sample(nrow(tstDat2))]
  }
  
  # Test model
  test_output <- test_optimal_model(optimal_trained_model, tstDat2, probability_report, probability_threshold)
  confusion_matrix <- test_output$confusion_matrix
  test_predictions <- test_output$test_predictions
  test_actual <- test_output$test_actual
  
  # try to replace the above with these previously coded functions if possible
  #metrics_df <- evaluate_model(test_predictions, valDat, target_behaviours = NULL)
  #summary_df <- save_model_results(metrics_df, num_individuals, num_behs, 
  #down_Hz, window, overlap, feature_normalisation, feature_selection, features_list, 
  #balancing_thresholds, ntrees)
  
  
  ## make the plots from the confusion matrix
  confusion_plot <- plot_confusion(confusion_matrix)
  stacked <- plot_stacked(confusion_matrix, test_predictions, test_actual)
  metrics <- display_metrics(confusion_matrix)
  
  testReturns <- list(confusion_matrix = confusion_matrix, 
                      confusion_plot = confusion_plot, 
                      stacked_plot = stacked$stacked_plot,
                      NA_loss_plot = stacked$Na_leftovers, 
                      metrics = metrics)
}