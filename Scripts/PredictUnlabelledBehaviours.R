# Predicting behaviours

predict_behaviours<- function(optimal_trained_model, unlabelled_data, down_Hz = 50, window_length = 1, 
                              overlap_percent = 0, features_list, feature_normalisation = FALSE){
  
  list(unlabelled_data)
  for (file in unlabelled_data){
    data <- fread(file)
    processed_data <- process_data(data, features_list, window_length = 1, 
                                   overlap_percent = 0, 50, feature_normalisation = FALSE) # second last one is down_Hz
    
    
    test_predictions <- predict(model, test_predictors)
    z  }
  
  
  
  
}