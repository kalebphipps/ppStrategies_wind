##Function that generates an ensemble of predictions based on ensemble weather data
#' @param prediction_data The dataframe containing the ensemble data to generate predictions
#' @param neural_models A list of the trained neural
#' @param horizons The forecast horizons for which predictions should be generated
#' @return A list of data frames with an ensemble of forecasts for each forecast horizon
#' @export


predict_neural_ensembles <- function(prediction_data, neural_models, horizons) {
  require(dplyr)
  
  #List to be returned
  list.p <- list()
  
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    #Select reference data (time, observations and origins)
    ref_df <- prediction_data[[1]]
    origins <- ref_df %>% filter(horizon == h)
    ref_df <- ref_df %>% filter(horizon == h)
    
    #thisIsSweden <- FALSE
    
    #if(unique(origins$horizon == 24)) {
    ref_df <- ref_df[-1,]
    origins <- origins[-length(origins$time),]
    #  thisIsSweden <- TRUE
    #} 
    
    origins <- origins$wind_power
    times <- ref_df$time
    obs <- ref_df$wind_power
    
    #Save data frame for results
    saved_df <- as.data.frame(matrix(0, nrow = length(times), ncol = 53))
    names <- c("time","obs", ensMemberNames)
    colnames(saved_df) <- names
    saved_df$time <- times
    saved_df$obs <- obs
    
    #Go through each ensemble
    for (j in 1:51) {
      
      ens_name <- paste("ens",j,sep = "")
      
      #Generate dataframe for predictions
      predict_df <- prediction_data[[j]]
      predict_df <- predict_df %>% filter(horizon == h)
      
      #if(thisIsSweden){
      predict_df <- predict_df[-1,]
      #}
      
      predict_df[,"origins"] <- origins
      predictions <- predict(neural_models[[hr]], newdata = predict_df)
      predictions <- ifelse(predictions <= 0, 0, predictions)
      saved_df[,ens_name] <- as.numeric(predictions)
      
    }
    
    list.p[[hr]] <- saved_df
    
  }
  return(list.p)
}