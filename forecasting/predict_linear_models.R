##Function that generates an ensemble of predictions based on ensemble weather data
#' @param prediction_data The dataframe containing the ensemble data to generate predictions
#' @param linear_models A list of the trained linear models
#' @param onlyPositive Boolean indicating whether only positive regression coefficients should be allowed
#' @param horizons The forecast horizons for which predictions should be generated
#' @param reg_params The regression parameters (needed only if a positive model is being used)
#' @return A list of data frames with an ensemble of forecasts for each forecast horizon
#' @export


predict_ensemble_model <- function(prediction_data, linear_models, onlyPositive, horizons,reg_params=NULL) {
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
    scaledData <- FALSE
    
    #if(unique(origins$horizon == 24)) {
    ref_df <- ref_df[-1,]
    origins <- origins[-length(origins$time),]
    #  thisIsSweden <- TRUE
    #} 
    
    origins <- origins$wind_power
    times <- ref_df$time
    obs <- ref_df$wind_power
    
    if(max(origins) <= 2) {
      scaledData <- TRUE
    }
    
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
      
      if(!onlyPositive) {
        #Case with postive and negative coefficients in the model
        predictions <- predict(linear_models[[hr]], predict_df)
       
        
        if(scaledData) {
          predictions <- ifelse(predictions <= 0, 0, predictions)
        }else if(!scaledData) {
          predictions <- ifelse(predictions <= 0, 0.01, predictions)
        }
      
       
        saved_df[,ens_name] <- as.numeric(predictions)
        
      }else if(onlyPositive) {
        #Case with only positive coefficients in the model
        A <- as.matrix(predict_df[,reg_params])
        coefs <- coef(linear_models[[hr]])
        predictions <- coefs%*%t(A)
        saved_df[,ens_name] <- as.numeric(predictions)
      }
    }
    
    list.p[[hr]] <- saved_df
  
  }
  return(list.p)
}


##Function that generates a single prediction since only dummy data is used
#' @param prediction_data The dataframe containing the data to generate predictions
#' @param linear_models A list of the trained linear models
#' @param onlyPositive Boolean indicating whether only positive regression coefficients should be allowed
#' @param includeIntercept Boolean indicating whether the intercept should be included or not. NOTE: It
#'                         is currently impossible to include the intercept when training a strictly positive linear model
#' @param horizons The forecast horizons for which predictions should be generated
#' @param reg_params The regression parameters (needed only if a positive model is being used)
#' @return A list of data frames with single forecasts for each forecast horizon
#' @export


predict_single_model <- function(prediction_data, linear_models, onlyPositive, includeIntercept, horizons, reg_params) {
  require(dplyr)
  
  #List to be returned
  list.p <- list()
  
  #select only one element out of the prediction_data list, since weather data is not included
  prediction_data <- prediction_data[[1]]
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    #Select reference data (time, observations and origins)
    ref_df <- prediction_data
    origins <- ref_df %>% filter(is_origin == 1)
    ref_df <- ref_df %>% filter(horizon == h)
    
    scaledData <- FALSE
    
    if(unique(origins$horizon == 24)) {
      ref_df <- ref_df[-1,]
      origins <- origins[-length(origins$time),]
    } 
    
    origins <- origins$wind_power
    times <- ref_df$time
    obs <- ref_df$wind_power
    
    if(max(origins) <= 2) {
      scaledData <- TRUE
    }
    
    
    #Save data frame for results
    saved_df <- as.data.frame(matrix(0, nrow = length(times), ncol = 3))
    names <- c("time","obs", "prediction")
    colnames(saved_df) <- names
    saved_df$time <- times
    saved_df$obs <- obs
    
    #Prediction
    ref_df[,"origins"] <- origins
    
    if(!onlyPositive) {
      #Case where negative coefficients are allowed
      predictions <- predict(linear_models[[hr]], ref_df)
      
      if(scaledData) {
        predictions <- ifelse(predictions <= 0, 0, predictions)
      }else if(!scaledData) { 
        predictions <- ifelse(predictions <= 0, 0, predictions)
      }
      
      saved_df[,"prediction"] <- as.numeric(predictions)
      
    } else if(onlyPositive) {
      #Case where only positive coefficients are allowed
      A <- as.matrix(ref_data[,reg_params])
      coefs <- coef(linear_models[[hr]])
      predictions <- coefs%*%t(A)
      saved_df[,"prediction"] <- as.numeric(predictions)
    }
    
    list.p[[hr]] <- saved_df
    
  }
  
  return(list.p)
}