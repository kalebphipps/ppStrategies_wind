##Function to prepare the data needed for predicting on the ensemble data
#' @param wp_name The name of the wind power csv to be loaded
#' @param ensemble_data The list containing all the ensemble data
#' @param weather_parameters The weather parameters (as named in the csv files) to be selected 
#' @param file_path The file path to where the data is stored
#' @param start_time The date used to determine the start of the dataset for prediction
#' @param end_time The date used to determine the end of the dataset for prediction
#' @param toScale Boolean indicating whether the wind_power should be scaled
#' @return A list containing a dataframe of data for each of the 51 ensembles.
#' @export


prepare_ensemble_data <- function(wp_name, ensemble_data, weather_params, file_path, start_time, end_time, toScale) {
  #Load packages
  require(lubridate)
  require(tidyverse)
  require(dplyr)
  
  f.list <- list()
  
  #Add file path to names
  wp_name <- paste(file_path, "/", wp_name, sep = "")
  
  #Load data required
  wp_data <- read_csv(wp_name)
  
  #Create dataframe to be returned
  training_data <- wp_data
  
  #Scale if wished
  if(toScale) {
    training_data$wind_power <- normalize(training_data$wind_power, method = "range")
  }
  
  #Filter Data
  start_time <- as.POSIXct(start_time, tz = "UTC")
  end_time <- as.POSIXct(end_time, tz = "UTC")
  training_data <- training_data %>% filter(time >= start_time)
  training_data <- training_data %>% filter(time <= end_time)
  for (w in weather_params) {
    ensemble_data[[w]] <- ensemble_data[[w]] %>% filter(time >= start_time)
    ensemble_data[[w]] <- ensemble_data[[w]] %>% filter(time <= end_time)
  }
  
  #Add in Origins
  training_data[,"is_origin"] <- ensemble_data[[1]][,"is_origin"]
  
  #Add in the dummy variables
  the_year <- year(training_data$time)
  unique_year <- unique(the_year)
  for (i in 1:length(unique_year)) {
    training_data[,as.character(unique_year[i])] <- ifelse(the_year == unique_year[i], 1, 0)
  }
  
  the_months <- month(training_data$time)
  
  training_data[,"feb"] <- ifelse(the_months == 2, 1, 0)
  training_data[,"mar"] <- ifelse(the_months == 3, 1, 0)
  training_data[,"apr"] <- ifelse(the_months == 4, 1, 0)
  training_data[,"may"] <- ifelse(the_months == 5, 1, 0)
  training_data[,"jun"] <- ifelse(the_months == 6, 1, 0)
  training_data[,"jul"] <- ifelse(the_months == 7, 1, 0)
  training_data[,"aug"] <- ifelse(the_months == 8, 1, 0)
  training_data[,"sep"] <- ifelse(the_months == 9, 1, 0)
  training_data[,"oct"] <- ifelse(the_months == 10, 1, 0)
  training_data[,"nov"] <- ifelse(the_months == 11, 1, 0)
  training_data[,"dec"] <- ifelse(the_months == 12, 1, 0)
  
  training_data[,"spring"] <- ifelse(the_months == 3 | the_months == 4 | the_months == 5, 1, 0)
  training_data[,"summer"] <- ifelse(the_months == 6 | the_months == 7 | the_months == 8, 1, 0)
  training_data[,"autumn"] <- ifelse(the_months == 9 | the_months == 10 | the_months == 11, 1, 0)
  
  for (j in 1:51) {
    
    ens_name <- paste("ens",j,sep = "")
    
    #Restore
    train_data <- training_data
    
    for (w in weather_params) {
      train_data[,w] <- ensemble_data[[w]][,ens_name]
    }
    
    train_data[,"speed3"] <- (ensemble_data[["speed"]][,ens_name])^3
    
    quantile90 <- as.numeric(quantile(unlist(ensemble_data[["speed"]][,ens_name]), probs = c(.9)))
    train_data[,"critical"] <- ifelse(ensemble_data[["speed"]][,ens_name]>quantile90, 1, 0)
    
    f.list[[j]] <- train_data
  }
  return(f.list)
}