##Function to prepare the data needed for training the prediction models
#' @param wp_name The name of the wind power csv to be loaded
#' @param obs_name The name of the observation data csv to be loaded
#' @param weather_parameters The weather parameters (as named in the csv files) to be selected 
#' @param file_path The file path to where the data is stored
#' @param train_time Time used to filter the end of the training dataset
#' @return 
#' @export


prepare_training_data <- function(wp_name, obs_name, weather_params, file_path, train_time) {
  #Load packages
  require(lubridate)
  require(tidyverse)
  require(dplyr)
  
  #Add file path to names
  wp_name <- paste(file_path, "/", wp_name, sep = "")
  obs_name <- paste(file_path, "/", obs_name, sep = "")
  
  #Load data required
  wp_data <- read_csv(wp_name)
  obs_data <- read_csv(obs_name)
  
  #Filter the data to remove extra dates at end
  train_time <- as.POSIXct(train_time, tz = "UTC")
  wp_data <- wp_data %>% filter(time <= train_time)
  obs_data <- obs_data %>% filter(time <= train_time)
  
  #Create dataframe to be returned
  training_data <- wp_data
  training_data[,"is_origin"] <- obs_data$is_origin
  
  #Add in weather observations
  for (w in weather_params) {
    training_data[,w] <- obs_data[,w]
  }
  
  training_data[,"speed3"] <- (obs_data[,"speed"])^3
  quantile90 <- as.numeric(quantile(unlist(obs_data[,"speed"]), probs = c(.9)))
  training_data[,"critical"] <- ifelse(obs_data[,"speed"]>quantile90, 1, 0)
  
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
  
  return(training_data)
}


