##Function to create a list structure that enables easier processing of the data
#' @param ens_name The name of the csv file containing the ensembles
#' @param control_name The name of the csv file containing the control forecasts
#' @param obs_name The name of the csv file containing the observations (renalysis data)
#' @param filter_time The date used to truncate the weather data
#' @param w_parameters The weather parameters (as named in the csv files) to be selected 
#' @return A list structure in which each list element contains a data frame of observations for
#'         a selected weather parameter. All ensembles including control forecast and the observations
#'         are included.
#' @export

create_correct_structure <- function(ens_name, control_name, obs_name, file_path, filter_time, w_parameters) {
  #Load required packages
  require(tidyverse)
  require(dplyr)
  
  #Add file path to names
  ens_name <- paste(file_path, "/", ens_name, sep = "")
  control_name <- paste(file_path, "/", control_name, sep = "")
  obs_name <- paste(file_path, "/", obs_name, sep = "")
  
  #Load data required
  ens_data <- read_csv(ens_name)
  control_data <- read_csv(control_name)
  obs_data <- read_csv(obs_name)
  
  #Filter the data to remove extra dates at end
  filter_time <- as.POSIXct(filter_time, tz = "UTC")
  ens_data <- ens_data %>% filter(time <= filter_time)
  control_data <- control_data %>% filter(time <= filter_time)
  obs_data <- obs_data %>% filter(time <= filter_time)
  
  #Sort the data so that it is in chronological order
  ens_data <- ens_data[order(ens_data$time, -ens_data$horizon),]
  control_data <- control_data[order(control_data$time, -control_data$horizon),]
  obs_data <- obs_data[order(obs_data$time, -obs_data$horizon),]
  
  #Generate ensemble names for data frame
  ensMemberNames <- NULL
  for (e in 1:51) {
    ensMemberNames[e] <- paste0("ens",e)
  }
  
  #Create the final names to be used for the data frame headings
  final_names <- c("time", "horizon", "is_origin", ensMemberNames, "obs")
  
  #Empty list that will be returned
  ensemble_list <- list()
  
  #Functions used to populate the list
  ensemble_list <- create_the_list(ens_data, w_parameters)
  ensemble_list <- add_in_ensembles(ensemble_list, ens_data, w_parameters, ensMemberNames)
  ensemble_list <- add_in_control(ensemble_list, control_data, w_parameters, ensMemberNames)
  ensemble_list <- add_in_observations(ensemble_list, obs_data, w_parameters)
  
  #Correct the names
  for (w in w_parameters) {
    colnames(ensemble_list[[w]]) <- final_names
  }
  
  return(ensemble_list)
  
}



##Function to initially populate the list
#' @param ens_data The data frame containing the ensembles
#' @param w_parameters The weather parameters (as named in the csv files) to be selected 
#' @return A list structure which has the correct times, horizon and origin boolean for
#'         each weather variable
#' @export
create_the_list <- function(ens_data, w_params) {
  #Temporary data frame
  temp_d <- ens_data %>% filter(number==1)
  #Select variables that are the same for each weather parameter and save in data frame
  start_df <- temp_d[,c("time", "horizon", "is_origin")]
  e_list <- list()
  for(j in w_params) {
    e_list[[j]] <- start_df
  }
  return(e_list)
}


##Function that adds in the ensemble data to the list
#' @param ens_list The list generated through the create_the_list function
#' @param ens_data The data frame containing the ensembles
#' @param w_parameters The weather parameters (as named in the csv files) to be selected 
#' @param ensNames The ensemble names
#' @return A list structure in which now includes the ensemble data for each weather variable
#' @export
add_in_ensembles <- function(ens_list, ens_data, w_params, ensNames) {
  for (i in 1:50) {
    temp_d <- ens_data %>% filter(number==i)
    for (j in w_params) {
      ens_list[[j]][,ensNames[i]] <- temp_d[,j]
    }
  }
  return(ens_list)
}


##Function that adds in the control forecast data to the list
#' @param ens_list The list generated through the previous functions
#' @param control_data The data frame containing the control forecasts
#' @param w_parameters The weather parameters (as named in the csv files) to be selected 
#' @param ensNames The ensemble names
#' @return A list structure in which now includes the control forecasts (ens51) for each weather variable
#' @export
add_in_control <- function(ens_list, control_data, w_params, ensNames) {
  for (j in w_params) {
    ens_list[[j]][,ensNames[51]] <- control_data[,j]
  }
  return(ens_list)
}


##Function that adds in the observation data to the list
#' @param ens_list The list generated through the previous functions
#' @param obs_data The data frame containing the observations
#' @param w_parameters The weather parameters (as named in the csv files) to be selected 
#' @return A list structure in which now includes the observations for each weather variable
#' @export
add_in_observations <- function(ens_list, obs_data, w_params) {
  for (j in w_params) {
    ens_list[[j]][,"obs"] <- obs_data[,j]
  }
  return(ens_list)
}