##Function that saves the scale parameters for wind power
#' @param training_data The data frame containing the training data
#' @return Returns the maximum and minimum used to scale the wind_power
#' @export


save_power_scale_params <- function(training_data) {
  
  wind_p <- training_data$wind_power
  max_s <- max(wind_p)
  min_s <- min(wind_p)
  
  saved_params <- data.frame(max_s,min_s)
  colnames(saved_params) <- c("max", "min")
  
  return(saved_params)

}

##Function that saves the scale parameters for wind power
#' @param ensemble_list A list of power ensemble predictions that are scaled
#' @param horizons A list of horizons to be used for the forecast
#' @param scale_params A dataframe containing the saved scale parameters
#' @param min Optional instead of the above dataframe the minium can be entered per hand
#' @param max Optional instead of the above dataframe the maximum can be entered per hand
#' @return Returns a list of power ensemble predictions rescaled to the appropriate level
#' @export

rescale_power <- function(ensemble_list, horizons, scale_params = NULL, min = NULL, max = NULL) {
  scale.list <- list()
  
  if(!is.null(scale_params)) { 
    max <- scale_params$max
    min <- scale_params$min
  }
  
  
  rescale_f <- function(val) {
    
    return(val*(max-min) + min)
    
  }
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    new_df <- ensemble_list[[hr]]
    new_df[,2:53] <- apply(new_df[,2:53], 2, rescale_f)
    scale.list[[hr]] <- new_df
  }
  return(scale.list)
}
