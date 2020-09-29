##Function to convert the EMOS weather calibration/ECC structure to a list structure used for forecasts
#' @param weather_ensembles A list of the calibrated weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param horizons A vector of the forecast horizons that need to be calibrated
#' @param isBenchmark A boolean to determine whether the benchmark dataset is being used
#' @return A list of data frames in the correct structure for wind power forecast models
#' @export


convert_to_forecast_structure <- function(weather_ensembles, weather_parameters, horizons, isBenchmark = FALSE) {
  new_list <- list()
  
  for (w in weather_parameters) {
    
    for (h in horizons) {
      hr <- paste("h",h,sep = "")
      weather_ensembles[[w]][[hr]][,"horizon"] <- h
      if(isBenchmark) {
        if(h == 0) {
          weather_ensembles[[w]][[hr]][,"is_origin"] <- 1
        } else {
          weather_ensembles[[w]][[hr]][,"is_origin"] <- 0
        }
      } else if(!isBenchmark) {
        if(h == 24) {
          weather_ensembles[[w]][[hr]][,"is_origin"] <- 1
        } else {
          weather_ensembles[[w]][[hr]][,"is_origin"] <- 0
        }
      }
    }
  }
  
  for (w in weather_parameters) {
    
    new_data <- weather_ensembles[[w]]
    save_df <- NULL
    
    for (h in horizons) {
      hr <- paste("h",h,sep = "")
      
      df <- new_data[[hr]]
      save_df <- rbind(save_df, df)
      
    }
    
    new_list[[w]] <- save_df
  }
  
  final_list <- list()
  
  for (w in weather_parameters) {
    df <- new_list[[w]]
    df <- df[order(df$time, -df$horizon),]
    final_list[[w]] <- df
  }
  return(final_list)
}