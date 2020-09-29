##Function that scales the data to either interval [0,1] or normalises it
#' @param ensemble_list The ensemble list
#' @param weather_parameters The weather parameters (as named in the csv files) to be selected 
#' @param normalise_l A list of parameters that should be normalised
#' @param scale_l A list of parameters that should be scaled
#' @return The ensemble list structure now scaled
#' @export


scale_structured_data <- function(ensemble_list, weather_parameters, normalise_l, scale_l) {
  require(BBmisc)
  
  #Generate ensembleMemberNames
  ensMemberNames <- NULL
  for (e in 1:51) {
    ensMemberNames[e] <- paste0("ens",e)
  }
  
  #Include observations to be scaled
  all_names <- c(ensMemberNames,"obs")
  
  #The variables that are not scaled
  take_vars <- c("time", "horizon", "is_origin")
  
  r_list <- list()
  
  
  for (w in weather_parameters) {
        
    w_ens <- ensemble_list[[w]]
    new_df <- data.frame(w_ens[,take_vars])
    
    if(w %in% normalise_l) {
      for (i in all_names) {
        temp <- normalize(w_ens[,i], method = "standardize")
        new_df[,i] <- temp
      }
    } else if(w %in% scale_l) {
      for (i in all_names) {
        temp <- normalize(w_ens[,i], method = "range")
        new_df[,i] <- temp
      }
    }
    
    r_list[[w]] <- new_df
  }
  return(r_list)
}