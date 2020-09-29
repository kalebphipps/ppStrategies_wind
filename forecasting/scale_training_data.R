##Function that scales the training data to either interval [0,1] or normalises it
#' @param training_data The data frame containing the training data
#' @param scale_parameters The parameters to be scaled
#' @param take_l The parameters that should not be touched (dummies etc.)
#' @param normalise_l A list of parameters that should be normalised
#' @param scale_l A list of parameters that should be scaled
#' @return The ensemble list structure now scaled
#' @export



scale_training_data <- function(training_data, scale_parameters, take_l, normalise_l, scale_l) {
  require(BBmisc)
  
  new_df <- training_data[,take_l]
  
  for (w in scale_parameters) {
    
    if(w %in% normalise_l) {
      temp <- normalize(training_data[,w], method = "standardize")
      new_df[,w] <- temp
    } else if(w %in% scale_l) {
      temp <- normalize(training_data[,w], method = "range")
      new_df[,w] <- temp
    }
  }
  return(new_df)
}

