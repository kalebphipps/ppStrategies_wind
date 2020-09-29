##Function that trains neural networks
#' @param training_data The dataframe containing the training data for the regression
#' @param horizons The forecast horizons for which models should be developed
#' @param reg_params The regression parameters to be used
#' @return A list of the trained neural networks
#' @export


train_neural_models <- function(training_data, horizons, reg_params, hidden_l, threshold_l = 0.01, stepmax_l = 1e+05) {
  require(dplyr)
  require(neuralnet)
  
  #Correct mistake in observation data
  if(training_data[length(training_data$time), "horizon"] == 0) {
    training_data <- training_data[-length(training_data$time),]
  }
  
  neural.m <- list()
  
  for (h in horizons) {
    hr <- paste("h", h, sep = "")
    
    message(paste("Currently Training for a Horizon of",h,"hours"))
    
    
    
    #Extract full data frame to use for origins
    origins <- training_data %>% filter(horizon == h)
    
    #Filter horizon
    train_data <- training_data %>% filter(horizon == h)
    
    #For Swedish Data correct size
    #if(unique(origins$horizon == 24)) {
    origins <- origins[-length(origins$time),]
    train_data <- train_data[-1,]
    #} 
    
    train_data[,"origins"] <- origins[,"wind_power"]
    
    h_reg_params <- reg_params
    info <- paste("wind_power ~", paste("", h_reg_params, collapse = "+"))
    message(paste("Neural Network trained as: ", info, sep = ""))
    nn_formula <- as.formula(paste("wind_power ~", paste("", h_reg_params, collapse = "+")))
    
    neural_model <- neuralnet(formula = nn_formula,
                              data = train_data,
                              hidden = hidden_l,
                              threshold = threshold_l,
                              stepmax = stepmax_l,
                              act.fct = "tanh",
                              err.fct = "sse",
                              lifesign = "full",
                              linear.output = TRUE)
    neural.m[[hr]] <- neural_model
  }

  return(neural.m)
}