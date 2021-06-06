##Function that trains random forrests
#' @param training_data The dataframe containing the training data for the regression
#' @param horizons The forecast horizons for which models should be developed
#' @param reg_params The regression parameters to be used
#' @return A list of the trained neural networks
#' @export


train_rf_models <- function(training_data, horizons, reg_params, ntree = 500, nsplit = round(length(reg_params)/3)) {
  require(dplyr)
  require(randomForest)
  
  #Correct mistake in observation data
  if(training_data[length(training_data$time), "horizon"] == 0) {
    training_data <- training_data[-length(training_data$time),]
  }
  
  rf.m <- list()
  
  for (h in horizons) {
    hr <- paste("h", h, sep = "")
    
    message(paste("Currently Training for a Horizon of",h,"hours"))
    
    
    
    #Extract full data frame to use for origins
    origins <- training_data %>% filter(is_origin == 1)
    
    #Filter horizon
    train_data <- training_data %>% filter(horizon == h)
    
    #For Swedish Data correct size
    if(unique(origins$horizon == 24)) {
      origins <- origins[-length(origins$time),]
      train_data <- train_data[-1,]
    } 
    
    train_data[,"origins"] <- origins[,"wind_power"]
    
    h_reg_params <- reg_params
    size_h <- length(h_reg_params)
    size_n <- 56 + (size_h * 9)
    s_weights <- rep(.5,size_n)
    info <- paste("wind_power ~", paste("", h_reg_params, collapse = "+"))
    message(paste("Random Forest trained as: ", info, sep = ""))
    rf_formula <- as.formula(paste("wind_power ~", paste("", h_reg_params, collapse = "+")))
    
    set.seed(12345)
    
    rf_model <- randomForest(formula = rf_formula,
                          data = train_data,
                          ntree = ntree,
                          mtry = nsplit,
                          importance = TRUE,
                          na.action = na.omit)
    

    rf.m[[hr]] <- rf_model
  }
  
  return(rf.m)
}