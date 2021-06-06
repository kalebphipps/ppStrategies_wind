##Function that performs the entire workflow for the one step calibration process
#' @param ensemble_name The name of the ensemble csv file
#' @param control_name The name of the control csv file
#' @param obs_name The name of the observation csv file
#' @param power_name The name of the wind power csv file
#' @param file_path The file path for the csv files
#' @param weather_params The weather parameters to be considered
#' @param horizons THe forecast horizons to be considered
#' @param normalise_params The parameters that should be normalised
#' @param scaleRequired Boolean to indicate whether the data should be scaled or not
#' @param scale_params The paramters that should be scaled
#' @param safe_params The parameters that should not be touched
#' @param structure_cutoff_time THe end time when generating the initial ensemble data structure
#' @param train_time The time used to divide the training data set
#' @param test_start_time The time used to select that start of the test data set
#' @param test_end_time The time used to select the end of the test data set
#' @param regression_parameters The parameters used for the regression models
#' @param linearModel A boolean to indicate whether a linear model should be used or not
#' @param linearOnlyPositives A boolean to indicate whether the linear model should be restricted to positive coefficients
#' @param linearIncludeIntercept A boolean to indicate whether the linear model should include an intercept
#' @param neuralModel A boolean to indicate whether a neural network should be used or not
#' @param neuralThreshold The threshold used for training the neural network
#' @param neuralConfig The layer configuration of the neural network
#' @param emosDaysToTrain The number of days used to train the EMOS models.
#' @param isBenchmark A boolean to indicate whether a benchmark dataset is being used or not
#' @param benchMaxCap The maximum capacity of the wind power generation for benchmark datasets
#' @return The list of important results - CRPS, plots etc.
#' @export

workflow_dummies <- function(ensemble_name,
                              control_name,
                              obs_name,
                              power_name,
                              file_path,
                              weather_params,
                              horizons,
                              normalise_params,
                              scaleRequired = TRUE,
                              scale_params,
                              safe_params,
                              structure_cutoff_time,
                              train_time,
                              test_start_time,
                              test_end_time,
                              regression_parameters,
                              linearModel = TRUE,
                              linearOnlyPositive = FALSE,
                              linearIncludeIntercept = TRUE,
                              neuralModel = TRUE,
                              neuralThreshold = 0.01,
                              neuralConfig = c(7,10),
                              emosDaysToTrain = 30,
                              isBenchmark = FALSE,
                              benchMaxCap = NULL,
                              RFModel = TRUE) {
  
  if(linearModel & neuralModel & RFModel) {
    stop("Currently not possible to implement a workflow for multiple models at the same time!")
  }
  
  source("~/Work/Gits/ensemble_post_processing/pre_processing/create_correct_structure.R")
  
  structured_data <- create_correct_structure(ens_name =  ensemble_name,
                                              control_name =  control_name,
                                              obs_name =  obs_name,
                                              file_path = file_path,
                                              filter_time = structure_cutoff_time,
                                              w_parameters = weather_params)
  
  if(scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/pre_processing/scale_structured_data.R")
    
    
    structured_data <- scale_structured_data(ensemble_list = structured_data, 
                                             weather_parameters = weather_params,
                                             normalise_l = normalise_params,
                                             scale_l = scale_params)
  }
  
  source("~/Work/Gits/ensemble_post_processing/forecasting/prepare_training_data.R")
  
  train_data <- prepare_training_data(wp_name = power_name, 
                                      obs_name = obs_name, 
                                      file_path = file_path, 
                                      train_time = train_time, 
                                      weather_params = weather_params)
  
  
  if(scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_params_power.R")
    
    power_scale_saved <- save_power_scale_params(train_data)
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_training_data.R")
    
    train_data <- scale_training_data(training_data = train_data, 
                                      scale_parameters = scale_params,
                                      take_l = safe_params, 
                                      normalise_l = normalise_params, 
                                      scale_l = scale_params)
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/prepare_ensemble_data.R")
    
    if(!isBenchmark) {
      prediction_data <- prepare_ensemble_data(wp_name = power_name, 
                                               ensemble_data = structured_data, 
                                               file_path = file_path, 
                                               start_time = test_start_time,
                                               end_time = test_end_time,
                                               weather_params = weather_params,
                                               toScale = TRUE)
    } else if(isBenchmark) {
      prediction_data <- prepare_ensemble_data(wp_name = power_name, 
                                               ensemble_data = structured_data, 
                                               file_path = file_path, 
                                               start_time = test_start_time,
                                               end_time = test_end_time,
                                               weather_params = weather_params,
                                               toScale = FALSE)
    }
    
    
  } else if(!scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/prepare_ensemble_data.R")
    
    prediction_data <- prepare_ensemble_data(wp_name = power_name, 
                                             ensemble_data = structured_data, 
                                             file_path = file_path, 
                                             start_time = test_start_time,
                                             end_time = test_end_time,
                                             weather_params = weather_params,
                                             toScale = FALSE)
  }
  
  
  if(linearModel) { 
    source("~/Work/Gits/ensemble_post_processing/forecasting/train_linear_model.R")
    
    linear_models <- train_linear_models(training_data = train_data, 
                                         onlyPositive =  linearOnlyPositive,
                                         includeIntercept = linearIncludeIntercept,
                                         horizons = horizons,
                                         reg_params = regression_parameters)
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/predict_linear_models.R")
    
    
    raw_ens_prediction <- predict_ensemble_model(prediction_data = prediction_data, 
                                                 linear_models = linear_models, 
                                                 onlyPositive = linearOnlyPositive,
                                                 horizons = horizons)
  } else if(neuralModel) {
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/train_neural_model.R")
    
    neural_models <- train_neural_models(training_data = train_data, 
                                         horizons = horizons, 
                                         reg_params = regression_parameters,
                                         hidden_l = neuralConfig, 
                                         threshold_l = neuralThreshold)
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/predict_neural_model.R")
    
    
    raw_ens_prediction <- predict_neural_ensembles(prediction_data = prediction_data,
                                                   neural_models = neural_models, 
                                                   horizons = horizons)
    
  } else if(RFModel) {
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/train_rf_model.R")
    
    rf_models <- train_rf_models(training_data = train_data, 
                                 horizons = horizons, 
                                 reg_params = regression_parameters)
    
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/predict_rf_models.R")
    
    raw_ens_prediction <- predict_rf_ensembles(prediction_data = prediction_data, 
                                               rf_models = rf_models, 
                                               onlyPositive = linearOnlyPositive,
                                               horizons = horizons)
    
  }
  
  if(scaleRequired) {
    if(!isBenchmark) {
      source("~/Work/Gits/ensemble_post_processing/forecasting/scale_params_power.R")
      
      raw_ens_prediction <- rescale_power(ensemble_list = raw_ens_prediction, 
                                          horizons = horizons, 
                                          scale_params = power_scale_saved)
    }
  }
  
  if(isBenchmark) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_params_power.R")
    
    raw_ens_prediction <- rescale_power(ensemble_list = raw_ens_prediction, 
                                        horizons = horizons, 
                                        max = benchMaxCap,
                                        min = 0)
  }
  
  source("~/Work/Gits/ensemble_post_processing/evaluation/calculate_dummy_eval.R")
  
  
  crps_eval <- calculate_mean_single_model_crps(pred_list = raw_ens_prediction,
                                                horizons = horizons)
  
  rmse_eval <- calculate_rmse(pred_list = raw_ens_prediction,
                              horizons = horizons)
  
  l <- length(crps_eval$CRPS)
  
  result_df <- as.data.frame(matrix(0, nrow = 2, ncol = l))
  
  result_df[1,] <- crps_eval$CRPS
  result_df[2,] <- rmse_eval$RMSE
  
  rownames(result_df) <- c("CRPS", "RMSE")
  colnames(result_df) <- horizons
  
  if(isBenchmark) {
    result_df[,'0'] <- NULL
  }
  
  all_results <- list()
  all_results[["CRPS"]] <- result_df
  
  return(all_results)
  
}