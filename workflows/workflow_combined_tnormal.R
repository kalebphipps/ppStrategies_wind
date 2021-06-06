##Function that performs the entire workflow for the weather only and two step calibration process with tnormal for power distributions
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
#' @param norm_l The list of weather parameters to be fitted with the normal distribution
#' @param tnorm_l The list of parameters to be fitted with the truncated normal distribution
#' @param gamma_l The list of parameters to be fitted with the gamma distribution
#' @return A list of important results - CRPS, Plots etc.
#' @export

workflow_combined_tnormal <- function(ensemble_name,
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
                                          neuralModel = FALSE,
                                          neuralThreshold = 0.01,
                                          neuralConfig = c(7,10),
                                          emosDaysToTrain = 30,
                                          isBenchmark = FALSE,
                                          benchMaxCap = NULL,
                                          norm_l,
                                          tnorm_l,
                                          gamma_l,
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
  
  
  source("~/Work/Gits/ensemble_post_processing/evaluation/ranked_histogram_functions.R")
  
  weather_raw_ranked_hist <- create_weather_ranked_histrogram(weather_ensemble_data = structured_data, 
                                                              weather_params = weather_params, 
                                                              horizons = horizons)
  
  weather_limits <- weather_verification_rank_limits(ranked_hist_list = weather_raw_ranked_hist,
                                                     weather_params = weather_params, 
                                                     horizons = horizons)
  
  
  source("~/Work/Gits/ensemble_post_processing/calibration/emos_weather_parameters_gamma.R")
  
  
  emos_weather <- emos_weather_variables(original_ensembles = structured_data,
                                         weather_params = weather_params,
                                         norm_l = norm_l,
                                         tnorm_l = tnorm_l,
                                         gamma_l = gamma_l,
                                         horizons = horizons,
                                         daysToTrain = emosDaysToTrain)
  
  emos_weather_distribution <- emos_weather_dist_parameters(emos_params = emos_weather,
                                                            weather_ensembles = structured_data,
                                                            weather_params = weather_params,
                                                            norm_l = norm_l,
                                                            tnorm_l = tnorm_l,
                                                            gamma_l = gamma_l,
                                                            horizons = horizons,
                                                            daysToTrain = emosDaysToTrain)
  
  weather_pit <- create_weather_pit_histogram(weather_distribution_list = emos_weather_distribution, 
                                              limits = weather_limits, 
                                              weather_params = weather_params, 
                                              horizons = horizons, 
                                              norm_l = norm_l, 
                                              tnorm_l = tnorm_l,
                                              gamma_l = gamma_l)
  
  
  emos_draw_weather <- emos_weather_forecast(emos_params = emos_weather,
                                             weather_ensembles = structured_data,
                                             weather_params = weather_params,
                                             norm_l = norm_l,
                                             tnorm_l = tnorm_l,
                                             gamma_l = gamma_l,
                                             horizons = horizons,
                                             daysToTrain = emosDaysToTrain)
  message("EMOS Draw Complete")
  
  source("~/Work/Gits/ensemble_post_processing/calibration/convert_to_forecast_structure.R")
  
  structured_emos <- convert_to_forecast_structure(weather_ensembles = emos_draw_weather,
                                                   weather_parameters = weather_params,
                                                   horizons = horizons,
                                                   isBenchmark = isBenchmark)
  
  if(scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/pre_processing/scale_structured_data.R")
    
    
    structured_emos <- scale_structured_data(ensemble_list = structured_emos,
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
    
    if(!isBenchmark){
      prediction_emos <- prepare_ensemble_data(wp_name = power_name,
                                               ensemble_data = structured_emos,
                                               file_path = file_path,
                                               start_time = test_start_time,
                                               end_time = test_end_time,
                                               weather_params = weather_params,
                                               toScale = TRUE)
      
      
    } else if(isBenchmark){
      prediction_emos <- prepare_ensemble_data(wp_name = power_name,
                                               ensemble_data = structured_emos,
                                               file_path = file_path,
                                               start_time = test_start_time,
                                               end_time = test_end_time,
                                               weather_params = weather_params,
                                               toScale = FALSE)
      
    }
    
    
  } else if(!scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/prepare_ensemble_data.R")
    
    prediction_emos <- prepare_ensemble_data(wp_name = power_name,
                                             ensemble_data = structured_emos,
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
    
    
    emos_prediction <- predict_ensemble_model(prediction_data = prediction_emos,
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
    
    
    emos_prediction <- predict_neural_ensembles(prediction_data = prediction_emos,
                                                neural_models = neural_models,
                                                horizons = horizons)
    
    
  } else if(RFModel) {
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/train_rf_model.R")
    
    rf_models <- train_rf_models(training_data = train_data, 
                                 horizons = horizons, 
                                 reg_params = regression_parameters)
    
    
    source("~/Work/Gits/ensemble_post_processing/forecasting/predict_rf_models.R")
    
    emos_prediction <- predict_rf_ensembles(prediction_data = prediction_emos, 
                                               rf_models = rf_models, 
                                               onlyPositive = linearOnlyPositive,
                                               horizons = horizons)
    
  }
  
  if(scaleRequired) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_params_power.R")
    
    if(!isBenchmark) {
      emos_prediction <- rescale_power(ensemble_list = emos_prediction,
                                       horizons = horizons,
                                       scale_params = power_scale_saved)
      
    }
    
    
  }
  
  if(isBenchmark) {
    source("~/Work/Gits/ensemble_post_processing/forecasting/scale_params_power.R")
    
    emos_prediction <- rescale_power(ensemble_list = emos_prediction,
                                     horizons = horizons,
                                     max = benchMaxCap,
                                     min = 0)
    
  }
  
  
  source("~/Work/Gits/ensemble_post_processing/evaluation/calculate_crps.R")
  
  raw_emos_eval <- calculate_mean_crps(pred_list = emos_prediction,
                                       horizons = horizons)
  
  
  
  power_emos_verification_rank <- create_ranked_histrogram(power_ensemble_prediction = emos_prediction,
                                                           horizons = horizons)
  
  
  power_emos_limits <- verification_rank_limits(ranked_hist_list = power_emos_verification_rank,
                                                horizons = horizons)
  
  
  source("~/Work/Gits/ensemble_post_processing/calibration/emos_power_ensembles.R")
  
  power_emos_emos <- emos_power_ensembles(power_ensembles = emos_prediction,
                                          horizons = horizons,
                                          daysToTrain = emosDaysToTrain)
  message("EMOS Power Calibration Complete")
  
  
  power_emos_emos_dist <- emos_power_distribution(emos_params = power_emos_emos,
                                                  power_ensembles = emos_prediction,
                                                  horizons = horizons,
                                                  daysToTrain = emosDaysToTrain)
  
  
  power_emos_cal_pit <- create_pit_histogram(power_distribution_list = power_emos_emos_dist,
                                             limits = power_emos_limits,
                                             horizons = horizons,
                                             distrib = "truncnormal")
  
  
  cal_emos_prediction <- emos_power_forecast(emos_params = power_emos_emos,
                                             power_ensembles = emos_prediction,
                                             horizons = horizons,
                                             daysToTrain = emosDaysToTrain)
  
  
  source("~/Work/Gits/ensemble_post_processing/evaluation/calculate_crps.R")
  
  cal_emos_eval <- calculate_mean_crps(pred_list = cal_emos_prediction,
                                       horizons = horizons)
  
  
  raw_emos_eval
  cal_emos_eval
  
  final_res <- data.frame(matrix(0, nrow = 2, ncol = length(raw_emos_eval$CRPS)))
  colnames(final_res) <- raw_emos_eval$horizon
  rownames(final_res) <- c("One-W", "Two-WP")
  final_res[1,] <- raw_emos_eval$CRPS
  final_res[2,] <- cal_emos_eval$CRPS
  
  full_weather_eval <- calculate_full_crps(pred_list = emos_prediction,
                                       horizons = horizons)
  
  full_power_eval <- calculate_full_crps(pred_list = cal_emos_prediction,
                                       horizons = horizons)
  
  full_crps <- list(full_weather_eval, full_power_eval)
  
  
  all_results <- list()
  all_results[["Weather_Ranked_Hist"]] <- weather_raw_ranked_hist
  all_results[["Weather_Pit"]] <- weather_pit
  all_results[["Power_Uncal_Rank_Hist"]] <- power_emos_verification_rank
  all_results[["Power_Cal_Pit"]] <- power_emos_cal_pit
  all_results[["CRPS"]] <- final_res
  all_results[["Full_CRPS"]] <- full_crps
  
  return(all_results)
}