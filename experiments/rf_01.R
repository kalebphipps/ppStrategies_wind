rm(list = ls())

source("~/Work/Gits/ensemble_post_processing/workflows/workflow_dummies.R")
source("~/Work/Gits/ensemble_post_processing/workflows/workflow_one_gamma.R")
source("~/Work/Gits/ensemble_post_processing/workflows/workflow_one_tnormal.R")
source("~/Work/Gits/ensemble_post_processing/workflows/workflow_combined_gamma.R")
source("~/Work/Gits/ensemble_post_processing/workflows/workflow_combined_tnormal.R")
require(rlist)
require(Metrics)

set.seed(12345)

##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------#####Linear Onshore Dataset######---------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##

onshore_rf_dummies <-
  workflow_dummies(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c("spring", "summer", "autumn", "origins"),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130
  )

list.save(onshore_rf_dummies, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_dummies_01.rds")



onshore_rf_one_gamma <-
  workflow_one_gamma(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130
  )

list.save(onshore_rf_one_gamma, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_one_gamma_01.rds")



onshore_rf_one_tnormal <-
  workflow_one_tnormal(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130
  )

list.save(onshore_rf_one_tnormal, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_one_tnormal_01.rds")



onshore_rf_combined_gg <-
  workflow_combined_gamma(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(onshore_rf_combined_gg, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_combined_gg_01.rds")



onshore_rf_combined_gt <-
  workflow_combined_tnormal(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(onshore_rf_combined_gt, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_combined_gt_01.rds")



onshore_rf_combined_tt <-
  workflow_combined_tnormal(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(onshore_rf_combined_tt, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_combined_tt_01.rds")



onshore_rf_combined_tg <-
  workflow_combined_gamma(
    ensemble_name = "Onshore_Ensembles.csv",
    control_name = "Onshore_Control.csv",
    obs_name = "Onshore_Observations.csv",
    power_name = "Onshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 130,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(onshore_rf_combined_tg, file = "~/Work/Gits/ensemble_post_processing/results/onshore_rf_combined_tg_01.rds")


##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------#####Linear Offshore Dataset######--------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##

offshore_rf_dummies <-
  workflow_dummies(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c("spring", "summer", "autumn", "origins"),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400
  )

list.save(offshore_rf_dummies, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_dummies_01.rds")



offshore_rf_one_gamma <-
  workflow_one_gamma(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400
  )

list.save(offshore_rf_one_gamma, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_one_gamma_01.rds")



offshore_rf_one_tnormal <-
  workflow_one_tnormal(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400
  )

list.save(offshore_rf_one_tnormal, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_one_tnormal_01.rds")



offshore_rf_combined_gg <-
  workflow_combined_gamma(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(offshore_rf_combined_gg, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_combined_gg_01.rds")



offshore_rf_combined_gt <-
  workflow_combined_tnormal(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(offshore_rf_combined_gt, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_combined_gt_01.rds")



offshore_rf_combined_tt <-
  workflow_combined_tnormal(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(offshore_rf_combined_tt, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_combined_tt_01.rds")



offshore_rf_combined_tg <-
  workflow_combined_gamma(
    ensemble_name = "offshore_Ensembles.csv",
    control_name = "offshore_Control.csv",
    obs_name = "offshore_Observations.csv",
    power_name = "offshore_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u10", "v10", "t2m", "speed"),
    horizons = c(0, 6, 12, 18, 24),
    normalise_params = c("u10", "v10"),
    scaleRequired = FALSE,
    scale_params = c("sp", "u10", "v10", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn",
      "wind_power"
    ),
    structure_cutoff_time = "2018-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2018-08-01 01:00:00",
    regression_parameters = c(
      "sp",
      "u10",
      "v10",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "origins",
      "spring",
      "summer",
      "autumn"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    isBenchmark = TRUE,
    benchMaxCap = 400,
    norm_l = c("sp", "u10", "v10", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(offshore_rf_combined_tg, file = "~/Work/Gits/ensemble_post_processing/results/offshore_rf_combined_tg_01.rds")

##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##------------------------------------#####Linear Sweden Zone 3 Dataset######------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##

Sweden_Zone3_rf_dummies <-
  workflow_dummies(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c("spring", "summer", "autumn", "origins"),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130
  )

list.save(Sweden_Zone3_rf_dummies, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_dummies_01.rds")



Sweden_Zone3_rf_one_gamma <-
  workflow_one_gamma(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130
  )

list.save(Sweden_Zone3_rf_one_gamma, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_one_gamma_01.rds")



Sweden_Zone3_rf_one_tnormal <-
  workflow_one_tnormal(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130
  )

list.save(Sweden_Zone3_rf_one_tnormal, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_one_tnormal_01.rds")



Sweden_Zone3_rf_combined_gg <-
  workflow_combined_gamma(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(Sweden_Zone3_rf_combined_gg, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_combined_gg_01.rds")



Sweden_Zone3_rf_combined_gt <-
  workflow_combined_tnormal(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(Sweden_Zone3_rf_combined_gt, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_combined_gt_01.rds")



Sweden_Zone3_rf_combined_tt <-
  workflow_combined_tnormal(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(Sweden_Zone3_rf_combined_tt, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_combined_tt_01.rds")



Sweden_Zone3_rf_combined_tg <-
  workflow_combined_gamma(
    ensemble_name = "Sweden_Zone3_Ensembles.csv",
    control_name = "Sweden_Zone3_Control.csv",
    obs_name = "Sweden_Zone3_Obs.csv",
    power_name = "Sweden_Zone3_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(Sweden_Zone3_rf_combined_tg, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone3_rf_combined_tg_01.rds")


##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##------------------------------------#####Linear Sweden Zone 4 Dataset######------------------------------------##
##---------------------------------------------------------------------------------------------------------------##
##---------------------------------------------------------------------------------------------------------------##



Sweden_Zone4_rf_dummies <-
  workflow_dummies(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c("spring", "summer", "autumn", "origins"),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130
  )

list.save(Sweden_Zone4_rf_dummies, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_dummies_01.rds")



Sweden_Zone4_rf_one_gamma <-
  workflow_one_gamma(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130
  )

list.save(Sweden_Zone4_rf_one_gamma, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_one_gamma_01.rds")



Sweden_Zone4_rf_one_tnormal <-
  workflow_one_tnormal(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130
  )

list.save(Sweden_Zone4_rf_one_tnormal, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_one_tnormal_01.rds")



Sweden_Zone4_rf_combined_gg <-
  workflow_combined_gamma(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(Sweden_Zone4_rf_combined_gg, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_combined_gg_01.rds")



Sweden_Zone4_rf_combined_gt <-
  workflow_combined_tnormal(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = NULL,
    gamma_l = c("speed")
  )

list.save(Sweden_Zone4_rf_combined_gt, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_combined_gt_01.rds")



Sweden_Zone4_rf_combined_tt <-
  workflow_combined_tnormal(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(Sweden_Zone4_rf_combined_tt, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_combined_tt_01.rds")



Sweden_Zone4_rf_combined_tg <-
  workflow_combined_gamma(
    ensemble_name = "Sweden_Zone4_Ensembles.csv",
    control_name = "Sweden_Zone4_Control.csv",
    obs_name = "Sweden_Zone4_Obs.csv",
    power_name = "Sweden_Zone4_Power.csv",
    file_path = "~/Work/Gits/ensemble_post_processing/pre_processing/Data",
    weather_params = c("sp", "u100", "v100", "t2m", "speed"),
    horizons = c(3, 6, 9, 12, 15, 18, 21, 24),
    normalise_params = c("u100", "v100"),
    scaleRequired = FALSE,
    scale_params = c("wind_power", "sp", "u100", "v100", "t2m", "speed"),
    safe_params = c(
      "time",
      "horizon",
      "is_origin",
      "2015",
      "2016",
      "2017",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec",
      "spring",
      "summer",
      "autumn"
    ),
    structure_cutoff_time = "2019-08-30 01:00:00",
    train_time = "2017-12-31 01:00:00",
    test_start_time = "2018-01-01 01:00:00",
    test_end_time = "2019-08-01 01:00:00",
    regression_parameters = c(
      "u100",
      "v100",
      "t2m",
      "speed",
      "speed3",
      "critical",
      "spring",
      "summer",
      "autumn",
      "origins"
    ),
    linearModel = FALSE,
    RFModel = TRUE,
    linearOnlyPositive = FALSE,
    linearIncludeIntercept = FALSE,
    neuralModel = FALSE,
    neuralThreshold = 0.01,
    neuralConfig = c(7),
    emosDaysToTrain = 40,
    benchMaxCap = 130,
    norm_l = c("sp", "u100", "v100", "t2m"),
    tnorm_l = c("speed"),
    gamma_l = NULL
  )

list.save(Sweden_Zone4_rf_combined_tg, file = "~/Work/Gits/ensemble_post_processing/results/Sweden_Zone4_rf_combined_tg_01.rds")

print("Congratulations - it's done :)")