##Function that calculates the mean CRPS values given a list containing predictions and observations for different horizons
#' @param pred_list The list containing dataframes of predictions and observations
#' @param horizons The forecast horizons for which CRPS values should be calculated
#' @return A data frame containing mean CRPS scores for each horizon
#' @export

calculate_mean_single_model_crps <- function(pred_list, horizons) {
  require(scoringRules)
  
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  #Name columns
  names_v <- c("horizon","CRPS")
  
  #Set up dataframe to save results
  crps_df <- as.data.frame(matrix(0, ncol = 2, nrow = length(horizons)))
  crps_df[,1] <- horizons
  counter <- 1
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    s_df <- pred_list[[hr]]
    rsd <- sqrt(sum((s_df[,"ens1"] - s_df[,"obs"])^2) / (length(s_df[,"ens1"]) - 2)) 
    crps_df[counter,2] <- mean(crps_norm(y=s_df[,"obs"], location = s_df[,"ens1"], scale = rsd))
    
    counter <- counter + 1
  }
  colnames(crps_df) <- names_v
  return(crps_df)
}


##Function that calculates the RMSE values given a list containing predictions and observations for different horizons
#' @param pred_list The list containing dataframes of predictions and observations
#' @param horizons The forecast horizons for which CRPS values should be calculated
#' @return A data frame containing mean CRPS scores for each horizon
#' @export

calculate_rmse <- function(pred_list, horizons) {
  require(scoringRules)
  
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  #Name columns
  names_v <- c("horizon","RMSE")
  
  #Set up dataframe to save results
  rmse_df <- as.data.frame(matrix(0, ncol = 2, nrow = length(horizons)))
  rmse_df[,1] <- horizons
  counter <- 1
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    s_df <- pred_list[[hr]]
    rmse_df[counter,2] <- rmse(s_df[,"ens1"], s_df[,"obs"])
    
    counter <- counter + 1
  }
  colnames(rmse_df) <- names_v
  return(rmse_df)
}