##Function that calculates all CRPS values given a list containing predictions and observations for different horizons
#' @param pred_list The list containing dataframes of predictions and observations
#' @param horizons The forecast horizons for which CRPS values should be calculated
#' @return A data frame containing CRPS scores for each point in time
#' @export

calculate_full_crps <- function(pred_list, horizons) {
  require(scoringRules)
  
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  #Name columns
  col_dates <- pred_list[[1]]$time
  names_v <- c("horizon", as.character(col_dates))
  
  #Set up dataframe to save results
  n_cols <- length(pred_list[[1]]$time) + 1
  crps_df <- as.data.frame(matrix(0, ncol = n_cols, nrow = length(horizons)))
  crps_df[,1] <- horizons
  counter <- 1
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    s_df <- pred_list[[hr]]
    crps_df[counter,2:n_cols] <- crps_sample(y = s_df[,"obs"], dat = as.matrix(s_df[,ensMemberNames]))
    
    counter <- counter + 1
  }
  colnames(crps_df) <- names_v
  return(crps_df)
}




##Function that calculates the mean CRPS values given a list containing predictions and observations for different horizons
#' @param pred_list The list containing dataframes of predictions and observations
#' @param horizons The forecast horizons for which CRPS values should be calculated
#' @return A data frame containing mean CRPS scores for each horizon
#' @export

calculate_mean_crps <- function(pred_list, horizons) {
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
    crps_df[counter,2] <- mean(crps_sample(y = s_df[,"obs"], dat = as.matrix(s_df[,ensMemberNames])))
    
    counter <- counter + 1
  }
  colnames(crps_df) <- names_v
  return(crps_df)
}