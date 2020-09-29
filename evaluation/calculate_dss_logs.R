##Function that calculates the mean LogS values given a list containing predictions and observations for different horizons
#' @param pred_list The list containing dataframes of predictions and observations
#' @param horizons The forecast horizons for which CRPS values should be calculated
#' @return A data frame containing mean logS scores for each horizon
#' @export

calculate_mean_logs <- function(pred_list, horizons) {
  require(scoringRules)
  
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  #Name columns
  names_v <- c("horizon","LogS")
  
  #Set up dataframe to save results
  logs_df <- as.data.frame(matrix(0, ncol = 2, nrow = length(horizons)))
  logs_df[,1] <- horizons
  counter <- 1
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    s_df <- pred_list[[hr]]
    logs_df[counter,2] <- mean(logs_sample(y = s_df[,"obs"], dat = as.matrix(s_df[,ensMemberNames])))
    
    counter <- counter + 1
  }
  colnames(logs_df) <- names_v
  return(logs_df)
}

##Function that calculates the mean DSS values given a list containing predictions and observations for different horizons
#' @param pred_list The list containing dataframes of predictions and observations
#' @param horizons The forecast horizons for which CRPS values should be calculated
#' @return A data frame containing mean logS scores for each horizon
#' @export

calculate_mean_dss <- function(pred_list, horizons) {
  require(scoringRules)
  
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  #Name columns
  names_v <- c("horizon","DSS")
  
  #Set up dataframe to save results
  dss_df <- as.data.frame(matrix(0, ncol = 2, nrow = length(horizons)))
  dss_df[,1] <- horizons
  counter <- 1
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    s_df <- pred_list[[hr]]
    dss_df[counter,2] <- mean(dss_sample(y = s_df[,"obs"], dat = as.matrix(s_df[,ensMemberNames])))
    
    counter <- counter + 1
  }
  colnames(dss_df) <- names_v
  return(dss_df)
}