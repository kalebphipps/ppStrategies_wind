##EMOS method to calibrate ensembles of wind power forecasts
#' @param power_ensembles A list containing power ensemble predictions for each forecast horizon
#' @param horizons A vector of the forecast horizons that need to be calibrated
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the EMOS parameters for each forecast horizon
#' @export

emos_power_ensembles <- function(power_ensembles, horizons, daysToTrain = 30, startDate = NULL) {
  require(dplyr)
  require(scoringRules)
  
  param.list <- list()
  
  for (h in horizons) {
    hr <- paste("h",h, sep = "")
    
    full_df <- power_ensembles[[hr]]
    
    if(!is.null(startDate)) {
      df_to_use <- full_df %>% filter(time >= startDate)
    } else {
      df_to_use <- full_df
    }
    
    
    #Create ensemble member names
    ensMemberNames <- 0
    for (i in 1:51) {
      n <- paste("ens", i, sep = "")
      ensMemberNames[i] <- n
    }
    
    only.ensembles <- df_to_use[,ensMemberNames]
    ensMeans <- rowMeans(only.ensembles)
    ensVars <- apply(only.ensembles, MARGIN = 1, FUN = var)
    only.obs <- df_to_use$obs
    times <- df_to_use$time
    
    #Data frame for training
    train_df <- data.frame(ensMeans, ensVars, only.obs)
    colnames(train_df) <- c("m", "vars", "obs")
    
    #Training index
    p_start <- daysToTrain + 1
    p_end <- length(only.obs)
    
    #Data frame to save results
    result_times <- times[p_start:(p_end)]
    result_df <- as.data.frame(matrix(0, ncol = 5, nrow = length(result_times)))
    r_names <- c("a","b","c","d")
    c_names <- c("time",r_names)
    colnames(result_df) <- c_names
    result_df$time <- result_times
    
    for (i in p_start:p_end) {
      
      tw_start <- i - daysToTrain
      tw_end <- tw_start + (daysToTrain - 1)
      
      subset_train <- train_df[tw_start:tw_end,]
      optim_out <- optim(par = c(1,1,1,1),
                         fn = objective_function_tnorm,
                         gr = gradient_wrapper_tnorm,
                         ensMean = subset_train$m,
                         ensVar = subset_train$vars,
                         obs = subset_train$obs,
                         method = "BFGS",
                         control = list(maxit = 1000000))
      if(optim_out$convergence != 0) {
        message("numerical optimisation did not converge")
        print(i)
      }
      result_df[i-daysToTrain,r_names] <- optim_out$par
    }
    param.list[[hr]] <- result_df
    message("Lifesign :)")
  }
  return(param.list)
}


##Function that generates a calibrated ensemble of predictions based on the emos_prediction parameters
#' @param emos_params The EMOS parameters calculated before
#' @param power_ensembles A list containing power ensemble predictions for each forecast horizon
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the calibrated forecasts for each forecast horizon
#' @export

emos_power_forecast <- function(emos_params, power_ensembles, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  require(scoringRules)
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  pred.list <- list()
  
  for (h in horizons) {
    
    hr <- paste("h",h,sep="")
    
    #Ensemble prediction for means and variance
    pred_df <- power_ensembles[[hr]]
    emos_df <- emos_params[[hr]]
    
    if(!is.null(startDate)) {
      use_df <- pred_df %>% filter(time >= startDate)
      emos_df <- emos_df %>% filter(time >= startDate)
    } else {
      use_df <- pred_df[-(1:daysToTrain),]
      emos_df <- emos_df
    }
    
    if(length(use_df$time) != length(emos_df$time)) {
      stop("Lengths are inconsistent, check the startDate and try again")
    }
    
    #Save times
    only.times <- use_df$time
    
    only.ensembles <- use_df[,ensMemberNames]
    ensMeans <- rowMeans(only.ensembles)
    ensVars <- apply(only.ensembles, MARGIN = 1, FUN = var)
    only.obs <- use_df$obs
    
    a <- emos_df$a
    b <- emos_df$b
    c <- emos_df$c
    d <- emos_df$d
    
    mean_matrix <- cbind(1,ensMeans)
    var_matrix <- cbind(1,ensVars)
    
    mp_matrix <- rbind(a,b)
    vp_matrix <- rbind(c,d)
    
    mu <- NULL
    sigma <- NULL
    
    for (i in 1:length(a)) {
      mu[i] <- mean_matrix[i,] %*% mp_matrix[,i]
      var_v <- var_matrix[i,] %*% vp_matrix[,i]
      if(var_v <= 0 ){
        sigma[i] <- 0.001
      } else {
        sigma[i] <- sqrt(var_v)
      }
    }
    
    f_params <- data.frame(time = only.times, mu = mu, sigma = sigma)
    
    res_df <- as.data.frame(matrix(0, ncol = 53, nrow = length(only.times)))
    res_names <- c("time", "obs", ensMemberNames)
    colnames(res_df) <- res_names
    res_df$time <- only.times
    res_df$obs <- only.obs
    
    for (i in 1:length(res_df$time)) {
      this_mu <- f_params$mu[i]
      if(this_mu < 0) {
        this_mu <- 0
      }
      res_df[i,ensMemberNames] <- rtnorm(51, mean = this_mu, sd = f_params$sigma[i], left = 0, right = Inf)
    }
    pred.list[[hr]] <- res_df
  }
  return(pred.list)
}




##Function that generates the truncated normal distribution parameters based on the emos prediction parameters
#' @param emos_params The EMOS parameters calculated before
#' @param power_ensembles A list containing power ensemble predictions for each forecast horizon
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the trunced normal distribution parameters
#' @export

emos_power_distribution <- function(emos_params, power_ensembles, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  require(scoringRules)
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  pred.list <- list()
  
  for (h in horizons) {
    
    hr <- paste("h",h,sep="")
    
    #Ensemble prediction for means and variance
    pred_df <- power_ensembles[[hr]]
    emos_df <- emos_params[[hr]]
    
    if(!is.null(startDate)) {
      use_df <- pred_df %>% filter(time >= startDate)
      emos_df <- emos_df %>% filter(time >= startDate)
    } else {
      use_df <- pred_df[-(1:daysToTrain),]
      emos_df <- emos_df
    }
    
    if(length(use_df$time) != length(emos_df$time)) {
      stop("Lengths are inconsistent, check the startDate and try again")
    }
    
    #Save times
    only.times <- use_df$time
    
    only.ensembles <- use_df[,ensMemberNames]
    ensMeans <- rowMeans(only.ensembles)
    ensVars <- apply(only.ensembles, MARGIN = 1, FUN = var)
    only.obs <- use_df$obs
    
    a <- emos_df$a
    b <- emos_df$b
    c <- emos_df$c
    d <- emos_df$d
    
    mean_matrix <- cbind(1,ensMeans)
    var_matrix <- cbind(1,ensVars)
    
    mp_matrix <- rbind(a,b)
    vp_matrix <- rbind(c,d)
    
    mu <- NULL
    sigma <- NULL
    
    for (i in 1:length(a)) {
      mu[i] <- mean_matrix[i,] %*% mp_matrix[,i]
      var_v <- var_matrix[i,] %*% vp_matrix[,i]
      if(var_v <= 0 ){
        sigma[i] <- 0.001
      } else {
        sigma[i] <- sqrt(var_v)
      }
    }
    
    f_params <- data.frame(time = only.times, obs = only.obs, mu = mu, sigma = sigma)
    
    pred.list[[hr]] <- f_params
  }
  return(pred.list)
}





##Objective function for the minimum CRPS estimation of EMOS parameters when using a truncated normal distribution
#' @param par The EMOS parameters to be optimised
#' @param ensMean The Ensemble Mean
#' @param ensVar The Ensemble Variance
#' @param obs The observations
#' @return The CRPS based on a truncated normal distribution
#' @export


objective_function_tnorm <- function(par, ensMean, ensVar, obs) {
  m <- cbind(1, ensMean) %*% par[1:2]
  ssq <- cbind(1, ensVar) %*% par[3:4]
  if(any(ssq < 0)) {
    return(999999)
  } else {
    s <- sqrt(ssq)
    return(sum(crps_tnorm(y = obs, location = m, scale = s, lower = 0, upper = Inf)))
  }
}

gradient_wrapper_tnorm <- function(par, obs, ensMean, ensVar){
  loc <- cbind(1, ensMean) %*% par[1:2]
  sc <- sqrt(cbind(1, ensVar) %*% par[3:4])
  dcrps_dtheta <- gradcrps_tnorm(y = obs, location = loc, scale = sc, lower = 0, upper = Inf) 
  out1 <- dcrps_dtheta[,1] %*% cbind(1, ensMean)
  out2 <- dcrps_dtheta[,2] %*% 
    cbind(1/(2*sqrt(par[3]+par[4]*ensVar)), 
          ensVar/(2*sqrt(par[3]+par[4]*ensVar)))
  return(as.numeric(cbind(out1,out2)))
}