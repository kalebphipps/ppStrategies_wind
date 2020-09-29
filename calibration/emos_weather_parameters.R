##EMOS method to calibrate the raw weather ensembles
#' @param original_ensembles A list containing the raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons that need to be calibrated
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of a list of dataframes containing the EMOS parameters for each weather variable and each forecast horizon
#' @export

emos_weather_variables <- function(original_ensembles, weather_params, norm_l, tnorm_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(dplyr)
  require(scoringRules)
  
  param.list <- list()
  
  for (w in weather_params) {
    
    param.list[[w]] <- list()
    
    #Select model type
    if(w %in% norm_l) {
      model_type <- "normal"
    } else if(w %in% tnorm_l) {
      model_type <- "truncnormal"
    }
    
    #Select only one weather variable
    ens <- original_ensembles[[w]]
    
    #save ensembles
    ens.t <- ens
    
    for (h in horizons) {
      hr <- paste("h",h, sep = "")
      
      full_df <- ens.t %>% filter(horizon == h)
      
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
        
        if(model_type == "normal") { 
          optim_out <- optim(par = c(1,1,1,1),
                             fn = objective_function_norm,
                             gr = gradient_wrapper_norm ,
                             ensMean = subset_train$m,
                             ensVar = subset_train$vars,
                             obs = subset_train$obs,
                             method = "BFGS",
                             control = list(maxit = 10000))
          if(optim_out$convergence != 0) {
            message("numerical optimisation did not converge")
            print(i)
            print("normal")
          }
        } else if(model_type == "truncnormal") {
          optim_out <- optim(par = c(1,1,1,1),
                             fn = objective_function_tnorm,
                             gr = gradient_wrapper_tnorm,
                             ensMean = subset_train$m,
                             ensVar = subset_train$vars,
                             obs = subset_train$obs,
                             method = "BFGS",
                             control = list(maxit = 10000))
          if(optim_out$convergence != 0) {
            message("numerical optimisation did not converge")
            print(i)
            print("tnorm")
          }
        
        }
        result_df[i-daysToTrain,r_names] <- optim_out$par
      }
      param.list[[w]][[hr]] <- result_df
      message("Lifesign :)")
    }
  }
  return(param.list)
}


##Function that generates a calibrated ensemble of predictions based on the emos_prediction parameters
#' @param emos_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the calibrated forecasts for each forecast horizon
#' @export

emos_weather_forecast <- function(emos_params, weather_ensembles, weather_params, norm_l, tnorm_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  require(scoringRules)
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  pred.list <- list()
  
  for (w in weather_params) {
    
    pred.list[[w]] <- list()
    ens <- weather_ensembles[[w]]
    ens.t <- ens
    
    
    for (h in horizons) {
      
      hr <- paste("h",h,sep="")
      
      #Ensemble prediction for means and variance
      pred_df <- ens.t %>% filter(horizon == h)
      emos_df <- emos_params[[w]][[hr]]
      
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
      
      if(w %in% norm_l) {
        for (i in 1:length(res_df$time)) {
          res_df[i,ensMemberNames] <- rnorm(51, mean = f_params$mu[i], sd = f_params$sigma[i])
        }
      } else if(w %in% tnorm_l){ 
        for (i in 1:length(res_df$time)) {
          this_mu <- f_params$mu[i]
          if(this_mu < 0) {
            this_mu <- 0
          }
          res_df[i,ensMemberNames] <- rtnorm(51, mean = this_mu, sd = f_params$sigma[i], left = 0, right = Inf)
        }
      }
      pred.list[[w]][[hr]] <- res_df
    }
  }
  return(pred.list)
}

##Function that returns the distribution parameters for weather ensembles
#' @param emos_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the distribution parameters for each forecast horizon
#' @export

emos_weather_dist_parameters <- function(emos_params, weather_ensembles, weather_params, norm_l, tnorm_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  param.list <- list()
  
  for (w in weather_params) {
    
    param.list[[w]] <- list()
    ens <- weather_ensembles[[w]]
    ens.t <- ens
    
    
    for (h in horizons) {
      
      hr <- paste("h",h,sep="")
      
      #Ensemble prediction for means and variance
      pred_df <- ens.t %>% filter(horizon == h)
      emos_df <- emos_params[[w]][[hr]]
      
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
      
      param.list[[w]][[hr]] <- f_params
    }
  }
  return(param.list)
}


##Objective function for the minimum CRPS estimation of EMOS parameters when using a truncated normal distribution
#' @param parameter The EMOS parameters to be optimised
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



##Objective function for the minimum CRPS estimation of EMOS parameters when using a normal distribution
#' @param parameter The EMOS parameters to be optimised
#' @param ensMean The Ensemble Mean
#' @param ensVar The Ensemble Variance
#' @param obs The observations
#' @return The CRPS based on a normal distribution
#' @export


objective_function_norm <- function(par, ensMean, ensVar, obs) {
  m <- cbind(1, ensMean) %*% par[1:2]
  ssq <- cbind(1, ensVar) %*% par[3:4]
  if(any(ssq < 0)) {
    return(999999)
  } else {
    s <- sqrt(ssq)
    return(sum(crps_norm(y = obs, location = m, scale = s)))
  }
}

##Gradient of the objective function to use in optim() when optimising the CRPS for the normal distribution
#' @param parameter The EMOS parameters to be optimised
#' @param ensMean The Ensemble Mean
#' @param ensVar The Ensemble Variance
#' @param obs The observations
#' @return The gradient function to be used for optimisation
#' @export


gradient_wrapper_norm <- function(par, ensMean, ensVar, obs) {
  loc <- cbind(1, ensMean) %*% par[1:2]
  sc <- cbind(1, ensVar) %*% par[3:4]
  #Calculate derivative
  dcrps_dtheta <- gradcrps_norm(y = obs, location = loc, scale = sc)

  #Calculate gradient based on EMOS equations and their derivative
  out1 <- dcrps_dtheta[,1] %*% cbind(1, ensMean)
  out2 <- dcrps_dtheta[,2] %*% cbind(1/(2*sqrt(par[3] + par[4]*ensVar)),
                                     ensVar/(2*sqrt(par[3] + par[4]*ensVar)))

  return(as.numeric(cbind(out1, out2)))
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