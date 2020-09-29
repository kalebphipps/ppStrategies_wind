##Function that generates a ecc-r draw from calibrated emos weather ensembles
#' @param emos_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the ecc-r draws for each forecast horizon and weather variables
#' @export

ecc_r_weather_forecast <- function(emos_params, weather_ensembles, weather_params, norm_l, tnorm_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  require(BNPdensity)
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
      
      #Convert to data frame
      pred_df <- as.data.frame(pred_df)
      
      if(w %in% norm_l) {
        for (i in 1:length(res_df$time)) {
          uniforms <- runif(51,0,1)
          random_draw <- qnorm(uniforms, mean = f_params$mu[i], sd = f_params$sigma[i])
          original_rank <- rank(pred_df[i,ensMemberNames])
          sorted_random_draw <- random_draw[original_rank]
          res_df[i,ensMemberNames] <- sorted_random_draw
        }
      } else if(w %in% tnorm_l){ 
        for (i in 1:length(res_df$time)) {
          uniforms <- runif(51,0,1)
          random_draw <- qtnorm(p = uniforms, mean = f_params$mu[i], sd = f_params$sigma[i], lower = 0)
          original_rank <- rank(pred_df[i,ensMemberNames])
          sorted_random_draw <- random_draw[original_rank]
          res_df[i,ensMemberNames] <- sorted_random_draw
        }
      }
      pred.list[[w]][[hr]] <- res_df
    }
  }
  return(pred.list)
}


##Function that generates a ecc-q draw from calibrated emos weather ensembles
#' @param emos_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the ecc-q draws for each forecast horizon and weather vriables
#' @export

ecc_q_weather_forecast <- function(emos_params, weather_ensembles, weather_params, norm_l, tnorm_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
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
      
      #Convert to data frame
      pred_df <- as.data.frame(pred_df)
      
      if(w %in% norm_l) {
        for (i in 1:length(res_df$time)) {
          quantiles <- seq((1/52),(51/52),length = 51)
          random_draw <- qnorm(quantiles, mean = f_params$mu[i], sd = f_params$sigma[i])
          original_rank <- rank(pred_df[i,ensMemberNames])
          sorted_random_draw <- random_draw[original_rank]
          res_df[i,ensMemberNames] <- sorted_random_draw
        }
      } else if(w %in% tnorm_l){ 
        for (i in 1:length(res_df$time)) {
          quantiles <- seq((1/52),(51/52),length = 51)
          random_draw <- qtnorm(p = quantiles, mean = f_params$mu[i], sd = f_params$sigma[i], left = 0, right = 999999)
          original_rank <- rank(pred_df[i,ensMemberNames])
          sorted_random_draw <- random_draw[original_rank]
          res_df[i,ensMemberNames] <- sorted_random_draw
        }
      }
      pred.list[[w]][[hr]] <- res_df
    }
  }
  return(pred.list)
}


##Function that generates a ecc-t draw from calibrated emos weather ensembles
#' @param emos_params The EMOS parameters calculated before
#' @param weather_ensembles A list containing raw weather ensembles
#' @param weather_params A vector containing the weather params that need to be calibrated
#' @param norm_l A vector containing the parameters that need to be calibrated with the normal distribution
#' @param tnorm_l A vector containing the parameters that need to be calibrated with the truncated normal distribution
#' @param horizons A vector of the forecast horizons to be used
#' @param daysToTrain number of days used to train
#' @param startDate Optional parameter to determine when the training will start (if not from the begining of the dataset)
#' @return A list of dataframes containing the ecc-t draws for each forecast horizon and weather vriables
#' @export

ecc_t_weather_forecast <- function(emos_params, weather_ensembles, weather_params, norm_l, tnorm_l, horizons, daysToTrain = 30, startDate = NULL) {
  require(crch)
  require(BNPdensity)
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
      
      #Convert to data frame
      pred_df <- as.data.frame(pred_df)
      
      if(w %in% norm_l) {
        for (i in 1:length(res_df$time)) {
          o_mean <- ensMeans[i]
          o_sd <- sqrt(ensVars[i])
          s_mean <- f_params$mu[i]
          s_sd <- f_params$sigma[i]
          news <- s_mean + (s_sd/o_sd)*(only.ensembles[i,] - o_mean)
          res_df[i,ensMemberNames] <- news
        }
      } else if(w %in% tnorm_l){ 
        for (i in 1:length(res_df$time)) {
          o_mean <- ensMeans[i]
          o_sd <- sqrt(ensVars[i])
          s_mean <- f_params$mu[i]
          s_sd <- f_params$sigma[i]
          news <- s_mean + (s_sd/o_sd)*(only.ensembles[i,] - o_mean)
          res_df[i,ensMemberNames] <- news
        }
      }
      pred.list[[w]][[hr]] <- res_df
    }
  }
  return(pred.list)
}