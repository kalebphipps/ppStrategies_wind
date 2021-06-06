# Function that returns the verification ranked histogram of raw ensemble data
#' @param power_ensemble_prediction A list containing the power ensemble predictions from the raw ensemble data
#' @param horizons vector of different forecast horizons to be considered
#' @return A list containing verification ranked histograms
#' @export

create_ranked_histrogram <- function(power_ensemble_prediction, horizons) {
  require(ggplot2)
  
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  rank_list <- list()
  
  for (h in horizons) {
    hr <- paste("h", h, sep = "")
    
    work_df <- power_ensemble_prediction[[hr]]
    the_ensembles <- work_df[,ensMemberNames]
    the_obs <- work_df[,"obs"]
    
    rank <- apply(cbind(the_obs,the_ensembles), 1, function(x)
      rank(x, ties="random")[1])
    
    min_wt <- min(rank)
    df <- as.data.frame(rank)
    
    rank_hist_plot <- ggplot(df, aes(x = rank, y = stat(density)*51)) +
      geom_histogram(binwidth = 2.55, color = "grey", fill = "black", boundary = min_wt) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
      theme_bw() + theme(panel.border = element_blank()) +
      ylab("Density") + xlab("Bins")
    rank_list[[hr]] <- rank_hist_plot
  }
  
  return(rank_list)
}

# Function that returns the verification ranked histogram of the raw weather data
#' @param weather_ensemble_data A list containing the power ensemble predictions from the raw ensemble data
#' @param weather_params A list of weather parameters to be considered
#' @param horizons vector of different forecast horizons to be considered
#' @return A list containing verification ranked histograms
#' @export

create_weather_ranked_histrogram <- function(weather_ensemble_data, weather_params, horizons) {
  require(ggplot2)
  
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  rank_list <- list()
  
  for (w in weather_params) {
    rank_list[[w]] <- list()
    
    temp_df <- weather_ensemble_data[[w]]
    
    for (h in horizons) {
      hr <- paste("h", h, sep = "")
      
      work_df <- temp_df %>% filter(horizon == h)
      the_ensembles <- work_df[,ensMemberNames]
      the_obs <- work_df[,"obs"]
      
      rank <- apply(cbind(the_obs,the_ensembles), 1, function(x)
        rank(x, ties="random")[1])
      
      min_wt <- min(rank)
      df <- as.data.frame(rank)
      
      rank_hist_plot <- ggplot(df, aes(x = rank, y = stat(density)*51)) +
        geom_histogram(binwidth = 2.55, color = "grey", fill = "black", boundary = min_wt) +
        geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
        theme_bw() + theme(panel.border = element_blank()) +
        ylab("Density") + xlab("Bins")
      
      rank_list[[w]][[hr]] <- rank_hist_plot
    }
  }
  return(rank_list)
}



# Function that returns the limits of the ranked histogram plots
#' @param ranked_hist_list A list containing the verification ranked histogram plots
#' @param horizons A vector of different forecast horizons to be considered
#' @return A list of the limits from the verification ranked histogram plots
#' @export

verification_rank_limits <- function(ranked_hist_list, horizons) {
  
  limits_list <- list()
  
  for(h in horizons) {
    hr <- paste("h", h, sep = "")
    
    limits <- as.data.frame(matrix(0, ncol = 2, nrow = 2))
    colnames(limits) <- c("lower", "upper")
    limits[1,] <- ggplot_build(ranked_hist_list[[hr]])$layout$panel_scales_x[[1]]$range$range
    limits[2,] <- ggplot_build(ranked_hist_list[[hr]])$layout$panel_scales_y[[1]]$range$range
    
    limits_list[[hr]] <- limits
  }
  
  return(limits_list)
}


# Function that returns the limits of the weather ranked histogram plots
#' @param ranked_hist_list A list containing the verification ranked histogram plots
#' @param weather_params A list of weather parameters to be considered
#' @param horizons A vector of different forecast horizons to be considered
#' @return A list of the limits from the weather verification ranked histogram plots
#' @export

weather_verification_rank_limits <- function(ranked_hist_list, weather_params, horizons) {
  
  limits_list <- list()
  
  for (w in weather_params) {
    
    limits_list[[w]] <- list()
    
    for(h in horizons) {
      hr <- paste("h", h, sep = "")
      
      limits <- as.data.frame(matrix(0, ncol = 2, nrow = 2))
      colnames(limits) <- c("lower", "upper")
      limits[1,] <- ggplot_build(ranked_hist_list[[w]][[hr]])$layout$panel_scales_x[[1]]$range$range
      limits[2,] <- ggplot_build(ranked_hist_list[[w]][[hr]])$layout$panel_scales_y[[1]]$range$range
      
      limits_list[[w]][[hr]] <- limits
    }
    
  }
  return(limits_list)
}


#' Calculate the probability integral transformation (PIT) (Code from Nicole Ludwig)
#'
#' @param mu vector of the mu paramter resulting from post-processing
#' @param sigma vector of the sigma paramter resulting from post-processing
#' @param scale vector of the scale paramter resulting from post-processing
#' @param shape vector of the shape paramter resulting from post-processing
#' @param distribution select which distribution to be used
#' (normal, truncnormal, gamma0, csg0)
#' @param observations the true observations
#' @return Returns the PIT values
#' @export


pit <- function(mu = NULL, sigma = NULL, scale = NULL, shape = NULL,
                distribution = c('normal', 'truncnormal', 'gamma'),
                observations) {
  
  pit <- c()
  
  if (distribution == 'normal') {
    
    for (i in 1:length(observations)) {
      
      pit[i] <- pnorm(observations[i], mean = mu[i], sd = sigma[i])
      
    }
    
  } else if (distribution == 'truncnormal') {
    
    for (i in 1:length(observations)) {
      
      pit[i] <- (pnorm(observations[i], mean = mu[i], sd = sigma[i]) +
                   pnorm(mu[i]/sigma[i]) - 1) *
        (observations[i] >= 0) / pnorm(mu[i]/sigma[i])
      
    }
    
  } else if (distribution == 'gamma') {
    
    for (i in 1:length(observations)) {
      
      pit[i] <- pgamma(observations[i], shape = shape[i], scale = scale[i])
      
    }
  }
  
  
  return(pit)
  
}


# This function generates a list of PIT histograms for calibrated power ensemble forecasts
#' @param power_distribution_list A list containing the emos power distribution parameters and the original observations
#' @param limits A list containing the limits for plotting the PIT
#' @param horizons A vector of different forecast horizons to be considered
#' @param distrib A string to indicate whether a gamma or truncnormal distribution should be used
#' @return A list of PIT histograms for each forecast horizon
#' @export

create_pit_histogram <- function(power_distribution_list, limits, horizons, distrib) {
  require(ggplot2)
  
  pit_list <- list()
  
  for (h in horizons) {
    hr <- paste("h",h,sep = "")
    
    if(distrib == "truncnormal") {
      pit_v <- pit(mu = power_distribution_list[[hr]][,"mu"], sigma = power_distribution_list[[hr]][,"sigma"], 
                   distribution = "truncnormal", observations = power_distribution_list[[hr]][,"obs"])
      
    } else if(distrib == "gamma") {
      pit_v <- pit(shape = power_distribution_list[[hr]][,"shape"], scale = power_distribution_list[[hr]][,"scale"], 
                  distribution = "gamma", observations = power_distribution_list[[hr]][,"obs"])
      
    }
    
    
    
    min_wt <- min(pit_v)
    
    y_l <- as.numeric(limits[[hr]][2,"lower"])
    y_up <- as.numeric(limits[[hr]][2,"upper"])
    
    df <- as.data.frame(pit_v)
    
    pit_hist_plot <- ggplot(df, aes(x = pit_v, y = ..density..)) +
      geom_histogram(binwidth = 0.05, color = "grey", fill = "black", boundary = min_wt) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed") + 
      theme_bw() + theme(panel.border = element_blank()) +
      ylab("Density") + xlab("PIT") + ylim(y_l, y_up)
    
    pit_list[[hr]] <- pit_hist_plot
  }
  
  return(pit_list)
}


# This function generates a list of PIT histograms for calibrated weather forecasts
#' @param weather_distribution_list A list containing the emos weather distribution parameters and the original observations
#' @param limits A list containing the limits for plotting the PIT
#' @param weather_params A list of weather parameters to be considered
#' @param horizons vector of different forecast horizons to be considered
#' @return A list of PIT histograms for each forecast horizon
#' @export

create_weather_pit_histogram <- function(weather_distribution_list, limits, weather_params, horizons, norm_l, tnorm_l, gamma_l) {
  require(ggplot2)
  
  pit_list <- list()
  
  for (w in weather_params) {
    
    pit_list[[w]] <- list()
    
    for (h in horizons) {
      hr <- paste("h",h,sep = "")
      
      if(w %in% norm_l) {
        pit_v <- pit(mu = weather_distribution_list[[w]][[hr]][,"mu"], sigma = weather_distribution_list[[w]][[hr]][,"sigma"], 
                     distribution = "normal", observations = weather_distribution_list[[w]][[hr]][,"obs"])
      } else if(w %in% tnorm_l) {
        pit_v <- pit(mu = weather_distribution_list[[w]][[hr]][,"mu"], sigma = weather_distribution_list[[w]][[hr]][,"sigma"], 
                     distribution = "truncnormal", observations = weather_distribution_list[[w]][[hr]][,"obs"])
      } else if(w %in% gamma_l) {
        pit_v <- pit(shape = weather_distribution_list[[w]][[hr]][,"shape"], scale = weather_distribution_list[[w]][[hr]][,"scale"], 
                     distribution = "gamma", observations = weather_distribution_list[[w]][[hr]][,"obs"])
      }
      
      min_wt <- min(pit_v)
      
      y_l <- as.numeric(limits[[w]][[hr]][2,"lower"])
      y_up <- as.numeric(limits[[w]][[hr]][2,"upper"])
      
      df <- as.data.frame(pit_v)
      
      pit_hist_plot <- ggplot(df, aes(x = pit_v, y = ..density..)) +
        geom_histogram(binwidth = 0.05, color = "grey", fill = "black", boundary = min_wt) +
        geom_hline(yintercept = 1, color = "red", linetype = "dashed") + 
        theme_bw() + theme(panel.border = element_blank()) +
        ylab("Density") + xlab("PIT") + ylim(y_l, y_up)
      
      pit_list[[w]][[hr]] <- pit_hist_plot
    }
    
  }

  return(pit_list)
}