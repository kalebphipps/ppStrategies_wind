# Function that generates a scatterplot matrix showing dependency structures
#' @param Test Description
#' @return Descibr what is returned
#' @export


create_scatterplot_matrix <- function(structured_list, w_params, iteration_names, filter_horizon, filter_day, title_names) {
  require(dplyr)
  require(ggplot2)
  require(GGally)
  require(scales)
  
  #Create ensemble member names
  ensMemberNames <- 0
  for (i in 1:51) {
    n <- paste("ens", i, sep = "")
    ensMemberNames[i] <- n
  }
  
  scatter.list <- list()
  
  for (i in iteration_names) {
    
    weather_list <- structured_list[[i]]
    save_df <- as.data.frame(matrix(0, nrow = 51, ncol = length(w_params)))
    colnames(save_df) <- w_params
    
    for (w in w_params) {
      
      single_df <- weather_list[[w]]
      single_df <- single_df %>% filter(horizon == filter_horizon)
      single_df <- single_df %>% filter(time == as.POSIXct(filter_day, tz = "UTC"))
      save_df[,w] <- as.numeric(single_df[,ensMemberNames])
    }
    
    colnames(save_df) <- title_names
    
    gg <- ggpairs(save_df, columns = 1:length(w_params), diag = list(continuous = wrap("barDiag", bins = 10)),
                  upper = list(continuous = "points"))
    
    gg <- gg + theme_grey(base_size = 20)
    #gg <- gg + scale_x_continuous(breaks = scales::pretty_breaks(n=0)) + scale_y_continuous(breaks = scales::pretty_breaks(n=0))
    gg <- gg + theme(axis.text = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
   
          
    
    scatter.list[[i]] <- gg
    
  }
  
  return(scatter.list)
 
}
