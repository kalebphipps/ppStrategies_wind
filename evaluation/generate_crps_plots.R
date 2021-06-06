##Function that generates a CRPS plot
#' @param ***_f THe file name of all the different results
#' @return A LaTeX table of the reults
#' @export


generate_crps_plots <- function(linear_g_f, linear_t_f, linear_gg_f, linear_gt_f, linear_tg_f, linear_tt_f, linear_d_f, 
                                neural_g_f, neural_t_f, neural_gg_f, neural_gt_f, neural_tg_f, neural_tt_f, neural_d_f, 
                                variant, type, y_low, y_high) {
  require(rlist)
  require(ggplot2)
  linear_g <- list.load(linear_g_f)
  linear_t <- list.load(linear_t_f)
  linear_gg <- list.load(linear_gg_f)
  linear_gt <- list.load(linear_gt_f)
  linear_tg <- list.load(linear_tg_f)
  linear_tt <- list.load(linear_tt_f)
  linear_d <- list.load(linear_d_f)
  neural_g <- list.load(neural_g_f)
  neural_t <- list.load(neural_t_f)
  neural_gg <- list.load(neural_gg_f)
  neural_gt <- list.load(neural_gt_f)
  neural_tg <- list.load(neural_tg_f)
  neural_tt <- list.load(neural_tt_f)
  neural_d <- list.load(neural_d_f)
  
  
  linear_g <- linear_g$CRPS
  linear_t <- linear_t$CRPS
  linear_gg <- linear_gg$CRPS
  linear_gt <- linear_gt$CRPS
  linear_tg <- linear_tg$CRPS
  linear_tt <- linear_tt$CRPS
  linear_d <- linear_d$CRPS
  neural_g <- neural_g$CRPS
  neural_t <- neural_t$CRPS
  neural_gg <- neural_gg$CRPS
  neural_gt <- neural_gt$CRPS
  neural_tg <- neural_tg$CRPS
  neural_tt <- neural_tt$CRPS
  neural_d <- neural_d$CRPS
  
  
  linear_g[,"0"] <- NULL
  linear_t[,"0"] <- NULL
  linear_gg[,"0"] <- NULL
  linear_gt[,"0"] <- NULL
  linear_tg[,"0"] <- NULL
  linear_tt[,"0"] <- NULL
  linear_d[,"0"] <- NULL
  neural_g[,"0"] <- NULL
  neural_t[,"0"] <- NULL
  neural_gg[,"0"] <- NULL
  neural_gt[,"0"] <- NULL
  neural_tg[,"0"] <- NULL
  neural_tt[,"0"] <- NULL
  neural_d[,"0"] <- NULL
  
  
  crps_table <- rbind(linear_g, linear_t, linear_gg, linear_gt, linear_tg, linear_tt, linear_d, 
                      neural_g, neural_t, neural_gg, neural_gt, neural_tg, neural_tt, neural_d)
  
  
  new_names <- c("Linear Raw", "Linear Power Gamma", "Delete1", "Linear Power Tnorm", "Linear Weather Gamma", "Linear Two GG",
                 "Delete2", "Linear Two GT", "Linear Weather Tnorm", "Linear Two TG", "Delete3", "Linear Two TT", "Linear Dummy CRPS",
                 "Delete4",
                 "Neural Raw", "Neural Power Gamma", "Delete5", "Neural Power Tnorm", "Neural Weather Gamma", "Neural Two GG",
                 "Delete6", "Neural Two GT", "Neural Weather Tnorm", "Neural Two TG", "Delete7", "Neural Two TT", "Neural Dummy CRPS",
                 "Delete8")
  
  rownames(crps_table) <- new_names
  
  
  row.names.remove <- c("Delete1","Delete2","Delete3","Delete4","Delete5","Delete6","Delete7","Delete8")
  crps_table <- crps_table[!(row.names(crps_table) %in% row.names.remove), ]
  
  if(type == "linear") {
    
    if(variant == "gg") {
      
      select_names <-  c("Linear Raw", "Linear Power Gamma", "Linear Weather Gamma", "Linear Two GG")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
      
    } else if(variant == "gt") {
      
      select_names <-  c("Linear Raw", "Linear Power Tnorm", "Linear Weather Gamma", "Linear Two GT")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
      
    } else if(variant == "tg") { 
      
      select_names <-  c("Linear Raw", "Linear Power Gamma", "Linear Weather Tnorm", "Linear Two TG")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
    } else if(variant == "tt") {
      
      select_names <-  c("Linear Raw", "Linear Power Tnorm", "Linear Weather Tnorm", "Linear Two TT")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
    }
  } else if(type == "neural") {
    
    
    if(variant == "gg") {
      
      select_names <-  c("Neural Raw", "Neural Power Gamma", "Neural Weather Gamma", "Neural Two GG")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
      
    } else if(variant == "gt") {
      
      select_names <-  c("Neural Raw", "Neural Power Tnorm", "Neural Weather Gamma", "Neural Two GT")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
      
    } else if(variant == "tg") { 
      
      select_names <-  c("Neural Raw", "Neural Power Gamma", "Neural Weather Tnorm", "Neural Two TG")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
    } else if(variant == "tt") {
      
      select_names <-  c("Neural Raw", "Neural Power Tnorm", "Neural Weather Tnorm", "Neural Two TT")
      
      table_to_plot <- crps_table[select_names,]
      
      new_new_names <- c("raw_ens", "one_power", "one_weather", "two_step")
      rownames(table_to_plot) <- new_new_names
      table_to_plot <- as.data.frame(t(table_to_plot))
      ax <- (rownames(table_to_plot))
      ax <- factor(ax, levels=unique(ax))
      table_to_plot[,"Horizon"] <- ax
      
      the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + 
        geom_point(aes(y = raw_ens, color = "Raw"), shape = 15, size = 4) + geom_line(aes(y = raw_ens, color = "Raw"), size = 1) + 
        geom_point(aes(y = one_power, color = "One-Step-P"), shape = 16, size = 4) + geom_line(aes(y = one_power, color = "One-Step-P"), size = 1) + 
        geom_point(aes(y = one_weather, color = "One-Step-W"), shape = 17, size = 4) + geom_line(aes(y = one_weather, color = "One-Step-W"), size = 1) + 
        geom_point(aes(y = two_step, color = "Two-Step-WP"), shape = 18, size = 4) + geom_line(aes(y = two_step, color = "Two-Step-WP"), size = 1) +
        theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                           legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
        ylab("CRPS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
        labs(colour = "Post-Processing Strategy") + scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#CC79A7"), breaks =c("Raw","One-Step-P","One-Step-W", "Two-Step-WP"))
      
    }
    
  }
  
  return(the_plot)
  
}


