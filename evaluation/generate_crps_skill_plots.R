##Function that generates a CRPS plot
#' @param ***_f THe file name of all the different results
#' @return A LaTeX table of the reults
#' @export


generate_crpss_plots <-
  function(linear_g_f,
           linear_t_f,
           linear_gg_f,
           linear_gt_f,
           linear_tg_f,
           linear_tt_f,
           linear_d_f,
           rf_g_f,
           rf_t_f,
           rf_gg_f,
           rf_gt_f,
           rf_tg_f,
           rf_tt_f,
           rf_d_f,
           variant,
           y_low,
           y_high) {
    require(rlist)
    require(ggplot2)
    linear_g <- list.load(linear_g_f)
    linear_t <- list.load(linear_t_f)
    linear_gg <- list.load(linear_gg_f)
    linear_gt <- list.load(linear_gt_f)
    linear_tg <- list.load(linear_tg_f)
    linear_tt <- list.load(linear_tt_f)
    linear_d <- list.load(linear_d_f)
    rf_g <- list.load(rf_g_f)
    rf_t <- list.load(rf_t_f)
    rf_gg <- list.load(rf_gg_f)
    rf_gt <- list.load(rf_gt_f)
    rf_tg <- list.load(rf_tg_f)
    rf_tt <- list.load(rf_tt_f)
    rf_d <- list.load(rf_d_f)
    
    
    linear_g <- linear_g$CRPS
    linear_t <- linear_t$CRPS
    linear_gg <- linear_gg$CRPS
    linear_gt <- linear_gt$CRPS
    linear_tg <- linear_tg$CRPS
    linear_tt <- linear_tt$CRPS
    linear_d <- linear_d$CRPS
    rf_g <- rf_g$CRPS
    rf_t <- rf_t$CRPS
    rf_gg <- rf_gg$CRPS
    rf_gt <- rf_gt$CRPS
    rf_tg <- rf_tg$CRPS
    rf_tt <- rf_tt$CRPS
    rf_d <- rf_d$CRPS
    
    
    linear_g[, "0"] <- NULL
    linear_t[, "0"] <- NULL
    linear_gg[, "0"] <- NULL
    linear_gt[, "0"] <- NULL
    linear_tg[, "0"] <- NULL
    linear_tt[, "0"] <- NULL
    linear_d[, "0"] <- NULL
    rf_g[, "0"] <- NULL
    rf_t[, "0"] <- NULL
    rf_gg[, "0"] <- NULL
    rf_gt[, "0"] <- NULL
    rf_tg[, "0"] <- NULL
    rf_tt[, "0"] <- NULL
    rf_d[, "0"] <- NULL
    
    
    crps_table <-
      rbind(
        linear_g,
        linear_t,
        linear_gg,
        linear_gt,
        linear_tg,
        linear_tt,
        linear_d,
        rf_g,
        rf_t,
        rf_gg,
        rf_gt,
        rf_tg,
        rf_tt,
        rf_d
      )
    
    
    new_names <-
      c(
        "Linear Raw",
        "Linear Power Gamma",
        "Delete1",
        "Linear Power Tnorm",
        "Linear Weather Gamma",
        "Linear Two GG",
        "Delete2",
        "Linear Two GT",
        "Linear Weather Tnorm",
        "Linear Two TG",
        "Delete3",
        "Linear Two TT",
        "Linear Dummy CRPS",
        "Delete4",
        "rf Raw",
        "rf Power Gamma",
        "Delete5",
        "rf Power Tnorm",
        "rf Weather Gamma",
        "rf Two GG",
        "Delete6",
        "rf Two GT",
        "rf Weather Tnorm",
        "rf Two TG",
        "Delete7",
        "rf Two TT",
        "rf Dummy CRPS",
        "Delete8"
      )
    
    rownames(crps_table) <- new_names
    
    
    row.names.remove <-
      c(
        "Delete1",
        "Delete2",
        "Delete3",
        "Delete4",
        "Delete5",
        "Delete6",
        "Delete7",
        "Delete8"
      )
    crps_table <-
      crps_table[!(row.names(crps_table) %in% row.names.remove),]
    
    
    if (variant == "gg") {
      select_names <-
        c("Linear Raw",
          "Linear Power Gamma",
          "Linear Weather Gamma",
          "Linear Two GG")
      
      l_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "gt") {
      select_names <-
        c("Linear Raw",
          "Linear Power Tnorm",
          "Linear Weather Gamma",
          "Linear Two GT")
      
      l_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "tg") {
      select_names <-
        c("Linear Raw",
          "Linear Power Gamma",
          "Linear Weather Tnorm",
          "Linear Two TG")
      
      l_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "tt") {
      select_names <-
        c("Linear Raw",
          "Linear Power Tnorm",
          "Linear Weather Tnorm",
          "Linear Two TT")
      
      l_table_to_plot <- crps_table[select_names, ]
    }
    
    if (variant == "gg") {
      select_names <-
        c("rf Raw",
          "rf Power Gamma",
          "rf Weather Gamma",
          "rf Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]

      
      
    } else if (variant == "gt") {
      select_names <-
        c("rf Raw",
          "rf Power Gamma",
          "rf Weather Gamma",
          "rf Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "tg") {
      select_names <-
        c("rf Raw",
          "rf Power Gamma",
          "rf Weather Gamma",
          "rf Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "tt") {
      select_names <-
        c("rf Raw",
          "rf Power Gamma",
          "rf Weather Gamma",
          "rf Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]
      
    }
    
    tl <- -(l_table_to_plot - c(l_table_to_plot[1,]))
    tr <- -(r_table_to_plot - c(r_table_to_plot[1,]))
    tln <- tl
    trn <- tr

    for(i in 1:length(tl[1,])) {
      tln[,i] <- tl[,i] / l_table_to_plot[1,i]
      trn[,i] <- trn[,i] / r_table_to_plot[1,i]
    }
    
    tln <- tln[-1,] * 100
    trn <- trn[-1,] * 100
    
    table_to_plot <- rbind(tln, trn)
    
    return(table_to_plot)
    
  }
    
  #   new_new_names <- c("linear_power", "linear_weather", "linear_two", "rf_power", "rf_weather", "rf_two")
  #   rownames(table_to_plot) <- new_new_names
  #   table_to_plot <- as.data.frame(t(table_to_plot))
  #   ax <- (rownames(table_to_plot))
  #   ax <- factor(ax, levels=unique(ax))
  #   table_to_plot[,"Horizon"] <- ax
  #   
  #   the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + geom_hline(yintercept=0, linetype="dashed", color = "red", size=1.5) +
  #     geom_point(aes(y = linear_power, color = "Linear One-Step-P", shape = 15), shape = 15, size = 4) + geom_line(aes(y = linear_power, color = "Linear One-Step-P"), size = 1) + 
  #     geom_point(aes(y = linear_weather, color = "Linear One-Step-W", shape = 16), shape = 16, size = 4) + geom_line(aes(y = linear_weather, color = "Linear One-Step-W"), size = 1) + 
  #     geom_point(aes(y = linear_two, color = "Linear Two-Step-WP", shape = 17), shape = 17, size = 4) + geom_line(aes(y = linear_two, color = "Linear Two-Step-WP"), size = 1) + 
  #     geom_point(aes(y = rf_power, color = "RF One-Step-P", shape = 16), shape = 15, size = 4) + geom_line(aes(y = rf_power, color = "RF One-Step-P"), size = 1) + 
  #     geom_point(aes(y = rf_weather, color = "RF One-Step-W", shape = 16), shape = 16, size = 4) + geom_line(aes(y = rf_weather, color = "RF One-Step-W"), size = 1) + 
  #     geom_point(aes(y = rf_two, color = "RF Two-Step-WP", shape = 17), shape = 17, size = 4) + geom_line(aes(y = rf_two, color = "RF Two-Step-WP"), size = 1) + 
  #     theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
  #                        legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
  #     ylab("CRPSS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
  #     labs(colour = "Post-Processing Strategy") + theme(legend.position="bottom") +
  #     scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"), breaks = c("Linear One-Step-P","Linear One-Step-W","Linear Two-Step-WP","RF One-Step-P","RF One-Step-W","RF Two-Step-WP"))
  #   
  #   return(the_plot)
  # }


# g.skill <- ggplot(res, aes(y = crps_skill, x = Horizon, col = Weather)) +
#   geom_line(size = 1) +
#   geom_point(aes(shape = Weather), size = 4) +
#   ylab('CRPS Skill') +
#   scale_color_viridis_d(option = 'inferno') +
#   labs(col = "Regression Model Inputs",
#        shape = "Regression Model Inputs")

