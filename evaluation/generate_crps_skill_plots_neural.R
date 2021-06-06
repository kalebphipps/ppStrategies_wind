##Function that generates a CRPS plot
#' @param ***_f THe file name of all the different results
#' @return A LaTeX table of the reults
#' @export


generate_crpss_n_plots <-
  function(linear_g_f,
           linear_t_f,
           linear_gg_f,
           linear_gt_f,
           linear_tg_f,
           linear_tt_f,
           linear_d_f,
           Neural_g_f,
           Neural_t_f,
           Neural_gg_f,
           Neural_gt_f,
           Neural_tg_f,
           Neural_tt_f,
           Neural_d_f,
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
    Neural_g <- list.load(Neural_g_f)
    Neural_t <- list.load(Neural_t_f)
    Neural_gg <- list.load(Neural_gg_f)
    Neural_gt <- list.load(Neural_gt_f)
    Neural_tg <- list.load(Neural_tg_f)
    Neural_tt <- list.load(Neural_tt_f)
    Neural_d <- list.load(Neural_d_f)
    
    
    linear_g <- linear_g$CRPS
    linear_t <- linear_t$CRPS
    linear_gg <- linear_gg$CRPS
    linear_gt <- linear_gt$CRPS
    linear_tg <- linear_tg$CRPS
    linear_tt <- linear_tt$CRPS
    linear_d <- linear_d$CRPS
    Neural_g <- Neural_g$CRPS
    Neural_t <- Neural_t$CRPS
    Neural_gg <- Neural_gg$CRPS
    Neural_gt <- Neural_gt$CRPS
    Neural_tg <- Neural_tg$CRPS
    Neural_tt <- Neural_tt$CRPS
    Neural_d <- Neural_d$CRPS
    
    
    linear_g[, "0"] <- NULL
    linear_t[, "0"] <- NULL
    linear_gg[, "0"] <- NULL
    linear_gt[, "0"] <- NULL
    linear_tg[, "0"] <- NULL
    linear_tt[, "0"] <- NULL
    linear_d[, "0"] <- NULL
    Neural_g[, "0"] <- NULL
    Neural_t[, "0"] <- NULL
    Neural_gg[, "0"] <- NULL
    Neural_gt[, "0"] <- NULL
    Neural_tg[, "0"] <- NULL
    Neural_tt[, "0"] <- NULL
    Neural_d[, "0"] <- NULL
    
    
    crps_table <-
      rbind(
        linear_g,
        linear_t,
        linear_gg,
        linear_gt,
        linear_tg,
        linear_tt,
        linear_d,
        Neural_g,
        Neural_t,
        Neural_gg,
        Neural_gt,
        Neural_tg,
        Neural_tt,
        Neural_d
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
        "Neural Raw",
        "Neural Power Gamma",
        "Delete5",
        "Neural Power Tnorm",
        "Neural Weather Gamma",
        "Neural Two GG",
        "Delete6",
        "Neural Two GT",
        "Neural Weather Tnorm",
        "Neural Two TG",
        "Delete7",
        "Neural Two TT",
        "Neural Dummy CRPS",
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
        c("Neural Raw",
          "Neural Power Gamma",
          "Neural Weather Gamma",
          "Neural Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]

      
      
    } else if (variant == "gt") {
      select_names <-
        c("Neural Raw",
          "Neural Power Gamma",
          "Neural Weather Gamma",
          "Neural Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "tg") {
      select_names <-
        c("Neural Raw",
          "Neural Power Gamma",
          "Neural Weather Gamma",
          "Neural Two GG")
      
      r_table_to_plot <- crps_table[select_names, ]
      
      
    } else if (variant == "tt") {
      select_names <-
        c("Neural Raw",
          "Neural Power Gamma",
          "Neural Weather Gamma",
          "Neural Two GG")
      
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
    
    table_to_plot <- trn
    
    
    new_new_names <- c("Neural_power", "Neural_weather", "Neural_two")
    rownames(table_to_plot) <- new_new_names
    table_to_plot <- as.data.frame(t(table_to_plot))
    ax <- (rownames(table_to_plot))
    ax <- factor(ax, levels=unique(ax))
    table_to_plot[,"Horizon"] <- ax
    
    the_plot <- ggplot(table_to_plot, aes(x = Horizon, group = 1)) + geom_hline(yintercept=0, linetype="dashed", color = "red", size=1.5) +
      geom_point(aes(y = Neural_power, color = "Neural One-Step-P", shape = 16), shape = 15, size = 4) + geom_line(aes(y = Neural_power, color = "Neural One-Step-P"), size = 1) + 
      geom_point(aes(y = Neural_weather, color = "Neural One-Step-W", shape = 16), shape = 16, size = 4) + geom_line(aes(y = Neural_weather, color = "Neural One-Step-W"), size = 1) + 
      geom_point(aes(y = Neural_two, color = "Neural Two-Step-WP", shape = 17), shape = 17, size = 4) + geom_line(aes(y = Neural_two, color = "Neural Two-Step-WP"), size = 1) + 
      theme_bw() + theme(panel.border = element_blank(), axis.title = element_text(size=22), axis.text = element_text(size=18), 
                         legend.text = element_text(size = 22), legend.title = element_text(size = 22)) + 
      ylab("CRPSS") + xlab("Forecast Horizon") + ylim(y_low,y_high) + 
      labs(colour = "Post-Processing Strategy") + theme(legend.position="bottom") +
      scale_colour_manual(values = c("#000000", "#E69F00", "#56B4E9"), breaks = c("Neural One-Step-P","Neural One-Step-W","Neural Two-Step-WP"))
      
    
    
    return(the_plot)
  }

