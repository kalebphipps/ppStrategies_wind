##Function that generates a table of mean CRPS results in a LaTeX format for papers
#' @param ***_f THe file name of all the different results
#' @param table_name The name of the table to be output
#' @return A LaTeX table of the reults
#' @export


get_crps_latex_table <- function(linear_g_f, linear_t_f, linear_gg_f, linear_gt_f, linear_tg_f, linear_tt_f, linear_d_f, 
                                 neural_g_f, neural_t_f, neural_gg_f, neural_gt_f, neural_tg_f, neural_tt_f, neural_d_f, table_name) {
  require(rlist)
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
                 "Delete2", " Linear Two GT", "Linear Weather Tnorm", "Linear Two TG", "Delete3", "Linear Two TT", "Linear Dummy CRPS",
                 "Delete4",
                 "Neural Raw", "Neural Power Gamma", "Delete5", "Neural Power Tnorm", "Neural Weather Gamma", "Neural Two GG",
                 "Delete6", " Neural Two GT", "Neural Weather Tnorm", "Neural Two TG", "Delete7", "Neural Two TT", "Neural Dummy CRPS",
                 "Delete8")
  
  rownames(crps_table) <- new_names
  
  
  row.names.remove <- c("Delete1","Delete2","Delete3","Delete4","Delete5","Delete6","Delete7","Delete8")
  crps_table <- crps_table[!(row.names(crps_table) %in% row.names.remove), ]

  require(xtable)
  
  print(xtable(crps_table, type = "latex"), file = table_name)
}


