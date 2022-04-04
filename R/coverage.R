#' Calculates p2r ratio and outputs coverage overview plot
#'
#' @param coverage coverage dataframe
#' @param mouse_id mouse to show
#' @param day day variable to select for
#' @param first_smoothing smoothing value for cacluation of p2r
#' @param cov_smoothing second smoothing value for visualization
#' @param title ggplot title
#' @return a List
#' @export
p2tPlot <- function(coverage,
                    mouse_id = "1681",
                    day = "0",
                    first_smoothing = 5000,
                    cov_smoothing = 100,
                    title = "Coverage"){

  max_cov <-  as.data.frame(
    coverage[which(coverage$mouse.id == mouse_id), ])
  max_all_days <- max(max_cov$score)
  min_all_days <- min(max_cov$score)

  coverage_one_sample <- as.data.frame(
    coverage[which(coverage$day == day &
                     coverage$mouse.id == mouse_id), ])

  #phage_subset <- phage_df[which(phage_df$chr == chr),]
  cov_reduced <- data.frame(start = rollapply(coverage_one_sample$start,
                                              width = first_smoothing,
                                              by = first_smoothing,
                                              FUN = min, align = "left"),
                            end =rollapply(coverage_one_sample$end,
                                           width = first_smoothing,
                                           by = first_smoothing,
                                           FUN = max, align = "left"),
                            cov = rollapply(coverage_one_sample$score,
                                            width = first_smoothing,
                                            by = first_smoothing,
                                            FUN = median, align = "left"))

  cov_reduced2 <- data.frame(start = rollapply(coverage_one_sample$start,
                                               width = cov_smoothing,
                                               by = cov_smoothing,
                                               FUN = min, align = "left"),
                             end =rollapply(coverage_one_sample$end,
                                            width = cov_smoothing,
                                            by = cov_smoothing,
                                            FUN = max, align = "left"),
                             cov = rollapply(coverage_one_sample$score,
                                             width = cov_smoothing,
                                             by = cov_smoothing,
                                             FUN = median, align = "left"))

  p <- ggplot(cov_reduced, aes(x = start,  y = cov ))
  p <- p + geom_smooth(formular = y ~ x + I(x^2), color = "red", size = 2)
  ld <- layer_data(p)
  ld <- ld[c(which.max(ld$y), which.min(ld$y)), ]
 # p <- p + geom_vline(data = phage_subset, aes(xintercept = start), color = "blue", size = .5, alpha = .5)
#  p <- p + geom_vline(data = phage_subset, aes(xintercept = end), color = "blue", size = .5, alpha = .5)
  p <- p + geom_line(data = cov_reduced2, aes(x = start, y = cov), size = .5)
  p <- p + geom_line(data = cov_reduced, aes(x = start, y = cov), size = .5, color = "blue")
  p <- p + theme_bw()
  # p <- p + theme_pmuench(base_size = 9)
  p <- p + scale_y_log10()
  p <- p + ggtitle(paste0("Day: ", day, "; PT:",
                          round(max(ld$y) / min(ld$y), digits= 2)))
  p <- p + xlab("Genome position") + ylab("Coverage")
  p <- p + geom_hline(yintercept = ld$y, linetype = 2, color = "red", size = .5)
  return(list(p = p, ratio = round(max(ld$y) / min(ld$y), digits= 2)))
}




p2tPlot2 <- function(coverage,
                     first_smoothing = 5000,
                     cov_filter = 10,
                     mouse_id = "1699",
                     day = 4,
                     cov_smoothing = 100,
                     title = "Coverage"){

  max_cov <-  as.data.frame(
    coverage[which(coverage$mouse.id == mouse_id), ])
  max_all_days <- max(max_cov$score)
  min_all_days <- min(max_cov$score)

  coverage_one_sample <- as.data.frame(
    coverage[which(coverage$day == day &
                     coverage$mouse.id == mouse_id), ])

  cov_reduced <- data.frame(start = rollapply(coverage_one_sample$start,
                                              width = first_smoothing,
                                              by = first_smoothing,
                                              FUN = min, align = "left"),
                            end =rollapply(coverage_one_sample$end,
                                           width = first_smoothing,
                                           by = first_smoothing,
                                           FUN = max, align = "left"),
                            cov = rollapply(coverage_one_sample$score,
                                            width = first_smoothing,
                                            by = first_smoothing,
                                            FUN = median, align = "left"))

  cov_reduced2 <- data.frame(start = rollapply(coverage_one_sample$start,
                                               width = cov_smoothing,
                                               by = cov_smoothing,
                                               FUN = min, align = "left"),
                             end =rollapply(coverage_one_sample$end,
                                            width = cov_smoothing,
                                            by = cov_smoothing,
                                            FUN = max, align = "left"),
                             cov = rollapply(coverage_one_sample$score,
                                             width = cov_smoothing,
                                             by = cov_smoothing,
                                             FUN = median, align = "left"))


  p <- ggplot(cov_reduced, aes(x = start,  y = cov ))
  p <- p + geom_smooth(formular = y ~ x + I(x^2), color = "red", size = 2)
  ld <- layer_data(p)
  ld <- ld[c(which.max(ld$y), which.min(ld$y)), ]
  # p <- p + geom_vline(data = phage_subset, aes(xintercept = start), color = "blue", size = .5, alpha = .5)
  #  p <- p + geom_vline(data = phage_subset, aes(xintercept = end), color = "blue", size = .5, alpha = .5)
  p <- p + geom_line(data = cov_reduced2, aes(x = start, y = cov), size = .5)
  p <- p + geom_line(data = cov_reduced, aes(x = start, y = cov), size = .5, color = "blue")
  p <- p + theme_bw()
  # p <- p + theme_pmuench(base_size = 9)
  p <- p + scale_y_log10()
  p <- p + ggtitle(paste0("Day: ", day, "; PT:",
                          round(max(ld$y) / min(ld$y), digits= 2)))
  p <- p + xlab("Genome position") + ylab("Coverage")
  p <- p + geom_hline(yintercept = ld$y, linetype = 2, color = "red", size = .5)

  pt <- ifelse(mean(cov_reduced2$cov) > 10, round(max(ld$y) / min(ld$y), digits= 2), NA)
  return(list(p = p, ratio = pt))
}



