#' Produces isolates heatmap
#'
#' @param iso_profile the AF profile of the genomic isolates
#' @param wgs_profile the AF profile of the WGS samples
#' @param mouse_ids
#' @param genome_hr the name of the genome to plot, sould be present in iso_profile
#' @param col_fun function that assigns the AF color
#' @param filtered boolean, if TRUE, SNPs will be filtered based on median
#' @return a ComplexHeamtap object
#' @export
create_heatmap_iso <- function(iso_profile,
                               wgs_profile,
                               mouse_ids = c("1683", "1688", "1692", "1699"),
                               genome_hr = "C. innocuum",
                               col_fun = col_fun,
                               filtered = F) {
  library(dplyr)
  # subset WGS data
  df <- data.frame(
    snp_id = wgs_profile$snp_id,
    AF = wgs_profile$AF,
    feature = wgs_profile$feature,
    genome_hr = wgs_profile$genome_hr,
    sample_id = paste0(wgs_profile$mouse.id, "-", wgs_profile$day)
  )

  df_wide <- spread(df, key = sample_id, value = AF)
  df_wide_bug <- df_wide[which(df_wide$genome_hr == genome_hr), ]

  # get order of columns right (according to position in genome)
  snp_pos <- as.integer(substr(df_wide_bug$snp_id, 1, nchar(df_wide_bug$snp_id) - 4))
  df_wide_bug <- df_wide_bug[order(snp_pos), ]
  # subset for relevant mouse ids
  df_wide_bug_mat <- df_wide_bug[, -c(1:3)]
  annot <- df_wide_bug[, 1:3]
  limited_mat <- df_wide_bug_mat[, which(substr(colnames(df_wide_bug_mat), 1, 4) %in% mouse_ids)]

  iso_bug <- iso_profile[which(iso_profile$genome_hr == genome_hr), ]
  stopifnot(iso_bug != 0)
  iso_bug$majorAF <- NULL
  iso_bug$QUAL <- NULL
  iso_bug$DP <- NULL
  iso_bug_wide <- spread(iso_bug, key = sample, value = AF)
  translateMouseIdToTreatmentGroup(translateIsolateIDtoMouse(colnames(iso_bug_wide[13:ncol(iso_bug_wide)])))
  iso_bug_wide_mat <- iso_bug_wide[, 13:ncol(iso_bug_wide)]
  iso_bug_wide_mat <- iso_bug_wide_mat
  iso_annot <- data.frame(
    snp_id = iso_bug_wide$snp_id,
    genome_hr = iso_bug_wide$genome_hr,
    feature = iso_bug_wide$feature
  )
  iso_bug_wide_mat[is.na(iso_bug_wide_mat)] <- 0
  wgs_subset <- limited_mat[match(iso_annot$snp_id, annot$snp_id), ]
  # wgs_subset[is.na(wgs_subset)] <- 0
  colnames(wgs_subset) <- colnames(limited_mat)

  ha1 <- rowAnnotation(WGS = anno_points(wgs_subset,
                                         gp = gpar(col = group2color(translateMouseIdToTreatmentGroup(substr(colnames(wgs_subset), 1, 4)))),
                                         pch = 73,
                                         size = unit(2, "points"),
                                         width = unit(3, "cm"),
                                         ylim = c(0, 1)
  ))

  iso_annot$type <- "normal"
  iso_annot[which(apply(iso_bug_wide_mat, 1, median) > .1 &
                    apply(iso_bug_wide_mat, 1, median) < .9), ]$type <- "intermediate"
  iso_annot[which(rowSums(iso_bug_wide_mat > 0.1) < 2), ]$type <- "singelton"
  row_ha_col <- c("normal" = "white", "intermediate" = "black", "singelton" = "grey50")

  # Annotation rows
  row_ha2 <- rowAnnotation(
    type = anno_simple(iso_annot$type,
                       border = TRUE,
                       col = row_ha_col,
                       simple_anno_size = unit(.2, "cm")
    ),
    show_annotation_name = FALSE
  )
  if (!filtered) {
    ht_iso <- Heatmap(data.matrix(iso_bug_wide_mat),
                      name = "AF",
                      right_annotation = ha1, # row_ha2,
                      row_title = genome_hr,
                      row_gap = unit(0, "mm"),
                      column_gap = unit(1, "mm"),
                      col = col_fun,
                      show_row_dend = F,
                      cluster_rows = T,
                      cluster_columns = T,
                      row_title_gp = gpar(fontsize = 4),
                      column_title_gp = gpar(fontsize = 4),
                      border_gp = gpar(col = "black", lty = 1),
                      gap = unit(0, "mm"),
                      column_names_gp = gpar(fontsize = 4),
                      row_names_gp = gpar(fontsize = 4),
                      show_column_dend = F,
                      column_split = translateMouseIdToTreatmentGroup(translateIsolateIDtoMouse(colnames(iso_bug_wide_mat)))
    )
    return(ht_iso)
  } else {
    iso_bug_wide_mat_limited <- iso_bug_wide_mat[which(iso_annot$type == "normal"), ]
    iso_annot_limited <- iso_annot[which(iso_annot$type == "normal"), ]
    wgs_subset <- limited_mat[match(iso_annot_limited$snp_id, annot$snp_id), ]

    ha1 <- rowAnnotation(
      AF_in_WGS = anno_points(wgs_subset,
                              gp = gpar(
                                fontsize = 7,
                                col = group2color(translateMouseIdToTreatmentGroup(substr(colnames(wgs_subset), 1, 4)))
                              ),
                              pch = 73,
                              #   size = unit(1.4, "points"),
                              width = unit(2.5, "cm"),
                              ylim = c(0, 1)
      ),
      annotation_name_gp = gpar(fontsize = 4)
    )

    ht_iso2 <- Heatmap(data.matrix(iso_bug_wide_mat_limited),
                       row_gap = unit(0, "mm"),
                       right_annotation = ha1,
                       column_gap = unit(1, "mm"),
                       col = col_fun,
                       show_row_dend = F,
                       cluster_rows = T,
                       cluster_columns = T,
                       row_title_gp = gpar(fontsize = 4),
                       column_title_gp = gpar(fontsize = 4),
                       border_gp = gpar(col = "black", lty = 1),
                       gap = unit(0, "mm"),
                       row_title_rot = 0,
                       column_names_gp = gpar(fontsize = 4),
                       row_names_gp = gpar(fontsize = 5),
                       show_column_dend = F,
                       show_row_names = F,
                       column_split = translateMouseIdToTreatmentGroup(translateIsolateIDtoMouse(colnames(iso_bug_wide_mat))),
                       row_split = iso_annot_limited$feature
    )
    return(ht_iso2)
  }
}
