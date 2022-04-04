#' Custom ggplolt2 theme
#'
#' @param base_size size of text
#' @param base_family font type
#' @export
theme_pmuench <- function(base_size = 6, base_family = "")
{
  half_line <- base_size/2
  theme(
    line = element_line(colour = "black", size = 0.5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black",
                        size = 0.5, linetype = 1),
    text = element_text(family = base_family, face = "plain",
                        colour = "black", size = base_size,
                        lineheight = 0.9,  hjust = 0.5,
                        vjust = 0.5, angle = 0,
                        margin = margin(), debug = FALSE),

    axis.line = element_line(colour = "black", size = 0.5),
    axis.text = element_text(size = rel(0.8), colour = "black"),
    axis.text.x = element_text(margin = margin(t = 0.8*half_line/2),
                               vjust = 1),
    axis.text.y = element_text(margin = margin(r = 0.8*half_line/2),
                               hjust = 1),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.length = unit(half_line/2, "pt"),
    axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                b = 0.8 * half_line/2)),
    axis.title.y = element_text(angle = 90,
                                margin = margin(r = 0.8 * half_line,
                                                l = 0.8 * half_line/2)),

    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(fill = "white", colour = "white"),
    legend.key.size = unit(1, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = rel(0.8)),
    legend.text.align = NULL,
    legend.title = element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,

    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.margin = unit(half_line, "pt"), panel.margin.x = NULL,
    panel.margin.y = NULL, panel.ontop = FALSE,

    strip.background = element_rect(colour = "white", fill = "white"),
    strip.text = element_text(colour = "black", face="bold", size = rel(0.8)),
    strip.text.x = element_text(margin = margin(t = half_line,
                                                b = half_line)),
    strip.text.y = element_text(angle = -90,
                                margin = margin(l = half_line,
                                                r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"),
    strip.switch.pad.wrap = unit(0.1, "cm"),

    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2),
                              margin = margin(b = half_line * 1.2)),
    plot.margin = margin(half_line, half_line, half_line, half_line),
    complete = TRUE)
}


#' Translates the short gnome ID to human readable format
#'
#' @param dat vector of short genome names
#' @importFrom magrittr %>%
#' @export
translateGenomeIdToFullName <- function(dat) {
  require(dplyr)
  dat <- dat %>% replace(. == "yl44", "A. muciniphila") %>%
    replace(. == "i48", "B. caecimuris")  %>%
    replace(. == "yl27", "M. intestinale")  %>%
    replace(. == "yl45", "T. muris")  %>%
    replace(. == "yl2", "B. animalis")  %>%
    replace(. == "kb1", "E. faecalis")  %>%
    replace(. == "kb18", "A. muris")  %>%
    replace(. == "yl32", "E. clostridioformis")  %>%
    replace(. == "yl31", "F. plautii")  %>%
    replace(. == "yl58", "B. coccoides")  %>%
    replace(. == "i49", "L. reuteri")  %>%
    replace(. == "i46", "C. innocuum")
  return(dat)
}

#' Assigns the color to a treatment group
#'
#' @param dat vector of treatment groups
#' @importFrom magrittr %>%
#' @export
group2color <- function(dat) {
  require(dplyr)
  dat <- dat %>% replace(. == "Control", "#476090") %>%
    replace(. == "Tetracyclin", "#385337") %>%
    replace(. == "Vancomycin", "#8C3701") %>%
    replace(. == "Ciprofloxacin", "#E0A91B")
  return(dat)
}

#' Assigns one-letter code to mouse IDs for samples where WGS samples are present
#'
#' @param dat vector of treatment groups
#' @importFrom magrittr %>%
#' @export
translateMouseIdToABC <- function(dat) {
  require(dplyr)
  dat <- dat %>%  replace(. == "1692", "a") %>%
    replace(. == "1693", "b") %>%
    replace(. == "1694", "c") %>%
    replace(. == "1681", "a") %>%
    replace(. == "1683", "b") %>%
    replace(. == "1684", "c") %>%
    replace(. == "1686", "a") %>%
    replace(. == "1688", "b") %>%
    replace(. == "1690", "c") %>%
    replace(. == "1697", "a") %>%
    replace(. == "1698", "b") %>%
    replace(. == "1699", "c")
  return(dat)
}

#' Assigns treatment group to mouse id
#'
#' @param dat vector of mouse ids
#' @importFrom magrittr %>%
#' @export
translateMouseIdToTreatmentGroup <- function(dat) {
  require(dplyr)
  dat <- dat %>% replace(. == "1683", "Control") %>%
    replace(. == "1691", "Tetracyclin") %>%
    replace(. == "1692", "Tetracyclin") %>%
    replace(. == "1693", "Tetracyclin") %>%
    replace(. == "1694", "Tetracyclin") %>%
    replace(. == "1695", "Tetracyclin") %>%
    replace(. == "1681", "Control") %>%
    replace(. == "1682", "Control") %>%
    replace(. == "1683", "Control") %>%
    replace(. == "1684", "Control") %>%
    replace(. == "1685", "Control") %>%
    replace(. == "1686", "Ciprofloxacin") %>%
    replace(. == "1687", "Ciprofloxacin") %>%
    replace(. == "1688", "Ciprofloxacin") %>%
    replace(. == "1689", "Ciprofloxacin") %>%
    replace(. == "1690", "Ciprofloxacin") %>%
    replace(. == "1696", "Vancomycin") %>%
    replace(. == "1697", "Vancomycin") %>%
    replace(. == "1698", "Vancomycin") %>%
    replace(. == "1699", "Vancomycin") %>%
    replace(. == "1700", "Vancomycin")
  return(dat)
}

#' Assigns study phase to study day
#'
#' @param dat vector of days
#' @importFrom magrittr %>%
#' @export
binDaysByPhase <- function(dat){
  require(dplyr)
  dat <- dat %>% replace(. == 0, "pre-treatment") %>%
    replace(. == 4, "post-treatment") %>%
    replace(. == 9, "short-recovery") %>%
    replace(. == 14, "short-recovery") %>%
    replace(. == 18, "post-treatment") %>%
    replace(. == 23, "short-recovery") %>%
    replace(. == 30, "short-recovery") %>%
    replace(. == 37, "long-recovery") %>%
    replace(. == 44, "long-recovery") %>%
    replace(. == 49, "long-recovery") %>%
    replace(. == 53, "post-treatment") %>%
    replace(. == 58, "short-recovery") %>%
    replace(. == 63, "short-recovery") %>%
    replace(. == 67, "post-treatment") %>%
    replace(. == 72, "short-recovery") %>%
    replace(. == 79, "short-recovery")
  return(dat)
}

#' Assigns study phase (integer) with study day
#'
#' @param dat vector of days
#' @importFrom magrittr %>%
#' @export
binDaysByPhaseGroup <- function(dat){
  require(dplyr)
  dat <- dat %>% replace(. == 0, 1) %>%
    replace(. == 4, 1) %>%
    replace(. == 9, 1) %>%
    replace(. == 14, 2) %>%
    replace(. == 18, 2) %>%
    replace(. == 23, 3) %>%
    replace(. == 30, 4) %>%
    replace(. == 37, 1) %>%
    replace(. == 44, 2) %>%
    replace(. == 49, 3) %>%
    replace(. == 53, 3) %>%
    replace(. == 58, 5) %>%
    replace(. == 63, 6) %>%
    replace(. == 67, 3) %>%
    replace(. == 72, 7) %>%
    replace(. == 79, 8)
  return(dat)
}
#' Assigns a mouse ID to the isolate sample IDy
#'
#' @param dat vector of days
#' @importFrom magrittr %>%
#' @export
translateIsolateIDtoMouse <- function(dat) {
  require(dplyr)
  dat <- dat %>% replace(. == "DR1", "1681") %>%
    replace(. == "DR8", "1691")  %>%
    replace(. == "DR11", "1696")  %>%
    replace(. == "DR3", "1681")  %>%
    replace(. == "DR6", "1687")  %>%
    replace(. == "DR10", "1691")  %>%
    replace(. == "DR12", "1696")  %>%
    replace(. == "DR7", "1687")  %>%
    replace(. == "DR4", "1687")  %>%
    replace(. == "DR13", "1696") %>%
    replace(. == "SW61", "1691")  %>%
    replace(. == "SW62", "1691") %>%
    replace(. == "SW63", "1691") %>%
    replace(. == "SW64", "1691") %>%
    replace(. == "SW65", "1691") %>%
    replace(. == "SW66", "1691") %>%
    replace(. == "SW67", "1691") %>%
    replace(. == "SW68", "1691") %>%
    replace(. == "SW69", "1691") %>%
    replace(. == "SW70", "1691") %>%
    replace(. == "SW71", "1696") %>%
    replace(. == "SW72", "1696") %>%
    replace(. == "SW73", "1696") %>%
    replace(. == "SW74", "1696") %>%
    replace(. == "SW75", "1696") %>%
    replace(. == "SW76", "1696") %>%
    replace(. == "SW77", "1696") %>%
    replace(. == "SW78", "1696") %>%
    replace(. == "SW79", "1696") %>%
    replace(. == "SW80", "1696") %>%
    replace(. == "SW81", "1691") %>%
    replace(. == "SW82", "1691") %>%
    replace(. == "SW83", "1691") %>%
    replace(. == "SW84", "1691") %>%
    replace(. == "SW85", "1691") %>%
    replace(. == "SW86", "1691") %>%
    replace(. == "SW87", "1691") %>%
    replace(. == "SW88", "1691") %>%
    replace(. == "SW89", "1691") %>%
    replace(. == "SW90", "1691") %>%
    replace(. == "SW91", "1696") %>%
    replace(. == "SW92", "1696") %>%
    replace(. == "SW93", "1696") %>%
    replace(. == "SW94", "1696") %>%
    replace(. == "SW95", "1696") %>%
    replace(. == "SW96", "1696") %>%
    replace(. == "SW97", "1696") %>%
    replace(. == "SW98", "1696") %>%
    replace(. == "SW99", "1696") %>%
    replace(. == "SW100", "1696") %>%
    replace(. == "SW101", "1681") %>%
    replace(. == "SW102", "1681") %>%
    replace(. == "SW103", "1681") %>%
    replace(. == "SW104", "1681") %>%
    replace(. == "SW105", "1681") %>%
    replace(. == "SW106", "1681") %>%
    replace(. == "SW107", "1681") %>%
    replace(. == "SW108", "1681") %>%
    replace(. == "SW109", "1681") %>%
    replace(. == "SW110", "1681") %>%
    replace(. == "SW111", "1687") %>%
    replace(. == "SW112", "1687") %>%
    replace(. == "SW113", "1687") %>%
    replace(. == "SW114", "1687") %>%
    replace(. == "SW115", "1687") %>%
    replace(. == "SW116", "1687") %>%
    replace(. == "SW117", "1687") %>%
    replace(. == "SW118", "1687") %>%
    replace(. == "SW119", "1687") %>%
    replace(. == "SW120", "1687") %>%
    replace(. == "SW121", "1681") %>%
    replace(. == "SW122", "1681") %>%
    replace(. == "SW123", "1681") %>%
    replace(. == "SW124", "1681") %>%
    replace(. == "SW125", "1681") %>%
    replace(. == "SW126", "1681") %>%
    replace(. == "SW127", "1681") %>%
    replace(. == "SW128", "1681") %>%
    replace(. == "SW129", "1681") %>%
    replace(. == "SW130", "1681") %>%
    replace(. == "SW131", "1687") %>%
    replace(. == "SW132", "1687") %>%
    replace(. == "SW133", "1687") %>%
    replace(. == "SW134", "1687") %>%
    replace(. == "SW135", "1687") %>%
    replace(. == "SW136", "1687") %>%
    replace(. == "SW137", "1687") %>%
    replace(. == "SW138", "1687") %>%
    replace(. == "SW139", "1687") %>%
    replace(. == "SW140", "1687")
  return(dat)
}

#' Assigns short genome ID to the
#'
#' @param dat vector of days
#' @importFrom magrittr %>%
#' @export
translateIsolateIDtoBug <- function(dat) {
  require(dplyr)
  dat <- dat %>% replace(. == "DR1", "kb1") %>%
    replace(. == "DR8", "kb1")  %>%
    replace(. == "DR11", "kb1")  %>%
    replace(. == "DR3", "i46")  %>%
    replace(. == "DR6", "i46")  %>%
    replace(. == "DR10", "i46")  %>%
    replace(. == "DR12", "kb1")  %>%
    replace(. == "DR7", "yl58")  %>%
    replace(. == "DR4", "kb1")  %>%
    replace(. == "DR13", "yl58") %>%
    replace(. == "SW61", "kb1")  %>%
    replace(. == "SW62", "kb1") %>%
    replace(. == "SW63", "kb1") %>%
    replace(. == "SW64", "kb1") %>%
    replace(. == "SW65", "kb1") %>%
    replace(. == "SW66", "kb1") %>%
    replace(. == "SW67", "kb1") %>%
    replace(. == "SW68", "kb1") %>%
    replace(. == "SW69", "kb1") %>%
    replace(. == "SW70", "kb1") %>%
    replace(. == "SW71", "kb1") %>%
    replace(. == "SW72", "kb1") %>%
    replace(. == "SW73", "kb1") %>%
    replace(. == "SW74", "kb1") %>%
    replace(. == "SW75", "kb1") %>%
    replace(. == "SW76", "kb1") %>%
    replace(. == "SW77", "kb1") %>%
    replace(. == "SW78", "kb1") %>%
    replace(. == "SW79", "kb1") %>%
    replace(. == "SW80", "kb1") %>%
    replace(. == "SW81", "i46") %>%
    replace(. == "SW82", "i46") %>%
    replace(. == "SW83", "i46") %>%
    replace(. == "SW84", "i46") %>%
    replace(. == "SW85", "i46") %>%
    replace(. == "SW86", "i46") %>%
    replace(. == "SW87", "i46") %>%
    replace(. == "SW88", "i46") %>%
    replace(. == "SW89", "i46") %>%
    replace(. == "SW90", "i46") %>%
    replace(. == "SW91", "i46") %>%
    replace(. == "SW92", "i46") %>%
    replace(. == "SW93", "i46") %>%
    replace(. == "SW94", "i46") %>%
    replace(. == "SW95", "i46") %>%
    replace(. == "SW96", "i46") %>%
    replace(. == "SW97", "i46") %>%
    replace(. == "SW98", "i46") %>%
    replace(. == "SW99", "i46") %>%
    replace(. == "SW100", "i46") %>%
    replace(. == "SW101", "kb1") %>%
    replace(. == "SW102", "kb1") %>%
    replace(. == "SW103", "kb1") %>%
    replace(. == "SW104", "kb1") %>%
    replace(. == "SW105", "kb1") %>%
    replace(. == "SW106", "kb1") %>%
    replace(. == "SW107", "kb1") %>%
    replace(. == "SW108", "kb1") %>%
    replace(. == "SW109", "kb1") %>%
    replace(. == "SW110", "kb1") %>%
    replace(. == "SW111", "kb1") %>%
    replace(. == "SW112", "kb1") %>%
    replace(. == "SW113", "kb1") %>%
    replace(. == "SW114", "kb1") %>%
    replace(. == "SW115", "kb1") %>%
    replace(. == "SW116", "kb1") %>%
    replace(. == "SW117", "kb1") %>%
    replace(. == "SW118", "kb1") %>%
    replace(. == "SW119", "kb1") %>%
    replace(. == "SW120", "kb1") %>%
    replace(. == "SW121", "i46") %>%
    replace(. == "SW122", "i46") %>%
    replace(. == "SW123", "i46") %>%
    replace(. == "SW124", "i46") %>%
    replace(. == "SW125", "i46") %>%
    replace(. == "SW126", "i46") %>%
    replace(. == "SW127", "i46") %>%
    replace(. == "SW128", "i46") %>%
    replace(. == "SW129", "i46") %>%
    replace(. == "SW130", "i46") %>%
    replace(. == "SW131", "i46") %>%
    replace(. == "SW132", "i46") %>%
    replace(. == "SW133", "i46") %>%
    replace(. == "SW134", "i46") %>%
    replace(. == "SW135", "i46") %>%
    replace(. == "SW136", "i46") %>%
    replace(. == "SW137", "i46") %>%
    replace(. == "SW138", "i46") %>%
    replace(. == "SW139", "i46") %>%
    replace(. == "SW140", "i46")
  return(dat)
}


# day to phase
sampleIDtoWGS <- function(dat){
  require(dplyr)
  dat <- dat %>%
    replace(. == "1683_0", TRUE) %>%
    replace(. == "1683_4", TRUE) %>%
    replace(. == "1683_9", TRUE) %>%
    replace(. == "1683_14", TRUE) %>%
    replace(. == "1683_18", TRUE) %>%
    replace(. == "1683_23", TRUE) %>%
    replace(. == "1683_30", TRUE) %>%
    replace(. == "1683_37", TRUE) %>%
    replace(. == "1683_44", TRUE) %>%
    replace(. == "1683_49", TRUE) %>%
    replace(. == "1683_53", TRUE) %>%
    replace(. == "1683_58", TRUE) %>%
    replace(. == "1683_63", TRUE) %>%
    replace(. == "1683_67", TRUE) %>%
    replace(. == "1683_72", TRUE) %>%
    replace(. == "1683_79", TRUE) %>%

    replace(. == "1688_0", TRUE) %>%
    replace(. == "1688_4", TRUE) %>%
    replace(. == "1688_9", TRUE) %>%
    replace(. == "1688_14", TRUE) %>%
    replace(. == "1688_18", TRUE) %>%
    replace(. == "1688_23", TRUE) %>%
    replace(. == "1688_30", TRUE) %>%
    replace(. == "1688_37", TRUE) %>%
    replace(. == "1688_44", TRUE) %>%
    replace(. == "1688_49", TRUE) %>%
    replace(. == "1688_53", TRUE) %>%
    replace(. == "1688_58", TRUE) %>%
    replace(. == "1688_63", TRUE) %>%
    replace(. == "1688_67", TRUE) %>%
    replace(. == "1688_72", TRUE) %>%
    replace(. == "1688_79", TRUE) %>%

    replace(. == "1692_0", TRUE) %>%
    replace(. == "1692_4", TRUE) %>%
    replace(. == "1692_9", TRUE) %>%
    replace(. == "1692_14", TRUE) %>%
    replace(. == "1692_18", TRUE) %>%
    replace(. == "1692_23", TRUE) %>%
    replace(. == "1692_30", TRUE) %>%
    replace(. == "1692_37", TRUE) %>%
    replace(. == "1692_44", TRUE) %>%
    replace(. == "1692_49", TRUE) %>%
    replace(. == "1692_53", TRUE) %>%
    replace(. == "1692_58", TRUE) %>%
    replace(. == "1692_63", TRUE) %>%
    replace(. == "1692_67", TRUE) %>%
    replace(. == "1692_72", TRUE) %>%
    replace(. == "1692_79", TRUE) %>%

    replace(. == "1699_0", TRUE) %>%
    replace(. == "1699_4", TRUE) %>%
    replace(. == "1699_9", TRUE) %>%
    replace(. == "1699_14", TRUE) %>%
    replace(. == "1699_18", TRUE) %>%
    replace(. == "1699_23", TRUE) %>%
    replace(. == "1699_30", TRUE) %>%
    replace(. == "1699_37", TRUE) %>%
    replace(. == "1699_44", TRUE) %>%
    replace(. == "1699_49", TRUE) %>%
    replace(. == "1699_53", TRUE) %>%
    replace(. == "1699_58", TRUE) %>%
    replace(. == "1699_63", TRUE) %>%
    replace(. == "1699_67", TRUE) %>%
    replace(. == "1699_72", TRUE) %>%
    replace(. == "1699_79", TRUE) %>%

    replace(. == "1686_0", TRUE) %>%
    replace(. == "1686_14", TRUE) %>%
    replace(. == "1686_30", TRUE) %>%
    replace(. == "1686_49", TRUE) %>%
    replace(. == "1686_63", TRUE) %>%
    replace(. == "1686_79", TRUE) %>%

    replace(. == "1690_0", TRUE) %>%
    replace(. == "1690_14", TRUE) %>%
    replace(. == "1690_30", TRUE) %>%
    replace(. == "1690_49", TRUE) %>%
    replace(. == "1690_63", TRUE) %>%
    replace(. == "1690_79", TRUE) %>%

    replace(. == "1693_0", TRUE) %>%
    replace(. == "1693_14", TRUE) %>%
    replace(. == "1693_30", TRUE) %>%
    replace(. == "1693_49", TRUE) %>%
    replace(. == "1693_63", TRUE) %>%
    replace(. == "1693_79", TRUE) %>%

    replace(. == "1694_0", TRUE) %>%
    replace(. == "1694_14", TRUE) %>%
    replace(. == "1694_30", TRUE) %>%
    replace(. == "1694_49", TRUE) %>%
    replace(. == "1694_63", TRUE) %>%
    replace(. == "1694_79", TRUE) %>%

    replace(. == "1697_0", TRUE) %>%
    replace(. == "1697_14", TRUE) %>%
    replace(. == "1697_30", TRUE) %>%
    replace(. == "1697_49", TRUE) %>%
    replace(. == "1697_63", TRUE) %>%
    replace(. == "1697_79", TRUE) %>%

    replace(. == "1698_0", TRUE) %>%
    replace(. == "1698_14", TRUE) %>%
    replace(. == "1698_30", TRUE) %>%
    replace(. == "1698_49", TRUE) %>%
    replace(. == "1698_63", TRUE) %>%
    replace(. == "1698_79", TRUE) %>%

    replace(. == "1681_0", TRUE) %>%
    replace(. == "1681_14", TRUE) %>%
    replace(. == "1681_30", TRUE) %>%
    replace(. == "1681_49", TRUE) %>%
    replace(. == "1681_63", TRUE) %>%
    replace(. == "1681_79", TRUE) %>%

    replace(. == "1684_0", TRUE) %>%
    replace(. == "1684_14", TRUE) %>%
    replace(. == "1684_30", TRUE) %>%
    replace(. == "1684_49", TRUE) %>%
    replace(. == "1684_63", TRUE) %>%
    replace(. == "1684_79", TRUE) %>%

    replace(. == "1684_4", FALSE) %>%
    replace(. == "1684_9", FALSE) %>%
    replace(. == "1684_18", FALSE) %>%
    replace(. == "1684_23", FALSE) %>%
    replace(. == "1684_37", FALSE) %>%
    replace(. == "1684_44", FALSE) %>%
    replace(. == "1684_53", FALSE) %>%
    replace(. == "1684_58", FALSE) %>%
    replace(. == "1684_67", FALSE) %>%
    replace(. == "1684_72", FALSE) %>%

    replace(. == "1681_4", FALSE) %>%
    replace(. == "1681_9", FALSE) %>%
    replace(. == "1681_18", FALSE) %>%
    replace(. == "1681_23", FALSE) %>%
    replace(. == "1681_37", FALSE) %>%
    replace(. == "1681_44", FALSE) %>%
    replace(. == "1681_53", FALSE) %>%
    replace(. == "1681_58", FALSE) %>%
    replace(. == "1681_67", FALSE) %>%
    replace(. == "1681_72", FALSE) %>%

    replace(. == "1697_4", FALSE) %>%
    replace(. == "1697_9", FALSE) %>%
    replace(. == "1697_18", FALSE) %>%
    replace(. == "1697_23", FALSE) %>%
    replace(. == "1697_37", FALSE) %>%
    replace(. == "1697_44", FALSE) %>%
    replace(. == "1697_53", FALSE) %>%
    replace(. == "1697_58", FALSE) %>%
    replace(. == "1697_67", FALSE) %>%
    replace(. == "1697_72", FALSE) %>%

    replace(. == "1698_4", FALSE) %>%
    replace(. == "1698_9", FALSE) %>%
    replace(. == "1698_18", FALSE) %>%
    replace(. == "1698_23", FALSE) %>%
    replace(. == "1698_37", FALSE) %>%
    replace(. == "1698_44", FALSE) %>%
    replace(. == "1698_53", FALSE) %>%
    replace(. == "1698_58", FALSE) %>%
    replace(. == "1698_67", FALSE) %>%
    replace(. == "1698_72", FALSE) %>%

    replace(. == "1694_4", FALSE) %>%
    replace(. == "1694_9", FALSE) %>%
    replace(. == "1694_18", FALSE) %>%
    replace(. == "1694_23", FALSE) %>%
    replace(. == "1694_37", FALSE) %>%
    replace(. == "1694_44", FALSE) %>%
    replace(. == "1694_53", FALSE) %>%
    replace(. == "1694_58", FALSE) %>%
    replace(. == "1694_67", FALSE) %>%
    replace(. == "1694_72", FALSE) %>%

    replace(. == "1693_4", FALSE) %>%
    replace(. == "1693_9", FALSE) %>%
    replace(. == "1693_18", FALSE) %>%
    replace(. == "1693_23", FALSE) %>%
    replace(. == "1693_37", FALSE) %>%
    replace(. == "1693_44", FALSE) %>%
    replace(. == "1693_53", FALSE) %>%
    replace(. == "1693_58", FALSE) %>%
    replace(. == "1693_67", FALSE) %>%
    replace(. == "1693_72", FALSE) %>%

    replace(. == "1690_4", FALSE) %>%
    replace(. == "1690_9", FALSE) %>%
    replace(. == "1690_18", FALSE) %>%
    replace(. == "1690_23", FALSE) %>%
    replace(. == "1690_37", FALSE) %>%
    replace(. == "1690_44", FALSE) %>%
    replace(. == "1690_53", FALSE) %>%
    replace(. == "1690_58", FALSE) %>%
    replace(. == "1690_67", FALSE) %>%
    replace(. == "1690_72", FALSE) %>%

    replace(. == "1686_4", FALSE) %>%
    replace(. == "1686_9", FALSE) %>%
    replace(. == "1686_18", FALSE) %>%
    replace(. == "1686_23", FALSE) %>%
    replace(. == "1686_37", FALSE) %>%
    replace(. == "1686_44", FALSE) %>%
    replace(. == "1686_53", FALSE) %>%
    replace(. == "1686_58", FALSE) %>%
    replace(. == "1686_67", FALSE) %>%
    replace(. == "1686_72", FALSE) %>%

    replace(. == "1682_0", FALSE) %>%
    replace(. == "1682_4", FALSE) %>%
    replace(. == "1682_9", FALSE) %>%
    replace(. == "1682_14", FALSE) %>%
    replace(. == "1682_18", FALSE) %>%
    replace(. == "1682_23", FALSE) %>%
    replace(. == "1682_30", FALSE) %>%
    replace(. == "1682_37", FALSE) %>%
    replace(. == "1682_44", FALSE) %>%
    replace(. == "1682_49", FALSE) %>%
    replace(. == "1682_53", FALSE) %>%
    replace(. == "1682_58", FALSE) %>%
    replace(. == "1682_63", FALSE) %>%
    replace(. == "1682_67", FALSE) %>%
    replace(. == "1682_72", FALSE) %>%
    replace(. == "1682_79", FALSE) %>%

    replace(. == "1685_0", FALSE) %>%
    replace(. == "1685_4", FALSE) %>%
    replace(. == "1685_9", FALSE) %>%
    replace(. == "1685_14", FALSE) %>%
    replace(. == "1685_18", FALSE) %>%
    replace(. == "1685_23", FALSE) %>%
    replace(. == "1685_30", FALSE) %>%
    replace(. == "1685_37", FALSE) %>%
    replace(. == "1685_44", FALSE) %>%
    replace(. == "1685_49", FALSE) %>%
    replace(. == "1685_53", FALSE) %>%
    replace(. == "1685_58", FALSE) %>%
    replace(. == "1685_63", FALSE) %>%
    replace(. == "1685_67", FALSE) %>%
    replace(. == "1685_72", FALSE) %>%
    replace(. == "1685_79", FALSE) %>%

    replace(. == "1687_0", FALSE) %>%
    replace(. == "1687_4", FALSE) %>%
    replace(. == "1687_9", FALSE) %>%
    replace(. == "1687_14", FALSE) %>%
    replace(. == "1687_18", FALSE) %>%
    replace(. == "1687_23", FALSE) %>%
    replace(. == "1687_30", FALSE) %>%
    replace(. == "1687_37", FALSE) %>%
    replace(. == "1687_44", FALSE) %>%
    replace(. == "1687_49", FALSE) %>%
    replace(. == "1687_53", FALSE) %>%
    replace(. == "1687_58", FALSE) %>%
    replace(. == "1687_63", FALSE) %>%
    replace(. == "1687_67", FALSE) %>%
    replace(. == "1687_72", FALSE) %>%
    replace(. == "1687_79", FALSE) %>%

    replace(. == "1689_0", FALSE) %>%
    replace(. == "1689_4", FALSE) %>%
    replace(. == "1689_9", FALSE) %>%
    replace(. == "1689_14", FALSE) %>%
    replace(. == "1689_18", FALSE) %>%
    replace(. == "1689_23", FALSE) %>%
    replace(. == "1689_30", FALSE) %>%
    replace(. == "1689_37", FALSE) %>%
    replace(. == "1689_44", FALSE) %>%
    replace(. == "1689_49", FALSE) %>%
    replace(. == "1689_53", FALSE) %>%
    replace(. == "1689_58", FALSE) %>%
    replace(. == "1689_63", FALSE) %>%
    replace(. == "1689_67", FALSE) %>%
    replace(. == "1689_72", FALSE) %>%
    replace(. == "1689_79", FALSE) %>%

    replace(. == "1691_0", FALSE) %>%
    replace(. == "1691_4", FALSE) %>%
    replace(. == "1691_9", FALSE) %>%
    replace(. == "1691_14", FALSE) %>%
    replace(. == "1691_18", FALSE) %>%
    replace(. == "1691_23", FALSE) %>%
    replace(. == "1691_30", FALSE) %>%
    replace(. == "1691_37", FALSE) %>%
    replace(. == "1691_44", FALSE) %>%
    replace(. == "1691_49", FALSE) %>%
    replace(. == "1691_53", FALSE) %>%
    replace(. == "1691_58", FALSE) %>%
    replace(. == "1691_63", FALSE) %>%
    replace(. == "1691_67", FALSE) %>%
    replace(. == "1691_72", FALSE) %>%
    replace(. == "1691_79", FALSE) %>%

    replace(. == "1695_0", FALSE) %>%
    replace(. == "1695_4", FALSE) %>%
    replace(. == "1695_9", FALSE) %>%
    replace(. == "1695_14", FALSE) %>%
    replace(. == "1695_18", FALSE) %>%
    replace(. == "1695_23", FALSE) %>%
    replace(. == "1695_30", FALSE) %>%
    replace(. == "1695_37", FALSE) %>%
    replace(. == "1695_44", FALSE) %>%
    replace(. == "1695_49", FALSE) %>%
    replace(. == "1695_53", FALSE) %>%
    replace(. == "1695_58", FALSE) %>%
    replace(. == "1695_63", FALSE) %>%
    replace(. == "1695_67", FALSE) %>%
    replace(. == "1695_72", FALSE) %>%
    replace(. == "1695_79", FALSE) %>%

    replace(. == "1696_0", FALSE) %>%
    replace(. == "1696_4", FALSE) %>%
    replace(. == "1696_9", FALSE) %>%
    replace(. == "1696_14", FALSE) %>%
    replace(. == "1696_18", FALSE) %>%
    replace(. == "1696_23", FALSE) %>%
    replace(. == "1696_30", FALSE) %>%
    replace(. == "1696_37", FALSE) %>%
    replace(. == "1696_44", FALSE) %>%
    replace(. == "1696_49", FALSE) %>%
    replace(. == "1696_53", FALSE) %>%
    replace(. == "1696_58", FALSE) %>%
    replace(. == "1696_63", FALSE) %>%
    replace(. == "1696_67", FALSE) %>%
    replace(. == "1696_72", FALSE) %>%
    replace(. == "1696_79", FALSE) %>%


    replace(. == "1700_0", FALSE) %>%
    replace(. == "1700_4", FALSE) %>%
    replace(. == "1700_9", FALSE) %>%
    replace(. == "1700_14", FALSE) %>%
    replace(. == "1700_18", FALSE) %>%
    replace(. == "1700_23", FALSE) %>%
    replace(. == "1700_30", FALSE) %>%
    replace(. == "1700_37", FALSE) %>%
    replace(. == "1700_44", FALSE) %>%
    replace(. == "1700_49", FALSE) %>%
    replace(. == "1700_53", FALSE) %>%
    replace(. == "1700_58", FALSE) %>%
    replace(. == "1700_63", FALSE) %>%
    replace(. == "1700_67", FALSE) %>%
    replace(. == "1700_72", FALSE) %>%
    replace(. == "1700_79", FALSE)

  return(dat)
}


# day to phase
binDaysByShortPhase <- function(dat){
  require(dplyr)
  dat <- dat %>%
    replace(. == 4, TRUE) %>%
    replace(. == 9, FALSE) %>%
    replace(. == 14, FALSE) %>%
    replace(. == 18, TRUE) %>%
    replace(. == 23, FALSE) %>%
    replace(. == 30, FALSE) %>%
    replace(. == 37, FALSE) %>%
    replace(. == 44, FALSE) %>%
    replace(. == 49, FALSE) %>%
    replace(. == 53, TRUE) %>%
    replace(. == 58, FALSE) %>%
    replace(. == 63, FALSE) %>%
    replace(. == 67, TRUE) %>%
    replace(. == 72, FALSE) %>%
    replace(. == 79, FALSE)
  return(dat)
}

