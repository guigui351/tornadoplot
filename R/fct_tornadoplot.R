#' Create a tornado plot
#'
#' @param data A dataframe list
#' @param settings Named list of settings
#'
#' @return a ggplot object render in the shinyapp
#'
#' @examples
#' settings<-list(
#'   stratification_col="AEBODSYS",
#'   group_col="ARM",
#'   severity_col = "AESEV",
#'   serious_col = "AESER",
#'   reference_group="Placebo",
#'   comparison_group="Xanomeline High Dose",
#'   id_col="USUBJID"
#' )
#' tornadoplot(data, settings)
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import ggtext
#' @import patchwork
#'
#'@export

tornadoplot <- function(data, settings, groupvar = "None"){

  #########################################
  #   Prep data
  #########################################

  data <- data %>%
    filter(group_col == {{ groupvar }})

  unique_grp <- data %>% dplyr::ungroup() %>% dplyr::distinct(group_val) %>% dplyr::pull()

  if (length(unique_grp)-1 %in% c(0,1)) {
    grouping_col_trt <- c("#D0F1BF")
    grouping_col_plb <- c("#cbc0d3")
    legend_col <- c("grey80")
  }

  if (length(unique_grp) - 1 == 2) {
    grouping_col_trt <- c("#D0F1BF", "#97C684")
    grouping_col_plb <- c("#cbc0d3", "#8e9aaf")
    legend_col <- c("grey55", "grey80")
  }

  if (length(unique_grp) - 1 == 3) {
    grouping_col_trt <- c("#D0F1BF", "#97C684", "#497135")
    grouping_col_plb <- c("#cbc0d3", "#8e9aaf", "#444E5F")
    legend_col <- c("grey30", "grey55", "grey80")
  }

  if (length(unique_grp) > 4) {
    "Only 3 colors has been defined, update the app"
  }

  data_out <<- data
  grouping_col_trt_out <<- grouping_col_trt

  treatment_plot <-
    # Order plot by descending frequency of total percentage (placebo + treatment combined)
    ggplot2::ggplot(
      data = data %>% dplyr::filter(group_val != "Total"),
      ggplot2::aes(forcats::fct_reorder(term_col, total_perc))
    ) +

    # Create barplot, fill the bar AE severity frequency
    ggplot2::geom_col(ggplot2::aes(y = treatment_perc, fill = group_val)) +

    # Create a line to show the difference between Placebo and Treatment frequencies
    ggplot2::geom_segment(
      data = data %>% dplyr::filter(group_val == "Total"),
      ggplot2::aes(xend = after_stat(x), y = 0, yend = diff_pos),
      col = "#497135",
      arrow = arrow(angle = 20, length = unit(1, "mm"))
    ) +

    # Add PT text information near the bars
    ggplot2::geom_text(
      data = data %>% dplyr::filter(group_val == "Total"),
      ggplot2::aes(y = treatment_perc, label = term_col),
      hjust = 0,
      nudge_y = 2,
      col = "grey4",
      size = 2
    ) +

    # Y scale formatting
    ggplot2::scale_y_continuous("Percentage (%)", expand = c(0, 0), position = "right", breaks = seq(0, 100, 10), labels = abs) +

    # Use custom colors/fills for AE severity
    ggplot2::scale_fill_manual(values = grouping_col_trt) +
    ggplot2::scale_color_manual(values = grouping_col_trt) +

    # Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal
    ggplot2::coord_flip(clip = "off", ylim = c(0, 100)) +

    # Change legend keys (colors) in order to have a grey scale instead of AE severity colors chosen above
    ggplot2::guides(fill = guide_legend("AE Grouping:", reverse = TRUE, byrow = TRUE, override.aes = list(fill = legend_col))) +

    # Set void theme
    ggplot2::theme_void(base_family = "sans") +

    # Update global theme
    ggplot2::theme(

      # The default font color when not explicitly specified
      text = element_text(color = "grey24"),

      # Use a light color for the background of the plot and the panel.
      plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),

      # Adjust axis parameters such as size and color.
      axis.text.x.top = element_text(size = 6, color = "grey24"),
      axis.ticks.x.top = element_line(size = 0.2, color = "grey24"),
      axis.ticks.length.x.top = unit(1, "mm"),
      axis.title.x.top = element_text(hjust=0.965, size = 6, margin = margin(0, 0, 5, 0)),

      # remove Y axis elements
      axis.title.y = NULL,
      axis.text.y = NULL,
      axis.ticks.y = NULL,

      # Customize legend text and position
      legend.position = c(0.77, 0.5),
      legend.direction = "vertical",
      legend.key.width = unit(1, "cm"),
      legend.box.spacing = unit(.5, "cm"),
      legend.spacing.y = unit(.5, "cm"),
      legend.margin = margin(),

      # Set plot margins
      plot.margin = margin(10, 0, 0, 0),
      panel.spacing = margin(10, 0, 0, 0)
    )


  # Create barplot of each AE by AE severity for placebo group
  placebo_plot <-
    # Order plot by descending frequency of total percentage (placebo + treatment combined)
    ggplot2::ggplot(
      data = data %>% dplyr::filter(group_val != "Total"),
      ggplot2::aes(forcats::fct_reorder(term_col, total_perc))
    ) +

    # Create barplot, fill the bar AE severity frequency
    ggplot2::geom_col(ggplot2::aes(y = -placebo_perc, fill = group_val)) +

    # Create a line to show the difference between Placebo and Treatment frequencies
    ggplot2::geom_segment(
      data = data %>% dplyr::filter(group_val == "Total"),
      ggplot2::aes(xend = after_stat(x), y = 0, yend = diff_neg),
      col = "#444E5F",
      arrow = arrow(angle = 20, length = unit(1, "mm"))
    ) +

    # Add PT text information near the bars
    ggplot2::geom_text(
      data = data %>% dplyr::filter(group_val == "Total"),
      ggplot2::aes(y = -placebo_perc, label = term_col),
      hjust = 1,
      nudge_y = -2,
      col = "grey4",
      size = 2
    ) +

    # Y scale formatting
    ggplot2::scale_y_continuous("Percentage (%)", expand = c(0, 0), position = "right", breaks = seq(-100, 0, 10), labels = abs) +

    # Use custom colors/fills for AE severity
    ggplot2::scale_fill_manual(values = grouping_col_plb) +
    ggplot2::scale_color_manual(values = grouping_col_plb) +

    # Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal
    ggplot2::coord_flip(clip = "off", ylim = c(-100, -0)) +

    # Remove legends
    ggplot2::guides(fill = "none", color = "none")  +

    # Set void theme
    ggplot2::theme_void(base_family = "sans") +

    # Update global theme
    ggplot2::theme(

      # The default font color when not explicitly specified
      text = element_text(color = "grey24"),

      # Use a light color for the background of the plot and the panel.
      plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),

      # Adjust axis parameters such as size and color.
      axis.text.x.top = element_text(size = 6, color = "grey24"),
      axis.ticks.x.top = element_line(size = 0.2, color = "grey24"),
      axis.ticks.length.x.top = unit(1, "mm"),
      axis.title.x.top = element_text(hjust=0.035, size = 6, margin = margin(0, 0, 5, 0)),

      # remove Y axis elements
      axis.title.y = NULL,
      axis.text.y = NULL,
      axis.ticks.y = NULL,

      # Set plot margins
      plot.margin = margin(10, 0, 0, 0),
      panel.spacing = margin(10, 0, 0, 0)
    )

  # Combine both ggplot elements into the same graphic
  final <- placebo_plot + treatment_plot + patchwork::plot_layout(widths = c(1,1)) +
    patchwork::plot_annotation(
      caption = "(CDISC Pilot data)",
      theme = ggplot2::theme(panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                    plot.caption = element_text(hjust = 0.005, size = 8, face= "italic"),
                    plot.caption.position =  "plot",
                    plot.margin = margin(20, 25, 20, 25))
    )



  # Add subtitle inside the plot
  final1 <- final +
    ggtext::geom_richtext(
             x = 11, y = -70, hjust = 0,
             label = "<b style='color:#444E5F'>Placebo</b>",
             label.colour = "grey80") +
    ggtext::geom_richtext(
             x = 4, y = 45, hjust = 0,
             label = "<b style='color:#497135'>Xanomeline High Dose</b>",
             label.colour = "grey80") +
    ggtext::geom_textbox(
      inherit.aes = FALSE,
      data = tibble(
        x = 2.8,
        y = -80,
        label = "The **bars** indicate the frequency of adverse events for <b style='color:#444E5F'>Placebo group</b>
    and <b style='color:#497135'>Treatment group</b>. Only AEs with at least 1% occurence are presented.<br>
    The **arrows** show the difference of percentage between both groups. Arrows pointing to the right indicate a positive difference in favor of placebo,
    while arrows pointing to the left indicate a higher frequency of AEs in placebo group than treatment group."),
      aes(
        x = x,
        y = y,
        label = label),
      hjust = 0, vjust = 0,
      box.size = 0,
      color = "grey38",
      width = 0.42,
      size = 3,
      family = "sans",
      fill = NA
    )

  return(final1)

}
