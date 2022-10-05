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
#' @importFrom dplyr ungroup distinct pull filter select mutate left_join if_else
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_col geom_segment geom_point geom_text scale_y_continuous coord_cartesian scale_size scale_fill_manual
#'  scale_color_manual coord_flip guides theme theme_void labs theme_light arrow unit guide_legend element_text element_line element_rect element_blank after_stat
#' @importFrom ggtext element_markdown geom_richtext
#' @importFrom patchwork plot_layout plot_annotation
#'
#'@export

tornadoplot_wtable <- function(data, settings, groupvar = "None", ref_arm, comp_arm){

  #########################################
  #   Prep data
  #########################################

  if(!is.null(groupvar)){
    data <- data %>%
      filter(group_col == {{ groupvar }})
  }
  else {
    data <- data %>%
      filter(group_col == "None")
  }

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
  if (length(unique_grp) - 1 > 3) {
    print( "Only 3 colors has been defined, update the app")
  }

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
      ggplot2::aes(xend = ggplot2::after_stat(x), y = 0, yend = diff_pos),
      col = "#497135",
      arrow = ggplot2::arrow(angle = 20, length = unit(1, "mm"))
    ) +

    # Y scale formatting
    ggplot2::scale_y_continuous("Percentage (%)", expand = c(0, 0), position = "right", breaks = seq(0, 100, 10), labels = abs) +

    # Use custom colors/fills for AE severity
    ggplot2::scale_fill_manual(values = grouping_col_trt) +
    ggplot2::scale_color_manual(values = grouping_col_trt) +

    # Flip cartesian coordinates so that horizontal becomes vertical, and vertical, horizontal
    ggplot2::coord_flip(clip = "off", ylim = c(0, 100)) +

    # Change legend keys (colors) in order to have a grey scale instead of AE severity colors chosen above
    ggplot2::guides(fill = ggplot2::guide_legend("AE Grouping:", reverse = TRUE, byrow = TRUE, override.aes = list(fill = legend_col))) +

    # Set void theme
    ggplot2::theme_void(base_family = "sans") +

    # Update global theme
    ggplot2::theme(

      # The default font color when not explicitly specified
      text = ggplot2::element_text(color = "grey24"),

      # Use a light color for the background of the plot and the panel.
      plot.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      panel.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),

      # Adjust axis parameters such as size and color.
      axis.text.x.top = ggplot2::element_text(size = 6, color = "grey24"),
      axis.ticks.x.top = ggplot2::element_line(size = 0.2, color = "grey24"),
      axis.ticks.length.x.top = ggplot2::unit(1, "mm"),
      axis.title.x.top = ggplot2::element_text(hjust=0.965, size = 6, margin = ggplot2::margin(0, 0, 5, 0)),

      # remove Y axis elements
      axis.title.y = NULL,
      axis.text.y = NULL,
      axis.ticks.y = NULL,

      # Customize legend text and position
      legend.position = c(0.77, 0.5),
      legend.direction = "vertical",
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.box.spacing = ggplot2::unit(.5, "cm"),
      legend.spacing.y = ggplot2::unit(.5, "cm"),
      legend.margin = ggplot2::margin(),

      # Set plot margins
      plot.margin = ggplot2::margin(10, 0, 0, 0),
      panel.spacing = ggplot2::margin(10, 0, 0, 0)
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
      ggplot2::aes(xend = ggplot2::after_stat(x), y = 0, yend = diff_neg),
      col = "#444E5F",
      arrow = ggplot2::arrow(angle = 20, length = ggplot2::unit(1, "mm"))
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
      text = ggplot2::element_text(color = "grey24"),

      # Use a light color for the background of the plot and the panel.
      plot.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      panel.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),

      # Adjust axis parameters such as size and color.
      axis.text.x.top = ggplot2::element_text(size = 6, color = "grey24"),
      axis.ticks.x.top = ggplot2::element_line(size = 0.2, color = "grey24"),
      axis.ticks.length.x.top = ggplot2::unit(1, "mm"),
      axis.title.x.top = ggplot2::element_text(hjust=0.035, size = 6, margin = ggplot2::margin(0, 0, 5, 0)),

      # remove Y axis elements
      axis.title.y = NULL,
      axis.text.y = NULL,
      axis.ticks.y = NULL,

      # Set plot margins
      plot.margin = ggplot2::margin(10, 0, 0, 0),
      panel.spacing = ggplot2::margin(10, 0, 0, 0)
    )

  ## Present n(%) of AEs per treatment group
  freqtable <- data %>%
    dplyr::filter(group_val == "Total") %>%
    dplyr::ungroup() %>%
    dplyr::select(term_col, starts_with("placebo") & !ends_with("minus"), starts_with("treatment")) %>%
    tidyr::pivot_longer(-term_col,
                 names_to = c("trt", ".value"),
                 names_sep="_" ) %>%
    dplyr::mutate(trtn = dplyr::if_else(trt == "placebo", 1, 2))

  aetable <-
    ggplot2::ggplot(data = freqtable, ggplot2::aes(x = trtn, y = forcats::fct_reorder(term_col,perc))) +

    # Draw bubble point, size depends of  number of events by trt grp
    ggplot2::geom_point(ggplot2::aes(color = trt, size = n), alpha = 0.66) +

    # Add text to draw number of events for each PT and related %
    ggplot2::geom_text(
      ggplot2::aes(x = trtn+0.8, label = glue::glue("{n} ({perc}%)")),
      stat = "unique",
      family = "Arial",
      fontface = "bold",
      size = 2.5,
      hjust = 1
    ) +

    # include plot points overlapping Y/X axis (if any)
    ggplot2::coord_cartesian(expand = TRUE, clip = "off", xlim = c(0.9, 3)) +

    # Customize color gradient legend
    ggplot2::scale_color_manual(
      label = c(paste({{ ref_arm }}, collapse = " + ", sep = "\n"), paste({{ comp_arm }}, collapse = " + ", sep = "<br>")),
      values = c("#8e9aaf", "#97C684"),
      name = NULL
    ) +

    # Customize size legend
    ggplot2::scale_size(range = c(0.1, 5), guide = "none") +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL, label.position = "bottom",
                                        override.aes = list(shape = 18, size = 8, color = c("#8e9aaf", "#97C684")))
    ) +

    # remove x axis title
    ggplot2::labs(x = NULL) +

    ggplot2::theme_light() +

    # Customize theme
    ggplot2::theme(

      # The default font color when not explicitly specified
      text = ggplot2::element_text(color = "grey24"),

      # Use a light color for the background of the plot and the panel.
      plot.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      panel.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),

      # remove grid elements and border
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x  = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),

      # remove ticks and title elements on Y axis
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(family = "Arial", size = 8),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),

      # change legend appearance
      legend.position = "top",
      legend.text = ggtext::element_markdown(family = "Arial", size = 8),
      legend.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      legend.key = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
      legend.spacing.x = ggplot2::unit(1.5, 'cm'),
      legend.margin = ggplot2::margin(0,90,-10,20),

      # Plot margins
      plot.margin = ggplot2::margin(10, 15, 0, 0)
    )

  # Combine both ggplot elements into the same graphic
  final <- placebo_plot + treatment_plot + patchwork::plot_layout(widths = c(1,1)) +
    ggtext::geom_richtext(
      x = 11, y = -80, hjust = 0,
      label = paste0("<b style='color:#444E5F'>",paste({{ ref_arm }}, collapse = " + ", sep = "\u000A"),"</b>"),
      label.colour = "grey80", size = 2.7) +
    ggtext::geom_richtext(
      x = 4, y = 32, hjust = 0,
      label = paste0("<b style='color:#497135'>", paste({{ comp_arm }}, collapse  = " + ", sep = "\u000A"), "</b>"),
      label.colour = "grey80", size = 2.7)

  # Add caption inside the plot
  barplot <- aetable + final + patchwork::plot_layout(widths = c(0.25,0.75)) +
    patchwork::plot_annotation(
      caption =
paste0("The **bars** indicate the frequency of adverse events for <b style='color:#444E5F'>",paste({{ ref_arm }}, collapse = " + "),"</b>
and <b style='color:#497135'>",paste({{ comp_arm }}, collapse = " + "), " </b>. Only AEs with at least 1% occurence are presented.<br>
The **arrows** show the difference of percentage between both groups. Arrows pointing to the right indicate a positive difference in favor of ", paste({{ ref_arm }}, collapse = " + "),
"while arrows pointing to the left indicate a higher frequency of AEs<br> in ", paste({{ ref_arm }}, collapse = " + "), " than ", paste({{ comp_arm }}, collapse = " + "),".\n\n<i>(CDISC Pilot data)</i>"),
      theme = ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                             plot.background = ggplot2::element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
                             plot.caption.position =  "plot",
                             plot.margin = ggplot2::margin(10, 25, 10, 25),
                             plot.caption = ggtext::element_markdown(hjust = 0.01, size = 8, color = "grey38"))
    )

  return(barplot)

}

