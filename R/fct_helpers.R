#' helpers
#'
#' @description A fct function to create theme of ggplot
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @export

# Theme for the plot
theme_swim <- function(base_size = 28, base_family = "Roboto Condensed", ...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Playfair Display",
                          color = "grey10"),
      # remove all axes
      axis.line.x = ggplot2::element_line(color = "grey20", size = 3),
      axis.line.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(color = "grey20", size = 24),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggtext::element_markdown(size = 18,
                                              color = "grey20",
                                              hjust = 0.42,
                                              family = "Roboto Condensed",
                                              margin = ggplot2::margin(
                                                              t = 0.4,
                                                              unit = "cm")),
      axis.title.y = ggplot2::element_blank(),
      # add a subtle grid
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # background colors
      plot.background = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.background = ggplot2::element_rect(fill = "grey92", color = NA),
      legend.background = ggplot2::element_rect(fill = "grey92", color = NA),
      # borders and margins
      plot.margin = ggplot2::unit(c(1, 1, 1, 0.1), "cm"),
      panel.border = ggplot2::element_blank(),
      #legend
      legend.position =  c(.9, .88),
      # titles
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 9, hjust = 0,  color = "grey20"),
      legend.title.align = 0.5,
      legend.text.align = 0.1,
      plot.title = ggtext::element_markdown(size = 25/ggplot2::.pt, hjust = 0.5,
                                            color = "grey20"),
      plot.subtitle = ggtext::element_markdown(size = 16, hjust = 0.5,
                                               color = "grey20",
                                               margin = ggplot2::margin(
                                                               b = -0.1,
                                                               t = -0.1,
                                                               l = 2,
                                                               unit = "cm"),
                                               debug = F),
      # captions
      plot.caption = ggtext::element_markdown(size = 9,
                                              family = "Roboto Condensed",
                                              hjust = .98,
                                              margin = ggplot2::margin(
                                                              t = 0.2,
                                                              b = 0,
                                                              unit = "cm"),
                                              color = "grey30"),
      ...
    )
}
#' @importFrom ggplot2 ggplot ylab xlab theme_bw scale_x_date scale_colour_manual margin element_blank element_text
#' @export
# function to plot cumulative enrollment by date
cumulative_plot_enrl = function(enrolled_aggregated, plot_date) {
  plot_df = subset(enrolled_aggregated, month <= plot_date)
  g1 = ggplot2::ggplot(plot_df, ggplot2::aes(x = month, y = cumsum_ce)) + ggplot2::geom_line(color = "#cc4c02") + ggplot2::geom_point(size = 1, alpha = 0.8, color = "#cc4c02") +
    ggplot2::ylab("Cum. enrolled") +  ggplot2::xlab("Date") + ggplot2::theme_bw() +
    ggplot2::scale_colour_manual(values="#cc4c02") +
    ggplot2::scale_x_date(date_labels="%Y-%b",date_breaks  ="6 month") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "", axis.title = ggplot2::element_text(size=20/ggplot2::.pt), axis.text = ggplot2::element_text(size=15/ggplot2::.pt),
          plot.margin = ggplot2::margin(5, 12, 5, 5))
  g1
}

#' @importFrom ggplot2 ggplot ylab xlab theme_bw scale_x_date scale_colour_manual margin element_blank element_text
#' @export
# function to plot cumulative rando by date
cumulative_plot_rando = function(rando_aggregated, plot_date) {
  plot_df = subset(rando_aggregated, month <= plot_date)
  g2 = ggplot2::ggplot(plot_df, ggplot2::aes(x = month, y = cumsum_cr)) + ggplot2::geom_line(color = "#cc4c02") + ggplot2::geom_point(size = 1, alpha = 0.8, color = "#cc4c02") +
    ggplot2::ylab("Cum. rando") +  ggplot2::xlab("Date") + ggplot2::theme_bw() +
    ggplot2::scale_colour_manual(values="#cc4c02") +
    ggplot2::scale_x_date(date_labels="%Y-%b",date_breaks  ="6 month") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "", axis.title = ggplot2::element_text(size=20/ggplot2::.pt), axis.text = ggplot2::element_text(size=15/ggplot2::.pt),
          plot.margin = ggplot2::margin(5, 12, 5, 5))
  g2
}
