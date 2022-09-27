#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @export
app_ui <- function(request) {

options <- list()

options(
  spinner.type = 7,
  spinner.color="#11a579",
  spinner.size = 1
)

tagList(
  # Leave this function for adding external resources
  golem_add_external_resources(),
  # List the first level UI elements here
  fullPage::pagePiling(
    sections.color = c('#2f2f2f', '#f9f7f1', '#f9f7f1'),
    opts = options,
    menu = c(
      "Home" = "home",
      "Tornado plot" = "tornado"
    ),
    fullPage::pageSectionImage(
      center = TRUE,
      img = "www/img/healthanalytics.jpg",
      menu = "home",
      h1(typedjs::typedOutput("title"), class = "header shadow-dark"),
      h3(
        class = "light footer",
        "Guillaume Abgrall: ", tags$a("Send Feedback!", href = "mailto:gabgrall@its.jnj.com?subject=Feedback regarding Study Tracker application!", class = "link"), emo::ji("coffee")
      )
    ),
    fullPage::pageSection(
      center = FALSE,
      menu = "swimmer",
      mod_tornado_ui("tornado_1")
    ),
  )
)
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "tornadoplot"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # tags$link(
    #   rel = "stylesheet", href = shinythemes::shinytheme("sandstone")
    # ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/style.css"),
  )
}
