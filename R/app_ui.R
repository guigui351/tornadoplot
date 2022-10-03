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

tagList(
  # Leave this function for adding external resources
  golem_add_external_resources(),
  shiny::bootstrapLib(),
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="description", content="..."),
    tags$meta(name="keywords", content="..."),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")
  ),
  tags$script(src = "https://kit.fontawesome.com/ba26632dd7.js"),
  # List the first level UI elements here
  navbarPage(title = "Study Tracker",
             header = "",
             footer = "",
             ## Home page
             tabPanel(
               title = "Home",
               value = "home",
               icon = icon("house-crack", class="thin fa-2x"),
               fluidPage(
                  # Image background
                  tags$div(class = "landing-block background-content",
                     img(src = glue::glue("www/img/", "healthanalytics.jpg"), style = "width:100%; background-position: center; background-repeat: no-repeat; background-size: cover; -webkit-background-size: cover;
           -moz-background-size: cover;
           -o-background-size: cover;")
                  ),
                  h1(typedjs::typedOutput("title"), class = "header shadow-light"),
                  h4(class = "light footer", style="text-align:center",
                      "Guillaume Abgrall: ", tags$a("Send Feedback!", href = "mailto:gabgrall@its.jnj.com?subject=Feedback regarding Study Tracker application!", class = "link"), emo::ji("coffee"))
               )
             ),
             ## Exposure plot page
             tabPanel(
               title = "Enrolment map",
               icon = icon("earth-americas", class = "fa-2x thin"),
               value = "enrolmap",
               mod_enrolmap_ui("enrolmap_1")
             ),
             ## Tornado plot page
             tabPanel(
               title = "Tornado plot",
               value = "tornado",
               icon = icon("chart-gantt", class = "fa-2x thin"),
               mod_tornado_ui("tornado_1")
             ),
             ## Exposure plot page
             tabPanel(
               title = "Exposure plot",
               icon = icon("chart-bar", class = "fa-2x thin"),
               value = "exposure"
              ),
             # Additional drop down menu about project/myself
             navbarMenu("More",
                        icon = icon("search", class = "fa-2x thin"),
                        tabPanel("About the project"),
                        tabPanel("About me")
             ),
             # navbarPage options
             selected="home",
             position="static-top",
             collapsible=FALSE,
             fluid=TRUE
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
    tags$link(href='https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500&display=swap', rel='stylesheet'),
    tags$link(href='https://fonts.googleapis.com/css2?family=Bitter&display=swap', rel='stylesheet'),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/old_style.css"),
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-162672430-2"),
    tags$script(
      "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'UA-162672430-2');"
    )
  )
}
