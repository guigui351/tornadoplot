#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @export

app_server <- function(input, output, session) {
  # Your application server logic

  # Title on home page using typedjs package
  output$title <- typedjs::renderTyped({
    typedjs::typed(c("Study Tracker^400", "Safety<br> Visualisation"), typeSpeed = 15, smartBackspace = TRUE)
  })

  # Reactive values
  r_global <- reactiveValues()

  # params contains the params object (data used in the app and settings) defined by the user (see run_app.R)
  params <- golem::get_golem_options("mapping")

  # Initialize Settings, i.e fill missing settings if not provided by user
  r_global$params <-
    init_params(
      params$data,
      params$settings
    )

  r_global$params_with_adsl <- reactive(
    init_adsl(
      r_global$params$data,
      r_global$params$settings
    )
  )

  mod_tornado_server("tornado_1", r_global = r_global)
  #mod_enrolmap_server("enrolmap_1", r_global = r_global)
}
