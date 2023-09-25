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

  # Add small ADSL created with SDTM domains / use of ADMIRAL package
  r_global$params <-
    isolate(init_adsl(
      r_global$params$data,
      r_global$params$settings
    )
  )

  # Add small ADEX created with SDTM domains and ADSL / use of ADMIRAL package
  r_global$params <-
    isolate(init_adex(
      r_global$params$data,
      r_global$params$settings
    )
  )

  observeEvent(r_global$params, {
    mytest <<- r_global$params
  })

  mod_tornado_server("tornado_1", r_global = r_global)
  mod_enrolmap_server("enrolmap_1", r_global = r_global)
}
