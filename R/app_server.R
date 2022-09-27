#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @export

app_server <- function(input, output, session) {
  # Your application server logic

  output$title <- typedjs::renderTyped({
    typedjs::typed(c("Study Tracker^400", "Safety<br> Visualisation"), typeSpeed = 15, smartBackspace = TRUE)
  })

  mod_tornado_server("tornado_1")
}
