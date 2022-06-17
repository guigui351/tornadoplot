#' tornado UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @export

tornado_ui <- function(id){
  ns <- NS(id)
  tagList(fluidPage(
    sidebarLayout(
      position = c("left"),
      fluid = TRUE,
      sidebarPanel(
        width = 3,
        style = "font-family: monospace; position: fixed; width: 20%; top: 88px; height: 300px; overflow-y: auto;",
        selectizeInput(
          ns('groupvar'),
          "AE Grouping:",
          choices = NULL,
          multiple = FALSE
        ),
      ),
      mainPanel(
        width = 9,
       style = "position: fixed; left: 21%; top: 88px; height: 800px; overflow-y: auto;",
       tabsetPanel(tabPanel(
          "Plot", plotOutput(ns("tornadoExplorer"))
       )))
    ))
  )
}

#' tornado Server Functions
#'
#' @export
tornado_server <- function(input, output, session, params) {
  ns <- session$ns

  observe({
    # Define choices for AE grouping
    updateSelectizeInput(
      session,
      'groupvar',
      choices = c(
        'None',
        "Severity" = params()$settings$aes$severity_col,
        "Seriousness" = params()$settings$aes$serious_col
      ),
      selected = 'None'
    )

  })

  #draw the chart
  output$tornadoExplorer <- renderPlot({
      tornadoplot(params()$data,
                  params()$settings,
                  input$groupvar)
  }, width = 1100, height = 800, res = 80)
}
