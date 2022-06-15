#' tornado UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
#' @export

mod_tornado_ui <- function(id){
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
mod_tornado_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mapping <- golem::get_golem_options("mapping")

    param_in <- reactive(
      init_tornado(data = mapping$data, settings = mapping$settings)
    )

    observe({

      # Define choices for AE grouping
      updateSelectizeInput(
        session,
        'groupvar',
        choices = c(
          'None',
          param_in()$settings$aes$severity_col,
          param_in()$settings$aes$serious_col
        ),
        selected = 'None'
      )

    })

    #draw the chart
    output$tornadoExplorer <- renderPlot({
        tornadoplot(param_in()$data,
                    param_in()$settings)
    }, width = 1100, height = 800, res = 80)

  })
}

## To be copied in the UI
# mod_tornado_ui("tornado_1")

## To be copied in the server
# mod_tornado_server("tornado_1")
