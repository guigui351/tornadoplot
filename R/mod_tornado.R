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
        style = "font-family: monospace; position: fixed; width: 20%; top: 88px; height: 600px; overflow-y: auto;",
        shinyWidgets::pickerInput(
          ns("ref_arm"),
          "Reference Arm",
          choices = NULL,
          selected = "Placebo",
          #options = list(`actions-box` = TRUE),
          multiple = TRUE,
        ),
        span(h5("Multiple arms automatically combined into a single arm if more than one value selected"), style="color:#045a8d"),
        tags$br(),
        shinyWidgets::pickerInput(
          ns("comp_arm"),
          "Comparison Arm",
          choices = NULL,
          selected = "Treatment",
          #options = list(`actions-box` = TRUE),
          multiple = TRUE,
        ),
        span(h5("Multiple arms automatically combined into a single arm if more than one value selected"), style="color:#045a8d"),
        tags$br(),
        shinyWidgets::pickerInput(
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

    mapping <- golem::get_golem_options("mapping") # mapping contains the params object, either defined by the user / or by the safetyGraphics app (see run_app.R)

    trt_grp <- reactive(
      mapping$data$dm %>% dplyr::distinct(!!rlang::sym(mapping$settings$dm$treatment_col)) %>% dplyr::pull(),
    )

    observe({

      # Define choices for Reference ARM group
      shinyWidgets::updatePickerInput(
        session,
        'ref_arm',
        choices = c(
          trt_grp()
        ),
        selected = trt_grp()[1]
      )


      # Define choices for Comparison ARM group
      shinyWidgets::updatePickerInput(
        session,
        'comp_arm',
        choices = c(
          trt_grp()
        ),
        selected = trt_grp()[-1]
      )

      # Define choices for AE grouping
      shinyWidgets::updatePickerInput(
        session,
        'groupvar',
        choices = c(
          'None',
          "Severity" = mapping$settings$aes$severity_col,
          "Seriousness" = mapping$settings$aes$serious_col
        ),
        selected = 'None'
      )

    })

    #draw the chart
    output$tornadoExplorer <- renderPlot({
      req(input$ref_arm)
      req(input$comp_arm)

     params_in <- reactive(init_tornado(mapping$data, mapping$settings, ref_arm = input$ref_arm, comp_arm = input$comp_arm))
     tornadoplot(params_in()$data,
                 params_in()$settings,
                 input$groupvar,
                 input$ref_arm,
                 input$comp_arm)
    }, width = 1100, height = 800, res = 80)

  })
}

## To be copied in the UI
# mod_tornado_ui("tornado_1")

## To be copied in the server
# mod_tornado_server("tornado_1")
