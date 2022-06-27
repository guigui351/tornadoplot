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
    shinybrowser::detect(),
    sidebarLayout(
      position = c("left"),
      fluid = TRUE,
      sidebarPanel(
        width = 3,
        style = "font-family: monospace; position: fixed; width: 20%; top: 88px; height: 1200px; overflow-y: auto;",
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
        tags$br(),tags$br(),
        span(h5("Arrange the following inputs to make the plot as per your wiches"), style="color:#045a8d"),
        shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
        sliderInput(ns("width"), "Plot Width (in)", min = 4, max = 25, value = 13),
        sliderInput(ns("height"), "Plot Height (in)", min = 4, max = 25, value = 8),
        sliderInput(ns('plotRes'), "Resolution (dpi)", min=60, max=320, value=110),
        tags$br(),tags$br(),
        span(h5("\n Once you're satisfied, please enter a filename and an extension and download the plot"), style="color:#045a8d"),

        textInput(ns("filename"), "Choose a filename:", value = "myplot"),
        shinyWidgets::prettyCheckboxGroup(
          inputId = ns("filetype"),
          label = "Select the file type!",
          choices = list("png", "pdf"),
          selected = c("png"),
          shape = "round",
          status = "info",
          fill = TRUE,
          inline = TRUE
        ),
        downloadButton(ns("download"), "Download Selected Plot"),
        span(h6("Increase DPI for better plot resolution"), style="color:#045a8d"),
      ),
      mainPanel(
        width = 9,
       style = "position: fixed; left: 21%; top: 88px; height: 1200px; overflow-y: auto;",
       tabsetPanel
       (tabPanel("Interactive plot", ggiraph::girafeOutput(ns('tornadoExplorer_interact'))),
        tabPanel("Static Plot", imageOutput(ns("tornadoExplorer")))
       ))
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

      #ref_arm_selected <- reactive(c(-input$ref_arm))

      # Define choices for Comparison ARM group
      shinyWidgets::updatePickerInput(
        session,
        'comp_arm',
        choices = c(
          trt_grp()
        ),
        selected = trt_grp()[-1] #ref_arm_selected()
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

    # Make inputs user reactive
    width <- reactive({input$width})
    height <- reactive({input$height})
    plotRes <- reactive({input$plotRes})

    #draw the chart
    individualGraph <- reactive({
      req(input$ref_arm)
      req(input$comp_arm)

     params_in <- reactive(init_tornado(mapping$data, mapping$settings, ref_arm = input$ref_arm, comp_arm = input$comp_arm))

     # Create main plot
     tornadoplot(params_in()$data,
                 params_in()$settings,
                 input$groupvar,
                 input$ref_arm,
                 input$comp_arm)
     })

    # Render interactive plot with ggiraph
    # To make the responsive to the change in UI size
    output$tornadoExplorer_interact <- renderPlot({
        individualGraph()
      },  height = height(), width = width(), dpi = plotRes())


    # Render plot as an image
    output$tornadoExplorer <- renderImage({

      # A temp file to save the output.
      outfile <- tempfile(fileext='.png')

      # Adapt showtext DPI based on resolution chosen by user.
      showtext::showtext_opts(dpi = input$plotRes)

      # Save png plot in the temp folder
      ggplot2::ggsave(
        filename = outfile,
        plot = individualGraph(),
        height = height(), width = width(), dpi = plotRes(), units = "in",
        type = "cairo",
      )

      # Return a list containing the filename
      list(src = normalizePath(outfile),
           width = width,
           height = height,
           contentType = 'image/png',
           alt = "This is alternate text")
    }, deleteFile = TRUE)



    # Download the plot chosen in png or pdf format
    #extrafont::loadfonts(device="pdf")
    output$download <- downloadHandler(
      file =  function() {
        paste(input$filename, input$filetype, sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        req(individualGraph())
        if(input$filetype == "png"){
          ggplot2::ggsave(
            filename = file,
            plot = individualGraph(),
            height = height(), width = width(), dpi = plotRes(), units = "in", # open the png device
            type = "cairo",
          )
        }
        else
          ggplot2::ggsave(
            filename = file,
            plot = individualGraph(),
            device = cairo_pdf, #grDevices::cairo_pdf(),
            height = height(), width = width(), dpi = plotRes(), units = "in",) # open the pdf device
      }
    )

  })
}

## To be copied in the UI
# mod_tornado_ui("tornado_1")

## To be copied in the server
# mod_tornado_server("tornado_1")
