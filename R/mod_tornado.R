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
        fluidRow(
          column(2,
                 shinyWidgets::dropdownButton(
                   span(h2("Plot parameters & Save"), style="color:#045a8d"),
                   span(h5("Arrange the following inputs to make the plot as per your wiches"), style="color:#045a8d"),
                   span(h6("You'll ve to save and download your plot to see the result"), style="color:#045a8d"),
                   shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
                   fluidRow(
                     column(4, sliderInput(ns("width"), "Plot Width (cm)", min = 14, max = 42, value = 22, step = 4)),
                     column(4, sliderInput(ns("height"), "Plot Height (cm)", min = 18, max = 40, value = 26, step = 4)),
                     column(4, sliderInput(ns('plotRes'), "Resolution (dpi)", min=60, max=320, value=  200, step = 10))
                   ),
                   tags$br(),
                   span(h5("\n Once you're satisfied, please enter a filename and an extension and download the plot"), style="color:#045a8d"),
                   fluidRow(
                     column(6, textInput(ns("filename"), "Choose a filename:", value = "myplot")),
                     column(6, shinyWidgets::radioGroupButtons(
                       inputId = ns("filetype"),
                       label = "Select the file type!",
                       choices = c(`<i class="fa fa-file-pdf-o" aria-hidden="true"></i>` = "pdf", `<i class="fa fa-file-image-o" aria-hidden="true"></i>` = "png"),
                       selected = c("png"),
                       justified = TRUE
                     )),
                   ),
                   shinyWidgets::downloadBttn(
                     ns("download"),
                     label = "Download Plot",
                     style = "material-circle",
                     color = "royal",
                     size = "sm",
                     block = FALSE,
                     no_outline = TRUE,
                     icon = shiny::icon("download")
                   ),
                   span(h6("Increase DPI for better plot resolution"), style="color:#045a8d"),
                   circle = TRUE, status = "danger", icon = icon("cloud-arrow-down", class="thin"), width = "600px",
                   tooltip = shinyWidgets::tooltipOptions(title = "Save it !")
                 )),
          column(
            9, align = "left",
            shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
            sliderInput(ns("height_window"), label = "Plot height (px) in the windows", min = 600, max = 1500, step = 100, value = 900)
          )
        ),
        span(h2("Data parameters"), style="color:#045a8d"),
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
        tags$br(),
        sliderInput(
          ns('threshold'),
          label = "AE Threshold (%):",
          min = 0, max = 15, value = 5
        ),
        span(h5("Specify the frequency of occurrence that an Adverse Event must exceed, within any arm or comparison group, to be reported"), style="color:#045a8d"),
        tags$br(),
        shinyWidgets::switchInput(
          inputId = ns("freqtable"),
          label = "Switch to frequency table",
          labelWidth = "290px",
          width = '360px',
        ),
        tags$br()
      ),
      mainPanel(
        width = 9,
        plotOutput(ns('tornadoExplorer'),  width = "auto", height = "900px")
       )
    ))
  )
}

#' tornado Server Functions
#'
#' @export
mod_tornado_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_local <- reactiveValues(
      trt_grp = NULL, # Buffer variable for trt_grp
      comp_arm_choices = NULL,  # Buffer variable for comp_arm
      filter_ready = NULL # Indicate if filters are finished computing
    )

    observe({

      # Update buffer variable trt_grp with available treatment groups in DM data
      r_local$trt_grp <- r_global$params$data$dm %>%
        dplyr::filter(!(!!rlang::sym(r_global$params$settings$dm$treatment_col) %in% c('SCRNFAIL','NOTASSGN', ''))) %>%
        dplyr::distinct(!!rlang::sym(r_global$params$settings$dm$treatment_col)) %>%
        dplyr::pull()

      # Update choices for Reference ARM
      shinyWidgets::updatePickerInput(session, 'ref_arm', choices = r_local$trt_grp, selected = r_local$trt_grp[1])
    })

    observeEvent(
      input$ref_arm,
      {

      # Update Comparison Arm choices with the ones not defined in input$ref_arm
      comp_arm_choices <- r_local$trt_grp[!r_local$trt_grp %in% input$ref_arm]

      if (!setequal(comp_arm_choices, r_local$comp_arm_choices)) {

        # Update choices for Comparison ARM
        shinyWidgets::updatePickerInput(session, 'comp_arm', choices = comp_arm_choices, selected = comp_arm_choices)

        r_local$comp_arm_choices <- comp_arm_choices
        return(NULL)
      }}, ignoreNULL = FALSE
    )

    observe({
      # Update choices for AE grouping by severity or seriousness
      shinyWidgets::updatePickerInput(session, 'groupvar',
                                      choices = c('None', "Severity" = r_global$params$settings$aes$severity_col, "Seriousness" = r_global$params$settings$aes$serious_col),
                                      selected = 'None')
    })

    # Draw the chart with inputs chosen
    individualGraph <- reactive({
      req(input$ref_arm)
      req(input$comp_arm)

      # Create specific dataset and settings for Adverse Event Tornado plot
      params_in <-
        reactive(
          init_tornado(
            r_global$params$data,
            r_global$params$settings,
            ref_arm = input$ref_arm,
            comp_arm = input$comp_arm,
            threshold = input$threshold
          )
        )

      # Use frequency table plot using input data/settings from init_tornado function
      if (input$freqtable) {
        # Create main plot
        tornadoplot_wtable(
          params_in()$data,
          params_in()$settings,
          input$groupvar,
          input$ref_arm,
          input$comp_arm
        )
      # Without frequency table plot using input data/settings from init_tornado function
      } else {
        tornadoplot(
          params_in()$data,
          params_in()$settings,
          input$groupvar,
          input$ref_arm,
          input$comp_arm
        )
      }

    })

    # Download parameters : set width/height/plotRes reactive
    width <- reactive({input$width})
    height_window <- reactive({input$height_window})
    height <- reactive({input$height})
    plotRes <- reactive({input$plotRes})

    # Render the plot object
    observe({
         output$tornadoExplorer <- renderPlot({
           individualGraph()
         }, res = 110, height = height_window())
    })

    # Download the plot chosen in png or pdf format
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
            height = height(), width = width(), dpi = plotRes(), units = "cm", # open the png device
            type = "cairo"
          )
        }
        else
          ggplot2::ggsave(
            filename = file,
            plot = individualGraph(),
            device = cairo_pdf, #grDevices::cairo_pdf(),
            height = height(), width = width(), dpi = plotRes(), units = "cm") # open the pdf device
      }
    )

  })
}

## To be copied in the UI
# mod_tornado_ui("tornado_1")

## To be copied in the server
# mod_tornado_server("tornado_1")
