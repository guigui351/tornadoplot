#' Tornado App
#'
#' @param dfAE AE Data
#' @param dfDemog demog data
#' @param settings safetyGraphics settings
#'
#' @import shiny
#' @import safetyData
#' @import dplyr
#'
#' @export

TornadoApp <- function(dfAE=NULL, dfDemog = NULL, settings=NULL, runNow=TRUE){

    # Set defaults
    if(is.null(dfDemog)){
      dfDemog <- safetyData::sdtm_dm %>% 
        filter (ARMCD != "Scrnfail") %>%
        mutate (ARM=if_else (ARM != "Placebo", "Treatment", "Placebo"))
    }

    if(is.null(dfAE)){
      dfAE <- safetyData::sdtm_ae
    }

    if(is.null(settings)){
      settings <-list(
        aes=list(id_col="USUBJID", bodsys_col="AEBODSYS", term_col="AEDECOD", severity_col="AESEV", serious_col="AESER"),
        dm=list(id_col="USUBJID", treatment_col="ARM",  "treatment_values"=list(group1="Placebo", "group2" = "Xanomeline High Dose"))
      )
    }

    # Initialize Tornado settings and data
    params <- reactive(
      init_tornado(data = list(aes=dfAE, dm=dfDemog), settings = settings)
    )

    # params <- reactive({
    #     list(
    #         data=list(aes=dfAE, dm=dfDemog),
    #         settings=settings
    #     )
    # })

    # Call app
    app <- shinyApp(
        ui =  tornado_ui("tp"),
        server = function(input,output,session){
            callModule(tornado_server, "tp", params)
        }
    )

    if(runNow)
        runApp(app)
    else
    app
}
