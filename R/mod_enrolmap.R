#' enrolmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importClassesFrom sp SpatialPolygonsDataFrame SpatialPolygons
#' @importMethodsFrom sp $

mod_enrolmap_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class="outer",
        leaflet::leafletOutput(ns("mymap"), width="100%", height="100%"),

        absolutePanel(id = "controls", class = "panel panel-default",
                      top = 95, left = 65, width = 250, fixed=TRUE,
                      draggable = TRUE, height = "auto",

                      span(tags$i(h6("Reported enrolled and randomized participants are subject to variation depending of when data has been extracted")), style="color:#045a8d"),
                      h3(textOutput(ns("reactive_enrolled_count")), align = "right"),
                      h4(textOutput(ns("reactive_rando_count")), align = "right"),
                      h6(textOutput(ns("clean_date_reactive")), align = "right"),
                      h6(textOutput(ns("reactive_country_count")), align = "right"),
                      h6(textOutput(ns("reactive_siteid_count")), align = "right"),
                      plotOutput(ns("cumulative_plot_enrl"), height="140px", width="100%"),
                      plotOutput(ns("cumulative_plot_rando"), height="140px", width="100%"),

                      shinyWidgets::sliderTextInput(ns("plot_date"),
                                                   label = h5("Select mapping date"),
                                                   choices = format(as.Date(Sys.Date(),"%Y-%m-%d"), "%d %b %y"),
                                                   selected = format(as.Date(Sys.Date(),"%Y-%m-%d"), "%d %b %y"),
                                                   grid = FALSE,
                                                   animate=animationOptions(interval = 2000, loop = FALSE)),
                      shinyWidgets::materialSwitch(
                        inputId = ns("providertitles"),
                        label = "Light / Black mode",
                        value = TRUE,
                        right = TRUE
                      )

        )
    )
  )
}

#' map Server Functions
#'
#' @noRd
mod_enrolmap_server <- function(id, r_global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # mapping contains the params object, either defined by the user / or by the safetyGraphics app (see run_app.R)
    mapping <- golem::get_golem_options("mapping")

    # Get treatment groups from DM data
    trt_grp <- reactive(
      mapping$data$dm %>% dplyr::distinct(!!rlang::sym(mapping$settings$dm$treatment_col)) %>% dplyr::pull(),
    )

    # Load init_enrolmap function to get updated datasets ready to be used
    params_in <-
      reactive(
        init_enrolmap(
          mapping$data,
          mapping$settings
        )
      )

    # Use for first tabpanel / number of enrolled/randomized patients at yyyy date
    observe(priority = 1, {

      current_date = as.Date(Sys.Date(),"%Y-%m-%d")
      choice_date = sort(unique(params_in()$data$data_enrolled$rficdt))
      choice_date_latest = choice_date[length(choice_date)]

      # Updates choices dates using study informed consent date
      shinyWidgets::updateSliderTextInput(
        session,
        "plot_date",
        choices = format(choice_date, "%d %b %y"),
        selected = format(choice_date_latest, "%d %b %y")
       )
    })

    # Format mapping date entered by user
    formatted_date = reactive({
      format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
    })

    # Render mapping date entered by user
    output$clean_date_reactive <- renderText({
      format(as.POSIXct(formatted_date()),"%d %B %Y")
    })

    # Get data of enrolled participants up to the mapping date
    reactive_db = reactive({
      params_in()$data$data_enrolled %>% dplyr::filter(rficdt <= formatted_date())
    })

    # Get data of number of country enrolled up to the mapping date
    reactive_country = reactive({
      params_in()$data$enrolled_bycountry %>% dplyr::filter(month <= formatted_date()) %>%
        dplyr::distinct(!!rlang::sym(params_in()$settings$dm$country_col)) %>%
        dplyr::count()
    })

    # Get data of number sites opened at the mapping date
    reactive_siteid = reactive({
      params_in()$data$enrolled_bysiteid %>% dplyr::filter(month <= formatted_date()) %>%
        dplyr::distinct(!!rlang::sym(params_in()$settings$dm$siteid_col)) %>%
        dplyr::count()
    })

    # Get aggregated data of number of enrolled participants by country up to the mapping date
    reactive_db_sf = reactive({
      params_in()$data$country_centroid %>% dplyr::filter(month <= formatted_date()) %>%
        dplyr::group_by(iso3) %>%
        dplyr::slice(dplyr::n())
    })

    # Render total number of enrolled participants up to the mapping date
    output$reactive_enrolled_count <- renderText({
      paste0(prettyNum(sum(reactive_db()$case_enrolled), big.mark=","), " enrolled")
    })

    # Render total number of randomized participants up to the mapping date
    output$reactive_rando_count <- renderText({
      paste0(prettyNum(sum(reactive_db()$case_rando), big.mark=","), " randomized")
    })

    # Render number of countries that started to enroll participants
    output$reactive_country_count <- renderText({
      paste0(prettyNum(reactive_country()$n, big.mark=","), "  countries started to enroll")
    })

    # Render number of sites opened
    output$reactive_siteid_count <- renderText({
      paste0(prettyNum(reactive_siteid()$n, big.mark=","), "  sites opened")
    })

    # Plot cumulative number of enrolled participants up to the mapping date
    output$cumulative_plot_enrl <- renderPlot({
       cumulative_plot_enrl(params_in()$data$enrolled_aggregated, formatted_date())
    })

    # Plot cumulative number of randomized participants up to the mapping date
    output$cumulative_plot_rando <- renderPlot({
      cumulative_plot_rando(params_in()$data$rando_aggregated, formatted_date())
    })

    # # Leaflet / filter world map with countries that started to enroll participants
    reactive_polygons = reactive({
       tornadoplot::worldcountry[tornadoplot::worldcountry$"ADM0_A3" %in% reactive_db()$country, ]
    })


  #     cartoDB <- reactive({
  #       req(input$providertitles)
  #       if (input$providertitles){
  #         leaflet::providers$CartoDB.DarkMatter
  #       }
  #       else {
  #         leaflet::providers$CartoDB.Positron
  #       }
  # })


    observe(priority = 2, {
       bins <- c(0, 5, 10, 20, 30, Inf)
       qpal <- leaflet::colorBin("Blues", params_in()$data$enrolled_bycountry$cumsum_country_ce, bins = bins)

       if (input$providertitles) {
       # Leaflet / Render basemap
       output$mymap <- leaflet::renderLeaflet(
           # create base map
           leaflet::leaflet(reactive_polygons()) %>%
             leaflet::addTiles() %>%
             leaflet::addLayersControl(
               position = "bottomright",
               overlayGroups = c("Enrolled (cumulative)", "Randomized (cumulative)"),
               options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
             leaflet::hideGroup(c("Enrolled (new)")) %>%
             # Black mode
             leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
             leaflet::fitBounds(~-100,-60,~60,70) %>%
             leaflet::addLegend("bottomleft", pal = qpal, values = ~params_in()$data$enrolled_bycountry$cumsum_country_ce,
                                title = "<small>Enrolled by country</small>") %>%
             leaflet::addMiniMap()
        )} else {
          output$mymap <- leaflet::renderLeaflet(
            # create base map
            leaflet::leaflet(reactive_polygons()) %>%
              leaflet::addTiles() %>%
              leaflet::addLayersControl(
                position = "bottomright",
                overlayGroups = c("Enrolled (cumulative)", "Randomized (cumulative)"),
                options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
              leaflet::hideGroup(c("Enrolled (new)")) %>%
              # Light mode
              leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
              leaflet::fitBounds(~-100,-60,~60,70) %>%
              leaflet::addLegend("bottomleft", pal = qpal, values = ~params_in()$data$enrolled_bycountry$cumsum_country_ce,
                                 title = "<small>Enrolled by country</small>") %>%
              leaflet::addMiniMap()
       )}
     })

    # Update basemap and draw filled polygons and add circles for number of enrolled and randomized participants
    observeEvent(c(input$plot_date, input$providertitles), priority = 3, {
      # qpal <- leaflet::colorQuantile("Blues", params_in()$data$enrolled_bycountry$cumsum_country_ce, n = 7)
      bins <- c(0, 5, 10, 20, 30, Inf)
      qpal <- leaflet::colorBin("Blues", params_in()$data$enrolled_bycountry$cumsum_country_ce, bins = bins)
      leaflet::leafletProxy("mymap") %>%
        leaflet::clearMarkers() %>%
        leaflet::clearShapes() %>%

        leaflet::addCircleMarkers(data = reactive_db_sf(), lat = ~centroid.lat, lng = ~centroid.lon, weight = 3, radius = ~cumsum_country_ce*0.5,
                                  fillOpacity = 0.1, color = "#cc4c02", group = "Enrolled (cumulative)") %>%
        leaflet::addCircleMarkers(data = reactive_db_sf(), lat = ~centroid.lat, lng = ~centroid.lon, weight = 3, radius = ~cumsum_country_cr*0.3,
                                  fillOpacity = 0.4, color = "#cc4c02", group = "Randomized (cumulative)") %>%
        leaflet::addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.5, fillOpacity = 0.25, fillColor = ~qpal(reactive_db_sf()$cumsum_country_ce),
                    popup = paste0(
                      "<b>Country: </b>"
                      , reactive_polygons()$NAME_SORT
                      , "<br>"
                      ,"<b>Enrolled: </b>"
                      , reactive_db_sf()$cumsum_country_ce
                      , "<br>"
                      , "<b>Randomized: </b>"
                      , reactive_db_sf()$cumsum_country_cr
                      , "<br>"),
                    popupOptions = leaflet::popupOptions(closeOnClick = TRUE))

    })

  })
}

## To be copied in the UI
# mod_enrolmap_ui("enrolmap_1")

## To be copied in the server
# mod_enrolmap_server("enrolmap_1")
