#' Initialize Settings and Data for Enrollement map widget
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#'
#' init_enrolmap(params$data, settings=params$settings, ref_arm= "Treatment", comp_arm = "Placebo")
#' @return returns list with data and settings used for mod_enrolmap
#'
#' @importFrom dplyr mutate filter arrange select group_by summarize if_else slice ungroup left_join right_join
#' @importFrom lubridate floor_date
#' @importFrom magrittr "%>%"
#' @export

init_enrolmap <- function(data, settings) {

  # Define mandatory settings, if user didn't
  if(is.null(settings)){
    settings<-list(
      dm=list(id_col="USUBJID", treatment_col="ARM", "treatment_values"=list(group1="Placebo", "group2" = "Xanomeline High Dose"), siteid_col="SITEID", country_col="COUNTRY", ifcdt_col="RFICDTC")
    )
  }

  # Convert settings to symbols ready for standard evaluation
  dm_id_sym <- rlang::sym(settings$dm$id_col)
  dm_treatment_sym <- rlang::sym(settings$dm$treatment_col)
  dm_siteid_sym <- rlang::sym(settings$dm$siteid_col)
  dm_country_sym <- rlang::sym(settings$dm$country_col)
  dm_ifcdt_sym <- rlang::sym(settings$dm$ifcdt_col)

  # Flag enrolled/randomized participants
  data_enrolled <- data$adsl %>%
    dplyr::mutate(RFICDT = dplyr::if_else(is.na(!!dm_ifcdt_sym),
                                          RANDDT,
                                          as.Date(!!dm_ifcdt_sym))) %>%
    dplyr::filter(!is.na(RFICDT)) %>%
    dplyr::select(!!dm_id_sym, !!dm_country_sym, !!dm_siteid_sym, RFICDT, RANDDT) %>%
    dplyr::mutate(case_enrolled = 1,
                  case_rando = dplyr::if_else(!is.na(RANDDT), 1, 0),
                  country = !!dm_country_sym)

  # Sum (and cumsum) of enrolled participants by date
  enrolled_aggregated <- data_enrolled %>%
    dplyr::group_by(month = lubridate::floor_date(RFICDT, "month")) %>%
    dplyr::summarize(sum_ce = sum(case_enrolled)) %>%
    dplyr::mutate(cumsum_ce = cumsum(sum_ce))

  # Sum (and cumsum) of randomized participants by date
  rando_aggregated <- data_enrolled %>%
    dplyr::group_by(month = lubridate::floor_date(RFICDT, "month")) %>%
    dplyr::summarize(sum_cr = sum(case_rando)) %>%
    dplyr::mutate(cumsum_cr = cumsum(sum_cr))

  # Number of enrolled participants by country
  enrolled_bycountry <- data_enrolled %>%
    dplyr::group_by(!!dm_country_sym, month = lubridate::floor_date(RFICDT, "month")) %>%
    dplyr::summarize(sum_country_ce = sum(case_enrolled)) %>%
    dplyr::mutate(cumsum_country_ce = cumsum(sum_country_ce)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(bins = cut(cumsum_country_ce, breaks = 10))
  #bins_quantile = cut(cumsum_country_ce, breaks=quantile(cumsum_country_ce, probs = seq(0, 1, by = 10), na.rm=TRUE), include.lowest = TRUE))

  # Number of enrolled participants by site
  enrolled_bysiteid <- data_enrolled %>%
    dplyr::group_by(!!dm_siteid_sym, month = lubridate::floor_date(RFICDT, "month")) %>%
    dplyr::summarize(sum_siteid_ce = sum(case_enrolled)) %>%
    dplyr::mutate(cumsum_siteid_ce = cumsum(sum_siteid_ce))  %>%
    dplyr::ungroup()

  # Number of randomized participants by country
  rando_bycountry <- data_enrolled %>%
    dplyr::group_by(!!dm_country_sym, month = lubridate::floor_date(RFICDT, "month")) %>%
    dplyr::summarize(sum_country_cr = sum(case_rando)) %>%
    dplyr::mutate(cumsum_country_cr = cumsum(sum_country_cr))  %>%
    dplyr::ungroup()

  # Get centroid coordinates of each country
  countryref <- CoordinateCleaner::countryref
  country_centroid <- countryref %>%
    dplyr::filter(is.na(source) & type == "country") %>%
    dplyr::group_by(name) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(code = as.character(iso3)) %>%
    # combine with DM data and country column
    dplyr::right_join(enrolled_bycountry, by = c("code" = settings$dm$country_col)) %>%
    dplyr::left_join(rando_bycountry, by = c("code" = settings$dm$country_col, "month" = "month"))

  # return params with new datasets and settings ready to be used in the mod_enrolmap
  params <-
    list(
      data = list(data_enrolled= data_enrolled,
                  enrolled_aggregated=enrolled_aggregated,
                  rando_aggregated=rando_aggregated,
                  enrolled_bycountry=enrolled_bycountry,
                  enrolled_bysiteid=enrolled_bysiteid,
                  rando_bycountry=rando_bycountry,
                  country_centroid=country_centroid),
      settings = settings
    )

  return(params)

}



