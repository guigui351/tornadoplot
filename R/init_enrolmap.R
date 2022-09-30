#' Initialize Settings and Data for Enrollement map widget
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#'
#' init_tornado(params$data, settings=params$settings, ref_arm= "Treatment", comp_arm = "Placebo")
#' @return returns list with data and settings used for fct_tornadoplot
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import stringr
#' @importFrom magrittr "%>%"
#' @export

init_enrolmap <- function(data, settings, ref_arm= "Treatment", comp_arm = "Placebo", threshold = 1) {

  # Print treatment group enter by users to the console
  print(comp_arm)
  print({{ ref_arm}})

  # Define mandatory settings, if user didn't
  if(is.null(settings)){
    settings<-list(
      dm=list(id_col="USUBJID", treatment_col="ARM", "treatment_values"=list(group1="Placebo", "group2" = "Xanomeline High Dose"), siteid_col="SITEID", country_col="COUNTRY", ifcdt_col="RFICDTC", rando_col="RFSTDTC")
    )
  }

  # Convert settings to symbols ready for standard evaluation
  dm_id_sym <- rlang::sym(settings$dm$id_col)
  dm_treatment_sym <- rlang::sym(settings$dm$treatment_col)
  dm_siteid_sym <- rlang::sym(settings$dm$siteid_col)
  dm_country_sym <- rlang::sym(settings$dm$country_col)
  dm_ifcdt_sym <- rlang::sym(settings$dm$ifcdt_col)
  dm_rando_sym <- rlang::sym(settings$dm$rando_col)

  # Flag enrolled/randomized participants
  data_enrolled <- data$dm %>%
    dplyr::mutate(rficdt = dplyr::if_else(is.na(!!dm_ifcdt_sym), as.Date(!!dm_rando_sym), as.Date(!!dm_ifcdt_sym)),
                  randdt = as.Date(!!dm_rando_sym)) %>%
    dplyr::filter(!is.na(rficdt)) %>%
    dplyr::select(!!dm_id_sym, !!dm_country_sym, !!dm_siteid_sym, rficdt, randdt) %>%
    dplyr::mutate(case_enrolled = 1,
                  case_rando = dplyr::if_else(!is.na(randdt), 1, 0))

  # Use for first tabpanel - number of enrolled/randomized patients at yyyy date
  current_date = as.Date(Sys.Date(),"%Y-%m-%d")
  rficdt_date = unique(data_enrolled$rficdt)
  choice_date = c(rficdt_date, current_date)

  # Sum (and cumsum) of enrolled participants by date
  enrolled_aggregated <- data_enrolled %>%
    dplyr::group_by(month = lubridate::floor_date(rficdt, "month")) %>%
    dplyr::summarize(sum_ce = sum(case_enrolled)) %>%
    dplyr::mutate(cumsum_ce = cumsum(sum_ce))

  # Sum (and cumsum) of randomized participants by date
  rando_aggregated <- data_enrolled %>%
    dplyr::group_by(month = lubridate::floor_date(rficdt, "month")) %>%
    dplyr::summarize(sum_cr = sum(case_rando)) %>%
    dplyr::mutate(cumsum_cr = cumsum(sum_cr))

  # Get treatments reference and comparators
  if(is.null(settings$dm[['treatment_values--group1']])){settings$dm[['treatment_values--group1']] <-  c({{ ref_arm}})  }
  if(is.null(settings$dm[['treatment_values--group2']])){settings$dm[['treatment_values--group2']] <-  c({{ comp_arm}})  }

  treatments <- c(
    settings$dm[['treatment_values--group1']],
    settings$dm[['treatment_values--group2']]
  )

  if (length(treatments) < 2) {
    all_treatments <- unique(data_enrolled %>% pull(!!dm_treatment_sym))
    treatments <- all_treatments[1:2]
  }

  params <-
    list(
      data = list(data_enrolled= data_enrolled, enrolled_aggregated=enrolled_aggregated, rando_aggregated=rando_aggregated),
      settings = settings
    )

  return(params)

}

mapping <- init_enrolmap(data = list(dm = sdtm_dm), settings = NULL)



