#' Initialize Settings and Input data for Exposure plot (Swimmer plot) widget
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#'
#' init_exposure(params$data, settings=params$settings, ref_arm= "Treatment", comp_arm = "Placebo", threshold = 1)
#' @return returns list with data and settings used for fct_tornadoplot
#'
#' @importFrom dplyr mutate filter arrange select group_by summarize if_else slice ungroup left_join right_join bind_rows
#' @importFrom tidyr starts_with pivot_longer pivot_wider fill
#' @importFrom stringr str_to_sentence
#' @importFrom tibble rowid_to_column
#' @importFrom magrittr "%>%"
#' @export

init_exposure <- function(data, settings, ref_arm= "Treatment", comp_arm = "Placebo", threshold = 1) {

  # Convert settings to symbols ready for standard evaluation
  dm_id_sym <- rlang::sym(settings$dm$id_col)
  dm_treatment_sym <- rlang::sym(settings$dm$treatment_col)
  dm_siteid_sym <- rlang::sym(settings$dm$siteid_col)
  dm_country_sym <- rlang::sym(settings$dm$country_col)
  dm_ifcdt_sym <- rlang::sym(settings$dm$ifcdt_col)
  baseline_cols_syms <- rlang::syms(settings$dm$baseline_cols)

  # Get treatments reference and comparators
  if(is.null(settings$dm[['treatment_values--group1']])){settings$dm[['treatment_values--group1']] <-  c({{ ref_arm}})  }
  if(is.null(settings$dm[['treatment_values--group2']])){settings$dm[['treatment_values--group2']] <-  c({{ comp_arm}})  }

  treatments <- c(
    settings$dm[['treatment_values--group1']],
    settings$dm[['treatment_values--group2']]
  )

  if (length(treatments) < 2) {
    all_treatments <- unique(aes_arm %>% pull(!!dm_treatment_sym))
    treatments <- all_treatments[1:2]
  }

  # ADSL - Derive variables of interest for exposure plot
  adsl <- data$adsl %>%
    dplyr::filter(SAFFL == "Y") %>%
    dplyr::mutate(LSTALVDT_ALL = max(LSTALVDT)) %>%
    dplyr::rowwise(!!dm_id_sym) %>%
    dplyr::mutate(EOSSTT = stringr::str_to_sentence(EOSSTT),
                  !!dm_country_sym := if_else(!!dm_country_sym != "", !!dm_country_sym, "Missing info"),
                  DCSREAS = if_else(DCSREAS != "", stringr::str_to_sentence(DCSREAS), "Study Ongoing"),
                  TRTDURW =  format(round(TRTDURD / 7, digits = 1), nsmall = 1),
                  RFSTDT = as.Date(min(as.Date(RANDDT), as.Date(TRTSDT), na.rm=TRUE)),
                  STDDURW = format(round(as.numeric((as.Date(EOSDT) - RFSTDT + 1)) / 7, digits = 1), nsmall = 1),
                  TIM2DTHW = format(round((as.numeric(as.Date(DTHDT) - RFSTDT + 1)) / 7, digits = 1), nsmall = 1),
                  TIM2CUTW = format(round((as.numeric(as.Date(LSTALVDT_ALL) - RFSTDT + 1)) / 7, digits = 1), nsmall = 1),
                  STDDURW = dplyr::if_else(!is.na(STDDURW) | STDDURW != "", as.numeric(STDDURW), NA_real_),
                  TIM2DTHW = dplyr::if_else(!is.na(TIM2DTHW) | TIM2DTHW != "", as.numeric(TIM2DTHW), NA_real_),
                  TIM2CUTW = dplyr::if_else(!is.na(TIM2CUTW) | TIM2CUTW != "", as.numeric(TIM2CUTW), NA_real_)) %>%
                  #EOS2CUT = diff(TIM2CUTW, STDDURW)) %>%
                  # STDDURW =  difftime(as.Date(EOSDT), RFSTDT, units = "days"),
                  # TIM2DTHW = difftime(as.Date(DTHDT), RFSTDT, units = "days"),
                  # TIM2CUTW = difftime(as.Date(LSTALVDT_ALL), RFSTDT, units = "days"),
                  #EOS2CUT = as.numeric(TIM2CUTW)-as.numeric(STDDURW)) %>%
     dplyr::ungroup() %>%
     dplyr::select(!!dm_id_sym, !!dm_siteid_sym, !!dm_country_sym, RFSTDT, RANDDT, TRTSDT, TRTEDT, TRTDURW, EOSDT, STDDURW, TIM2DTHW, TIM2CUTW, EOS2CUT, EOSSTT, DCSREAS, DTHDT, LSTALVDT_ALL, !!!baseline_cols_syms) %>%
     dplyr::arrange(STDDURW) %>%
     tibble::rowid_to_column()

params <-
  list(
    data = adsl,
    settings = settings
  )

return(params)

}
