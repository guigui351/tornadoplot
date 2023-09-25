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

  # mini adsl - Derive variables of interest for exposure plot
  swimmer <- data$adsl %>%
     dplyr::select(!!dm_id_sym, !!dm_siteid_sym, !!dm_country_sym, RANDDT, TRTSDT, TRTEDT, TRTDURW, EOSDT, STDDURW, TIM2DTHW, TIM2CUTW, EOSSTT, DCSREAS, EOTSTT, DCTREAS, DTHDT, LSTALVDT_ALL, !!!baseline_cols_syms) %>%
     dplyr::arrange(STDDURW) %>%
     tibble::rowid_to_column()

  params_expo <-
     list(
        data = adsl_expo,
        settings = settings
     )

  return(params_expo)

}
