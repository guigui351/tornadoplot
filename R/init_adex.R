#' Create a small ADEX dataset using SDTM sources datasets and settings
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#'
#' init_adex(data=params$data, settings=params$settings)
#' @return returns list with ADEX data added to other data and settings
#'
#' @importFrom dplyr mutate filter arrange select group_by summarize if_else slice ungroup left_join right_join n_distinct row_number n
#' @importFrom admiral derive_vars_dtm derive_vars_merged derive_vars_dtm_to_dt derive_var_trtdurd derive_vars_dt
#'              derive_vars_duration derive_var_extreme_dt convert_blanks_to_na date_source
#' @importFrom magrittr "%>%"
#' @importFrom tidyr starts_with
#' @importFrom forcats fct_expand
#' @export

init_adex <- function(data, settings) {

  # Convert settings to symbols ready for standard evaluation
  dm_id_sym <- rlang::sym(settings$dm$id_col)
  ex_id_sym <- rlang::sym(settings$ex$id_col)
  ex_stdtc_sym <- rlang::sym(settings$ex$stdtc_col)
  ex_endtc_sym <- rlang::sym(settings$ex$endtc_col)
  ex_dose_sym <- rlang::sym(settings$ex$dose_col)
  ex_trt_sym <- rlang::sym(settings$ex$extrt_col)

  # Convert blank to NA
  ex <- convert_blanks_to_na(data$ex)

  # Impute start/end dates
  adex <- ex %>%
    left_join(data$adsl %>% select(!!dm_id_sym, TRTSDT, TRTEDT, DTHDT, ACUTDT, LSTALVDT), by = settings$dm$id_col) %>%
    arrange(!!dm_id_sym, !!ex_stdtc_sym, !!ex_endtc_sym) %>%
    group_by(!!dm_id_sym) %>%
    mutate(!!ex_endtc_sym := if_else(row_number() == n() & (is.na(!!ex_endtc_sym) | !!ex_endtc_sym == ""), as.character(ACUTDT), !!ex_endtc_sym)) %>%
    ungroup() %>%
    admiral::derive_vars_dt(
      dtc = !!ex_stdtc_sym,
      new_vars_prefix = "EXST"
    ) %>%
    admiral::derive_vars_dt(
      dtc = !!ex_endtc_sym,
      new_vars_prefix = "EXEN",
      highest_imputation = "Y",
      date_imputation = "last",
      max_dates = vars(ACUTDT, DTHDT)
    ) %>%
    mutate(EXENDTF = if_else(!!ex_endtc_sym == as.character(ACUTDT), 'Y', EXENDTF),
           EXSTDY = difftime(EXSTDT, TRTSDT, units = "weeks"),
           EXENDY = difftime(EXENDT, TRTSDT, units = "weeks"),
           DIFF_WEEKS = (EXENDY-EXSTDY)/2,
           DIFF_WEEKS_FST = EXSTDY + DIFF_WEEKS,
           ADOSE_DISPLAY = if_else(DIFF_WEEKS > 1.5, EXDOSE, as.double(NA)))

  # List of colors to be used in exposure plot depending of dose regimen given in group_dose df
  if (n_distinct(data$group_dose$dose_regimen) > 18) {
    list_colors <- c('#e3f8dc', '#ddf2d6', '#d8ecd1', '#d2e6cb', '#cce1c5', '#c7dbc0', '#c1d5ba', '#bbcfb5', '#b6caaf',
                     '#b0c4aa', '#aabfa4', '#a5b99f', '#a0b39a', '#9aae94', '#95a88f', '#8fa38a', '#8a9d84', '#85987f',
                     '#80937a', '#7b8d75', '#758870', '#70836b', '#6b7e66', '#667861', '#61735c', '#5c6e57', '#576952',
                     '#52644d', '#4e5f48', '#495a44', '#44553f', '#40503a', '#3b4c36', '#364731', '#32422d', '#2d3e28',
                     '#293924')
  }
  if (n_distinct(data$group_dose$dose_regimen) < 19) {
    list_colors <- c('#e3f8dc', '#d8ecd1', '#cce1c5', '#c1d5ba', '#b6caaf', '#aabfa4', '#a0b39a', '#95a88f', '#8a9d84',
                     '#80937a', '#758870', '#6b7e66', '#61735c', '#576952', '#4e5f48', '#44553f', '#3b4c36', '#32422d',
                     '#293924')
  }
  if (n_distinct(data$group_dose$dose_regimen) < 9) {
    list_colors <- c("#e3f8dc", "#c6dabf", "#a9bda3", "#8ea087", "#73856c", "#596b53", "#40513b", "#293924")
  }

  # Assign the color for each groupval
  group_dose_colors <- data$group_dose %>%
    arrange(groupval, dose_regimen) %>%
    group_by(groupval) %>%
    mutate(dose_color = cut(dose_regimen,
                            breaks = c(-Inf, sort(unique(dose_regimen))),
                            labels = c(head(list_colors, n())),
                            include.lowest = TRUE))

  # groupvar variable specified by user in group_dose df. note that groupvar has to be included in DM df.
  by_groupvar <- rlang::sym(unique(data$group_dose$groupvar))

  adex <- adex %>% left_join(data$adsl %>% select(!!dm_id_sym, !!by_groupvar), by = settings$dm$id_col)

  # create grouval in ADEX, copy of groupvar variable specified by user in group_dose df.
  adex$groupval <- adex[[by_groupvar]]

  adex <- adex %>% left_join(group_dose_colors, by = c("groupval", "EXDOSE" = "dose_regimen")) %>%
    mutate(dose_color_unexp = if_else(is.na(groupvar), 'Unexpected dose', ''),
           dose_color = if_else(is.na(groupvar), dose_color %>% fct_expand('#d05f47'), dose_color),
           dose_color = replace(dose_color, is.na(dose_color), '#d05f47'))

  # return params with new datasets and settings ready to be used in the init_exposure / mod_exposure
  params_with_adex <-
      list(
        data = append(data, list(adex=adex), after = length(data)),
        settings = settings
      )

    return(params_with_adex)
}
