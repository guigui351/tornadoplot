#' Create a small ADSL dataset using SDTM sources datasets and settings
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#'
#' init_adsl(params$data, settings=params$settings)
#' @return returns list with ADSL data added to other data and settings
#'
#' @importFrom dplyr mutate filter arrange select group_by summarize if_else slice ungroup left_join right_join vars case_when rowwise across ends_with mutate_at
#' @importFrom admiral derive_vars_dtm derive_vars_merged derive_vars_dtm_to_dt derive_var_trtdurd derive_vars_dt
#'             derive_var_disposition_status derive_vars_duration derive_var_merged_exist_flag derive_var_extreme_dt
#'             date_source derive_vars_disposition_reason
#' @importFrom magrittr "%>%"
#' @importFrom tidyr starts_with
#' @export

init_adsl <- function(data, settings) {

    # Convert settings to symbols ready for standard evaluation
    dm_id_sym <- rlang::sym(settings$dm$id_col)
    dm_treatment_sym <- rlang::sym(settings$dm$treatment_col)
    dm_siteid_sym <- rlang::sym(settings$dm$siteid_col)
    dm_country_sym <- rlang::sym(settings$dm$country_col)
    dm_ifcdt_sym <- rlang::sym(settings$dm$ifcdt_col)
    dm_dthdt_sym <- rlang::sym(settings$dm$dthdt_col)
    baseline_cols_syms <- rlang::syms(settings$dm$baseline_cols)

    ex_id_sym <- rlang::sym(settings$ex$id_col)
    ex_stdtc_sym <- rlang::sym(settings$ex$stdtc_col)
    ex_endtc_sym <- rlang::sym(settings$ex$endtc_col)
    ex_dose_sym <- rlang::sym(settings$ex$dose_col)
    ex_trt_sym <- rlang::sym(settings$ex$extrt_col)

    ds_id_sym <- rlang::sym(settings$ds$id_col)
    ds_dtc_sym <- rlang::sym(settings$ds$dsdtc_col)

    ae_stdtc_sym <- rlang::sym(settings$aes$stdtc_col)
    ae_endtc_sym <- rlang::sym(settings$aes$endtc_col)

    lb_lbdtc_sym <- rlang::sym(settings$lb$lbdtc_col)

    ex_ext <- data$ex %>%
      admiral::derive_vars_dtm(
        dtc = !!ex_stdtc_sym,
        new_vars_prefix = "EXST"
      ) %>%
      admiral::derive_vars_dtm(
        dtc = !!ex_endtc_sym,
        new_vars_prefix = "EXEN",
        time_imputation = "last"
      )

    adsl <- data$dm %>%
       dplyr::select(-c(!!!baseline_cols_syms)) %>%
       # derive treatment variables (TRT01P) ----
       dplyr::mutate(TRT01P = !!dm_treatment_sym)  %>%
       # derive treatment start date (TRTSDTM) ----
      admiral::derive_vars_merged(
        dataset_add = ex_ext,
        by_vars = vars(!!dm_id_sym),
        filter_add = (!!ex_dose_sym > 0 |
                     (!!ex_dose_sym == 0 &
                         stringr::str_detect(!!ex_trt_sym, "PLACEBO"))) &
                     !is.na(EXSTDTM),
        new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
        order = vars(EXSTDTM, EXSEQ),
        mode = "first"
      ) %>%
      ## derive treatment end date (TRTEDTM) ----
      admiral::derive_vars_merged(
        dataset_add = ex_ext,
        filter_add = (!!ex_dose_sym > 0 |
                     (!!ex_dose_sym == 0 &
                         stringr::str_detect(!!ex_trt_sym, "PLACEBO"))) &
                     !is.na(EXENDTM),
        new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
        order = vars(EXENDTM, EXSEQ),
        mode = "last",
        by_vars = vars(!!dm_id_sym)
      ) %>%
      ## Derive treatment end/start date TRTSDT/TRTEDT ----
      admiral::derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM)) %>%
      ## derive treatment duration (TRTDURD) ----
      admiral::derive_var_trtdurd()

      ## Disposition dates, status ----
      # convert character date to numeric date without imputation
      ds_ext <- admiral::derive_vars_dt(
        data$ds,
        dtc = !!ds_dtc_sym,
        new_vars_prefix = "DSST"
      )

    adsl <- adsl %>%
      # Screen failure date
      admiral::derive_vars_merged(
        dataset_add = ds_ext,
        by_vars = vars(!!ds_id_sym),
        new_vars = vars(SCRFDT = DSSTDT),
        filter_add = eval(rlang::parse_expr(settings$ds$filter_scrdt))
      ) %>%
        # EOS date
        admiral::derive_vars_merged(
          dataset_add = ds_ext,
          by_vars = vars(STUDYID, USUBJID),
          new_vars = vars(EOSDT = DSSTDT),
          filter_add = eval(rlang::parse_expr(settings$ds$filter_eosdt))
        ) %>%
        # EOS status
        admiral::derive_var_disposition_status(
          dataset_ds = ds_ext,
          new_var = EOSSTT,
          status_var = DSDECOD,
          format_new_var = format_eoxxstt,
          filter_ds = eval(rlang::parse_expr(settings$ds$filter_eosdt))
        ) %>%
        # EOS reason
        admiral::derive_vars_disposition_reason(
          dataset_ds = ds_ext,
          new_var = DCSREAS,
          reason_var = DSDECOD,
          new_var_spe = DCSREASP,
          reason_var_spe = DSTERM,
          #format_new_vars = format_dcsreas,
          filter_ds = eval(rlang::parse_expr(settings$ds$filter_eosdt))
        ) %>%
        # EOT date
        admiral::derive_vars_merged(
          dataset_add = ds_ext,
          by_vars = vars(STUDYID, USUBJID),
          new_vars = vars(EOTDT = DSSTDT),
          filter_add = eval(rlang::parse_expr(settings$ds$filter_eotdt))
        ) %>%
        # EOT status
        admiral::derive_var_disposition_status(
          dataset_ds = ds_ext,
          new_var = EOTSTT,
          status_var = DSDECOD,
          format_new_var = format_eoxxstt,
          filter_ds = eval(rlang::parse_expr(settings$ds$filter_eotdt))
        ) %>%
        # EOT reason
        admiral::derive_vars_disposition_reason(
          dataset_ds = ds_ext,
          new_var = DCTREAS,
          reason_var = DSDECOD,
          new_var_spe = DCTREASP,
          reason_var_spe = DSTERM,
          #format_new_vars = format_dctreas,
          filter_ds = eval(rlang::parse_expr(settings$ds$filter_eotdt))
        ) %>%
        # Derive Randomization Date
        admiral::derive_vars_merged(
          dataset_add = ds_ext,
          filter_add = eval(rlang::parse_expr(settings$ds$filter_randdt)),
          by_vars = vars(STUDYID, USUBJID),
          new_vars = vars(RANDDT = DSSTDT)
        ) %>%
        # Death date - impute partial date to first day/month
        admiral::derive_vars_dt(
          new_vars_prefix = "DTH",
          dtc = DTHDTC,
          highest_imputation = "M",
          date_imputation = "first"
        ) %>%
        # Relative Day of Death
        admiral::derive_vars_duration(
          new_var = DTHADY,
          start_date = TRTSDT,
          end_date = DTHDT
        ) %>%
        # Elapsed Days from Last Dose to Death
        admiral::derive_vars_duration(
          new_var = LDDTHELD,
          start_date = TRTEDT,
          end_date = DTHDT,
          add_one = FALSE
        )

    ## Last known alive date ----
    ae_start_date <- admiral::date_source(
      dataset_name = "ae",
      date = AESTDT
    )
    ae_end_date <- admiral::date_source(
      dataset_name = "ae",
      date = AEENDT
    )
    lb_date <- admiral::date_source(
      dataset_name = "lb",
      date = LBDT,
      filter = !is.na(LBDT)
    )
    trt_end_date <- admiral::date_source(
      dataset_name = "adsl",
      date = TRTEDT
    )

    # impute AE start and end date to first
    ae_ext <- data$aes %>%
      admiral::derive_vars_dt(
        dtc = !!ae_stdtc_sym,
        new_vars_prefix = "AEST",
        highest_imputation = "M"
      ) %>%
      admiral::derive_vars_dt(
        dtc = !!ae_endtc_sym,
        new_vars_prefix = "AEEN",
        highest_imputation = "M"
      )

    # impute LB date to first
    lb_ext <- admiral::derive_vars_dt(
      data$lb,
      dtc = !!lb_lbdtc_sym,
      new_vars_prefix = "LB",
      highest_imputation = "M"
    )

    adsl <- adsl %>%
      admiral::derive_var_extreme_dt(
        new_var = LSTALVDT,
        ae_start_date, ae_end_date, lb_date, trt_end_date,
        source_datasets = list(ae = ae_ext, lb = lb_ext, adsl = adsl),
        mode = "last"
      ) %>%
      ## Safety population ----
      admiral::derive_var_merged_exist_flag(
        dataset_add = data$ex,
        by_vars = vars(!!dm_id_sym),
        new_var = SAFFL,
        condition = (!!ex_dose_sym > 0 | (!!ex_dose_sym == 0 & stringr::str_detect(!!ex_trt_sym, "PLACEBO")))
      ) %>%
      mutate(ACUTDT = as.Date(settings$dm$cutoff_val, "%Y-%m-%d"))

    ## Add baseline variables defined by user ----
    if(!is.null(settings$dm$baseline_cols)){

      vars_already_indm <- intersect(names(data$dm %>% select(!!!baseline_cols_syms)), names(adsl))

      adsl <- adsl %>%
        select(STUDYID, !!dm_id_sym, !!dm_treatment_sym, !!dm_siteid_sym, !!dm_country_sym, !!dm_ifcdt_sym, !!dm_dthdt_sym,
               RFSTDTC, SCRFDT, RANDDT, tidyr::starts_with("TRT"),
               tidyr::starts_with("EOT"), tidyr::starts_with("DCT"),tidyr::starts_with("EOS"),  tidyr::starts_with("DCS"),
               tidyr::starts_with("DTH"), ACUTDT, LSTALVDT, SAFFL)

      if(!is.null(vars_already_indm)){
          adsl <- adsl %>%
            select(-all_of(vars_already_indm))
      }

      adsl <- adsl %>%
        left_join(data$dm %>% select("USUBJID", !!!baseline_cols_syms), by = "USUBJID") %>%
        mutate(LSTALVDT_ALL = max(LSTALVDT)) %>%
        rowwise(!!dm_id_sym) %>%
        mutate(EOSDT = if_else(EOSSTT == "Ongoing", LSTALVDT, EOSDT),
               EOSSTT = stringr::str_to_sentence(EOSSTT),
               !!dm_country_sym := if_else(!!dm_country_sym != "", !!dm_country_sym, "Missing info"),
               DCSREAS = if_else(DCSREAS != ""| !is.na(DCSREAS), stringr::str_to_sentence(DCSREAS), "Study Ongoing"),
               TRTDURW =  ifelse(!is.na(TRTDURD), round(TRTDURD / 7, digits = 1), NA),
               STDDURW = ifelse(!is.na(EOSDT), round(as.numeric((EOSDT - TRTSDT + 1)) / 7, digits = 1), NA),
               TIM2DTHW = ifelse(!is.na(DTHDT) , round((as.numeric(DTHDT - TRTSDT + 1)) / 7, digits = 1), NA),
               TIM2CUTW = ifelse(!is.na(LSTALVDT_ALL), round((as.numeric(LSTALVDT_ALL - TRTSDT + 1)) / 7, digits = 1), NA)) %>%
        ungroup()
    }

    # return params with new datasets and settings ready to be used in the init_exposure / mod_exposure
    params_with_adsl <-
      list(
        data = append(data, list(adsl=adsl), after = length(data)),
        settings = settings
      )

    return(params_with_adsl)
  }

