#' Initialize Settings and Data for the app, fill missing settings if not provided by user
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#' init_params(data = list(ex = sdtm_ex, dm = sdtm_dm, ds = sdtm_ds), settings = list(ex=list(id_col="SUBJID")))
#'
#' @return returns list with data and settings used through the app. The data is loaded by the user as explained in the README
#' @export
#'
init_params <- function(data, settings) {

  # Demographics settings
  if(is.null(settings$dm$id_col))          {settings$dm$id_col           <- "USUBJID"}
  if(is.null(settings$dm$treatment_col))   {settings$dm$treatment_col    <- "ARM"}
  if(is.null(settings$dm$treatment_values)){settings$dm$treatment_values <- list(group1 = "Placebo", "group2" = "Xanomeline High Dose")}
  if(is.null(settings$dm$siteid_col))      {settings$dm$siteid_col       <- "SITEID"}
  if(is.null(settings$dm$country_col))     {settings$dm$country_col      <- "COUNTRY"}
  if(is.null(settings$dm$ifcdt_col))       {settings$dm$ifcdt_col        <- "RFICDTC"}
  if(is.null(settings$dm$dthdt_col))       {settings$dm$dthdt_col        <- "DTHDTC"}
  if(is.null(settings$dm$baseline_cols))   {settings$dm$baseline_cols    <- c("AGE", "RACE", "ETHNIC")}
  if(is.null(settings$dm$cutoff_val))      {settings$dm$cutoff_val       <- as.Date(Sys.Date(), "%Y-%m-%d")}

  # Exposure settings
  if(is.null(settings$ex$id_col))   {settings$ex$id_col      <- "USUBJID"}
  if(is.null(settings$ex$stdtc_col)){settings$ex$stdtc_col   <- "EXSTDTC"}
  if(is.null(settings$ex$endtc_col)){settings$ex$endtc_col   <- "EXENDTC"}
  if(is.null(settings$ex$dose_col)) {settings$ex$dose_col    <- "EXDOSE"}
  if(is.null(settings$ex$extrt_col)){settings$ex$extrt_col   <- "EXTRT"}

  # Disposition settings
  if(is.null(settings$ds$id_col))       {settings$ds$id_col        <- "USUBJID"}
  if(is.null(settings$ds$dsdtc_col))    {settings$ds$dsdtc_col     <- "DSSTDTC"}
  if(is.null(settings$ds$filter_scrdt)) {settings$ds$filter_scrdt  <- "DSCAT == 'DISPOSITION EVENT' & DSDECOD == 'SCREEN FAILURE'"}
  if(is.null(settings$ds$filter_eosdt)) {settings$ds$filter_eosdt  <- "DSCAT == 'DISPOSITION EVENT' & DSDECOD != 'SCREEN FAILURE'"}
  if(is.null(settings$ds$filter_eotdt)) {settings$ds$filter_eotdt  <- "DSCAT == 'DISPOSITION EVENT' & DSDECOD != 'SCREEN FAILURE'"}
  if(is.null(settings$ds$filter_randdt)){settings$ds$filter_randdt <- "DSDECOD == 'RANDOMIZED'"}

  # Adverse Events settings
  if(is.null(settings$aes$id_col))       {settings$aes$id_col        <- "USUBJID"}
  if(is.null(settings$aes$bodsys_col))   {settings$aes$bodsys_col    <- "AEBODSYS"}
  if(is.null(settings$aes$term_col))     {settings$aes$term_col      <- "AEDECOD"}
  if(is.null(settings$aes$severity_col)) {settings$aes$severity_col  <- "AESEV"}
  if(is.null(settings$aes$serious_col))  {settings$aes$serious_col   <- "AESER"}
  if(is.null(settings$aes$stdtc_col))    {settings$aes$stdtc_col     <- "AESTDTC"}
  if(is.null(settings$aes$endtc_col))    {settings$aes$endtc_col     <- "AEENDTC"}

  # Laboratory settings
  if(is.null(settings$lb$id_col))       {settings$lb$id_col        <- "USUBJID"}
  if(is.null(settings$lb$lbdtc_col))    {settings$lb$lbdtc_col     <- "LBDTC"}

  settings <- list(
    dm=list(
      id_col=settings$dm$id_col,
      treatment_col=settings$dm$treatment_col,
      treatment_values=settings$dm$treatment_values,
      siteid_col=settings$dm$siteid_col,
      country_col=settings$dm$country_col,
      ifcdt_col=settings$dm$ifcdt_col,
      dthdt_col=settings$dm$dthdt_col,
      baseline_cols=settings$dm$baseline_cols,
      cutoff_val=settings$dm$cutoff_val
    ),
    ex=list(
      id_col=settings$ex$id_col,
      stdtc_col=settings$ex$stdtc_col,
      endtc_col=settings$ex$endtc_col,
      dose_col=settings$ex$dose_col,
      extrt_col=settings$ex$extrt_col
    ),
    ds=list(
      id_col=settings$ds$id_col,
      dsdtc_col=settings$ds$dsdtc_col,
      filter_scrdt=settings$ds$filter_scrdt,
      filter_eosdt=settings$ds$filter_eosdt,
      filter_eotdt=settings$ds$filter_eotdt,
      filter_randdt=settings$ds$filter_randdt
    ),
    aes=list(
      id_col=settings$aes$id_col,
      bodsys_col=settings$aes$bodsys_col,
      term_col=settings$aes$term_col,
      severity_col=settings$aes$severity_col,
      serious_col=settings$aes$serious_col,
      stdtc_col=settings$aes$stdtc_col,
      endtc_col=settings$aes$endtc_col
    ),
    lb=list(
      id_col=settings$lb$id_col,
      lbdtc_col=settings$lb$lbdtc_col
    )
  )

  # Assign dose regimen for the variable entered by the user in settings$dm$treatment_col
  # This is solution for user that does not provide the group_dose df in pre-process
  if (is.null(data$group_dose)) {

    unique_trtcol <- unique(convert_blanks_to_na(data$dm[settings$dm$treatment_col]))
    unique_trtcol <- unique_trtcol[!is.na(unique_trtcol)]
    unique_dose <- unique(convert_blanks_to_na(data$ex$EXDOSE))
    unique_dose <- unique_dose[!is.na(unique_dose)]

    data$group_dose <- data.frame(groupvar = rep(settings$dm$treatment_col, n_distinct(unique_trtcol)*n_distinct(unique_dose)),
                                  groupval = rep(unique(unique_trtcol), each = n_distinct(unique_dose)),
                                  dose_regimen = c(rep(unique_dose, n_distinct(unique_trtcol))))

  }

  # return params with new datasets and settings ready to be used in the different modules
  params <-
    list(
      data = data,
      settings = settings
    )

  return(params)

}
