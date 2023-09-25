#' Initialize Settings for Adverse Event Tornado plot widget
#'
#' @param data named list of data
#' @param settings named list of settings
#'
#' @examples
#'
#' init_tornado(params$data, settings=params$settings, ref_arm= "Treatment", comp_arm = "Placebo", threshold = 1)
#' @return returns list with data and settings used for fct_tornadoplot
#'
#' @importFrom dplyr mutate filter arrange select group_by summarize if_else slice ungroup left_join right_join bind_rows
#' @importFrom tidyr starts_with pivot_longer pivot_wider fill
#' @importFrom stringr str_to_sentence
#' @importFrom magrittr "%>%"
#' @export

init_tornado <- function(data, settings, ref_arm= "Treatment", comp_arm = "Placebo", threshold = 1) {

  # Print treatment group enter by users to the console
  print(comp_arm)
  print({{ ref_arm}})

  # Convert settings to symbols ready for standard evaluation
  dm_id_sym <- rlang::sym(settings$dm$id_col)
  dm_treatment_sym <- rlang::sym(settings$dm$treatment_col)

  ae_id_sym <- rlang::sym(settings$aes$id_col)
  ae_bodsys_sym <- rlang::sym(settings$aes$bodsys_col)
  ae_decod_sym <- rlang::sym(settings$aes$term_col)
  ae_severity_sym <- rlang::sym(settings$aes$severity_col)
  ae_serious_sym <- rlang::sym(settings$aes$serious_col)

  # Merge treatment with adverse events.
  aes_arm <- data$aes %>%
    dplyr::left_join(
      data$dm %>% dplyr::select(!!dm_id_sym, !!dm_treatment_sym),
      by = settings$dm$id_col
    )

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

  # Multiple arms automatically combined into a single arm if more than one value selected
  # Assign defined treatments references to "Treatment" (right side) and treatments comparators to "Placebo" (left side)
  # create bign variable: number of patients assigned by treatments combination
  if(!is.null(ref_arm) & !is.null(comp_arm)){
  data_dm <- data$dm %>%
    dplyr::mutate(!!dm_treatment_sym := dplyr::if_else(!!dm_treatment_sym %in% c({{ ref_arm }}), "Placebo", dplyr::if_else(!!dm_treatment_sym %in% c({{ comp_arm }}), "Treatment", "zDelete"))) %>%
    dplyr::filter(!!dm_treatment_sym != "zDelete") %>%
    dplyr::group_by(!!dm_treatment_sym) %>%
    dplyr::mutate(bign = dplyr::n_distinct(!!dm_treatment_sym, !!dm_id_sym)) %>%
    dplyr::select(!!dm_id_sym, !!dm_treatment_sym, bign)
  } else {
    data_dm <- data$dm %>%
      dplyr::mutate(!!dm_treatment_sym := dplyr::if_else(dplyr::row_number()/dplyr::n() > 0.5, "Treatment", "Placebo")) %>%
      dplyr::filter(!!dm_treatment_sym != "zDelete") %>%
      dplyr::group_by(!!dm_treatment_sym) %>%
      dplyr::mutate(bign = dplyr::n_distinct(!!dm_treatment_sym, !!dm_id_sym)) %>%
      dplyr::select(!!dm_id_sym, !!dm_treatment_sym, bign)
  }


  # Combine AE and DM datasets
  data_ae <- data$aes %>%
    dplyr::inner_join(data_dm, by = settings$dm$id_col)  %>%
    dplyr::mutate(treatment_group = stringr::str_to_sentence(!!dm_treatment_sym),
                  bodsys_col = stringr::str_to_sentence(!!ae_bodsys_sym),
                  term_col = stringr::str_to_sentence(!!ae_decod_sym),
                  !!ae_severity_sym := factor(stringr::str_to_sentence(!!ae_severity_sym), levels = c("Severe", "Moderate", "Mild", "Total")),
                  !!ae_serious_sym := factor(stringr::str_to_sentence(!!ae_serious_sym), levels = c("Y", "N", "Total"),labels = c("Serious", "Not serious", "Total"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!dm_id_sym, treatment_group, bign, bodsys_col, term_col, !!ae_severity_sym, !!ae_serious_sym)


  # Frequency py PT and AE grouping, as well as difference of frequency between both treatment arms combination
  data_ae1 <- data_ae %>%
    dplyr::mutate(None = factor("All")) %>%
    # Transpose
    tidyr::pivot_longer(cols = c(None, !!ae_severity_sym, !!ae_serious_sym), #pivot longer on flag data
                        names_to = "group_col",
                        values_to = "group_val") %>%
    # Keep the worst severe case only, ie the last one by 'group_val' (Mild, Moderate, Severe & Not serious, Serious)
    dplyr::group_by(!!dm_id_sym, bign, treatment_group, term_col, group_col) %>%
    dplyr::arrange(!!dm_id_sym, bign, treatment_group, term_col, group_col, group_val) %>%
    dplyr::slice(dplyr::n())  %>%
    dplyr::ungroup() %>%
    # Create Total severity rows
    dplyr::bind_rows(dplyr::filter(., !is.na(group_val)) %>% dplyr::mutate(None = "Total", group_val= "Total")) %>%
    dplyr::filter(!is.na(group_val)) %>%
    # Calculate frequencies using add_count function
    dplyr::group_by(!!dm_id_sym, bign, treatment_group, term_col, group_col, group_val) %>%
    dplyr::arrange(!!dm_id_sym, bign, treatment_group, term_col, group_col, group_val) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::add_count(bign, treatment_group, term_col, group_col, group_val, sort = TRUE) %>%
    dplyr::distinct(bign, treatment_group, term_col, group_col, group_val, n) %>%
    dplyr::arrange(bign, treatment_group, term_col, group_col, group_val, n) %>%
    # Create a variable 'tokeep' to keep only AEs with at least 3 occurrences within both treatment arms
    dplyr::mutate(tokeep = dplyr::if_else(group_val == "Total" & round(n/bign*100, 2) > {{ threshold }}, 1, NA_real_)) %>%
    dplyr::group_by(term_col, group_col) %>%
    # Fill 'tokeep' created above within each AE decod
    tidyr::fill(tokeep, .direction = "downup") %>%
    dplyr::filter(tokeep == 1) %>%
    # Create percentage
    dplyr::mutate(perc_char = dplyr::if_else(n !=0, paste0(format(round(n/bign*100, 2), nsmall = 2), "%"),"0%"),
                  perc = dplyr::if_else(n !=0, round(n/bign*100, 2), 0),
                  treatment_group = dplyr::if_else(grepl("PLA", toupper(treatment_group)), "placebo", "treatment")) %>%
    # Pivot dataframe, to have a placebo and a treatment column
    tidyr::pivot_wider(
      id_cols = c(term_col, group_col, group_val),
      names_from = treatment_group,
      names_glue = "{treatment_group}_{.value}",
      values_from = c(perc, n),
      values_fill = 0
    ) %>%
    # Create difference of frequency
    dplyr::mutate(placebo_perc_minus = -placebo_perc,
                  total_perc = dplyr::if_else(group_val == "Total", placebo_perc + treatment_perc, NA_real_),
                  diff = treatment_perc - placebo_perc,
                  diff_pos = dplyr::if_else(diff > 0, diff, NA_real_),
                  diff_neg = dplyr::if_else(diff < 0, diff, NA_real_)) %>%
    tidyr::fill(total_perc, .direction = "downup")

  params <-
    list(
      data = data_ae1,
      settings = settings
    )

  return(params)
}

