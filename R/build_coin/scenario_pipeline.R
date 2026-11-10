## apply_scenario ##
# Reads one scenario/group block from settings and builds its inputs
# (drops branches, sets weights) via scenario_inputs().
apply_scenario <- function(inputs, settings, scenario, group) {
  s <- settings$scenarios[[scenario]][[group]]
  if (is.null(s)) {
    stop("No settings for scenario '", scenario, "' and group '", group,
         "' in scenarios.yaml")
  }
  scenario_inputs(inputs, drop = unlist(s$drop), weights = unlist(s$weights))
}

## build_treated_coin ##
# inputs:      list($iMeta, $iData), e.g. coin_p or coin_a
# flags_res:   output of flag_log_transform() on inputs$iData
# level_names: one name per level, indicators first; auto-generated if NULL
build_treated_coin <- function(inputs, flags_res, level_names = NULL) {
  
  iMeta <- prepare_coinr_meta(inputs$iMeta)
  n_lvl <- max(iMeta$Level)
  
  if (is.null(level_names)) {
    level_names <- c("Indicator",
                     if (n_lvl > 2) paste0("Agg_L", 2:(n_lvl - 1)),
                     "Index")
  }
  stopifnot(length(level_names) == n_lvl)
  
  coin <- COINr::new_coin(
    iData       = inputs$iData,
    iMeta       = iMeta,
    level_names = level_names,
    quietly     = TRUE
  )
  
  # only specs for indicators that are in the coin
  ind_codes <- iMeta$iCode[iMeta$Type == "Indicator"]
  specs     <- flags_res$indiv_specs[names(flags_res$indiv_specs) %in% ind_codes]
  
  COINr::Treat(coin, dset = "Raw", indiv_specs = specs)
}

## normalise_scenario ##
normalise_scenario <- function(coin, settings) {
  nm <- settings$normalisation
  COINr::Normalise(
    coin,
    dset         = "Treated",
    global_specs = list(f_n = nm$f_n, f_n_para = list(l_u = unlist(nm$range)))
  )
}

## summarise_na ##
# Proportion of NAs per normalised indicator, highest first.
summarise_na <- function(coin_norm) {
  COINr::get_dset(coin_norm, "Normalised") |>
    dplyr::select(-dplyr::any_of(c("uCode", "uName", "Time"))) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), \(x) mean(is.na(x)))) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "iCode", values_to = "prop_na") |>
    dplyr::arrange(dplyr::desc(prop_na))
}

## aggregate_scenario ##
aggregate_scenario <- function(coin_norm, settings, group) {
  ag <- settings$aggregation[[group]]
  COINr::Aggregate(coin_norm, dset = "Normalised",
                   f_ag = unlist(ag$f_ag), dat_thresh = ag$dat_thresh)
}