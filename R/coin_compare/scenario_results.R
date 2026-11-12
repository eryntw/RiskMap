## scenario_results ##
# One tidy results table for one scenario x group coin: uCode + uName, score
# and rank at every aggregate, data completeness, and the top-scoring stressor.
# coin_agg: aggregated coin; inputs: the scenario's inputs list (for the
# stressor codes under `driver_parent`).
scenario_results <- function(coin_agg, inputs, scenario, group,
                             top = "vuln", driver_parent = "exposure",
                             variant = "baseline") {
  
  get_tab <- \(use) COINr::get_results(
    coin_agg, dset = "Aggregated", tab_type = "Aggs",
    use = use, also_get = "uName", nround = 2
  )
  
  scores    <- get_tab("scores")
  ranks     <- get_tab("ranks")
  agg_codes <- setdiff(names(scores), c("uCode", "uName", "Rank"))
  
  scores <- scores |>
    dplyr::select(uCode, dplyr::any_of("uName"), dplyr::all_of(agg_codes)) |>
    dplyr::rename_with(\(x) paste0("score_", x), dplyr::all_of(agg_codes))
  
  ranks <- ranks |>
    dplyr::select(uCode, dplyr::all_of(agg_codes)) |>
    dplyr::rename_with(\(x) paste0("rank_", x), dplyr::all_of(agg_codes))
  
  ## data completeness: unknown indicators per species ##
  norm <- COINr::get_dset(coin_agg, "Normalised")
  ind  <- setdiff(names(norm), c("uCode", "uName", "Time"))
  completeness <- tibble::tibble(
    uCode        = norm$uCode,
    n_indicators = length(ind),
    n_unknown    = rowSums(is.na(norm[ind]))
  ) |>
    dplyr::mutate(pct_known = round(100 * (1 - n_unknown / n_indicators), 1))
  
  ## which stressor scores highest for each species ##
  drivers <- inputs$iMeta$iCode[inputs$iMeta$Parent %in% driver_parent]
  sc <- as.matrix(scores[paste0("score_", drivers)])
  top_driver <- drivers[max.col(replace(sc, is.na(sc), -Inf), ties.method = "first")]
  top_driver[rowSums(!is.na(sc)) == 0] <- NA_character_
  
  scores |>
    dplyr::left_join(ranks, by = "uCode") |>
    dplyr::left_join(completeness, by = "uCode") |>
    dplyr::mutate(top_driver = top_driver) |>
    dplyr::mutate(group = group, scenario = scenario,
                  variant = variant, .before = 1) |>
    dplyr::arrange(.data[[paste0("rank_", top)]])
}

