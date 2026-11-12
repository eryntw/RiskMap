## build_grid ##
# One row per scenario x group x variant. YAML variants are added for every
# scenario/group (or only the groups listed under `groups:`); leave-one-out
# variants are generated from the base iMeta, one per stressor that the
# scenario still has.
# inputs: named list of base inputs per group, e.g. list(animal = coin_a, plant = coin_p)
build_sensitivity_grid <- function(settings, inputs) {
  
  ## scenario x group combinations present in the YAML ##
  combos <- purrr::imap(settings$scenarios, \(by_group, scn) {
    tibble::tibble(scenario = scn, group = names(by_group))
  }) |>
    dplyr::bind_rows()
  
  ## variants defined in the YAML ##
  yaml_rows <- purrr::imap(settings$variants, \(v, nm) {
    keep <- if (is.null(v$groups)) combos else
      dplyr::filter(combos, group %in% unlist(v$groups))
    dplyr::mutate(keep, variant = nm, extra_drop = NA_character_)
  }) |>
    dplyr::bind_rows()
  
  ## leave-one-stressor-out variants ##
  loo_rows <- NULL
  if (!is.null(settings$leave_one_out)) {
    parent <- settings$leave_one_out$parent
    loo_rows <- purrr::pmap(combos, \(scenario, group) {
      meta      <- inputs[[group]]$iMeta
      dropped   <- unlist(settings$scenarios[[scenario]][[group]]$drop)
      stressors <- setdiff(meta$iCode[meta$Parent %in% parent], dropped)
      if (!length(stressors)) return(NULL)
      tibble::tibble(scenario = scenario, group = group,
                     variant = paste0("no_", stressors), extra_drop = stressors)
    }) |>
      dplyr::bind_rows()
  }
  
  grid <- dplyr::bind_rows(yaml_rows, loo_rows)
  stopifnot("variants must include 'baseline'" = "baseline" %in% grid$variant)
  
  # symbol of the base input object for each group (read at the top of _targets.R)
  grid$base <- rlang::syms(unname(unlist(settings$inputs)[grid$group]))
  grid
}