library(targets)
library(tarchetypes)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 50))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# scenario settings ------
settings <- yaml::read_yaml("settings/scenarios.yaml")

# one row per scenario x group; `base` holds the symbol of the input object
grid <- purrr::imap(settings$scenarios, \(by_group, scn) {
  tibble::tibble(scenario = scn, group = names(by_group))
}) |>
  dplyr::bind_rows()
grid$base <- rlang::syms(unname(unlist(settings$inputs)[grid$group]))

# targets -------

coin_p <- tar_read(coin_p, store = tars$coin_prep$store)
coin_a <- tar_read(coin_a, store = tars$coin_prep$store)

mapped <- tar_map(
  values = grid,
  names  = c(scenario, group),
  
  tar_target(inputs,     apply_scenario(base, settings, scenario, group)),
  tar_target(trans,      flag_log_transform(inputs$iData)),
  tar_target(treated,    build_treated_coin(inputs, trans)),
  tar_target(norm,       normalise_scenario(treated, settings)),
  tar_target(na_summary, summarise_na(norm)),
  tar_target(agg,        aggregate_scenario(norm, settings, group)),
  tar_target(results,    scenario_results(agg, inputs, scenario, group))
)

tar_plan(
  mapped,
  
  # all scenarios and variants for each species group
  tar_combine(results_all_animal, mapped[["results"]][grid$group == "animal"],
              command = dplyr::bind_rows(!!!.x)),
  tar_combine(results_all_plant,  mapped[["results"]][grid$group == "plant"],
              command = dplyr::bind_rows(!!!.x)),
  
  # scenario comparison uses the baseline rows only
  tar_target(compare_animal,
             compare_scenarios(dplyr::filter(results_all_animal, 
                                             variant == "baseline"))),
  tar_target(compare_plant,
             compare_scenarios(dplyr::filter(results_all_plant, 
                                             variant == "baseline"))),
  
  ## This will build around 500 targets. Wait for the storage arrive.
  # # sensitivity summaries
  # tar_target(sens_animal,
  #            summarise_variants(results_all_animal, 
  #                               top_n = settings$sensitivity$top_n)),
  # tar_target(sens_plant,
  #            summarise_variants(results_all_plant, 
  #                               top_n = settings$sensitivity$top_n))
)

# full table for one scenario
# tar_read(results_wind_animal)
# cmp <- tar_read(compare_animal)

# species most sensitive to scenario choice first
# cmp$table                                  

# are the scenarios telling the same story?
# cmp$rank_cor

# shared top-20 species per pair
# cmp$top_overlap

# where dat_thresh is removing species
# cmp$na_by_aggregate                        