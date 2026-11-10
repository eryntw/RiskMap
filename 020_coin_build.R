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
  
  # row-bind each group's scenario tables (order follows the rows of `grid`)
  tar_combine(results_animal, mapped[["results"]][grid$group == "animal"],
              command = dplyr::bind_rows(!!!.x)),
  tar_combine(results_plant,  mapped[["results"]][grid$group == "plant"],
              command = dplyr::bind_rows(!!!.x)),
  
  tar_target(compare_animal, compare_scenarios(results_animal)),
  tar_target(compare_plant,  compare_scenarios(results_plant))
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