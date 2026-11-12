library(targets)
library(tarchetypes)
library(geotargets)
library(crew)

## terra is memory and IO heavy: keep workers low (one per map is plenty) ##
n_workers <- 6

tar_option_set(
  packages = yaml::read_yaml("settings/packages.yaml")$packages,
  controller = crew::crew_controller_local(workers = n_workers),
  garbage_collection = TRUE
)

tars <- yaml::read_yaml("_targets.yaml")
settings <- yaml::read_yaml("settings/scenarios.yaml")

tar_source()

## SDMs ----
store_sdm <- tars$envPIA$final$store# boundary 1

sdm_tables <- list(
  usg = tar_read(final, store = store_sdm),
  bp  = tar_read(final, store = fs::path(store_sdm, "..", "..",
                                          "pia__boundary__2__0__50000/final"))
) # the tibbles are tracked, but the SDM tiffs are not

sdm_meta <- prep_sdm_meta(sdm_tables)
keep_use <- NULL    # whether to filter out epbc and assoc

## Coin results ----
results_names <- c(animal = "results_animal", plant = "results_plant")

## Output directory ----
out_dir  <- fs::path(tars$mapsdm$store, "../riskmap_tiff")

## upstream objects, read across stores (same pattern as coin_p / coin_a) ##
vuln_results <- purrr::map(results_names, 
                           \(nm) tar_read_raw(nm, store = tars$coin_build$store)) |>
  dplyr::bind_rows()

## one row per scenario x group x region ##
grid <- purrr::imap(settings$scenarios, \(by_group, scn) {
  tibble::tibble(scenario = scn, group = names(by_group))
}) |>
  dplyr::bind_rows() |>
  tidyr::crossing(region = names(sdm_tables))

# first run: test on one coin and a handful of species
grid <- dplyr::filter(grid, scenario == "wind", group == "animal")

tar_plan(
  
  #### Match scores to SDMs ----
  tar_target(vuln_sdm,
             match_scores_to_sdms(vuln_results, sdm_meta, keep_use = keep_use)),
  tar_target(vuln_sdm_tbl,  vuln_sdm$data),
  tar_target(vuln_sdm_diag, vuln_sdm$diagnostics),
  
  #### One map per scenario x group x region----
  tar_map(
    values = grid,
    names  = c(scenario, group, region),
    tar_target(
      vuln_map,
      build_vuln_map(vuln_sdm_tbl, scenario, group, region, out_dir = out_dir,
                     chunk_size = 100, memfrac = 0.5 / n_workers),
      format = "file"
    )
  )
)