library(dplyr)
library(targets)

# packages --------
envFunc::check_packages(yaml::read_yaml("settings/packages.yaml") |> 
                          unlist() |> 
                          unname() |> 
                          unique(), 
                        update_env = TRUE
)

# tars --------

## local ------

tars_local <- envTargets::make_tars(settings = envFunc::extract_scale("RiskMap"),
                                    save_yaml = FALSE)

## Other projects ------
tars_rec <- envTargets::make_tars(settings = envFunc::extract_scale("RecExtract"),
                                  project_base = fs::path("..", "RecExtract"),
                                  local = FALSE)

tars_pia <- envTargets::make_tars(settings = envFunc::extract_scale("envPIA"),
                                   project_base = fs::path("..", "envPIA"),
                                   local = FALSE,
                                   list_names = c("extent", "grain", "aoi"))

tars_pressure <- envTargets::make_tars(settings = envFunc::extract_scale("Pressure"),
                                     project_base = fs::path("..", "Pressure"),
                                     local = FALSE)

tars_exposure <- envTargets::make_tars(settings = envFunc::extract_scale("Exposure"),
                                     project_base = fs::path("..", "Exposure"),
                                     local = FALSE)

tars = c(tars_local, tars_rec, tars_pia, tars_pressure, tars_exposure)

envTargets::write_tars(tars)

# run everything ----------
# in _targets.yaml
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_make(script = x, store = y)
)

# prune everything ----------
purrr::walk2(purrr::map(tars_local, "script")
             , purrr::map(tars_local, "store")
             , \(x, y) targets::tar_prune(script = x, store = y)
)
