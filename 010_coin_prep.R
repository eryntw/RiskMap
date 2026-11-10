library(targets)
library(tarchetypes)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 50))


# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

# targets -------

datadir <- "/mnt/envshare/data"

tar_plan(
  
  #### Exposure ----
  
  tarchetypes::tar_file_read(
    plant_exp,
    fs::path(tars$Exposure$plants$store, "objects", "plant_trait_scored"),
    readRDS(!!.x) |> 
      rename(uCode = taxa, uName = common)
  ),
  
  tarchetypes::tar_file_read(
    animal_exp,
    fs::path(tars$Exposure$animals$store, "objects", "animal_trait_scored"),
    readRDS(!!.x) |> 
      rename(uCode = taxa, uName = common)
  ),
  
  #### Pressure ----
  tarchetypes::tar_file_read(
    pressure,
    fs::path(tars$Pressure$regcont$store, "objects", "pressure"),
    readRDS(!!.x) |> 
      dplyr::rename(
        iucn_status = status_score,
        iucn_threat = score_sum_sp,
        epbc_status = epbc_score,
        aoo = sa_aoo_km_2,
        eoo = sa_eoo_km_2,
        legalprotected = pct_in_protected,
        capadprotected = pct_in_capad,
        sdm_area = suitable_area_ha,
        eooSA = sa_aoo_region_contrib_percent,
        aooSA = sa_eoo_region_contrib_percent
      )
  ),
  
  #### iMeta ----
  tar_target(imeta_path,
             "/mnt/envshare/data/traits/meta/ExposureTraits.xlsx",
             format = "file"
  ),
  
  tar_target(
    imeta_p,
    readxl::read_excel(imeta_path, sheet = "iMeta_plant")
  ),

  tar_target(
    imeta_a,
    readxl::read_excel(imeta_path, sheet = "iMeta_animal")
  ),
  
  #### iData ----
  tar_target(idata_a,
             animal_exp |> 
               dplyr::left_join(pressure, by = c("uCode" = "sa_taxa")) |> 
               dplyr::mutate(uCode = gsub("\\s+", "_", uCode))),
  
  tar_target(idata_p,
             plant_exp |> 
               dplyr::left_join(pressure, by = c("uCode" = "sa_taxa")) |> 
               dplyr::mutate(uCode = gsub("\\s+", "_", uCode))),
  
  
  #### Sync iMeta and iData to check ----
  tar_target(
    sync_animal,
    sync_coinr_inputs(idata_a, imeta_a)
  ),
  
  tar_target(
    sync_plant,
    sync_coinr_inputs(idata_p, imeta_p)
  ),
  
  #### Record deficient species ----
  # species that had less than 10 records in the past 50 years
  tar_target(
    rd_plant, 
    sync_plant$iData |>
      dplyr::filter(is.na(climate)) |>
      dplyr::distinct(uCode) |>
      dplyr::pull(uCode)
  ),
  
  tar_target(
    rd_animal, 
    sync_animal$iData |>
      dplyr::filter(is.na(iucn_status) & is.na(sdm_area)) |>
      dplyr::distinct(uCode) |>
      dplyr::pull(uCode)
  ),
  
  #### Branch duplication & Base coins ----
  tar_target(
    coin_p,
    duplicate_branch(
      iMeta = sync_plant$iMeta, iData = sync_plant$iData,
      branch_root = "recovery", new_parent = "iasplant", suffix = "_ias"
    ) |>
      (\(x) duplicate_branch(
        iMeta = x$iMeta, iData = x$iData,
        branch_root = "dispersal", new_parent = "fragment", suffix = "_frag"
      ))() |> 
      (\(x) duplicate_branch(
        iMeta = x$iMeta, iData = x$iData,
        branch_root = "specialisation", new_parent = "pollution", suffix = "_po"
      ))() |> 
      remove_branch("iucn_epbc") |> 
      remove_units(rd_plant)
  ),
  
  tar_target(
    coin_a,
    duplicate_branch(
      iMeta = sync_animal$iMeta, iData = sync_animal$iData,
      branch_root = "dispersal", new_parent = "fragment", suffix = "_ias"
    ) |>
      (\(x) duplicate_branch(
        iMeta = x$iMeta, iData = x$iData,
        branch_root = "hb", new_parent = "fragment", suffix = "_frag"
      ))() |> 
      (\(x) duplicate_branch(
        iMeta = x$iMeta, iData = x$iData,
        branch_root = "ubag_tolerance", new_parent = "pollution", suffix = "_po"
      ))() |> 
      remove_branch("iucn_epbc") |> 
      collapse_to_any("irreplace_feature") |>
      remove_units(rd_animal)
  )
)
