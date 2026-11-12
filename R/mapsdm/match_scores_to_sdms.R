## match_scores_to_sdms ##
# Pure function: joins each species' vulnerability score to its SDM file(s).
# A species with SDMs in both regions appears twice (once per region).
# Returns list($data, $diagnostics) so unmatched species are queryable.
match_scores_to_sdms <- function(results, sdm_meta, score_col = "score_vuln",
                                 keep_use = NULL) {
  
  ## Step 1: baseline run only (once sensitivity variants exist) ##
  if ("variant" %in% names(results)) {
    results <- dplyr::filter(results, variant == "baseline")
  }
  
  ## Step 2: one SDM file per species and region ##
  sdm <- unique_sdm(sdm_meta, keep_use)
  
  ## Step 3: tidy scores and make the matching key ##
  scores <- results |>
    dplyr::select(group, scenario, uCode, uName, score = dplyr::all_of(score_col)) |>
    dplyr::mutate(key = sdm_key(uCode))
  
  ## Step 4: attach SDM path(s); a species can match 0, 1 or 2 regions ##
  # many-to-many is expected: one SDM key matches several scenario rows.
  joined <- scores |>
    dplyr::left_join(
      dplyr::select(sdm, key, region, search_term, use, path),
      by = "key", relationship = "many-to-many"
    ) |>
    dplyr::mutate(has_file = !is.na(path) & file.exists(path))
  
  ## Step 5: safety checks ##
  # no species lost, and no species x region counted twice
  stopifnot(nrow(dplyr::distinct(joined, group, scenario, uCode)) == nrow(scores))
  stopifnot(!anyDuplicated(joined[c("group", "scenario", "uCode", "region")]))
  
  ## Step 6: coverage summaries (distinct species, not rows) ##
  coverage <- joined |>
    dplyr::group_by(group, scenario) |>
    dplyr::summarise(
      n_species      = dplyr::n_distinct(uCode),
      n_na_score     = dplyr::n_distinct(uCode[is.na(score)]),
      n_no_sdm       = dplyr::n_distinct(uCode[!is.na(score) & is.na(path)]),
      n_missing_file = dplyr::n_distinct(uCode[!is.na(path) & !has_file]),
      n_mapped       = dplyr::n_distinct(uCode[!is.na(score) & has_file]),
      .groups = "drop"
    )
  coverage_region <- joined |>
    dplyr::filter(!is.na(score), has_file) |>
    dplyr::count(group, scenario, region, name = "n_mapped")
  
  ## Step 7: usable table + diagnostics ##
  list(
    data = dplyr::filter(joined, !is.na(score), has_file),
    diagnostics = list(
      coverage           = coverage,
      coverage_region    = coverage_region,
      use_counts         = dplyr::count(sdm, region, use),
      scored_without_sdm = dplyr::filter(joined, !is.na(score), is.na(path)) |>
        dplyr::distinct(group, uCode, uName),
      missing_files      = dplyr::filter(joined, !is.na(path), !has_file) |>
        dplyr::distinct(uCode, region, path),
      sdm_without_score  = dplyr::filter(sdm, !key %in% scores$key) |>
        dplyr::select(region, search_term, use)
    )
  )
}

## check_same_grid ##
# Stops with a readable message if the files do not share one grid
# (extent, resolution, projection), instead of terra's generic error.
check_same_grid <- function(paths) {
  sig <- purrr::map_chr(paths, \(p) {
    r <- terra::rast(p)
    paste(c(round(as.vector(terra::ext(r)), 6), round(terra::res(r), 6),
            terra::crs(r, proj = TRUE)), collapse = "|")
  })
  tab <- table(sig)
  if (length(tab) > 1) {
    common <- names(tab)[which.max(tab)]
    bad <- paths[sig != common]
    stop(length(bad), " of ", length(paths), " files differ from the most common grid. ",
         "First: ", paste(utils::head(bad, 3), collapse = "; "))
  }
  invisible(TRUE)
}