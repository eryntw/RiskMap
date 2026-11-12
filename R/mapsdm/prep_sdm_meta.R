## sdm_key ##
# Normalise species names so SDM search terms and coin uCodes can be matched:
# lower case, any run of non-alphanumerics collapsed to a single underscore.
sdm_key <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove_all("^_|_$")
}

## prep_sdm_meta ##
# Stack the per-region SDM tables into one table with a `region` column and a
# usable `path`. use_tif is relative to the Status project, so sdm_root says
# which folder to resolve it from (NULL = use the paths as they are).
# sp_tables: named list, e.g. list(usg = usg_sp, bp = bp_sp)
prep_sdm_meta <- function(sp_tables, sdm_root = NULL) {
  
  purrr::imap(sp_tables, \(tbl, region) {
    stopifnot(all(c("taxa", "use", "use_tif") %in% names(tbl)))
    dplyr::transmute(tbl, region = region, search_term = as.character(taxa),
                     use = use, path_raw = use_tif)
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      path = purrr::map_chr(path_raw, \(p) {
        if (is.na(p) || is.null(sdm_root)) return(p)
        as.character(fs::path_norm(fs::path_abs(p, start = sdm_root)))
      })
    ) |>
    dplyr::select(-path_raw)
}

## unique_sdm ##
# One SDM file per species and region. Optionally keeps only some values of
# `use`. Stops if two search terms collapse to the same key, or if a species
# still has several files in one region (silent row loss is a known failure).
unique_sdm <- function(sdm_meta, keep_use = NULL) {
  
  stopifnot(all(c("region", "search_term", "use", "path") %in% names(sdm_meta)))
  
  sdm <- sdm_meta |>
    dplyr::mutate(key = sdm_key(search_term))
  if (!is.null(keep_use)) sdm <- dplyr::filter(sdm, use %in% keep_use)
  
  coll <- sdm |>
    dplyr::distinct(search_term, key) |>
    dplyr::count(key) |>
    dplyr::filter(n > 1)
  if (nrow(coll)) {
    stop("Different SDM names give the same key: ",
         paste(utils::head(coll$key, 5), collapse = ", "))
  }
  
  amb <- sdm |> dplyr::count(region, key) |> dplyr::filter(n > 1)
  if (nrow(amb)) {
    stop(nrow(amb), " species have more than one SDM file in the same region, e.g. ",
         paste(utils::head(amb$key, 5), collapse = ", "),
         ". Use keep_use or fix the table.")
  }
  sdm
}

## peek_sdm_values ##
# Interactive check (not a target): min and max of a couple of rasters per
# region and `use`, to confirm they are 0/1 (or 1/NA) before mapping.
peek_sdm_values <- function(sdm_meta, n = 2) {
  sdm_meta |>
    dplyr::filter(!is.na(path), file.exists(path)) |>
    dplyr::group_by(region, use) |>
    dplyr::slice_head(n = n) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      min = purrr::map_dbl(path, \(p) terra::minmax(terra::rast(p), compute = TRUE)[1, 1]),
      max = purrr::map_dbl(path, \(p) terra::minmax(terra::rast(p), compute = TRUE)[2, 1])
    ) |>
    dplyr::select(region, use, min, max, path)
}