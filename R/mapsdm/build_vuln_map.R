## build_vuln_map ##
# One vulnerability map for one scenario x group x region. Writes a GeoTIFF and
# returns its path (use with format = "file"). Works in chunks and never
# materialises score * SDM per species.
build_vuln_map <- function(tbl, scn, grp, reg, out_dir, chunk_size = 100,
                           memfrac = 0.1, max_species = Inf) {
  
  ## Step 1: this map's species, highest score first (the sort is essential) ##
  x <- tbl |>
    dplyr::filter(scenario == scn, group == grp, region == reg) |>
    dplyr::arrange(dplyr::desc(score))
  if (is.finite(max_species)) x <- utils::head(x, max_species)
  if (nrow(x) == 0) stop("No mapped species for ", scn, " / ", grp, " / ", reg)
  
  ## Step 2: all files must share one grid ##
  check_same_grid(x$path)
  
  ## Step 3: output and scratch folders ##
  fs::dir_create(out_dir)
  out_path <- fs::path(out_dir, paste0("vuln_", scn, "_", grp, "_", reg, ".tif"))
  tmp_dir  <- fs::dir_create(fs::path(out_dir, paste0(".tmp_", scn, "_", grp, "_", reg)))
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)
  
  ## Step 4: terra settings ##
  terra::terraOptions(memfrac = memfrac)
  wopt <- list(datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  
  ## Step 5: chunks of ~100 species, highest scores first ##
  chunks <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / chunk_size))
  
  ## Step 6: one map per chunk (score of highest-scoring species present) ##
  chunk_files <- purrr::imap_chr(chunks, \(idx, k) {
    f   <- fs::path(tmp_dir, paste0("chunk_", k, ".tif"))
    stk <- terra::rast(x$path[idx])
    pres  <- terra::app(stk, "max", na.rm = TRUE)
    first <- terra::app(stk, "which.max", na.rm = TRUE)
    sc    <- terra::subst(first, from = seq_along(idx), to = x$score[idx])
    res   <- sc * pres
    terra::ifel(res > 0, res, NA, filename = f, overwrite = TRUE, wopt = wopt)
    terra::tmpFiles(current = TRUE, remove = TRUE)
    as.character(f)
  })
  
  ## Step 7: pixel-wise max across chunks ##
  terra::app(terra::rast(chunk_files), "max", na.rm = TRUE,
             filename = out_path, overwrite = TRUE, wopt = wopt)
  
  as.character(out_path)
}