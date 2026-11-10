#' Sync COINr iData and iMeta
#'
#' - iMeta: drops Indicator rows whose iCode is not a column in iData;
#'   keeps all Aggregate rows (optionally pruning those left childless)
#' - iData: keeps unit columns (uCode, uName, Time) plus columns whose
#'   name is an Indicator iCode in iMeta
#' - Reports everything that was dropped or unmatched
sync_coinr_inputs <- function(iData, iMeta,
                              unit_cols       = c("uCode", "uName", "Time"),
                              type_col        = "Type",
                              indicator_label = "Indicator",
                              prune_empty     = TRUE,
                              verbose         = TRUE) {
  
  stopifnot("uCode" %in% names(iData),
            all(c("iCode", "Parent", type_col) %in% names(iMeta)))
  
  dup_icode <- iMeta$iCode[duplicated(iMeta$iCode)]
  is_ind    <- iMeta[[type_col]] == indicator_label
  ind_codes <- iMeta$iCode[is_ind]
  agg_codes <- iMeta$iCode[!is_ind]
  data_cols <- setdiff(names(iData), unit_cols)
  
  meta_not_in_data <- setdiff(ind_codes, data_cols)   # indicators with no data
  data_not_in_meta <- setdiff(data_cols, iMeta$iCode) # data cols with no metadata
  data_is_agg      <- intersect(data_cols, agg_codes) # data col named like an aggregate
  keep_ind         <- intersect(ind_codes, data_cols)
  
  # --- iMeta: drop unmatched indicators, then prune empty aggregates ---
  iMeta_out    <- filter(iMeta, !iCode %in% meta_not_in_data)
  pruned_agg   <- character()
  repeat {
    childless <- iMeta_out$iCode[iMeta_out[[type_col]] != indicator_label &
                                   !iMeta_out$iCode %in% iMeta_out$Parent]
    if (!prune_empty || length(childless) == 0) break
    pruned_agg <- c(pruned_agg, childless)
    iMeta_out  <- filter(iMeta_out, !iCode %in% childless)
  }
  
  # --- iData: unit columns + matched indicators (in iMeta order) ---
  iData_out <- iData |>
    select(any_of(unit_cols),
           all_of(intersect(iMeta_out$iCode, keep_ind)))
  
  report <- list(
    kept_indicators          = intersect(iMeta_out$iCode, keep_ind),
    in_iMeta_not_in_iData    = meta_not_in_data,
    in_iData_not_in_iMeta    = data_not_in_meta,
    iData_col_matches_aggregate = data_is_agg,
    aggregates_pruned_empty  = pruned_agg,
    aggregates_still_empty   = setdiff(
      iMeta_out$iCode[iMeta_out[[type_col]] != indicator_label],
      iMeta_out$Parent),
    duplicated_iCode         = unique(dup_icode)
  )
  
  if (verbose) {
    f <- function(x) if (length(x)) paste(x, collapse = ", ") else "none"
    message("Indicators kept:                     ", length(report$kept_indicators))
    message("In iMeta, NOT in iData (dropped):    ", f(report$in_iMeta_not_in_iData))
    message("In iData, NOT in iMeta (dropped):    ", f(report$in_iData_not_in_iMeta))
    message("iData col named like an aggregate:   ", f(report$iData_col_matches_aggregate))
    message("Empty aggregates pruned:             ", f(report$aggregates_pruned_empty))
    message("Duplicated iCode in iMeta:           ", f(report$duplicated_iCode))
  }
  
  list(iData = iData_out, iMeta = iMeta_out, report = report)
}