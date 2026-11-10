#' Remove a branch (aggregate + all descendants, or a single indicator)
#'
#' Drops the branch rows from iMeta and the matching indicator columns from
#' iData. Optionally prunes aggregates left with no children (COINr errors on
#' an empty aggregate). The top-level aggregate (Parent = NA) cannot be removed.
#'
#' @param x            list with $iMeta and $iData (e.g. coin_a, coin_p)
#' @param branch_root  iCode of the aggregate or indicator to remove
#' @param prune_empty  also remove parents left with no children
#' @return list with updated $iMeta and $iData
remove_branch <- function(x, branch_root, prune_empty = TRUE) {
  
  stopifnot(all(c("iMeta", "iData") %in% names(x)))
  if (!branch_root %in% x$iMeta$iCode) {
    stop("branch_root '", branch_root, "' not found in iMeta")
  }
  if (is.na(x$iMeta$Parent[x$iMeta$iCode == branch_root][1])) {
    stop("'", branch_root, "' is the top-level aggregate and cannot be removed")
  }
  
  ## collect the branch: root + all descendants ##
  codes <- branch_root
  repeat {
    kids <- x$iMeta$iCode[x$iMeta$Parent %in% codes & !x$iMeta$iCode %in% codes]
    if (!length(kids)) break
    codes <- c(codes, kids)
  }
  
  iMeta <- x$iMeta |> dplyr::filter(!iCode %in% codes)
  
  ## prune aggregates left childless ##
  pruned <- character()
  repeat {
    childless <- iMeta$iCode[iMeta$Type != "Indicator" &
                               !iMeta$iCode %in% iMeta$Parent]
    if (!prune_empty || !length(childless)) break
    pruned <- c(pruned, childless)
    iMeta  <- iMeta |> dplyr::filter(!iCode %in% childless)
  }
  
  iData <- x$iData |> dplyr::select(-dplyr::any_of(c(codes, pruned)))
  
  message("Removed ", length(codes), " iMeta rows: ",
          paste(codes, collapse = ", "))
  if (length(pruned)) {
    message("Also pruned empty parents: ", paste(pruned, collapse = ", "))
  }
  
  list(iMeta = iMeta, iData = iData)
}