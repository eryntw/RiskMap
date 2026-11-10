#' Remove units (species) from a coin input list
#'
#' Drops rows of $iData whose uCode is in `ucodes`. iMeta is untouched.
#' Warns if some listed uCodes are not in the data (usually a naming mismatch,
#' e.g. spaces vs underscores).
#' @param x      list with $iMeta and $iData
#' @param ucodes character vector of uCodes to remove
#' @return list with updated $iData
remove_units <- function(x, ucodes) {
  missing_u <- setdiff(ucodes, x$iData$uCode)
  if (length(missing_u)) {
    warning(length(missing_u), " uCode(s) not found in iData: ",
            paste(utils::head(missing_u, 5), collapse = ", "),
            if (length(missing_u) > 5) ", ...")
  }
  n0 <- nrow(x$iData)
  x$iData <- dplyr::filter(x$iData, !uCode %in% ucodes)
  message("Removed ", n0 - nrow(x$iData), " unit(s); ", nrow(x$iData), " remain")
  x
}