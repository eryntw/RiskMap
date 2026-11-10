#' Set weights on iMeta rows (e.g. stressors under Exposure)
#'
#' Sets the Weight of the given iCodes. COINr weights are relative among
#' siblings under the same parent, so only ratios matter. Siblings not listed
#' keep their current Weight (NA is treated as 1 later in prepare_coinr_meta).
#' Stops if an iCode is not in iMeta or if all children of a parent would end
#' up with zero weight.
#'
#' @param x       list with $iMeta and $iData
#' @param weights named numeric vector: names are iCodes, values are weights
#' @return list with updated $iMeta
set_weights <- function(x, weights) {
  
  stopifnot(is.numeric(weights), !is.null(names(weights)), all(weights >= 0))
  unknown <- setdiff(names(weights), x$iMeta$iCode)
  if (length(unknown)) {
    stop("iCode(s) not in iMeta (was the branch dropped?): ",
         paste(unknown, collapse = ", "))
  }
  
  x$iMeta$Weight <- as.numeric(x$iMeta$Weight)
  x$iMeta$Weight[match(names(weights), x$iMeta$iCode)] <- weights
  
  tot <- tapply(dplyr::coalesce(x$iMeta$Weight, 1), x$iMeta$Parent, sum)
  if (any(tot <= 0)) {
    stop("All children have zero weight under: ",
         paste(names(tot)[tot <= 0], collapse = ", "))
  }
  x
}
