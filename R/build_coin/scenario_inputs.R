#' Build the inputs for one development scenario
#'
#' Removes the stressor branches that do not apply (using remove_branch) and
#' sets the weights of the rest.
#'
#' @param x       list with $iMeta and $iData (e.g. coin_p_inputs)
#' @param drop    iCodes of branches to remove (NULL = none)
#' @param weights named numeric vector of weights for the remaining branches
#' @return list with updated $iMeta and $iData
scenario_inputs <- function(x, drop = NULL, weights = NULL) {
  
  clash <- intersect(names(weights), drop)
  if (length(clash)) {
    stop("Weights given for branches that are dropped: ",
         paste(clash, collapse = ", "))
  }
  
  x <- purrr::reduce(drop, \(acc, code) remove_branch(acc, code), .init = x)
  if (!is.null(weights)) x <- set_weights(x, weights)
  x
}