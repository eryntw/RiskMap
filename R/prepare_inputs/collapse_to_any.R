#' Collapse an aggregate of 0/1 indicators into one "any" indicator
#'
#' Replaces aggregate `branch_root` and its child indicators with a single
#' binary indicator: 1 if any child is 1, 0 if all children are known and 0.
#' COINr sets aggregation methods per level, not per aggregate, so this is how
#' an OR-style rule is applied to one aggregate without affecting its
#' siblings at the same level.
#'
#' The aggregate's row is kept (same iCode, parent, Direction, Weight) and its
#' Type becomes "Indicator"; the child rows and columns are removed. Run it
#' before flag_log_transform() and build_treated_coin().
#'
#' @param x          list with $iMeta and $iData
#' @param branch_root iCode of the aggregate; all children must be 0/1 indicators
#' @param na_as_zero  if FALSE (default), a species with no 1 among its known
#'                    children but at least one NA is NA (unknown); if TRUE,
#'                    it is scored 0
#' @return list with updated $iMeta and $iData
collapse_to_any <- function(x, branch_root, na_as_zero = FALSE) {
  
  kids <- x$iMeta$iCode[x$iMeta$Parent %in% branch_root]
  if (!length(kids)) stop("'", branch_root, "' has no children in iMeta")
  if (any(x$iMeta$Type[x$iMeta$iCode %in% kids] != "Indicator")) {
    stop("All children of '", branch_root, "' must be indicators")
  }
  miss <- setdiff(kids, names(x$iData))
  if (length(miss)) stop("Children missing from iData: ", paste(miss, collapse = ", "))
  
  m <- as.matrix(x$iData[kids])
  if (!all(m %in% c(0, 1) | is.na(m))) {
    stop("Children of '", branch_root, "' must be coded 0/1 (or NA)")
  }
  
  any_one   <- rowSums(m == 1, na.rm = TRUE) > 0
  all_known <- rowSums(is.na(m)) == 0
  flag <- dplyr::case_when(
    any_one                 ~ 1,
    all_known | na_as_zero  ~ 0,
    TRUE                    ~ NA_real_
  )
  
  x$iData[[branch_root]] <- flag
  x$iData <- dplyr::select(x$iData, -dplyr::all_of(kids))
  x$iMeta <- x$iMeta |>
    dplyr::filter(!iCode %in% kids) |>
    dplyr::mutate(Type = dplyr::if_else(iCode == branch_root, "Indicator", Type))
  
  message("'", branch_root, "': ", sum(flag == 1, na.rm = TRUE), " = 1, ",
          sum(flag == 0, na.rm = TRUE), " = 0, ", sum(is.na(flag)), " NA")
  x
}