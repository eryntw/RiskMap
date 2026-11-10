library(dplyr)

#' Duplicate a branch of a COINr hierarchy under a new parent
#'
#' COINr's iMeta is a strict tree: every iCode has exactly one Parent, so an
#' aggregate (and its indicators) cannot sit under two parents. When the same
#' aggregate is needed under a second parent (e.g. a "recovery" aggregate of
#' five traits reused by another stressor), this function copies the whole
#' branch with suffixed iCodes and duplicates the matching iData columns.
#'
#' Because the copy has the same data, directions, weights and treatment
#' settings, Treat(), Normalise() and Aggregate() give it identical scores to
#' the original branch.
#'
#' What it does:
#'   1. Collects `branch_root` and all of its descendants from iMeta.
#'   2. Copies those rows, appending `suffix` to each iCode and to each Parent
#'      inside the branch. The copied root is re-pointed to `new_parent`.
#'   3. Shifts the copied Level values so they match the new parent's level.
#'      Warns if a shift was needed, because COINr expects a balanced tree
#'      (all indicators at the same depth); pad with single-child dummy
#'      aggregates if so.
#'   4. Optionally sets the Weight of the copied root (`root_weight`).
#'   5. Duplicates each indicator's iData column under its new suffixed name.
#'
#' Run it before new_coin(), after sync_coinr_inputs() (or before it; either
#' order works because the new iData columns match the new iCodes).
#'
#' @param iMeta        COINr iMeta data frame (needs iCode, Parent, Level,
#'                     and Weight if `root_weight` is used).
#' @param iData        COINr iData data frame, with indicator columns named by
#'                     iCode.
#' @param branch_root  iCode of the aggregate to copy (e.g. "recovery").
#' @param new_parent   iCode of the aggregate the copy should sit under.
#' @param suffix       Text appended to every copied iCode. Must be unique
#'                     among existing iCodes (default "_2"). Use a different
#'                     suffix for each duplication of the same branch.
#' @param root_weight  Optional numeric weight for the copied root under its
#'                     new parent. Weights inside the branch are copied
#'                     unchanged. Use this to avoid double counting.
#'
#' @return A list with the updated `iMeta` and `iData`.
#'
#' @examples
#' dup <- duplicate_branch(sync$iMeta, sync$iData,
#'                         branch_root = "recovery",
#'                         new_parent  = "stressor_B",
#'                         suffix      = "_B",
#'                         root_weight = 0.5)
duplicate_branch <- function(iMeta, iData, branch_root, new_parent,
                             suffix = "_2", root_weight = NULL) {
  
  # 1. collect the branch: root + all descendants
  codes <- branch_root
  repeat {
    kids <- iMeta$iCode[iMeta$Parent %in% codes & !iMeta$iCode %in% codes]
    if (!length(kids)) break
    codes <- c(codes, kids)
  }
  
  old_parent <- iMeta$Parent[iMeta$iCode == branch_root]
  shift <- iMeta$Level[iMeta$iCode == new_parent] -
    iMeta$Level[iMeta$iCode == old_parent]
  if (shift != 0)
    warning("New parent is at a different level than the original parent (shift = ",
            shift, "). COINr needs a balanced tree: add single-child dummy ",
            "aggregates so indicators all sit at the same depth.")
  
  # 2. copy rows with new codes, re-pointed parents, adjusted levels
  copy <- iMeta |>
    filter(iCode %in% codes) |>
    mutate(Parent = if_else(iCode == branch_root, new_parent,
                            paste0(Parent, suffix)),
           iCode  = paste0(iCode, suffix),
           Level  = Level + shift)
  
  if (!is.null(root_weight))
    copy$Weight[copy$iCode == paste0(branch_root, suffix)] <- root_weight
  
  # 3. duplicate the matching data columns for indicator rows
  ind_old <- intersect(codes, names(iData))
  for (nm in ind_old) iData[[paste0(nm, suffix)]] <- iData[[nm]]
  
  message("Duplicated ", nrow(copy), " rows (", length(ind_old),
          " indicators) under '", new_parent, "'")
  list(iMeta = bind_rows(iMeta, copy), iData = iData)
}