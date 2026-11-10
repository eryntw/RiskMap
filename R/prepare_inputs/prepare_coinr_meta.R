#' Prepare a Vulnerability iMeta table for COINr::new_coin()
#'
#' Context: the Exposure Project builds a composite "vulnerability" index with
#' COINr from species trait indicators (plants, animals). iMeta is written
#' top-down (top aggregate at the lowest Level, indicators deepest) and is
#' ragged: some indicators hang directly off a stressor while others sit under
#' sub-aggregates, and duplicate_branch() copies can land one level shallower
#' than their originals. COINr needs a clean, balanced hierarchy, so this
#' function converts the table into the format new_coin() accepts.
#'
#' Steps:
#'   1. Checks the tree has exactly one root (Parent = NA) at the lowest Level.
#'   2. Pads the tree (pad_to_balanced) so every indicator sits at the same
#'      depth, by inserting single-child dummy aggregates above shallow
#'      indicators. With a weighted mean, a single child passes straight
#'      through, so scores are unchanged.
#'   3. Reverses Level so indicators are Level 1 and the index has the highest
#'      Level, which is the order COINr expects.
#'   4. Makes Weight numeric (NA becomes 1).
#'
#' Run it AFTER duplicate_branch() and remove_branch(), because those change
#' indicator depths. It is not taxon-specific: the same function is used for
#' plants and animals, as long as iMeta has the columns iCode, iName, Parent,
#' Level, Weight and Type ("Aggregate" / "Indicator") and Level runs top-down
#' (index lowest, indicators deepest).
#'
#' @param iMeta Data frame in the project's iMeta format (top-down Levels).
#' @return iMeta in COINr format: balanced, Level 1 = indicators, numeric
#'   Weight. Dummy aggregates are named `<iCode>_grp1`, `_grp2`, ... with
#'   iName `<iName> (group)`.
prepare_coinr_meta <- function(iMeta) {
  
  ## checks ##
  roots <- iMeta$iCode[is.na(iMeta$Parent)]
  if (length(roots) != 1L) {
    stop("iMeta must have exactly one root (Parent = NA); found ",
         length(roots), ": ", paste(roots, collapse = ", "))
  }
  if (iMeta$Level[iMeta$iCode == roots] != min(iMeta$Level)) {
    stop("Level must run top-down: the root '", roots,
         "' should have the lowest Level.")
  }
  
  ## pad, then convert to COINr's level order (indicators = Level 1) ##
  iMeta   <- pad_to_balanced(iMeta)
  max_lvl <- max(iMeta$Level)
  iMeta$Level <- max_lvl + 1 - iMeta$Level
  
  iMeta |>
    dplyr::mutate(Weight = dplyr::coalesce(as.numeric(Weight), 1))
}

#' Pad every indicator shallower than the deepest one to the same Level
#'
#' Helper for prepare_coinr_meta(). The deepest indicator Level is the target.
#' @param iMeta iMeta with top-down Levels.
#' @return iMeta with dummy aggregates added; all indicators at one Level.
pad_to_balanced <- function(iMeta) {
  is_ind  <- iMeta$Type == "Indicator"
  target  <- max(iMeta$Level[is_ind])
  shallow <- iMeta$iCode[is_ind & iMeta$Level < target]
  
  out <- purrr::reduce(shallow, \(m, code) pad_one(m, code, target),
                       .init = iMeta)
  
  message("Padded ", length(shallow), " indicator(s) to Level ", target,
          if (length(shallow)) paste0(": ", paste(shallow, collapse = ", ")))
  out
}

#' Insert single-child dummy aggregates above one indicator
#'
#' Helper for pad_to_balanced(). The dummies copy the indicator's row (so
#' Direction and Weight carry over) and are chained between the indicator and
#' its original parent.
#' @param iMeta   iMeta.
#' @param code    iCode of the indicator to pad.
#' @param target  Level the indicator should end up at.
pad_one <- function(iMeta, code, target) {
  ind     <- iMeta[iMeta$iCode == code, ]
  n_pad   <- target - ind$Level
  d_codes <- paste0(code, "_grp", seq_len(n_pad))
  
  dummies <- ind[rep(1, n_pad), ] |>
    dplyr::mutate(
      iCode  = d_codes,
      iName  = paste0(ind$iName, " (group)"),
      Parent = c(ind$Parent, d_codes[-n_pad]),
      Level  = ind$Level + seq_len(n_pad) - 1,
      Type   = "Aggregate"
    )
  
  iMeta$Parent[iMeta$iCode == code] <- d_codes[n_pad]
  iMeta$Level[iMeta$iCode == code]  <- target
  dplyr::bind_rows(iMeta, dummies)
}