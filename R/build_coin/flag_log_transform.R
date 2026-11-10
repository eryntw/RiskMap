library(dplyr)

#' Decide which indicators get a log transform, and build COINr indiv_specs
#'
#' Rule (all must hold for a column to be flagged):
#'   1. continuous: more than `min_unique` distinct values
#'   2. non-negative (no negative values)
#'   3. right-skewed: skewness > `skew_thresh` OR spans more than
#'      `span_thresh` orders of magnitude (max / smallest positive value)
#'
#' @param data         wide data frame, one row per species
#' @param exclude      non-indicator columns (ids, names, grouping)
#' @param log_fun      COINr treatment function name used for flagged columns
#' @param na_rm        passed to the COINr treatment function
#' @return list: $flags (descriptive table) and $indiv_specs (for Treat())
flag_log_transform <- function(data,
                               exclude     = c("uCode", "uName"),
                               min_unique  = 6,
                               skew_thresh = 2,
                               span_thresh = 2,
                               log_fun     = "log_CT",
                               na_rm       = TRUE) {
  
  num <- data |>
    select(where(is.numeric), -any_of(exclude))
  
  skew <- function(x) {
    m <- mean(x); s <- sqrt(mean((x - m)^2))
    if (s == 0) NA_real_ else mean((x - m)^3) / s^3
  }
  
  flags <- bind_rows(lapply(names(num), function(nm) {
    x   <- num[[nm]]
    x   <- x[is.finite(x)]
    
    if (!length(x)) {
      return(tibble(indicator = nm, n_unique = 0L, min = NA_real_,
                    max = NA_real_, n_zero = 0L, skew = NA_real_,
                    span = NA_real_))
    }
    
    pos <- x[x > 0]
    tibble(
      indicator = nm,
      n_unique  = n_distinct(x),
      min       = min(x),
      max       = max(x),
      n_zero    = sum(x == 0),
      skew      = skew(x),
      span      = if (length(pos)) log10(max(pos) / min(pos)) else NA_real_
    )
  })) |>
    mutate(
      reason = case_when(
        n_unique <= 1           ~ "constant or all-NA: drop or check",
        n_unique <= min_unique  ~ "ordinal/binary: no transform",
        min < 0                 ~ "negative values: no log",
        skew > skew_thresh | span > span_thresh ~ "continuous + right-skewed: log",
        TRUE                    ~ "continuous but not skewed: no transform"
      ),
      log_transform = reason == "continuous + right-skewed: log",
      note = if_else(log_transform & n_zero > 0,
                     "zeros present: check for structural zeros (e.g. non-flyers)",
                     NA_character_)
    )
  
  make_spec <- function(flag) {
    if (!flag) return("none")
    list(f1 = log_fun,
         f1_para = if (log_fun %in% c("log_CT", "log_CT_plus")) list(na.rm = na_rm) else list())
  }
  
  indiv_specs <- setNames(lapply(flags$log_transform, make_spec), flags$indicator)
  
  message("Log-transform flagged: ",
          paste(flags$indicator[flags$log_transform], collapse = ", "))
  
  list(flags = flags, indiv_specs = indiv_specs)
}