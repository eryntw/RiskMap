## summarise_variants ##
# res: scenario_results() tables for one species group, all scenarios and
# variants row-bound. Returns:
#  $summary: per scenario and variant, how much the ranking moves vs baseline
#  $robust:  per species, how often it lands in the top_n and its rank interval
#  $shifts:  species-level rank shift vs baseline for every variant
summarise_variants <- function(res, top = "vuln", top_n = 20) {
  
  rk <- paste0("rank_", top)
  
  long <- res |>
    dplyr::transmute(scenario, variant, uCode, uName, rank = .data[[rk]])
  
  base <- long |>
    dplyr::filter(variant == "baseline") |>
    dplyr::select(scenario, uCode, base_rank = rank)
  
  shifts <- long |>
    dplyr::filter(variant != "baseline") |>
    dplyr::left_join(base, by = c("scenario", "uCode")) |>
    dplyr::mutate(shift = rank - base_rank)
  
  summary <- shifts |>
    dplyr::group_by(scenario, variant) |>
    dplyr::summarise(
      n_scored     = sum(!is.na(rank)),
      n_lost       = sum(is.na(rank) & !is.na(base_rank)),   # species losing a score
      spearman     = suppressWarnings(
        stats::cor(rank, base_rank, method = "spearman", use = "complete.obs")),
      top_overlap  = sum(rank <= top_n & base_rank <= top_n, na.rm = TRUE) /
        sum(base_rank <= top_n, na.rm = TRUE),                # share of baseline top_n kept
      median_shift = stats::median(abs(shift), na.rm = TRUE),
      p90_shift    = stats::quantile(abs(shift), 0.9, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    dplyr::arrange(scenario, spearman)
  
  robust <- long |>
    dplyr::group_by(scenario, uCode, uName) |>
    dplyr::summarise(
      pct_runs_top  = mean(!is.na(rank) & rank <= top_n),
      median_rank   = stats::median(rank, na.rm = TRUE),
      rank_p05      = stats::quantile(rank, 0.05, na.rm = TRUE, names = FALSE),
      rank_p95      = stats::quantile(rank, 0.95, na.rm = TRUE, names = FALSE),
      n_runs_scored = sum(!is.na(rank)),
      .groups = "drop"
    ) |>
    dplyr::arrange(scenario, dplyr::desc(pct_runs_top), median_rank)
  
  list(summary = summary, robust = robust, shifts = shifts)
}