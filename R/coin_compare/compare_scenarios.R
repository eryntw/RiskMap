## compare_scenarios ##
# res: scenario_results() tables for ONE species group, row-bound.
# Returns list: $table (one row per species, scores and ranks per scenario,
# sorted by rank_range), $rank_cor (Spearman correlation of the top-level
# ranks), $top_overlap (species shared by each pair's top_n), and
# $na_by_aggregate (units left NA at each aggregate, per scenario).
compare_scenarios <- function(res, top = "vuln", top_n = 20) {
  
  scen <- unique(res$scenario)
  sc   <- paste0("score_", top)
  rk   <- paste0("rank_", top)
  
  wide <- res |>
    dplyr::select(uCode, uName, scenario, dplyr::all_of(c(sc, rk))) |>
    tidyr::pivot_wider(names_from = scenario,
                       values_from = dplyr::all_of(c(sc, rk)))
  
  rank_cols <- paste0(rk, "_", scen)
  rmat <- as.matrix(wide[rank_cols])
  
  wide <- wide |>
    dplyr::mutate(
      rank_min   = apply(rmat, 1, min, na.rm = TRUE),
      rank_max   = apply(rmat, 1, max, na.rm = TRUE),
      rank_range = rank_max - rank_min
    ) |>
    dplyr::arrange(dplyr::desc(rank_range))
  
  rank_cor <- stats::cor(rmat, method = "spearman",
                         use = "pairwise.complete.obs")
  dimnames(rank_cor) <- list(scen, scen)
  
  sets <- stats::setNames(
    purrr::map(rank_cols, \(cc) wide$uCode[!is.na(wide[[cc]]) & wide[[cc]] <= top_n]),
    scen
  )
  top_overlap <- utils::combn(scen, 2, simplify = FALSE) |>
    purrr::map(\(p) tibble::tibble(
      scenario_a = p[1], scenario_b = p[2], top_n = top_n,
      n_shared = length(intersect(sets[[p[1]]], sets[[p[2]]]))
    )) |>
    dplyr::bind_rows()
  
  na_by_aggregate <- res |>
    dplyr::group_by(scenario) |>
    dplyr::summarise(dplyr::across(dplyr::starts_with("score_"),
                                   \(x) sum(is.na(x))),
                     .groups = "drop")
  
  list(table = wide, rank_cor = rank_cor,
       top_overlap = top_overlap, na_by_aggregate = na_by_aggregate)
}