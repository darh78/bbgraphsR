# internal cache env for player-level game logs (fast in-session)
.bbg_graphs_cache <- new.env(parent = emptyenv())

#' Return the in-memory cache environment (advanced use)
#' @return An environment
#' @export
bbgr_mem_cache <- function() .bbg_graphs_cache

.onLoad <- function(libname, pkgname) {
  # keep your existing theme setup
  ggplot2::theme_set(theme_bbgraphs())

  # ensure default parquet dir exists early (harmless no-op if already there)
  if (requireNamespace("rappdirs", quietly = TRUE)) {
    base <- rappdirs::user_cache_dir("bbgraphsR")
    dir.create(file.path(base, "players_parquet"), recursive = TRUE, showWarnings = FALSE)
  }
}
