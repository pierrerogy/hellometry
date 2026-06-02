#' Read a bundled extdata CSV, caching the parsed result
#'
#' Internal helper. Reads a CSV shipped in `inst/extdata`, parsing it with
#' `read.csv` on first use and returning a cached copy on every subsequent
#' call within the same session. Several functions (e.g. `get_measurements()`,
#' `get_bwgnames()`) read the same file, so caching avoids re-parsing it.
#' Copy-on-modify semantics mean callers can safely transform the returned
#' data frame without affecting the cache.
#'
#' @param file Name of the CSV file located in `inst/extdata`.
#' @return A data frame with the file contents.
#' @keywords internal
read_extdata <- function(file){
  if (is.null(.hellometry_cache[[file]]))
    .hellometry_cache[[file]] <-
      read.csv(system.file("extdata", file, package = "hellometry"))
  .hellometry_cache[[file]]
}

# Package-level cache for bundled extdata CSVs, keyed by file name.
# `emptyenv()` as parent keeps lookups from falling through to other scopes.
.hellometry_cache <- new.env(parent = emptyenv())
