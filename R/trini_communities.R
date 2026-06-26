#' Read the `trini_communities` dataset
#'
#' Reads the `trini_communities` dataset, a set of invertebrate
#' community samples from 20 Trinidadian bromeliads (from @Rogy2024). It is provided
#' as a toy dataset to try out the package, alongside [bromeliad_inverts_measurements()].
#' Each taxon also carries a set of discrete (fuzzy) trait columns, matched from
#' @Cereghino2018, that can be returned with
#' `traits = TRUE` to illustrate the trait-based grouping functions.
#'
#' @param traits Logical; if `TRUE`, the returned tibble includes the fuzzy
#' trait columns. If `FALSE` (the default), only the taxonomy, abundance,
#' bromeliad ID and stage columns are returned.
#' @return A tibble with one row per taxon per bromeliad, with columns for
#' taxonomy, abundance, and bromeliad ID, and, when `traits = TRUE`, a set of
#' discrete (fuzzy) trait columns.
#' @export
trini_communities <- function(traits = FALSE) {

  # Locate the dataset shipped with the package
  path <-
    system.file("extdata", "trini_communities.csv",
                package = "hellometry",
                mustWork = TRUE)

  # Read it as a tibble
  dats <-
    tibble::as_tibble(
      utils::read.csv(path,
                      check.names = FALSE,
                      fileEncoding = "UTF-8",
                      stringsAsFactors = FALSE))

  # Drop the fuzzy trait columns unless the user asks for them
  if (!traits) {
    community_cols <-
      c("bromeliad_id", "class", "order", "family", "subfamily",
        "genus", "species", "n", "stage")
    dats <-
      dats[, intersect(community_cols, names(dats)), drop = FALSE]
  }

  dats

}
