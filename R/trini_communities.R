#' Read the `trini_communities` dataset
#'
#' Reads the `trini_communities` dataset, a set of invertebrate
#' community samples from 20 Trinidadian bromeliads (from @Rogy2024). It is provided
#' as a toy dataset to try out the package, alongside [bromeliad_inverts_measurements()].
#'
#' @return A tibble with one row per taxon per bromeliad, with columns for
#' taxonomy, abundance, and bromeliad ID.
#' @export
trini_communities <- function() {

  # Locate the dataset shipped with the package
  path <-
    system.file("extdata", "trini_communities.csv",
                package = "hellometry",
                mustWork = TRUE)

  # Read it and return as a tibble
  tibble::as_tibble(
    utils::read.csv(path,
                    check.names = FALSE,
                    fileEncoding = "UTF-8",
                    stringsAsFactors = FALSE))

}
