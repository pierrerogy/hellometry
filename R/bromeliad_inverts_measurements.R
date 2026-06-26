#' Read the `bromeliad_inverts_measurements` dataset
#'
#' Reads the  `bromeliad_inverts_measurements` dataset, a compilation of real invertebrate
#' body size (mm) and body mass (mg) measurements from bromeliad communities. If you use the
#' data outside the package, please cite the package.
#'
#' @return A tibble with real body length and body size measurements from bromeliad invertebrates.
#' It contains columns for taxonomy ("phylum" to "species"), "stage", "abundance", "body_size_mm",
#' "body_mass_mg", "mass_type" (dry/wet), "data_providers" and "note".
#' @export
bromeliad_inverts_measurements <- function() {

  # Locate the dataset shipped with the package
  path <-
    system.file("extdata", "bromeliad_inverts_measurements.csv",
                package = "hellometry",
                mustWork = TRUE)

  # Read it and return as a tibble
  tibble::as_tibble(
    utils::read.csv(path,
                    check.names = FALSE,
                    fileEncoding = "UTF-8",
                    stringsAsFactors = FALSE))

}
