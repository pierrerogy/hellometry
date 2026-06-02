#' @keywords internal
"_PACKAGE"

## Namespace imports for functions called without a `pkg::` prefix.
## (Qualified calls such as `dplyr::filter()` only need the package in
## DESCRIPTION Imports, not an importFrom here.)
#' @importFrom dplyr group_by n ntile row_number summarise
#' @importFrom purrr imap
#' @importFrom rlang .data .env expr sym syms
#' @importFrom stats lm predict
#' @importFrom tidyselect all_of
NULL

# Column names used in non-standard evaluation (data-masked dplyr verbs).
# Declared here so R CMD check does not flag them as undefined globals.
utils::globalVariables(c(
  ".", "abundance", "biomass", "biomass_col", "biomass_lower",
  "biomass_upper", "level", "model", "model_level", "model_taxon_name",
  "name", "prediction_interval", "size_category", "size_col",
  "size_col.x", "size_col.y", "size_level", "size_taxon_name", "stage"
))
