#' Finds which species have matching traits
#'
#' Evaluates all unique trait groupings and returns species that have the same
#' traits +/- 1
#'
#' @param measurement_table A table with the numerical measurements and biomass
#' used to compute allometric lms
#' @param trait_columns List of traits to match, should be column names in measurement_table
#' @param id_col Name of the column holding a unique identifier per species/taxon.
#' Default "species".


#' @return Tibble of four columns: level = "traits", name = focus species,
#' stage of focus species, id = matched species
#' @examples
#' # Fuzzy traits of three species, sp_a and sp_b within +/- 1 of each other
#' measurement_table <-
#'   data.frame(species = rep(c("sp_a", "sp_b", "sp_c"), each = 4),
#'              stage = "larva",
#'              size_col = c(1, 2, 3, 4, 1.2, 2.1, 3.3, 4.2, 8, 9, 10, 11),
#'              biomass_col = c(0.1, 0.4, 1, 2, 0.2, 0.5, 1.1, 2.2, 12, 15, 20, 26),
#'              AS1 = rep(c(1, 2, 5), each = 4),
#'              AS2 = rep(c(3, 3, 9), each = 4))
#'
#' # sp_a and sp_b match each other, sp_c only itself
#' matcher_of_traits(measurement_table, trait_columns = c("AS1", "AS2"))
#' @export
#'
  matcher_of_traits <- function(measurement_table, trait_columns, id_col = "species") {

    # First filter dataset with unique groupings of traits
    measurement_table <-
      measurement_table %>%
      dplyr::select(dplyr::all_of(c(id_col, trait_columns, "stage"))) %>%
      unique()

    # Identifiers of the trait groupings, in the same row order as the matrix
    ids <- measurement_table[[id_col]]

    # Print a little message saying that we are grouping species by traits
    message("Grouping species by trait similarities..")

    # Build a numeric trait matrix once (rows = trait groupings, cols = traits)
    ## Suppress warnings in case of non-numeric coercion, as in the original
    suppressWarnings(
      trait_mat <-
        vapply(measurement_table[trait_columns],
               as.numeric,
               numeric(nrow(measurement_table))))

    # Set progress bar with number of trait groupings
    pb <-
      progress::progress_bar$new(format = "[:bar] :current/:total (:percent)",
                                 total = nrow(measurement_table))
    ## Initiate it
    pb$tick(0)

    # For each row (unique trait grouping) of the filtered dataframe
    ret <-
      purrr::map_dfr(seq_len(nrow(measurement_table)), function(i) {

      ## Traits of the focus species for this row
      focus_traits <- trait_mat[i, ]

      ## Vectorised trait comparison against every grouping at once:
      ## a row matches if every trait differs from the focus by <= 1.
      ## NA differences (missing traits) never match, as before.
      diffs <- abs(sweep(trait_mat, 2, focus_traits, "-"))
      similar <- rowSums(diffs > 1 | is.na(diffs)) == 0

      ## Add tick to progress bar
      pb$tick()

      ## Make a little catch in case we do not find species with similar traits
      if (!any(similar)) {
        ## Returning an empty tibble
        return(tibble::tibble(level = character(),
                              name = character(),
                              stage = character(),
                              id = character()))
      }

      ## But if we get some species with similar traits, return them long format
      tibble::tibble(level = "traits",
                     name = ids[i],
                     stage = measurement_table$stage[i],
                     id = ids[similar])
      })


    # Return
    return(ret)

}
