#' Estimate biomass from a fitted allometric model
#'
#' Wraps `predict()` for every kind of model `full_estimation_table()` can fit,
#' and returns the biomass of a whole row on the scale of the supplied data
#'
#' Both kinds return a confidence interval, i.e. the uncertainty around the
#' mean predicted biomass. The estimate is scaled to the abundance of the row
#' while still on the scale the model was fitted on (log10 for an lm, the log
#' link for a Poisson glm), and then back-transformed.
#'
#' @param model A fitted allometric model
#' @param size The size to estimate biomass for
#' @param abundance The number of individuals the row holds
#' @return Three values: the biomass estimate, its lower and its upper bound
#' @export
predict_biomass <- function(model, size, abundance) {

  # A row with no individuals holds no biomass, and nothing to be uncertain about
  if (abundance == 0)
    return(c(0, 0, 0))

  # Data to estimate biomass for, named the way the models expect
  newdats <-
    data.frame(size_col = size)

  # If we have a Poisson glm
  ## Careful, glms also inherit the "lm" class, so they need to come first
  if (inherits(model, "glm")) {

    ## Get the estimate and its standard error, on the scale of the log link
    ret <-
      predict(model,
              newdata = newdats,
              type = "link",
              se.fit = TRUE)

    ## The log link is undone with exp(), i.e. a base of e
    base <-
      exp(1)

    ## The interval of a glm is a Wald one, so its bounds sit 1.96 SE away
    critical_value <-
      qnorm(0.975)

  } else {

    ## Get the estimate and its standard error, on the log10 scale
    ret <-
      predict(model,
              newdata = newdats,
              se.fit = TRUE)

    ## lms are fitted on log10(biomass_col), so they are undone with a base of 10
    base <-
      10

    ## Same critical value predict() uses for the confidence interval of an lm
    critical_value <-
      qt(0.975,
         df = df.residual(model))
  }

  # Scale the estimate to the abundance of the row, on the fitted scale
  ## Multiplying biomass by abundance means adding to it on the fitted scale
  ## The standard error needs no scaling, as every individual of the row is
  ## estimated from the same model, so they all share the same uncertainty
  fit <-
    ret$fit + log(abundance, base = base)

  # Back-transform the estimate and its bounds
  ## as.numeric() because predict() gives its output the row name of newdats
  ret <-
    as.numeric(base^c(fit,
                      fit - critical_value * ret$se.fit,
                      fit + critical_value * ret$se.fit))

  # Return
  return(ret)

}