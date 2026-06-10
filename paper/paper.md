---
title: 'hellometry: Imputing invertebrate body size and biomass from allometric relationships in R'
tags:
  - R
  - ecology
  - allometry
  - body size
  - biomass
  - invertebrates
authors:
  - name: Pierre Rogy
    orcid: 0000-0002-3002-0059
    affiliation: 1
affiliations:
  - name: Department of Biology, McGill University, Montréal, Québec, Canada
    index: 1
date: 10 June 2026
bibliography: paper.bib
---

# Summary
Body mass is a central trait to much of ecological theory. In fact, body mass scales reasonably well with many other traits, such as metabolic rate, growth rate, reproductive rate, abundance, trophic position [@Peters1983; @Brown2004;@Brose2006; @Woodward2005; @Smil2007; @White2007]. However, body mass is rarely measured directly: weighting specimens is slow, costly and destructive.






 so ecologists instead measure a linear body dimension
(typically length, in mm) and convert it to mass (in mg) through allometric
relationships of the form $M = aL^{b}$ [@Benke1999; @Sohlstrom2018]. In
practice, field datasets are incomplete in two ways at once: some specimens have
only categorical size information ("small", "medium", "large", or simply
"unknown") rather than a numeric length, and many have no mass at all.

`hellometry` is an R [@RCoreTeam] package that fills both gaps from a
researcher's *own* data. Given a table that follows a small set of column-naming
conventions, it (i) imputes missing numeric sizes from the empirical size
distribution of the relevant taxon, and (ii) fits log–log allometric models and
uses them to predict missing masses, together with prediction intervals. Crucially, every estimate is made at the *finest taxonomic level for which a defensible model exists*: the package fits candidate models at each level supplied by the user (e.g., species, genus, family, order), discards those that fail goodness-of-fit filters, and falls back up the taxonomic hierarchy only as far as necessary. The result is a reproducible, dependency-light pipeline wrapped in a single function, `hellometry()`, built on the `tidyverse` toolchain [@Wickham2019].

# Statement of need

Despite the central role of body mass in ecology, the software landscape for
estimating it is fragmented and, for invertebrates, largely absent. Existing
tools address adjacent but distinct problems. The `allodb` package estimates
biomass from stem diameter for *trees* in extratropical forest plots
[@GonzalezAkre2022]; `rfishbase` exposes length–weight coefficients for *fishes*
[@Boettiger2012]; and `GroupStruct` performs allometric *size correction* of
morphometric measurements rather than mass prediction [@Chan2022]. Curated trait
compilations such as `AnimalTraits` [@Herberstein2022] and general trait-imputation
frameworks [@Penone2014] provide measured or model-filled values, but they neither
fit allometric equations to a user's specimens nor respect the taxonomic
resolution of those specimens. For invertebrate biomass specifically, researchers
have therefore relied on transcribing published length–mass coefficients
[@Benke1999; @Sohlstrom2018; @Wardhaugh2013] into bespoke, error-prone scripts —
an approach that ignores the variation captured in their own measurements and is
difficult to reproduce.

`hellometry` targets exactly this gap. Rather than shipping a fixed library of
equations, it treats the user's measured rows as the training set, so estimates
are tailored to the taxa, life stages, and conditions actually sampled — the
better the supplied data, the better the estimates. Several design choices make
it practical for real ecological datasets:

- **Taxon- and stage-specific models with hierarchical fallback.** Models are
  fit separately for each taxon and each life `stage` (e.g., larva, pupa, adult),
  reflecting the strong stage-dependence of invertebrate length–mass scaling. When
  a taxon lacks enough data for a reliable model, `hellometry` automatically uses
  the next coarser taxonomic level, recording the level at which each estimate was
  made.
- **Automatic quality control.** Candidate models are dropped when their
  $p$-value exceeds a threshold (default $0.05$) or their $R^2$ falls outside a
  user-set window (default $0 < R^2 < 0.95$, the upper bound guarding against
  overfit models from tiny samples).
- **Categorical size imputation.** Where only a qualitative size is recorded, the
  package maps "small", "medium", and "large" onto terciles of the taxon's
  observed size distribution and "unknown" onto its mean, so that downstream mass
  prediction can still proceed.
- **Honest uncertainty.** Masses are returned with prediction intervals
  (uncertainty around a single new observation), scaled by specimen abundance,
  rather than narrower confidence intervals around a mean.
- **Data-format agnosticism.** Any taxonomy and any number of levels are
  supported through a `level_vec` argument and a handful of conventional column
  names, so the package is not tied to a particular taxonomic group or sampling
  scheme.

The package also includes an experimental, trait-based alternative to taxonomy:
when no satisfactory model exists below a coarse level, species can instead be
grouped by similarity in user-supplied discrete ("crisp") traits, matching taxa
whose trait values differ by at most one unit, following the fuzzy-coding
tradition in ecology [@Chevenet1994].

`hellometry` was developed to process community invertebrate datasets for
ecological research and is intended for ecologists, entomologists, and freshwater
and soil biologists who need reproducible biomass estimates from heterogeneous,
partially measured collections.

# Functionality and example

The user-facing workflow is a single call. The input table must contain columns
named `size_col` (numeric length, or one of `"small"`, `"medium"`, `"large"`,
`"unknown"`), `biomass_col` (mass, with `NA` where an estimate is wanted),
`abundance`, `stage`, `biomass_type` (`"dry"` or `"wet"`), and one column per
taxonomic level.

```r
library(hellometry)

# Taxonomic levels, from finest to coarsest resolution
level_vec <- c("species", "genus", "family", "order")

result <- hellometry(dats         = my_invertebrates,
                     level_vec    = level_vec,
                     biomass_type = "dry")
```

Internally, `hellometry()` builds a measurement table from the rows that carry a
numeric size (`make_measurement_table()`), computes every admissible size
estimate and allometric model across taxonomic levels
(`full_estimation_table()`), joins the finest available estimate back to each
target row, and predicts mass. It returns a list of three data frames: `data`
(the original table augmented with estimated sizes, masses, prediction-interval
bounds, and the taxonomic level and name used for each estimate),
`size_estimations` (the unique size estimates applied), and `model_estimations`
(the unique allometric models applied). The helper functions are also exported,
so users can inspect all possible estimates and models for their data without
committing to the full pipeline. A worked example is provided in the package
vignette (`vignette("hellometry")`).

# Limitations and future directions

`hellometry` fits ordinary least-squares models on $\log_{10}$-transformed data
and back-transforms predictions; it does not currently apply a correction factor
for the bias this introduces [@Sprugel1983], so estimated masses should be read as
median, not mean, predictions. Estimates are only as representative as the
measured specimens that train them, and the trait-based grouping is experimental
and not yet integrated into the main wrapper. Planned extensions include optional
bias correction and support for alternative model backends (e.g., Bayesian
regression) where small samples make uncertainty quantification especially
valuable.

# Acknowledgements


# References
