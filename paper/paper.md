---
title: '`hellometry`: Quickly estimating body length and body mass of organisms on R'
tags:
  - R
  - ecology
  - allometry
  - body size
  - body mass
  - biomass
authors:
  - name: Pierre Rogy
    orcid: 0000-0002-3002-0059
    affiliation: 1, 2
  - name: Diane Srivastava
    orcid: 0000-0003-4541-5595
    affiliation: 2
  - name: Olivier Dézerald
    orcid: 0000-0002-9987-9865
    affiliation: 3
  - name: Gustavo Q. Romero
    orcid: 0000-0002-3002-0059
    affiliation: 4
  - name: Paula M. de Omena
    orcid: 0000-0002-5221-7901
    affiliation: 5
  - name: Alathea Letaw
    orcid: 
    affiliation: 6
  - name: Sarah Abdelazim
    orcid: 0000-0002-6379-2733
    affiliation: 7                        
affiliations:
  - name: Department of Biology, McGill University, Montréal, QC, Canada
    index: 1
  - name: Department of Zoology and Biodiversity Research Centre, University of British Columbia, Vancouver, BC, Canada
    index: 2
  - name: DECOD, L'Institut Agro, Ifremer, INRAE, Rennes, France
    index: 3    
  - name: Laboratório de Interações Multitróficas e Biodiversidade, Departamento de Biologia Animal, Instituto de Biologia, Universidade Estadual de Campinas (UNICAMP), Campinas, SP, Brasil
    index: 4  
  - name: Laboratório Ecologia de Insetos Aquáticos, Universidade Vila Velha, Vila Velha, ES, Brasil
    index: 5  
  - name: 
    index: 6  
  - name: 
    index: 7         
date: 10 June 2026
bibliography: paper.bib
---

# Summary
Body mass is a trait central to much of ecological theory. In fact, body mass scales reasonably well with many other traits, such as metabolic rate, growth rate, reproductive rate, abundance, and trophic position [@Peters1983; @Brown2004; @Brose2006; @Woodward2005; @Smil2007]. Despite its conceptual simplicity, body mass is rarely measured directly because weighting organisms is slow, costly and often destructive. One workaround solution is to develop allometric equations, which convert a body dimension, for example body length, to an estimation of body mass [@Benke1999; @Meiri2010; @Froese2014;@Sohlstrom2018]. However, in practice, many data sets are incomplete, where body size can be categorical (e.g. "small"), and both body size and body mass can be missing.

# State of the field
The landscape to estimate body mass in ecological studies offers a variety of data sources. These tools exist in the form of R [@RCoreTeam] packages providing allometric relationships, such as `allodb` for extratropical trees [@GonzalezAkre2022] and `rfishbase`for fishes [@Boettiger2012], published allometric relationsips [@Benke1999; @Meiri2010; @Froese2014;@Sohlstrom2018], and published databases, such as `AnimalTraits` [@Herberstein2022]. Other tools focus on methodological considerations post-estimations such as`GroupStruct` [@Chan2022]. Nonetheless, none of these tools addresses the constraint of imputing data in a custom dataset, nor dataset that can reflect allometric relationships specific to the area of study. also do allow to get combine data from piecemeal studies


# Software design
An explanation of the trade-offs you weighed, the design/architecture you chose, and why it matters for your research application. This should demonstrate meaningful design thinking beyond a superficial code structure description.

`hellometry` is an R  package that fills both body size and body mass gaps from a researcher's own data. Given a table that follows a small set of column-naming conventions, it (i) imputes missing numeric sizes from the empirical size distribution of the relevant taxon, and (ii) fits log–log allometric models and uses them to estimate missing body masses, together with prediction intervals. Importantly, every estimate is made at the finest taxonomic level for which a defensible model exists: the package fits candidate models at each level supplied by the user (e.g., species, genus, family, order), discards those that fail customisable goodness-of-fit filters, and falls back up the taxonomic hierarchy only as far as necessary. The result is a reproducible, dependency-light pipeline wrapped in a single function, `hellometry()`, built on the `tidyverse` toolchain [@Wickham2019].

Several design choices make it practical for real ecological datasets:

- **Taxon- and stage-specific models.** Models are
  fit separately for each taxon and each life `stage` (e.g., larva, juvenile, adult),
  reflecting the influence of metamorphosis of some organisms for length–mass scaling. When
  a taxon lacks enough data for a reliable model, `hellometry` automatically uses
  the next coarser taxonomic level, recording the level at which each estimate was
  made. 
- **Categorical size imputation.** Where only a qualitative size is recorded, the
  package maps "small", "medium", and "large" onto terciles of the taxon's
  observed size distribution. "unknown" body masses lead to the computation of the weigthed mean, as smaller (younger) organisms tend to be more numerous than larger (older) ones.
- **Automatic thresholding.** Candidate models are dropped when their
  $p$-value exceeds a threshold (default $0.05$) or their $R^2$ falls outside a
  user-set window (default $0 < R^2 < 0.95$, the upper bound guarding against
  overfit models from tiny samples). Both thresholds can be modified by the user.
- **Handling of different taxa.** Any taxonomy and any number of levels are
  supported through a `level_vec` argument and a handful of conventional column
  names, so the package is not tied to a particular taxonomic group or sampling
  scheme.

The package also includes a trait-based alternative to taxonomy:
when no satisfactory model exists below a coarse level, species can instead be
grouped by similarity in user-supplied discrete ("fuzzy") traits, matching taxa
whose trait values differ by at most one unit, following the fuzzy-coding
tradition in ecology [@Chevenet1994; @Cereghino2018].

It is important to underline that `hellometry` uses the data supplied by the user to generate body size and body mass estimations. This means that, even if the user can collate large customised datasets, the generated estimations are ultimately a product of the input data. 

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
REPRESENTATIVE OF INPUT DATA


# Research impact statement
Earlier versions of `hellometry` have been successfully used by the author's wide network of collaborators, both in terms of published articles [@Srivastava2003; @Rogy2025] and a Ph.D. dissertation [@Westwood2025]. The package is also being used in articles currently being written [S. Ravoth, pers. comm.]

# AI usage disclosure
The original package was written entirely without generative AI assistance. Claude Opus 4.8 was used in the last stages of the project to make the package more efficient (e.g. correcting redundancies in the code). Here, changes were checked by PR before implementation, and outputs were carefuly checked to those before generative AI was used to ensure the quality and correctness of the content. Claude Opus 4.8 was also used to generate the first draft of the present manuscript, draft that was then considerably edited by all co-authors.

# Acknowledgements
This is a publication of the Bromeliad Working Group. We thank Barbara A. Richardson and the late Michael J. Richardson for their numerous contribution to bromeliad science in general, and contribtuing data to this project in particular.  