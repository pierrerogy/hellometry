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
  - name: Sarah Abdelazim
    orcid: 0000-0002-6379-2733
    affiliation: 7                  
  - name: Fabiola Ospina-Bautista
    orcid: 0000-0003-2498-1459
    affiliation: 8      
  - name: Nicholas Marino
    orcid:
    affiliation: 9 
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
  - name: 
    index: 8
  - name: 
    index: 9                        
date: 15 July 2026
bibliography: paper.bib
---

# Summary
Body mass is a trait central to much of ecological theory. In fact, body mass scales surprisingly well with many other traits, such as metabolic rate, growth rate, reproductive rate, abundance, and trophic position [@Peters1983; @Brown2004; @Brose2006; @Woodward2005; @Smil2007]. Despite its conceptual simplicity, body mass is rarely measured directly because measuring the mass of organisms is slow, costly and often destructive. One workaround solution is to develop allometric equations, which convert a body dimension, for example body length, into an estimate of body mass [@Benke1999; @Meiri2010; @Froese2014;@Sohlstrom2018]. However, in practice, many data sets are incomplete, where body size can be categorical (e.g. "small"), and both body size and body mass can be missing. Here, we present `hellometry`, an R package [@RCoreTeam] that addresses these challenges by estimating missing body sizes and body masses directly from a researcher's own dataset. Using data from similar taxa, `hellometry` imputes categorical or absent sizes, and fits log–log allometric models at the finest available taxonomic level, returning both predictions and computed models.

# State of the field
The methodological landscape to estimate body mass in ecological studies offers a variety of data sources. Beyond published allometric relationships [@Benke1999; @Meiri2010; @Froese2014;@Sohlstrom2018], there exists R packages, such as `allodb` for extratropical trees [@GonzalezAkre2022] and `rfishbase` for fishes [@Boettiger2012], or published databases, such as `AnimalTraits` [@Herberstein2022]. These allow users to either directly apply the relationships to their data, or, if available, the source data can be accessed to generate custom relationships by hand. Other tools focus on methodological considerations post-estimation such as `GroupStruct` [@Chan2022]. Nonetheless, none of these tools address the constraint of missing body size data in a custom dataset, and they do not allow the piecemeal compilation of data to generate allometric relationships directly inside the tool.


# Software design
`hellometry` is an R  package that fills both body size and body mass gaps from the researcher's own data. Given a table that follows a small set of column-naming conventions, the package (i) imputes missing numeric sizes from the provided size distribution of the relevant taxon, and (ii) fits log–log allometric models and uses these models to estimate missing body masses, together with prediction intervals. Importantly, every estimate is made at the finest taxonomic level for which an acceptable model exists: the package fits candidate models at each level supplied by the user (e.g., species, genus, family, order), discards those that fail customisable goodness-of-fit filters, and falls back up the taxonomic hierarchy only as far as necessary. The result is a reproducible, dependency-light pipeline wrapped in a single function, `hellometry()`. 

Several design choices make `hellometry` practical for real ecological datasets:

- **Taxon- and stage-specific estimates.** 
  Size estimates and allometric relationships are computed separately for each taxon and each life stage (e.g., larva, juvenile, adult), reflecting the influence of ontogeny (e.g. metamorphosis) of some organisms on their body size and body mass. When a reliable estimate cannot be generated at a given taxonomic level, `hellometry` automatically returns the estimate at the next level above. Nonetheless, to allow researchers to examine body size estimates and allometric relationships in detail, the package can return all computed estimates and models at every taxonomic level. This is especially important if researchers want to closely examine the suite of models that can be generated from their data.
  
- **Categorical body size estimates.** 
  The package can flexibly handle different levels of coarseness in body size estimate. So far, four categorical values are accepted. When "small", "medium" or "large" is supplied, the package splits available data of body size for a given taxon into terciles and computes a log-log relationship within that tercile. To estimate the expected body mass of a randomly selected individual from the population, the package requires "unknown" body masses. Here, we use a weighted rather than unweighted mean, for smaller (younger) organisms tend to be more numerous than larger (older) ones.

- **Incorporation of fuzzy traits.** 
  The package also includes a trait-based alternative to taxonomy: when no satisfactory model exists below a coarse level, species can instead be grouped by similarity using discrete categorical ("fuzzy") traits [@Chevenet1994; @Cereghino2018] supplied by the user. Here, taxa are matched when their trait values differ by at most one unit of affinity across all given modalities.

- **Automatic thresholding.** 
  Candidate models are dropped when their $p$-value exceeds a threshold (default $0.05$) or their $R^2$ falls outside a user-set window (default $0 < R^2 < 0.95$, the upper bound guarding against overfit models from small samples). Both thresholds can be modified by the user.

- **Handling of different data formats.** 
  Any taxonomy and any number of levels are supported through a `level_vec` argument and a handful of conventional column names, so the package is not tied to a particular taxonomic group or sampling scheme. For example, users can easily combined the databases mentioned above, or a geographically-relevant subset, with their own data.

- **Ease of customisation**
  The package was built in the R language, a langage commonly used in ecological research. Although this may make the computation of estimates relatively slower for massive datasets, `hellometry` allows users to easily examine the functions, and modify them if need be. The package was built using the simple `tidyverse` [@Wickham2019] library, and includes thorough commenting.

The user can compute estimates with a single call. The input table must contain columns
named `size_col` (numeric length, or one of `"small"`, `"medium"`, `"large"`,
`"unknown"`), `biomass_col` (mass, with `NA` where an estimate is wanted),
`abundance`, `stage` ("larva", "adult"...), `biomass_type` (`"dry"` or `"wet"`), and one column per taxonomic level. To illustrate its usage, the package comes with a toy dataset from communities of aquatic invertebrates sampled from Trinidadian bromeliads [@Rogy2024]. The package also includes a large dataset of body size (head to tail, mm) and body mass (mg) of bromeliad invertebrates collected across the Neotropics with by the authors of this manuscript.

```r
# Load library
library(hellometry)

# Define taxonomic levels, from finest to coarsest resolution
level_vec <- 
  c("species", "genus", "family", "order")

# Read in the package datasets
## Real body-size and body-mass measurements, used to build the models
measurements <- 
  bromeliad_inverts_measurements() %>% 
  ## Rename columns to those expected by hellometry()
  dplyr::rename(size_col = body_size_mm,
                biomass_col = body_mass_mg,
                biomass_type = mass_type) %>% 
  ## size_col must be character
  ## biomass_col must be numeric for the allometric models
  dplyr::mutate(size_col = as.character(size_col),
                biomass_col = as.numeric(biomass_col))

## Trinidadian communities, the taxa we want size and biomass estimates for
communities <- 
  trini_communities() %>% 
  ## Rename the abundance column and add the columns hellometry() needs
  dplyr::rename(abundance = n) %>% 
  ## Here we do not have any information on the invertebrates, so size is "unknown" and
  ## biomass NA
  dplyr::mutate(size_col = "unknown",   
                biomass_col = NA_real_, 
                biomass_type = "dry")

# Stack the reference measurements and the target communities into one table
my_invertebrates <- 
  measurements %>% 
  dplyr::bind_rows(communities)

# Generate estimates for the communities using the measurements as reference
estimates <- 
  hellometry(dats = my_invertebrates,
             level_vec = level_vec,
             biomass_type = "dry")

# Check outputs
## The ouptuts consist of a list of three elements
str(estimates)
# `data` is your data with estimated body sizes and biomasses, and column with information on the level at which were performed the estimations
dplyr::glimpse(estimates$data)
# `size_estimates` is a tibble with all unique size estimations that were joined to your data
dplyr::glimpse(estimates$size_estimates)
# `model_estimates` is a tibble with all unique allometric models that were joined to your data
dplyr::glimpse(estimates$model_estimates)
```

Internally, `hellometry()` builds a measurement table from the rows that carry a numeric size (`make_measurement_table()`), computes every admissible size estimate and allometric model across taxonomic levels (`full_estimation_table()`), joins the finest available estimate back to each
target row, and predicts mass. `hellometry` returns a list of three data frames: `data`
(the original table augmented with estimated sizes, masses, prediction-interval bounds, and the taxonomic level and name used for each estimate), `size_estimates` (the unique size estimates applied), and `model_estimates` (the unique allometric models applied). The helper functions are also exported, so users can inspect all possible estimates and models for their data without committing to the full pipeline. A worked example is provided in the package vignette (`vignette("hellometry")`).

It is important to underline that `hellometry` uses the data supplied by the user to generate body size and body mass estimates. This means that, even if the user can collate large customised datasets, the generated estimates are ultimately a product of the input data.  

# Research impact statement
Earlier versions of `hellometry` have been successfully used by the authors' network of collaborators, both in terms of published articles [@Srivastava2023; @Rogy2025] and a Ph.D. dissertation [@Westwood2025]. The package is also being used in articles currently being written [S.M. Ravoth, pers. comm.]

# AI usage disclosure
The original package was written entirely without generative AI assistance. Claude Opus 4.8 was used in the last stages of the project to make the package more efficient (e.g. correcting redundancies in the code). Here, changes were checked by PR before implementation, and outputs were carefuly checked to those before generative AI was used to ensure the quality and correctness of the content. Claude Opus 4.8 was also used to generate the first draft of the present manuscript, draft that was then considerably edited by all co-authors.

# Acknowledgements
This is a publication of the Bromeliad Working Group. We thank Barbara A. Richardson and the late Michael J. Richardson for their numerous contribution to bromeliad science in general, and contributing data to this project in particular.  