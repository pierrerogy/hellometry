# Load packages
library(bwgdata) # works with version 0.2.1 ONLY
library(tidyverse)
library(devtools)
library(fwdata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stringr)
library(stringdist)

# Get abundance by size data ----------------------------------------------
# Get list of datasets in database
## in list format
dats <-
  bwg_get("datasets")
dat_list <- 
  dats %>%
  dplyr::select(dataset_id, name)

# Make data frame to store all data
all_dats <- 
  data.frame()

# Make vector of row indexes to get datasets (i.e.  row 11 Discovery Bay, 47 test, 48 testest not useable here)
rows <- 
  c(1:10, 12:46)
# NOTE: Pitilla 2004 is a special case: Ben Gilbert, pipetted out only mosquitoes from bromeliads, so no other functional group
#)


# Loop to get the data from all different datasets
for(i in rows){
  # Get dataset id and name
  dataset_id <- 
    dat_list[i, 1]
  dataset_name <- 
    dat_list[i, 2]
    
  # Get raw data from dataset 
  dataset_raw <-
   bwg_get("matrix",
           opts = list(dataset_id = dataset_id))
  ## Print structure on two levels only
  #str(dataset_raw,
  #    max.level = 2)

  # Dealing with the nested list structure of data
  ## Need to extract first element of list, then pull out the first and second
  ## elements of the list associated with each species (number in list name). This corresponds to the
  ## bwgname and the measurement of that species
  dataset_meas <-
    ## first level of list is actually of length 1, and [[1]] includes everything else
    dataset_raw[[1]] %>%
    ## convert list of list into dataframe, with first column being species key, and second column the nested list
    enframe(name = "species.key") %>%
    ## from that second column, extract bwg name of species, and store rest of nested list within new column
    mutate(bwgname = map_chr(value, 1),
           measure = map(value,2)) %>%
    ## remove column coupling bwg names and nested lists
    select(-value)

  # Second round of unnesting
  dataset_almost_flat <-
    dataset_meas %>%
    ## measure is a nested list; extract and convert to a dataframe
    ### again list of length 1 with everything stored in first element, so keep onyl this
    mutate(meas_list = map(measure, 1),
           ### convert list to dataframe, again with list names in "listcontent" column
           meas_df = map(meas_list, enframe, name = "listcontents")) %>%
    ## remove old columns, to keep only species key, bwg name, and jsut created dataframe column
    select(-measure, -meas_list) %>%
    ## unpack the dataframe, basically multiplying the number of rows by the number of nested elements in df
    unnest(meas_df) %>%
    ## pivot the unpacked contents to columns
    pivot_wider(names_from = "listcontents", values_from = "value")

  # Flatten and unpack the columns
  dataset_flat <-
    dataset_almost_flat %>%
    ## flatten first level of list
    mutate(category_range = flatten_chr(category_range),
           ## extract first character element of value list in row
           measurement = map_chr(measurement, "value"),
           ## turn last level of nested list into data frame
          bromeliads = map(bromeliads, enframe)) %>%
    ## unnest data frame
    unnest(bromeliads) %>%
    ## flatten first level of list
    mutate(value = flatten_chr(value) %>%
             ## convert to numeric (character before)
             readr::parse_double(.)) %>%
    ## give more informative name to "name"
    rename(bromeliad = name) %>% 
    ## add column with dataset info
    mutate(dataset_id = dataset_id,
           dataset_name = dataset_name) %>% 
    # put columns in better order
    relocate(dataset_id, dataset_name,  .before = species.key)
  
  # bind rows to overall data frame
  all_dats <- 
    bind_rows(all_dats, 
              dataset_flat) 
  
}


# Split measurement column around "_" to get size stage and measurement columns
all_dats <- 
  all_dats %>% 
  separate(measurement, c("stage", "size"), sep = "_", remove = T) %>% 
  mutate(size = ifelse(is.na(size), stage, size))
## returns big warning message, it's ok because we if only one word, kept in "stage"

# Remove "mm" in some meas rows
for(i in 1:nrow(all_dats)){
  all_dats[i, 7] <- 
    str_remove(all_dats[i, 7], "mm")
}

# Remove bromeliad 12626 in Cardoso(2008), and give better name to data and columns
abundance_size <- 
  all_dats %>% 
  filter(bromeliad != 12626) %>% 
  rename(abundance = value)

# Give better column name, and harmonise size categories
abundance_size <- 
  abundance_size %>% 
  rename(bwg_name = bwgname) %>% 
  mutate(size = ifelse(size %in% c("tiny", "S", "xsmall"),
                       "small",
                       ifelse(size %in% c("huge", "xlarge"),
                              "large",
                              ifelse(size %in% c("average", "default", "regular"),
                                     "unknown",
                                     size))))

# Get raw data --------------------------------------------------------
# Sign in with secret password 
fw_auth()

# What datasets exist so far?
fw_versions(local = FALSE,
            biomass = TRUE)
fw_versions(local = FALSE,
            biomass = FALSE)
# Get allometry data
fw_biomass <- 
  fw_data("v.0.0.1_0.0.1",
          biomass = TRUE) %>%
  ## convert species_id to character
  mutate(species_id = as.character(species_id),
         ## Only keep raw biomass values
         biomass_mg = ifelse(provenance %in% c("length.raw", "category.raw"),
                             biomass_mg, NA))

# Get taxonomic and trait data
fw_database <- 
  fw_data("0.7.7")
trait_data <- 
  fw_database$traits

# Check and fix entry errors in taxonomy ------------------------------------------------------------
# Domain
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$domain[!is.na(trait_data$domain)]),
                   unique(trait_data$domain[!is.na(trait_data$domain)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$domain[!is.na(trait_data$domain)])
names(dist.matrix) <- 
  rep(unique(trait_data$domain[!is.na(trait_data$domain)]),
      length(unique(trait_data$domain[!is.na(trait_data$domain)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$domain[which(trait_data$domain %in% c("Eukaryota", "Insecta", "Animalia"))] <- 
  "Eukarya"

# Kingdom
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$kingdom[!is.na(trait_data$kingdom)]),
                   unique(trait_data$kingdom[!is.na(trait_data$kingdom)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$kingdom[!is.na(trait_data$kingdom)])
names(dist.matrix) <- 
  rep(unique(trait_data$kingdom[!is.na(trait_data$kingdom)]),
      length(unique(trait_data$kingdom[!is.na(trait_data$kingdom)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
#### No problems here

# Phylum
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$phylum[!is.na(trait_data$phylum)]),
                   unique(trait_data$phylum[!is.na(trait_data$phylum)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$phylum[!is.na(trait_data$phylum)])
names(dist.matrix) <- 
  rep(unique(trait_data$phylum[!is.na(trait_data$phylum)]),
      length(unique(trait_data$phylum[!is.na(trait_data$phylum)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
#### No problem here

# Subphylum
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$subphylum[!is.na(trait_data$subphylum)]),
                   unique(trait_data$subphylum[!is.na(trait_data$subphylum)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$subphylum[!is.na(trait_data$subphylum)])
names(dist.matrix) <- 
  rep(unique(trait_data$subphylum[!is.na(trait_data$subphylum)]),
      length(unique(trait_data$subphylum[!is.na(trait_data$subphylum)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$subphylum[which(trait_data$subphylum == "")] <- 
  NA

# Class
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$class[!is.na(trait_data$class)]),
                   unique(trait_data$class[!is.na(trait_data$class)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$class[!is.na(trait_data$class)])
names(dist.matrix) <- 
  rep(unique(trait_data$class[!is.na(trait_data$class)]),
      length(unique(trait_data$class[!is.na(trait_data$class)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
#### No problem here

# Subclass
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$subclass[!is.na(trait_data$subclass)]),
                   unique(trait_data$subclass[!is.na(trait_data$subclass)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$subclass[!is.na(trait_data$subclass)])
names(dist.matrix) <- 
  rep(unique(trait_data$subclass[!is.na(trait_data$subclass)]),
      length(unique(trait_data$subclass[!is.na(trait_data$subclass)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$subclass[which(trait_data$subclass == "Paleoptera")] <- 
  "Palaeoptera"
trait_data$subclass[which(trait_data$subclass == "")] <- 
  NA

# Order
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$ord[!is.na(trait_data$ord)]),
                   unique(trait_data$ord[!is.na(trait_data$ord)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$ord[!is.na(trait_data$ord)])
names(dist.matrix) <- 
  rep(unique(trait_data$ord[!is.na(trait_data$ord)]),
      length(unique(trait_data$ord[!is.na(trait_data$ord)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$ord[which(trait_data$ord == "Opisthopora<U+FFFD>")] <- 
  "Opisthopora"
trait_data$ord[which(trait_data$ord == "")] <- 
  NA

# Suborder
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$subord[!is.na(trait_data$subord)]),
                   unique(trait_data$subord[!is.na(trait_data$subord)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$subord[!is.na(trait_data$subord)])
names(dist.matrix) <- 
  rep(unique(trait_data$subord[!is.na(trait_data$subord)]),
      length(unique(trait_data$subord[!is.na(trait_data$subord)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$subord[which(trait_data$subord == "")] <- 
  NA
trait_data$subord[which(trait_data$subord == "Zigoptera")] <- 
  "Zygoptera"

# Family
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$family[!is.na(trait_data$family)]),
                   unique(trait_data$family[!is.na(trait_data$family)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$family[!is.na(trait_data$family)])
names(dist.matrix) <- 
  rep(unique(trait_data$family[!is.na(trait_data$family)]),
      length(unique(trait_data$family[!is.na(trait_data$family)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$family[which(trait_data$family == "Vellidae")] <- 
  "Veliidae"
trait_data$family[which(trait_data$family == "Daphnidae")] <- 
  "Daphniidae"
trait_data$family[which(trait_data$family == "")] <- 
  NA

# Subfamily
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$subfamily[!is.na(trait_data$subfamily)]),
                   unique(trait_data$subfamily[!is.na(trait_data$subfamily)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$subfamily[!is.na(trait_data$subfamily)])
names(dist.matrix) <- 
  rep(unique(trait_data$subfamily[!is.na(trait_data$subfamily)]),
      length(unique(trait_data$subfamily[!is.na(trait_data$subfamily)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data$subfamily[which(trait_data$subfamily == "Sphaerodinae")] <- 
  "Sphaeridiinae"

# Tribe
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$tribe[!is.na(trait_data$tribe)]),
                   unique(trait_data$tribe[!is.na(trait_data$tribe)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$tribe[!is.na(trait_data$tribe)])
names(dist.matrix) <- 
  rep(unique(trait_data$tribe[!is.na(trait_data$tribe)]),
      length(unique(trait_data$tribe[!is.na(trait_data$tribe)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
#### No problem here

# Genus
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$genus[!is.na(trait_data$genus)]),
                   unique(trait_data$genus[!is.na(trait_data$genus)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$genus[!is.na(trait_data$genus)])
names(dist.matrix) <- 
  rep(unique(trait_data$genus[!is.na(trait_data$genus)]),
      length(unique(trait_data$genus[!is.na(trait_data$genus)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
#### No problem here

# Species
## Distance matrix of strings
dist.matrix <- 
  stringdistmatrix(unique(trait_data$species[!is.na(trait_data$species)]),
                   unique(trait_data$species[!is.na(trait_data$species)]),
                   method = 'jw', p = 0.1)
## Give their actual names back to elements of the vector
## (converted to numbers through the function)
row.names(dist.matrix) <- 
  unique(trait_data$species[!is.na(trait_data$species)])
names(dist.matrix) <- 
  rep(unique(trait_data$species[!is.na(trait_data$species)]),
      length(unique(trait_data$species[!is.na(trait_data$species)])))
## Reconvert to distance matrix
dist.matrix <- 
  as.dist(dist.matrix)
## Convert distance matrix to clusters for plotting
clusters <- 
  hclust(dist.matrix, method = "ward.D2")
## Plot
plot(clusters)
## Now fix
trait_data <- 
  trait_data %>% 
  mutate(species = str_replace_all(species, "[()]", ""),
         species = str_replace_all(species, "Microculex ", ""),
         species = str_replace_all(species, "Aulophorus ", ""),
         species = str_replace_all(species, "Phoniomyia ", ""))



# Combine data for functions ----------------------------------------------
# Combine allometry and trait data
equation_table <- 
  trait_data %>% 
  ## Get taxonomic and trait information
  dplyr::select(species_id, bwg_name:species) %>% 
  ## Add equations
  left_join(fw_biomass %>% 
              ## remove columns not important (I hope) for this specific table
              dplyr::select(-measurement_id, -stage:-length_est_mm,
                            -biomass_mg:-num_relatives)) %>% 
  filter(!is.na(intercept)) %>% 
  ## If shared taxon is NA, it means the equation is from the species itself
  mutate(shared_taxon = ifelse(is.na(shared_taxon),
                               "species", shared_taxon),
         ln_intercept = NA) %>% 
  unique()

## Empty columns depending on level of equation
### Species
equation_table$bwg_name[which(equation_table$shared_taxon != "species")] <- 
  NA
equation_table$species_id[which(equation_table$shared_taxon != "species")] <- 
  NA
equation_table$species[which(equation_table$shared_taxon != "species")] <- 
  NA
### Genus
equation_table$genus[which(equation_table$shared_taxon %notin% 
                             c("species", "genus"))] <- 
  NA
### Subfamily
equation_table$subfamily[which(equation_table$shared_taxon %notin% 
                                 c("species", "genus", "subfamily"))] <- 
  NA
### Family
equation_table$family[which(equation_table$shared_taxon %notin% 
                                 c("species", "genus", "subfamily", 
                                   "family"))] <- 
  NA

### Suborder
equation_table$subord[which(equation_table$shared_taxon %notin% 
                              c("species", "genus", "subfamily", 
                                "family", "subord"))] <- 
  NA
### Order
equation_table$subord[which(equation_table$shared_taxon %notin% 
                              c("species", "genus", "subfamily", 
                                "family", "subord", "ord"))] <- 
  NA

## Final cleanup of the table
equation_table <- 
  equation_table %>% 
  unique() %>% 
  ### Remove one equation at kingdom level and remove column
  filter(shared_taxon != "kingdom") %>% 
  dplyr::select(-shared_taxon)
#### Still need to keep only unique row for each equation

# Combine abundance, weight and trait data
measurement_table <- 
  trait_data  %>% 
  ## keep only taxonomic information
  dplyr::select(species_id, bwg_name:species,
                functional_group, 
                BS1:BS5, 
                LO1:LO7,
                MD1:MD8,
                BF1:BF4) %>% 
  ## First add only raw measurements and biomass for species
  left_join(fw_biomass %>% 
              ## remove columns not important (I hope) for this specific table
              dplyr::select(-measurement_id, -length_est_mm, -length_measured_as, 
                            -biomass_ci_upr, -biomass_ci_lwr, -provenance_species:-slope)) %>% 
mutate(abundance = ifelse(is.na(length_mm), 0, 1))
  
# Extra data  ----------------------------------------------------
# From literature
## Equations
extra_equations <- 
  read.csv("raw_data/extra_equations.csv",
           stringsAsFactors = F)
# Bind to allometry table
equation_table <- 
  equation_table %>% 
  bind_rows(extra_equations)

## Measurements
extra_measurements <- 
  read.csv("raw_data/extra_measurements.csv",
           stringsAsFactors = F)
colnames(extra_measurements) <- 
  colnames(measurement_table)
# Bind to allometry table
measurement_table <- 
  measurement_table %>% 
  bind_rows(extra_measurements)

# Try function against dataset --------------------------------------------
# Load data
pitilla <- 
  read.csv("raw_data/Pitilla2002_clean.csv") %>% 
  mutate(wet_per_cap_biomass_mg = wet_per_cap_biomass_g * 1000,
         dry_per_cap_biomass_mg = dry_per_cap_biomass_g * 1000,
         abundance = 1) %>% 
  dplyr::select(-wet_per_cap_biomass_g, -dry_per_cap_biomass_g) %>% 
  rename(bwg_name = nickname,
         size_mm = size..mm.)

# Update measurement table with database
measurement_updated <- 
  measurement_supplement(measurement_table,
                         data_table)

# Use functions
pitilla_trial <- 
  hello_metry(equation_table,
              measurement_updated, 
              pitilla, 
              print = TRUE)
