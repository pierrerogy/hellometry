# Trying out functions
library(tidyverse)
library(ggplot2)
library(gridExtra)
source("preli/functions.R")

# To do row by row testing of functions
## Enter i as row number
i <- 638
## Enter data
allometry_table <- allometry_table
data_table <- abundance_size %>% 
  filter(!is.na(abundance)) 
## Extract row information
row <- data_table[i,]
specname <- row$bwg_name
size <- row$size
level <- "class"
abundance <- row$abundance
path <- ""
## Extract species information
taxo <- 
  allometry_table %>% 
  filter(bwg_name == specname) %>% 
  dplyr::select(species_id:genus) %>% 
  unique()
## Initiate list of levels to go through
level_list <- 
  c("bwg_name",
    "genus",
    "subfamily",
    "family",
    "traits",
    "subord",
    "ord",
    "subclass",
    "class")


## Run size estimation and biomass estimation through list of levels
for(level in level_list){
path <- ""
print(get_allometric_equations(specname, level, size, abundance, path, taxo, allometry_table))
print(sizest(specname, size, level, path, taxo, allometry_table, data_table))
}
## Run trait matching
matcher_of_traits(specname, allometry_table)


# Doing it on single row
biomass_row <- 
  hello_metry(allometry_table, row, print = TRUE)

# Doing it on an entire dataframe
biomass_data <- 
  hello_metry(allometry_table, data_table, print = TRUE)

write.csv(biomass_data,
          "preli/biomass_data.csv")

# Filter resulting database to assess those who haven't been assigned biomass so far
## Get list of species
leftover <- 
  biomass_data %>% 
  filter(biomass %in% c("plz_solve", "notindatabase")) %>% 
  dplyr::select(bwg_name, path) %>% 
  unique() %>% 
  ### Add their taxonomy
  left_join(fw_database$traits %>% 
              ## keep only taxonomic information
              dplyr::select(c(species_id, bwg_name:species))) 

write.csv(leftover,
          "preli/leftover_species.csv")


