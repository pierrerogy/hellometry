# Using the functions
library(tidyverse)
library(ggplot2)
library(gridExtra)
source("R_code/functions.R")

# Enter data
## Allometry table, where all the allometric information is stored
allometry_table <- 
  read.csv("data/allometry_table.csv",
           stringsAsFactors = F)
## Data table, your data frame
### Make sure abundance column named 'abundance'
### Make sure column with BWG species names named 'bwg_name'
### Make sure column with measurements names named 'size'
data_table <- 
  data_table

# Get biomass estimation for entire data frame
biomass_data <- 
  hello_metry(allometry_table, 
              data_table, 
              print = TRUE)

# Filter resulting database to assess those who haven't been assigned biomass so far
## Get list of species
leftover <- 
  biomass_data %>% 
  filter(biomass %in% c("plz_solve", "notindatabase")) %>% 
  dplyr::select(bwg_name, path) %>% 
  unique() 
## View it
View(leftover)
  
# Row by row testing of functions -----------------------------------
# Enter i as row number
i <- 
  64059

# Extract row information
row <- data_table[i,]
specname <- row$bwg_name
size_mm <- row$size_mm
level <- "family"
abundance <- row$abundance
path <- ""

# Extract species information
taxo <- 
  allometry_table %>% 
  filter(bwg_name == specname) %>% 
  dplyr::select(species_id:genus) %>% 
  unique()

# Initiate list of levels to go through
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

# Run size estimation through list of levels
for(level in level_list){
path <- ""
print(paste(c(level,
              sizest(specname, size_mm, level, path, taxo, allometry_table, data_table))))
}

# Run biomass estimation through list of levels
for(level in level_list){
  path <- ""
  print(paste(c(level, 
                get_allometric_equations(specname, level, size_mm, abundance, path, taxo, allometry_table))))
}



# Run trait matching
matcher_of_traits(specname, allometry_table)


# Get biomass estimation for single row
biomass_row <- 
  hello_metry(allometry_table, row, print = TRUE)


