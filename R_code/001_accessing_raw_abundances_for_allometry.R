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

# Get biomass data --------------------------------------------------------
# Sign in with secret password 
fw_auth()

# What datasets exist so far?
fw_versions(local = FALSE,
            biomass = TRUE)
fw_versions(local = FALSE,
            biomass = FALSE)
# Save data
fw_biomass <- 
  fw_data("v.0.0.1_0.0.1",
          biomass = TRUE) %>%
  ## convert species_id to character
  mutate(species_id = as.character(species_id))
fw_database <- 
  fw_data("0.7.7")

# Combine allometry table with trait data
allometry_table <- 
  fw_database$traits %>% 
  ## keep only taxonomic information
  dplyr::select(species_id, bwg_name:species,
                functional_group, 
                BS1:BS5, 
                LO1:LO7,
                MD1:MD8,
                BF1:BF4) %>% 
  left_join(fw_biomass %>% 
              ## filter only those rows where we have the allometry equation (i.e. not NA)
              # filter(!is.na(intercept)) %>% 
              ## remove columns not important (I hope) for this specific table
              dplyr::select(-measurement_id, -length_est_mm, -length_measured_as, 
                            -biomass_ci_upr, -biomass_ci_lwr)) %>% 
  unique()

# Fix entry errors
allometry_table$family[allometry_table$family == "Daphnidae"] <- 
  "Daphniidae"


# Add extra data
extra <- 
  read.csv("data/extra.csv",
           stringsAsFactors = F)
## Fix column nmes for smooth binding
colnames(extra) <- c(colnames(allometry_table), colnames(extra[54]))

# Bind to allometry table
allometry_table <- 
  extra %>% 
  bind_rows(allometry_table)



