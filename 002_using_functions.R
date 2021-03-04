# Using the functions
library(tidyverse)
library(ggplot2)
library(gridExtra)
source("R_code/functions.R")

# Enter data
## Allometry table, where all the allometric information is stored
equation_table <- 
  read.csv("data/equation_table.csv",
           stringsAsFactors = F)
measurement_table <- 
  read.csv("data/measurement_table.csv",
           stringsAsFactors = F)
## Data table, your data frame
### Make sure abundance column named 'abundance'
### Make sure column with BWG species names named 'bwg_name'
### Make sure column with measurements names named 'size'
data_table <- 
  read.csv("data/database_data.csv",
           stringsAsFactors = F) %>% 
  ### Change those gigantic Monopelopia
  mutate(size_mm = ifelse(bwg_name == "Diptera.175" & size_mm == "35",
         "unknown", size_mm)) %>% 
  ### Remove Collembola
  filter(bwg_name %notin% c("Collembola.1", "Collembola.2", "Collembola.3"))

# Get biomass estimation for entire data frame
biomass_data <- 
  hello_metry(equation_table, 
              measurement_table,
              data_table, 
              print = TRUE,
              biomass_kind = "dry")
## Save data
write.csv(biomass_data, "data/database_biomass_estimates.csv", row.names = F)

# Filter resulting database to assess those who haven't been assigned biomass so far
## Get list of species
leftover <- 
  biomass_data %>% 
  filter(biomass_mg %in% c("cannot_estimate", "notindatabase")) %>% 
  dplyr::select(bwg_name, path)
## View it
View(leftover)
  
# Row by row use of functions -----------------------------------
# Enter i as row number
i <- 
  1253

# Extract row information
row <- data_table[i,]
specname <- row$bwg_name
size_mm <- row$size_mm
level <- "subord"
abundance <- row$abundance
path <- ""

# Extract species information
taxo <- 
  measurement_table %>% 
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
              sizest(specname, size_mm, level, stage, path, taxo, 
                     equation_table, measurement_table, data_table))))
}

# Run biomass estimation through list of levels
for(level in level_list){
  path <- ""
  print(paste(c(level, 
                get_biomass(specname, level, size_mm, abundance, stage, path, taxo, 
                                         equation_table, measurement_table, biomass_kind = "dry"))))
}


# Run trait matching
matcher_of_traits(specname, measurement_table)


# Get biomass estimation for single row
biomass_row <- 
  hello_metry(equation_table, 
              measurement_table, 
              row, 
              print = TRUE,
              biomass_kind = "dry")

# Some data checking ------------------------------------------------------

# Combine measurements
updated_meas <- 
  measurement_supplement(measurement_table, data_table)

#Make histograms for families
family_list <- 
  unique(updated_meas$family)
family_list <- 
  family_list[which(family_list != "")]

pdf("family_hist.pdf")
par(mfrow=c(4,2))
for(i in family_list){
  dats <- 
    updated_meas %>% 
    filter(family == i,
           !is.na(size_mm))
  tryCatch(
    expr = {hist(dats$size_mm,
                 nclass = 50,
                 main = paste0(i))
            hist(log(dats$size_mm + 1),
                 nclass = 50,
                 main = paste0(i))
    },
    error = function(e){ 
      ### If error (species unconnected), just make a blank plot with site name
      plot.new()
      title(paste0(i))
      plot.new()
      title(paste0(i))
    })
  
  
  
}
dev.off()
par(mfrow=c(1,1))

#Make scatterplots for families
family_list <- 
  unique(updated_meas$family)
family_list <- 
  family_list[which(family_list != "")]

pdf("family_scat.pdf")
par(mfrow=c(4,2))
for(i in family_list){
  dats <- 
    updated_meas %>% 
    filter(family == i,
           !is.na(size_mm),
           !is.na(biomass_mg))
  tryCatch(
    expr = {plot(dats$size_mm ~
                 dats$biomass_mg,
                 main = paste0(i))

    },
    error = function(e){ 
      ### If error (species unconnected), just make a blank plot with site name
      plot.new()
      title(paste0(i))
    })
  
  
  
}
dev.off()
par(mfrow=c(1,1))

#Make scatterplots for families with estimated biomass
family_list <- 
  unique(biomass_data$family)
family_list <- 
  family_list[which(family_list != "")]

pdf("family_estimated_scat.pdf")
par(mfrow=c(4,2))
for(i in family_list){
  dats <- 
    biomass_data %>% 
    mutate(size_original = as.numeric(size_original),
           biomass_mg = as.numeric(biomass_mg)) %>% 
    filter(family == i,
           abundance > 0,
           !is.na(size_original),
           !is.na(biomass_mg)) %>% 
    mutate(type = ifelse(str_detect(path, "wet", negate = FALSE),
                         "wet", "dry"),
           biomass_per_cap = biomass_mg/abundance)
  
  tryCatch(
    expr = {plot(dats$biomass_per_cap ~
                   dats$size_original,
                 pch = ifelse(dats$type == "dry", 16, 17),
                 ylab = "per cap. biomass (mg)",
                 xlab = "length (mm)",
                 main = paste0(i))
      
    },
    error = function(e){ 
      ### If error (species unconnected), just make a blank plot with site name
      plot.new()
      title(paste0(i))
    })
  
  
  
}
dev.off()
par(mfrow=c(1,1))
