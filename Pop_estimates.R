
### Code used to estimate species population size from habitat sizes & density estimates ###
### Adapted from 2018 mammal review by Bronwen Hunter (27/01/22) ###

# Library dependencies

#install.packages('tidyverse')
#install.packages('rvest')

library(dplyr)
library(tidyverse)
library(boot)
library(gridExtra)
library(data.table)
library(ggplot2)
library(rvest)


##### SECTION FOR FUNCTIONS USED IN DATA WRANGLING & ANALYSIS ####

# This is where the population density esimtates are stored in github 
# (not required if working from cloned repo)
#url <- 'https://github.com/Mathews-Lab/Afforestation-writing-retreat/tree/main/density-estimates/'

#github_files <-function(folder_url){
  #listoffiles <-folder_url %>%
    #read_html() %>%
    #html_nodes(xpath = '//*[@role="rowheader"]') %>%
    #html_nodes('span a') %>%
    #html_attr('href') %>%
   #sub('blob/', '', .) %>%
    #paste0('https://raw.github.com', .)
  #return(listoffiles)
  #}

# Convert units it Km^2

convert_units_density <- function(df, species_name){
  if(species_name %in% c("Apodemus flavicollis",
                         "Apodemus sylvaticus",
                         "Glis glis",
                         "Microtus agrestis",
                         "Muscardinus avellanarius",
                         "Myodes glareolus",
                         "Sciurus carolinensis",
                         "Sciurus vulgaris",
                         "Sorex araneus",
                         "Sorex minutus")){
    df$Estimate<- df$Estimate *100
    df$LowerCI<- df$LowerCI *100
    df$UpperCI <- df$UpperCI *100
    print(df)
  }
  return(df)
}


# Function to reassign habitat type names

habitat_conversion_function <- function(area_subset, species_name, habitat_conversion){
  area_subset <- merge(area_subset, habitat_conversion, by = 'LCM_Broad')
  if(species_name == 'Vulpes vulpes'){
    area_subset$LCM_Broad <- area_subset$very_broad
  } else if(species_name == 'Mustela erminea'){
    area_subset$LCM_Broad <- 'All'
  } else {
    area_subset$LCM_Broad <- area_subset$density_name
  }
  area_subset2 <- aggregate(x=area_subset$Projected_Area, by = list(area_subset$LCM_Broad), FUN=sum)
  return(area_subset2)
}

# Function to calculate population estimates

resolve_habitat_area <- function(df){
  for (i in seq_along(nrow(df))){
    if (df$LCM_Broad[i] %in% c("HedgerowsAES", "HedgerowsNonAES")){
      df$Projected_Area[i] <- df$AreaTotal
      } 
  }
  return(df)
}


# Function to calculate population estimates
population_estimate <- function(area_df, 
                               density_df,
                               species_name,
                               habitat_key,
                               names_df){
  area_subset <- area_df %>% filter(Species == species_name)
  area_subset <- area_subset[, !(colnames(area_subset) %in% c("X", "X.1"))]
  area_subset <- stack(area_subset[9:length(area_subset)-1])
  names(area_subset) <- c("Projected_Area", "LCM_Broad")
  colnames(density_df)[colnames(density_df) == "UrbanRural"] ="LCM_Broad"
  density_df <- convert_units_density(density_df, species_name)
  area_subset2 <- habitat_conversion_function(area_subset, species_name, habitat_key)
  names(area_subset2) <- c("LCM_Broad", "Projected_Area")
  results <- merge(density_df, area_subset2, by = 'LCM_Broad')
  results <- resolve_habitat_area(results) 
  results$ProjectedPop <- results$Estimate * results$Projected_Area
  results$ProjectedPop_LowerCI <- results$LowerCI * results$Projected_Area
  results$ProjectedPop_UpperCI <- results$UpperCI * results$Projected_Area
  return(results)
}

  
# Function to calculate total population change from the estimates

calculate_change <- function(estimate_df, species_name){
  estimate_sums <- estimate_df %>% select_if(is.numeric) %>% map_dbl(sum)
  estimate_sums$Species <- species_name
  return(estimate_sums)
}

# Function to resolved the names in the LCM 2007 vs 2020 maps


# Applying this function to all species and returning new dfs - Wales

produce_all_results <- function(country,
                                area_df,
                                listoffiles,
                                names_df,
                                habitat_names,
                                directory){
  summaryfiles <- list()
  for(item in names(listoffiles)){
    df <- listoffiles[[item]]
    taxa <- str_split(item, '_')[[1]][1]
    name <- names_df$scientific[match(taxa, names_df$common)]
    if(name %in% area_df$Species){
    print(paste("Calculating population estimates for ", name))
    results <-population_estimate(area_df, df, name, habitat_names, names_df)
    if(names_df$density[match(name, species_names$scientific)] == 'Km'){
      df$Density_scale <- "Per_Ha"
      } else {
      df$Density_scale <- "Per_Km^2"
      }
    varName <- paste(item, "PopEstimate", country, sep="_")
    assign(varName, results, envir = parent.frame())
    write.csv(results, paste(directory, varName, ".csv", sep=""))
    summary <- calculate_change(results, name)
    summaryfiles[[name]] <- summary}
  }
  summary_all <- bind_rows(unname(summaryfiles))
  return(summary_all)
}
 
##### RUN THE ANALYSIS HERE ####


## First we need to produce a list of density estimates files & read them in: 

DensityFiles <- list()

for(file in list.files('density-estimates')){
  print(file)
  name <- gsub('.{4}$', '', file) # Saving each df based on its name
  df <- read.csv(paste('density-estimates/', file, sep=""))
  assign(name, df)
  DensityFiles[[name]] <- df # Adding these to a named list
  print(paste("Read in file ", name))
}

# Now the same with the planting simulations for England, Scotland & Wales

AreaFiles <- list()
for(file in list.files('planting-simulation')){
  name <- (gsub('.{4}$', '', file)) # Saving each df based on its name
  df <- read.csv(paste('planting-simulation/', file, sep=''))
  assign(name, df)
  AreaFiles[[name]] <- df # Adding these to a named list
  print(paste("Read in file ", name))
}

# Reading in the file used to match species names
species_names <- read.csv('species names.csv')
habitat_conversion <- read.csv('LCM_conversion.csv')

produce_all_results('England', Final_BizUs_England, DensityFiles, species_names, habitat_conversion, 'results/')

df <- population_estimate(Final_BizUs_England, CWDeer_Habitat_171107, "Hydropotes inermis", habitat_conversion, species_names)
