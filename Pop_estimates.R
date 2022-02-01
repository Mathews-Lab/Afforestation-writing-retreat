

### Code used to estimate species population size from habitat sizes & density estimates ###
### Adapted from 2018 mammal review by Bronwen Hunter (27/01/22) ###

# Library dependencies

#install.packages('tidyverse')
#install.packages('rvest')

library(dyplr)
library(tidyverse)
library(boot)
library(gridExtra)
library(data.table)
library(ggplot2)
library(rvest)



##### SECTION FOR FUNCTIONS USED IN DATA WRANGLING & ANALYSIS ####

# This is where the population density esimtates are stored in github
url <- 'https://github.com/Mathews-Lab/Afforestation-writing-retreat/tree/main/density-estimates/'

github_files <-function(folder_url){
  listoffiles <-folder_url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@role="rowheader"]') %>%
    html_nodes('span a') %>%
    html_attr('href') %>%
    sub('blob/', '', .) %>%
    paste0('https://raw.github.com', .)
  return(listoffiles)
  }

# Convert units it Km^2

convert_units <- function(Ha){
  km2 <- Ha*0.01
  return (km2)
}


# Function to calculate population estimates
population_esimate <- function(area_df, density_df, species_name){
  results <- density_df[1:7]
  results$ProjectedArea <- replicate(length(results$LCM_Broad), 0)
  area_subset <- area_df %>% filter(Species == species_name)
  for(habitat in results$LCM_Broad ){
    for(name in colnames(area_subset)){
      if(grepl(habitat, name) ==TRUE){
        print(name)
        results[match(habitat,results$LCM_Broad), 'ProjectedArea'] <- area_subset[1, match(name, colnames(area_subset))]
      }
    }
  }
  results$Estimate <- results$Estimate %>% as.numeric()
  results$ProjectedArea <- results$ProjectedArea %>% as.numeric
  results$ProjectedPop <- results$Estimate * results$ProjectedArea
  results$HabitatAreaChange <- results$ProjectedArea - as.numeric(results$AreaTotal)
  results$PopChange <- results$Estimate * as.numeric(results$AreaTotal)
  return(results)
  }
  

# Applying this function to all species and returning new dfs - Wales
directory <- '/Users/bronwenhunter/desktop/'


produce_all_results <- function(country,
                                area_df,
                                listoffiles,
                                names_df,
                                directory){
  area_df[4:29] <- lapply(area_df[4:29], convert_units)
  for(item in names(listoffiles)){
    df <- listoffiles[[item]]
    name <- names_df$scientific[match(item, names_df$common)]
    results <-population_esimate(area_df, df, name)
    varName <- paste(item, "PopEstimate", country, sep="_")
    assign(varName, results, envir = parent.frame())
    write.csv(results, paste(directory, varName, ".csv"))
    print(paste("Calculated population estimates for ", item))
  }
}
 

##### RUN THE ANALYSIS HERE ####

# First define the directory where you want the results to be saved
directory <- "/Users/bronwenhunter/Desktop/"

## First we need to produce a list of density estimates files & read them in: 
density <- "https://github.com/Mathews-Lab/Afforestation-writing-retreat/tree/main/density-estimates"
root <- "https://raw.github.com/Mathews-Lab/Afforestation-writing-retreat/main/density-estimates/"
DensityFiles <- list()
for(file in github_files(density)){
  name <- (str_match(file, paste(root, '\\s*(.*?)\\s*_', sep='')))[1,2] # Saving each df based on its name
  df <- read.csv(file)
  assign(name, df)
  DensityFiles[[name]] <- df # Adding these to a named list
  print(paste("Read in file ", name))
}


# Now the same with the planting simulations for England, Scotland & Wales
planting_simulations <- "https://github.com/Mathews-Lab/Afforestation-writing-retreat/tree/main/planting-simulation"
root <- 'https://raw.github.com/Mathews-Lab/Afforestation-writing-retreat/main/planting-simulation/'
AreaFiles <- list()
for(file in github_files(planting_simulations)){
  name <- (str_match(file, paste(root, 'Final_\\s*(.*?)\\_', sep='')))[1,2] # Saving each df based on its name
  df <- read.csv(file)
  assign(name, df)
  AreaFiles[[name]] <- df # Adding these to a named list
  print(paste("Read in file ", name))
}

# Reading in the file used to match species names
species_names <- read.csv('https://raw.githubusercontent.com/Mathews-Lab/Afforestation-writing-retreat/main/species%20names.csv')

# Now produce the results separately for England, Scotland & Wales
produce_all_results('Wales', AreaFiles$Wales, DensityFiles[2:4], species_names, directory)
