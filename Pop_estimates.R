
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

convert_units <- function(Ha, species, species_names){
  if(species_names$density[match(species, species_names$scientific)] == 'Km'){
    km2 <- Ha*0.01
    return(km2) 
  } else{
    return(Ha)
  }
}

# Function to calculate population estimates
population_estimate <- function(area_df, 
                               density_df,
                               species_name,
                               habitat_conversion,
                               names_df){
  area_subset <- area_df %>% filter(Species == species_name)
  area_subset <- stack(area_subset[4:29]) 
  names(area_subset) <- c("Projected_Area", "LCM_Broad")
  area_subset$Projected_Area <- convert_units(area_subset$Projected_Area, species_name, names_df)
  colnames(density_df)[colnames(density_df) == "UrbanRural"] ="LCM_Broad"
  area_subset <- merge(area_subset, habitat_conversion, by = 'LCM_Broad')
  if(species_name == 'Vulpes vulpes'){
    area_subset$LCM_Broad <- area_subset$very_broad
  } else if(species_name == 'Mustela erminea'){
    area_subset$LCM_Broad <- 'All'
  } else {
    area_subset$LCM_Broad <- area_subset$density_name
  }
  print(nrow(area_subset))
  area_subset2 <- aggregate(x=area_subset$Projected_Area, by = list(area_subset$LCM_Broad), FUN=sum)
  names(area_subset2) <- c("LCM_Broad", "Projected_Area")
  print(area_subset2)
  results1 <- left_join(density_df, area_subset2)
  results1$PriorPop <- results1$AreaTotal* results1$Estimate
  results1$PriorPop_LowerCI <- results1$LowerCI * results1$AreaTotal
  results1$PriorPop_UpperCI <- results1$UpperCI * results1$AreaTotal
  results1$HabitatAreaChange <- results1$Projected_Area - results1$AreaTotal
  results1$ProjectedPop <- results1$Estimate * results1$Projected_Area
  results1$ProjectedPop_LowerCI <- results1$LowerCI * results1$Projected_Area
  results1$ProjectedPop_UpperCI <- results1$UpperCI * results1$Projected_Area
  return(results1)
  }
  
# Function to calculate total population change from the estimates

calculate_change <- function(estimate_df, species_name){
  estimate_df <- estimate_df[11:18]
  estimate_sums <- as.data.frame(t(colSums(estimate_df)))
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
    results <-population_estimate(area_df, df, name, habitat_names)
    if(names_df$density[match(name, species_names$scientific)] == 'Km'){
      df$Density_scale <- "Per_Ha"
      } else {
      df$Density_scale <- "Per_Km^2"
      }
    varName <- paste(item, "PopEstimate", country, sep="_")
    assign(varName, results, envir = parent.frame())
    write.csv(results, paste(directory, varName, ".csv"))
    print(paste("Calculated population estimates for ", name))
    summary <- calculate_change(results, name)
    summaryfiles[[name]] <- summary
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

population_estimate(Final_Wales_Data, Fox_Habitat_171107,
                   'Vulpes vulpes',
                   habitat_conversion,
                   species_names) 

produce_all_results('Wales', Final_Wales_BizUsual, DensityFiles, species_names, habitat_conversion, 'results/')
       