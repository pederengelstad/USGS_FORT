# Citation/Authorship Metadata ----------------------------------------------------------------

# Title: Species Occurrence Script
# Author: Peder Engelstad
# Affiliation: Colorado State University
# Contact: pengel@colostate.edu
# Origin Date: 3/7/2018
# Last Edited: 5/7/2018
# Purpose: Download and combine species occurrence data, perform QA/QC, and export to CSV
# Script Purpose: 1. Determine all species considered invasive from USDA. 
#                 2. Pull occurrence records from API data sources.
#                 3. Generate background point data based on lat-longs from occurrence records
#
# Authors: Peder Engelstad (adapted from work by Helen Sofaer)
# Contact: pengel@colostate.edu
# Last Updated: 5/24/2018



list.of.packages <- c("devtools","gsheet","jsonlite","rgdal","rgeos","ritis","scrubr",'spocc',"stringr","taxize","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# !!! if you don't already have the latest version of spocc from github, uncomment the line below and run !!!
# devtools::install_github("ropensci/spocc")

library(tidyverse)
setwd('~/USGS/Scripts/SpeciesOccurrenceData')

################################################################################
#2. Check for synonyms from ITIS and develop a finalized species list

source('./SpeciesProcessing.R')

# Notes:
# 2.1 This function produces two objects: sp_df and species_search_list. The latter
#     is a vector of all accepted species names and their synonyms. This list extracts 
#     the genus and species ONLY!
#
# 2.2 The USDA parameter (TRUE/FALSE) will generate a list of official and 
#     synonym USDA codes that can be passed to data sources that require them.

sp_list = suppressWarnings(readLines('C:/Users/peder/Documents/USGS/Scripts/ShinyApps/FWS_Viz/fws_specieslist.txt'))

species_processing(sp_list, USDA=F)


################################################################################
#3. Pull data from API Sources
source('./API_Sources.R')

api_sources <- c('gbif','bison','eddmaps')
startdate <- '1980-01-01'
enddate <- as.Date(Sys.Date())

# Query data available from API and loads into df_list the resulting data frame.


# 3.1  api_data Function notes:
#      sources - choose from 'gbif','bison','eddmaps','inat', and/or 'ecoengine'
#      limit - is the number of results PER SPECIES and is not currently passed to EDDMapS
#              which pulls ALL available records with geospatial information
#
# 3.2  df_list must be created outside the function to avoid re-running data sources that fail
# 3.3  visit ... for information on the full range of bison and gbif options

df_list <- list()
bison_options = list(params=c('basisOfRecord: specimen, observation'))

api_data(species_list = species_search_list
         , sources = api_sources
         , limit = 99999
         , bisonopts = bison_options
         , startDate = startdate
         , endDate = enddate
         , US_only = T
)


################################################################################
#4. Perform QA/QC on occurrence records
source('./DataCleaning.R')

# 4.1  This function performs several actions to filter the downloaded data.
#      A. It first combines the data frames from the selected sources (occ_merged)
#      B. Then it removes records with dates (somehow) in the future and missing lat/long (occ_filter)
#      C. A new column of the officially accepted ITIS species name and genus is appended (occ_all)
#      D. occ_all is then de-duplicated and sorted alphabetically by species name as a final data object

Data_QAQC(df_list)

occ_all %>%
  select(ITIS_AcceptedName) %>%
  group_by(ITIS_AcceptedName) %>%
  summarize(count = n())

write.csv(occ_all, 'C:/Users/peder/Documents/USGS/Scripts/ShinyApps/FWS_Viz/FWS_OccurrenceData.csv')
################################################################################
