# Citation/Authorship Metadata ----------------------------------------------------------------

# Title: Species Occurrence Script
# Author: Peder Engelstad
# Affiliation: Colorado State University
# Contact: pengel@colostate.edu
# Origin Date: 3/7/2018
# Last Edited: 7/24/2018
# Purpose: Download and combine species occurrence data, perform QA/QC, and export to CSV
# Script Purpose: 1. Determine all species considered invasive from USDA. 
#                 2. Pull occurrence records from API and hard-coded data sources.
#                 3. Generate background point data based on lat-longs from occurrence records
#
# Authors: Peder Engelstad (adapted from work by Helen Sofaer)
# Contact: pengel@colostate.edu



list.of.packages <- c("devtools","gsheet","jsonlite","rgdal","rgeos","ritis","scrubr",'spocc',"stringr","taxize","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# !!! if you don't already have the latest version of spocc and/or taxize from github, uncomment the line below and run !!!
# devtools::install_github("ropensci/spocc")
# devtools::install_github("ropensci/taxize")

library(tidyverse)
setwd('~/GitHub/USGS_FORT/SpeciesOccurrenceData/')

# !!! make sure you have the latest version of the source scripts to run the following lines
# download.file(url="https://github.com/pederengelstad/USGS_FORT/archive/master.zip",destfile = 'OccScripts.zip', method = "curl")
# unzip('OccScripts.zip', overwrite = TRUE)

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

# sp_list = suppressWarnings(readLines('C:/Users/peder/Documents/USGS/Scripts/ShinyApps/FWS_Viz/fws_specieslist.txt'))
sp_list = c('Tamarix', 'Elaeagnus angustifolia', 'Ulmus pumila')

species_processing(sort(sp_list), USDA=T)
sp_df
sort(species_search_list)

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
         , US_only = F
)

# df_list$spocc %>%
#   filter(DataSet=='gbif') %>%
#   select(searched_term) %>%
#   group_by(searched_term) %>%
#   summarize(count = n())

########################################################################################################
# Add data from .csv or .txt files; choose from 'blm_aim', 'blm_lmf', 'nisims' or a vector of 2+
# Requires: sp_df and species_search_list objects!!!
# Note: AIM and LMF records without a date will be given the current date

source('./DataFromFiles.R')

aim_file = '~/USGS/Data/BLM/AIM.allsp.pnts.May2018.csv'
lmf_file = '~/USGS/Data/BLM/LMF.allsp.csv'
nisims_nps_file = '~/USGS/Data/NISIMS/NISIMS_Presences_05312018.csv'
nisims_blm_file = '~/USGS/Data/NISIMS/WeedInfestationData_Large.csv'

AddDataFromFiles(aim_file_loc = aim_file,
                 lmf_file_loc = lmf_file,
                 nisims_nps_file_loc = nisims_nps_file,
                 nisims_blm_file_loc = nisims_blm_file)
df_list$BLM_AIM

########################################################################################################
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


write.csv(occ_all, 'C:/Users/peder/Documents/USGS/ThreeSpRequest_20180808.csv')

########################################################################################################
# 5. Quickly view species of interest on a map for QA purposes
library(leaflet)
library(viridis)
csv = read.csv("~/USGS/ThreeSpRequest_20180808.csv", header=T, stringsAsFactors = F)
n = length(unique(csv$ITIS_AcceptedName))
pal = colorFactor(rainbow(n), csv$ITIS_AcceptedName)

# csv = csv[csv$ITIS_AcceptedName=='Oplismenus undulatifolius',]
spatial_occ <- SpatialPointsDataFrame(data = csv, coords = csv[c('latitude','longitude')],proj4string = CRS("+init=epsg:4326"))

m <- leaflet(data=spatial_occ) %>%
  addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png") %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng=~longitude, lat=~latitude, fillColor= ~pal(ITIS_AcceptedName), stroke=F, fillOpacity=0.8, radius = 2.8) %>%
  addLegend("topright", pal = pal, values = ~ITIS_AcceptedName, labels = "Species ", title = "Invasive Species")
m  # Print the map
################################################################################