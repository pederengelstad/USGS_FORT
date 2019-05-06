# Citation/Authorship Metadata ----------------------------------------------------------------

# Title: Species Occurrence Script
# Author: Peder Engelstad
# Affiliation: Colorado State University
# Contact: pengel@colostate.edu
# Origin Date: 3/7/2018
# Last Edited: 11/15/2018
# Purpose: Download and combine species occurrence data, perform QA/QC, and export to CSV
# Script Purpose: Pull occurrence records from API and hard-coded data sources
#
# Authors: Peder Engelstad, adapted from work by Helen Sofaer. 
#          Also, a huge thank you to Scott Chamberlin at rOpenSci for developing packages
#          like 'spocc' and for being so responsive/helpful!
#
# Contact: pengel@colostate.edu


################################
list.of.packages <- c("devtools","gsheet","jsonlite","rgdal","rgeos","ritis","scrubr",'spocc',"stringr","taxize","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# !!! if you don't already have the latest version of spocc and/or taxize from github, uncomment the line below and run !!!
# remotes::install_github("ropensci/spocc")
# remotes::install_github("ropensci/taxize")

setwd('E:/Users/engelstad/GitHub/USGS_FORT_2/SpeciesOccurrenceData/')

# !!! make sure you have the latest version of the source scripts to run the following lines
# download.file(url="https://github.com/pederengelstad/USGS_FORT/archive/master.zip",destfile = 'OccScripts.zip', method = "curl")
# unzip('OccScripts.zip', overwrite = TRUE)

################################################################################
#2. Check for synonyms from ITIS and develop a finalized species list
library(tidyverse, verbose = F)
source('./SpeciesProcessing.R')

# Notes:
# 2.1 This function produces two objects: sp_df and species_search_list. The latter
#     is a vector of all accepted species names and their synonyms. This list extracts 
#     the genus and species only--no subspecies, variants, or hybrids as they may not 
#     be ecologically representative of the species queried.
#
# 2.2 The USDA parameter (TRUE/FALSE) will generate a list of official and 
#     synonym USDA codes that can be passed to data sources that require them.

# sp_list = suppressWarnings(readLines(''))
sp_list = c('Cenchrus setaceus')
species_processing(sort(sp_list),USDA = T)
sp_df
species_search_list

################################################################################
#3. Pull data from API Sources
# source('./API_Sources.R')
source('./API_Sources_NoInat.R')

# Query data available from API and loads into df_list the resulting data frame.
# 3.1  api_data Function notes:
#      sources - choose from 'gbif','bison','eddmaps','inat', and/or 'ecoengine'
#      limit - is the number of results PER SPECIES and is not currently passed to EDDMapS
#              which pulls ALL available records with geospatial information
#
# 3.2  df_list must be created outside the function to avoid re-running data sources that fail
# 3.3  visit ... for information on the full range of bison and gbif options

df_list <- list()
api_sources <- c('bison', 'gbif', 'eddmaps')
startdate <- '2018-08-01'
enddate <- as.Date(Sys.Date())

api_data(species_list = species_search_list
         , sources = api_sources
         , limit = 999999
         , startDate = startdate
         , endDate = enddate
         , US_only = F
)

# As needed, review these occ records to see if the number of records is reasonable.
df_list$spocc %>%
  # filter(DataSet=='bison') %>%
  select(searched_term) %>%
  group_by(searched_term) %>%
  summarize(count = n())

########################################################################################################
# Add data from .csv or .txt files; choose from 'blm_aim', 'blm_lmf', 'nisims' or a vector of 2+
# Requires: sp_df and species_search_list objects!!!
# Note: AIM and LMF records without a date will be given the current date

source('./DataFromFiles.R')

aim_file = 'AIM.allsp.pnts.May2018.csv'
lmf_file = 'LMF.allsp.csv'
nisims_nps_file = 'NISIMS_NPS_L48.csv'
nisims_blm_file = 'NISIMS_BLM_L48.csv'

AddDataFromFiles(aim_file_loc = aim_file,
                 lmf_file_loc = lmf_file,
                 nisims_nps_file_loc = nisims_nps_file,
                 nisims_blm_file_loc = nisims_blm_file)

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
  select(DataSet) %>%
  group_by(DataSet) %>%
  summarize(count = n())

write.csv(occ_all, 'E:/Users/engelstad/USGS/OccurrenceData/bishopsGoutweed_testing/test.csv')

########################################################################################################
# 5. Quickly view species of interest on a map for QA purposes
library(leaflet)
library(viridis)
library(htmltools)
csv = read.csv('E:/Users/engelstad/USGS/OccurrenceData/bishopsGoutweed_testing/test.csv', header=T, stringsAsFactors = F)
n = length(unique(occ_all$DataSet))
pal = colorFactor(rainbow(n), occ_all$DataSet)

# csv = csv[csv$ITIS_AcceptedName=='Oplismenus undulatifolius',]
spatial_occ <- SpatialPointsDataFrame(data = occ_all, coords = occ_all[c('latitude','longitude')],proj4string = CRS("+init=epsg:4326"))

m <- leaflet(data=spatial_occ) %>%
  addTiles() %>%  # Add default tiles
  addCircleMarkers(lng=~longitude, lat=~latitude
                   , fillColor = ~pal(DataSet)
                   , stroke=F, fillOpacity=0.8, radius = 2.8, popup = ~htmlEscape(ObsDate)) %>%
  addLegend("topright", pal = pal, values = ~DataSet, labels = "Species ", title = "Data Source")
m  # Print the map
################################################################################

                       