# Citation/Authorship Metadata ----------------------------------------------------------------

# Title: Species Occurrence Script
# Author: Peder Engelstad
# Affiliation: Colorado State University
# Contact: pengel@colostate.edu
# Origin Date: 3/7/2018
# Last Edited: 04/04/2020
# Purpose: Download and combine species occurrence data, perform QA/QC, and export to CSV
# Script Purpose: Pull occurrence records from API and hard-coded data sources
#
# Authors: Peder Engelstad, adapted from work by Helen Sofaer. 
#          Also, a huge thank you to Scott Chamberlin at rOpenSci for developing packages
#          like 'spocc' and for being so responsive/helpful!
#
# Contact: peder.engelstad@colostate.edu


################################
list.of.packages <- c("devtools","gsheet","jsonlite","rgdal","rgeos","ritis","scrubr",'spocc',
                      "stringr","taxize","tidyverse","sqldf", "lubridate","Hmisc", "sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse, verbose = F, quietly = T)
source('./SpeciesProcessing.R')


# sp_list = suppressWarnings(readLines("path/to/file.csv or file.txt"))
sp_list = c('Bromus tectorum', 'Bromus rubens')
species_processing(sort(sp_list),USDA = F)

# This is the master list. It does contain var and ssp but only if they are explicitly defined in ITIS as synonyms
sp_df
species_search_list


###############################################
#3. Pull data from API Sources
###############################################
library(parallel)
source('./API_Sources.R')

api_sources <- c('bison', 'gbif', 'eddmaps')

# For a parallel run you really only need to do this once for EDDMapS
if('eddmaps' %in% api_sources){
  edd.spdf <- suppressWarnings(gsheet2tbl('https://docs.google.com/spreadsheets/d/1OnADbTEmHO4HbQd1mmPchJXz502ONOCjLDTNHxKkdwo'))
} else{
  edd.spdf = NULL
}

startdate <- '1980-01-01'
enddate <- as.Date(Sys.Date())

ncores = detectCores(logical = F)
sp.chunk = split(1:length(species_search_list), sort(1:length(species_search_list)) %% ncores)
sp.chunk = lapply(sp.chunk, function(x) c(species_search_list[x]))

cl = makeCluster(ncores)
clusterEvalQ(cl, lapply(c('spocc','jsonlite','tidyverse','stringr','gsheet','httr'), require, character.only=T))
aa = Sys.time()
api.data.raw = parLapply(cl, 
                     sp.chunk, 
                     fun = api_data
                   , sources = api_sources
                   , limit = 999999
                   , edd.spdf = edd.spdf
                   , startDate = startdate
                   , endDate = enddate
                   , US_only = T)
stopCluster(cl)
bb = Sys.time()
bb-aa

api.df = bind_rows(api.data.raw)


################################################################################################
# Add data from .csv or .txt files
# Requires: sp_df and species_search_list objects!!!
# Note: AIM and LMF records without a date will be given the current date - TO DO: resolve this!
################################################################################################

source('./DataFromFiles.R')

aim_file = 'J:/Projects/NPS/Data/OccDataSources/AIM.allsp.pnts.May2018.csv'
lmf_file = 'J:/Projects/NPS/Data/OccDataSources/LMF.allsp.csv'
nisims_nps_file = 'J:/Projects/NPS/Data/OccDataSources/NISIMS_NPS_L48.csv'
nisims_blm_file = 'J:/Projects/NPS/Data/OccDataSources/NISIMS_BLM_L48.csv'

file.data = AddDataFromFiles(aim_file_loc = aim_file,
                             lmf_file_loc = lmf_file,
                             nisims_nps_file_loc = nisims_nps_file,
                             nisims_blm_file_loc = nisims_blm_file
)

all.data = append(api, file.data)

########################################################################################################
#4. Perform QA/QC on occurrence records
source('./DataCleaning.R')

# 4.1  This function performs several actions to filter the downloaded data.
#      A. It first combines the data frames from the selected sources (occ_merged)
#      B. Then it removes records with dates (somehow) in the future and missing lat/long
#      C. A new column of the officially accepted ITIS species name and genus is appended (occ_all)
#      D. occ_all is then de-duplicated and sorted alphabetically by species name as a final data object

occ_all = Data_QAQC(api.data2)

occ_all %>%
  select(DataSet) %>%
  group_by(DataSet) %>%
  summarise(count = n())

occ_all %>%
  select(ITIS_final, source_sp_name) %>%
  group_by(ITIS_final, source_sp_name) %>%
  summarise(count = n())

write.csv(occ_all, './test.csv')

########################################################################################################
# 5. Quickly view species of interest on a map for QA purposes
library(leaflet)
library(viridis)
library(htmltools)
csv = read.csv('test.csv', header=T, stringsAsFactors = F)
n = length(unique(occ_all$DataSet))
pal = colorFactor(rainbow(n), occ_all$DataSet)

# csv = csv[csv$ITIS_AcceptedName=='Oplismenus undulatifolius',]
spatial_occ <- SpatialPointsDataFrame(data = occ_all, coords = occ_all[c('decimalLatitude','decimalLongitude')],
                                      proj4string = CRS("+init=epsg:4326"))

m <- leaflet(data=spatial_occ) %>%
  addTiles() %>%  # Add default tiles
  addCircleMarkers(lng=~decimalLongitude, lat=~decimalLatitude
                   , fillColor = ~pal(DataSet)
                   , stroke=F, fillOpacity=0.8, radius = 2.8, popup = ~htmlEscape(ObsDate)) %>%
  addLegend("topright", pal = pal, values = ~DataSet, labels = "Species ", title = "Data Source")
m  # Print the map
################################################################################

                       