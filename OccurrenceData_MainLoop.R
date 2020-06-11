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

# !!! if you don't already have the latest version of spocc and/or taxize from github, uncomment the line below and run !!!
# remotes::install_github("ropensci/spocc")
# remotes::install_github("ropensci/taxize")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# !!! make sure you have the latest version of the source scripts to run the following lines
# download.file(url="https://github.com/pederengelstad/USGS_FORT/archive/master.zip",destfile = 'OccScripts.zip', method = "curl")
# unzip('OccScripts.zip', overwrite = TRUE)

################################################################################
#2. Check for synonyms from ITIS and develop a finalized species list
library(tidyverse, verbose = F, quietly = T)
source('./SpeciesProcessing.R')

# Notes:
# 2.1 This function produces two objects: sp_df and species_search_list. The latter
#     is a vector of all accepted species names and their synonyms. This list extracts 
#     the genus and species only--no subspecies, variants, or hybrids as they may not 
#     be ecologically representative of the species queried.
#
# 2.2 The USDA parameter (TRUE/FALSE) will generate a list of official and 
#     synonym USDA codes that can be passed to data sources that require them.

sp_list = suppressWarnings(readLines("new_background_list_riparian.txt"))
sp_list = unique(word(sp_list,1,2))
sp_list = sort(sp_list)[3:length(sp_list)] #lingering hybrid names up top
species_processing(sort(sp_list),USDA = T)

# This is the master list. It does contain var and ssp but only if they are explicitly defined in ITIS as synonyms
# Removing lingering hybrids
sp_df = sp_df %>%
  filter(stringr::word(ITISacceptedName, start = 2, end = 2) != 'X') %>%
  mutate(synonym = ifelse(stringr::word(synonym, start = 2, end = 2) == 'x', NA, synonym))

write.csv(sp_df, file = './sp_df.csv', row.names = F)

# sp_df = read.csv('sp_df.csv', header = T, stringsAsFactors = F)
sp_df
species_search_list = sort(unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym))))

# Try running species search in chunks of 500.
sp.chunk = split(species_search_list, ceiling(seq_along(species_search_list)/1000))

source('API_Sources.R')
source('DataCleaning.R')

for(x in c(seq(1:length(sp.chunk)))){
  api_sources <- c('bison', 'gbif', 'eddmaps')
  startdate <- '1980-01-01'
  enddate <- as.Date(Sys.Date())
  
  api = api_data(species_list = as.vector(unlist(sp.chunk[x]))
                 , sources = api_sources
                 , limit = 999999
                 , startDate = startdate
                 , endDate = enddate
                 , US_only = T
  )
  
  if(length(api) == 0) next
  results = Data_QAQC(api)
  
  write.csv(results, paste0('targetbackground_api',x,'.csv'), row.names = F)
}

# compile csvs and de-duplicate
csvs = list.files(pattern = 'target')

full.df = plyr::ldply(csvs, read.csv, header=T, stringsAsFactors=F)

full.df2 = full.df %>% unique()

write.csv(full.df2, 'riparian_targetbackground_occdata.csv', row.names = F)
