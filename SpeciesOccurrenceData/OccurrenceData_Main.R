# Citation/Authorship Metadata ----------------------------------------------------------------

# Title: Species Occurrence Script
# Author: Peder Engelstad
# Affiliation: Colorado State University
# Contact: pengel@colostate.edu
# Origin Date: 3/7/2018
# Last Edited: 5/7/2018
# Purpose: Download and combine species occurrence data, perform QA/QC, and export to CSV

list.of.packages <- c("devtools","gsheet",'httr',"jsonlite","rgdal","rgeos","ritis","scrubr","stringr","taxize","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# if you don't already have the latest version of spocc from github, uncomment the line below and run
# devtools::install_github("ropensci/spocc")

setwd('C:/Users/peder/Documents/USGS/Scripts/SpeciesOccurrenceData/')

# load in source functions
source('./SpeciesProcessing.R')
source('./API_Sources.R')
source('./DataCleaning.R')

# load('./USGS/Data/species_data.Rdata.RData')

# user inputs go here
species_names = readLines('USGS/ShinyApps/FWS_Viz/fws_specieslist.txt')
species_names

startdate = '1980-01-01'
enddate = as.Date(Sys.Date())
api_sources = c('gbif','bison','eddmaps')          #options: gbif, bison, inat, ecoengine, eddmaps

# Begin processing species names, generating synonym lists, and USDA codes (usda=F to turn off)
species_processing(species_names, USDA=F)  # generates sp_df object
sp_df
species_search_list

# Query data available from API and loads into df_list the resulting data frame.
df_list <- list()
bison_options = list(params=c('basisOfRecord: specimen, observation'))
gbif_options = list()

# reminder: limit is the number of results PER SPECIES
# the only source with a rate limit is EDDMapS (3000)
api_data(species_list = c('Bromus tectorum','Lepidium draba')
         , sources = api_sources
         , limit = 20
         , gbifopts = gbif_options
         , bisonopts = bison_options
         , startDate = startdate
         , endDate = enddate
         , US_only = T
         )

#NISIMS integration coming soon!

#Merge sources and clean up data. It's not uncommon to see your results halved from de-duplication.
Data_QAQC(df_list)

#view a quick summary of the number of records by desired groupings
occ_all %>%
  select(ITIS_AcceptedName) %>%
  group_by(ITIS_AcceptedName) %>%
  summarize(count = n())

occ_all %>%
  select(DataSet, ITIS_AcceptedName) %>%
  group_by(DataSet,ITIS_AcceptedName) %>%
  summarize(count = n())

occ_all %>%
  select(searched_term) %>%
  group_by(searched_term) %>%
  summarize(count = n())

occ_all %>%
  select(source_sp_name) %>%
  group_by(source_sp_name) %>%
  summarize(count = n())

write.csv(occ_all, './USGS/ShinyApps/FWS_Viz/FWS_FullMetadataOutput.csv')

