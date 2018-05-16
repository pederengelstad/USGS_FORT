# Citation/Authorship Metadata ----------------------------------------------------------------

# Title: Species Occurrence Script
# Author: Peder Engelstad
# Affiliation: Colorado State University
# Contact: pengel@colostate.edu
# Origin Date: 3/7/2018
# Last Edited: 5/7/2018
# Purpose: Download and combine species occurrence data, perform QA/QC, and export to CSV

list.of.packages <- c("devtools","gsheet","jsonlite","rgdal","rgeos","ritis","scrubr","stringr","taxize","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# if you don't already have the latest version of spocc from github, uncomment the line below and run
devtools::install_github("ropensci/spocc")

setwd('C:/Users/peder/Documents/')

# load in source functions
source('./USGS/Scripts/R/Required/SpeciesProcessing.R')
source('./USGS/Scripts/R/Required/API_Sources.R')
# source('./USGS/Scripts/R/Required/NISIMS_Integration.R')
source('./USGS/Scripts/R/Required/DataCleaning.R')

# load('./USGS/Data/species_data.Rdata.RData')

# user inputs go here
species_names = readLines('USGS/ShinyApps/SpeciesOfInterest/species_list.txt')
# species_names = c('Taeniatherum caput-medusae')
species_names

startdate = '1980-01-01'
enddate = as.Date(Sys.Date())
api_sources = c('gbif','bison','eddmaps')          #options: gbif, bison, inat, ecoengine, eddmaps

# Begin processing species names, generating synonym lists, and USDA codes (usda=F to turn off)
species_processing(species_names, USDA=T)  # generates sp_df object
sp_df
species_search_list

# Query data available from API and loads into df_list the resulting data frame.
df_list <- list()
# inat_options = list(quality='research')
bison_options = list(params=c('basisOfRecord: specimen, observation'))

# reminder: limit is the number of results PER SPECIES
api_data(species_list = species_search_list
         , sources = api_sources
         , limit = 50000
         , inatopts = inat_options
         , bisonopts = bison_options          
         , startDate = startdate
         , endDate = enddate
         )

# Bring in data from hard files (i.e. NISIMS)
LoadNISIMS(type = 'db', filepath = './USGS/Data/NISIMS/USGS_presence_absence/USGS_presence_absence/NISIMS_Presences_toFY16.gdb', layername = 'WeedInfestationData')

Parse_NISIMS_Annual()

LoadNISIMS(type = 'db', filepath = './USGS/Data/NISIMS/USGS_presence_absence/USGS_presence_absence/NISIMS_Presences_toFY16.gdb', layername = 'WeedInfestationData')
Parse_NISIMS_Large()
# unsure if we should be using BEGIN_DT or END_DT for date field



#Merge sources and clean up data. It's not uncommon to see your results halved from de-duplication.
Data_QAQC(df_list)
write.csv(occ_all, './USGS/ShinyApps/FWS_Viz/FullOutput.csv')

occ_all

#Export list to file
write.csv(occ_all, './USGS/Data/output/medusahead_occall.csv')
