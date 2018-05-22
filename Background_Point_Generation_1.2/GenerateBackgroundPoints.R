# Script Purpose: 1. Determine all species considered invasive from USDA. 
#                 2. Pull occurrence records from API data sources.
#                 3. Generate background point data based on lat-longs from occurrence records
#
# Authors: Peder Engelstad (adapted from work by Helen Sofaer)
# Contact: pengel@colostate.edu
# Last Updated: 5/21/2018



list.of.packages <- c("devtools","gsheet","jsonlite","rgdal","rgeos","ritis","scrubr",'spocc',"stringr","taxize","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# !!! if you don't already have the latest version of spocc from github, uncomment the line below and run !!!
# devtools::install_github("ropensci/spocc")

library(tidyverse)
setwd('C:/Users/peder/Documents/USGS/Scripts/Background_Point_Generation_1.1')

# 1. Parse entire USDA plants database species list

# read in hard copy of USDA plants database (full)
dat <- read.csv('./USDA_FULL_LIST.txt')
USDAexotic <- dat[grep('L48(I)', dat$Native.Status, fixed=TRUE),] # rolling count: 5100

# drop the 1 word names:
USDAexotic <- USDAexotic[str_count(USDAexotic$Scientific.Name, "\\s") != 0, ] # rolling count 4249

# drop all ssp and var. terms (only want Genus and species)
USDAexotic$Scientific.Name <- word(string = USDAexotic$Scientific.Name, start = 1, end = 2, sep = " ")

## Remove names without the special x character denoting hybrids
USDAexotic <- USDAexotic[!str_detect(string = USDAexotic$Scientific.Name, pattern = 'Ã'),] # rolling count: 4120

# De-duplicate names
USDAexotic = USDAexotic[-which(duplicated(USDAexotic$Scientific.Name)), ] # final count: 3774


################################################################################
#2. Check for synonyms from ITIS and develop a finalized species list

source('./SpeciesProcessing.R')

# Notes:
# 1. 
species_processing(sample(USDAexotic$Scientific.Name, size=50), USDA=F)

################################################################################
#3. Pull data from API Sources
source('./API_Sources.R')

api_sources <- c('gbif','bison','eddmaps')
startdate <- '1980-01-01'
enddate <- as.Date(Sys.Date())

# Query data available from API and loads into df_list the resulting data frame.
df_list <- list()
bison_options = list(params=c('basisOfRecord: specimen, observation'))

# reminder: limit is the number of results PER SPECIES
api_data(species_list = species_search_list
         , sources = api_sources
         , limit = 10
         , bisonopts = bison_options          
         , startDate = startdate
         , endDate = enddate
)

################################################################################
#4. Perform QA/QC on occurrence records
source('./DataCleaning.R')

Data_QAQC(df_list)

write.csv(occ_all, './BackgroundPointsPresence_Output.csv')
################################################################################

