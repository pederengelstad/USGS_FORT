# Script Purpose: 1. Determine all species considered invasive from USDA. 
#                 2. Pull occurrence records from API data sources.
#                 3. Generate background point data based on lat-longs from occurrence records
#
# Authors: Peder Engelstad (adapted from work by Helen Sofaer)
# Contact: pengel@colostate.edu
# Last Updated: 5/7/2018


# 1. Parse entire USDA plants database species list

library(tidyverse)

# read in hard copy of USDA plants database (full)
dat <- read.csv('C:/Users/peder/Documents/USGS/Data/Tabular/USDA_FULL_LIST.txt')
USDAexotic <- dat[grep('L48(I)', dat$Native.Status, fixed=TRUE),] # rolling count: 5100

# drop the 1 word names:
USDAexotic <- USDAexotic[str_count(USDAexotic$Scientific.Name, "\\s") != 0, ] # rolling count 4249

# drop all ssp and var. terms (only want Genus and species)
USDAexotic$Scientific.Name <- word(string = USDAexotic$Scientific.Name, start = 1, end = 2, sep = " ")

## Remove names without the special x character denoting hybrids
USDAexotic <- USDAexotic[!str_detect(string = USDAexotic$Scientific.Name, pattern = 'Ã'),] # rolling count: 4120

# De-duplicate names
USDAexotic = USDAexotic[-which(duplicated(USDAexotic$Scientific.Name)), ] # count: 3774

# final review of USDA species list (manual for glaring errors)
write.csv(USDAexotic_test, "C:/Users/peder/Documents/USGS/test_specieslist.csv") # none found



################################################################################

#2. Check for synonyms from ITIS and develop a finalized species list

source('./USGS/Scripts/R/Required/SpeciesProcessing.R')

species_processing(head(USDAexotic$Scientific.Name, n=50))
species_search_list

################################################################################

# Pull data from API Sources
source('./USGS/Scripts/R/Required/API_Sources.R')

df_list = list()
startdate <- '1980-01-01'
enddate <- as.Date(Sys.Date())

api_data(species_list = species_search_list
         ,sources= c('gbif','bison','eddmaps')
         ,limit = 1000
         ,startDate = startdate
         ,endDate = enddate
)

source('./USGS/Scripts/R/Required/DataCleaning.R')
Data_QAQC(df_list)

write.csv(occ_all, './USGS/BackgroundPointsPresence_test.csv')
