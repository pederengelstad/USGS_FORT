library(tidyverse)

setwd('E:/Users/engelstad/GitHub/USGS_FORT/SpeciesOccurrenceData/')

# Full USDA Plants List
db = as_tibble(read.csv('usda_full_list.txt', header = T, stringsAsFactors = F))

# Metadata for synonyms is not repeated. So, wrangle the information and join
symb.stat = db %>%
  select(Accepted.Symbol,Native.Status, Common.Name, Growth.Habit) %>%
  filter(Native.Status != '') %>%
  unique()


db.sort = db %>%
  select(-c(Native.Status,Common.Name,Growth.Habit)) %>%
  inner_join(symb.stat, by = c('Accepted.Symbol' = 'Accepted.Symbol')) %>%
  unique() %>%                                                             # count: 85,859
  filter(Native.Status != '') %>%                                          # count: 85,859
  filter(str_count(Scientific.Name, '\\S+') == 2) %>%                      # count: 56,944   (remove one word, var, & ssp)
  filter(str_detect(Native.Status, 'L48\\(I\\)')) %>%                      # count:  6,910   (remove all natives)
  filter(!str_detect(Scientific.Name, '×|X')) %>%                          # count:  6,723   (remove all hybrids)
  select(Scientific.Name, Common.Name, Native.Status, Growth.Habit) %>%
  unique()                                                                 # count:  6,712

species_list = sort(unique(db.sort$Scientific.Name)) # count 6,668

source('./SpeciesProcessing.R')

species_processing(sp_list = species_list[1:200], USDA = T)

#Check sp_df against original db list because sometimes natives get thrown back in from synonym hunting

sp_df.filter = sp_df %>%
  select(ITISacceptedName, synonym_base, usda_codes) %>%
  left_join(db, by=c('ITISacceptedName' = 'Scientific.Name')) %>%
  filter(str_detect(Native.Status, 'L48\\(I\\)') | is.na(Native.Status) | Native.Status == '')

# construct actual search list
species_search = sort(unique(c(sp_df.filter$ITISacceptedName,sp_df.filter$synonym_base))) # Count: 

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
startdate <- '1980-01-01'
enddate <- as.Date(Sys.Date())

aa = Sys.time()
api_data(species_list = species_search[21:50]
         , sources = api_sources
         , limit = 999999
         , startDate = startdate
         , endDate = enddate
         , US_only = T
)
bb = Sys.time()
bb-aa

source('./DataFromFiles.R')

aim_file = 'E:/Users/engelstad/USGS/data/BLM/AIM.allsp.pnts.May2018.csv'
lmf_file = 'E:/Users/engelstad/USGS/data/BLM/LMF.allsp.csv'
nisims_nps_file = 'E:/Users/engelstad/USGS/data/NISIMS/NISIMS_NPS_L48.csv'
nisims_blm_file = 'E:/Users/engelstad/USGS/data/NISIMS/NISIMS_BLM_L48.csv'

AddDataFromFiles(aim_file_loc = aim_file,
                 lmf_file_loc = lmf_file,
                 nisims_nps_file_loc = nisims_nps_file,
                 nisims_blm_file_loc = nisims_blm_file)

source('./DataCleaning.R')

Data_QAQC(df_list)

occ_all %>%
  select(DataSet) %>%
  group_by(DataSet) %>%
  summarize(count = n())
