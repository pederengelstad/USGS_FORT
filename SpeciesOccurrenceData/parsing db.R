library(tidyverse)

setwd('C:/Users/pederengelstad/Downloads/Species Occurrence Script + Data-20190506T184628Z-001/Species Occurrence Script + Data/')

# Full USDA Plants List
db = as_tibble(read.csv('usda_full_nofilter.txt', header = T, stringsAsFactors = F))

# Metadata for synonyms is not repeated. So, wrangle the information and join
symb.stat = db %>%
  select(Accepted.Symbol,Native.Status, Common.Name, Growth.Habit) %>%
  filter(Native.Status != '') %>%
  unique()


db = db %>%
  select(-c(Native.Status,Common.Name,Growth.Habit)) %>%
  inner_join(symb.stat, by = c('Accepted.Symbol' = 'Accepted.Symbol')) %>%
  unique() %>%                                                             # count: 78,849
  filter(Native.Status != '') %>%                                          # count: 78,849
  filter(str_count(Scientific.Name, '\\S+') == 2) %>%                      # count: 50,616   (remove one word, var, & ssp)
  filter(str_detect(Native.Status, 'L48\\(I\\)')) %>%                      # count:  6,910   (remove all natives)
  filter(!str_detect(Scientific.Name, '×')) %>%                            # count:  6,739   (remove all hybrids)
  select(Scientific.Name, Common.Name, Native.Status, Growth.Habit) %>%
  unique()                                                                 # count:  6,728

species_list = sort(unique(db$Scientific.Name)) # count 6,684

source('./SpeciesProcessing.R')

species_processing(sp_list = head(species_list, n = 40), USDA = F)
sp_df
species_search_list
