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

source('./SpeciesProcessing.R')

aa = Sys.time()
species_processing(sp_list = species_list, USDA = T)   # total run time: ~ 1.3 hours
bb = Sys.time()
bb-aa

test = sp_df %>%
  select(ITISacceptedName, synonym_base, usda_codes) %>%
  left_join(db, by=c('ITISacceptedName' = 'Scientific.Name')) %>%
  filter(str_detect(Native.Status, 'L48\\(I\\)'))

test.2 = test %>%
  left_join(db, by=c('synonym_base'='Scientific.Name')) %>%
  rename(ITIS_Status = Native.Status.x, Syn_Status = Native.Status.y)

test.3 = test.2 %>%
  select(ITISacceptedName, synonym_base, usda_codes, ITIS_Status, Syn_Status, Common.Name.x, Growth.Habit.x) %>%
  filter(str_detect(ITIS_Status, 'L48\\(I\\)') & (str_detect(Syn_Status, 'L48\\(I\\)') | is.na(Syn_Status) | Syn_Status == '')) %>%
  unique()

species_list = sort(unique(c(test.3$ITISacceptedName,test.3$synonym_base))) # count 8,644
species_list_no_syn = sort(unique(c(test.3$ITISacceptedName)))

# save the sp_df dataframe and species lists
write.csv(test.3, './sp_df_target_bg.csv')
write.csv(species_list, './bg_sp_list.csv')
write.csv(species_list_no_syn, './bg_sp_list_with_syn.csv')
