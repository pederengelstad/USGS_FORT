#################################################################################
#     SpeciesProcessing.R
#     Author: Peder Engelstad
#     Updated: 7/18/2018
#     Purpose: Generate a list of search terms from ITIS based on accepted names
#              and accepted synonyms (NO SUBSPECIES, VAR, OR HYBRIDS!)
#################################################################################

species_processing <- function(sp_list=NULL, USDA=TRUE){
  
  library(ritis, verbose = F, quietly = T, warn.conflicts = F)
  library(taxize, verbose = F, quietly = T, warn.conflicts = F)
  library(tidyverse, verbose = F, quietly = T, warn.conflicts = F)
  library(gsheet, verbose = F, quietly = T, warn.conflicts = F)
  library(Hmisc)
  
  # get required TSNs to efficiently process species names using ITIS
  t0 <- unique(bind_rows(get_tsn_(sci_com = sort(sp_list),accepted = F, messages = T)))
  t0 <- t0[, c('nameUsage','scientificName','tsn')]
  
  # drop names that aren't in the original search list or are 'accepted'. this speeds up synonym search time.
  t <- t0[(t0$scientificName %in% sp_list | t0$nameUsage == 'accepted' | t0$nameUsage == 'valid'),]
  
  # drop hybrids
  t <- t[!t$scientificName %in% grep("×", t$scientificName, value = T, ignore.case = F),]
  
  # drop one word terms...too general
  t <- t[str_count(t$scientificName, pattern = '\\w+') > 1, ]
  
  # drop ssp and var using word term counts
  t <- t[str_count(t$scientificName, pattern = '\\S+') == 2, ]
  
  # find synonyms using function from the taxize library (make sure to have most recent build!)
  s0 <- suppressWarnings(suppressMessages(synonyms_df(synonyms(t$tsn, db = 'itis', ask=F))))
  
  if(nrow(s0) > 0){
    
    if(!('acc_name' %in% colnames(s0))) s0$acc_name <- NA
    
    # add accepted terms to synonym data frame
    s <- s0 %>%
      full_join(t[t$nameUsage=='accepted',], by=c('syn_tsn'='tsn')) %>%
      full_join(t[t$nameUsage!='not accepted',], by=c('acc_tsn'='tsn')) %>%
      mutate(scientificName = coalesce(scientificName.x, scientificName.y)) %>%
      mutate(nameUsage = coalesce(nameUsage.x, nameUsage.y)) %>%
      rowwise() %>%
      mutate(ITISacceptedName = ifelse(nameUsage=='accepted' | nameUsage=='valid', word(scientificName,1,2,' '), NA),
             synonym = ifelse(!is.null(syn_name), Hmisc::capitalize(tolower(syn_name)), NA)) %>%
      mutate(ITISacceptedName = ifelse(is.na(ITISacceptedName),word(acc_name,1,2,' '), ITISacceptedName))
    
    # simplify data frame, deduplicate, and double check that there aren't hybrids in the synonyms
    sp_df <- s %>%
      select(ITISacceptedName, synonym) %>%
      unique()
    
  } else {
    
    sp_df <- t %>%
      mutate(syn_name = NA) %>%
      mutate(ITISacceptedName = ifelse(nameUsage=='accepted' | nameUsage=='valid', word(scientificName,1,2,' '), NA),
             synonym = ifelse(!is.null(syn_name), Hmisc::capitalize(tolower(syn_name)), NA)) %>%
      mutate(ITISacceptedName = ifelse(is.na(ITISacceptedName),word(acc_name,1,2,' '), ITISacceptedName)) %>%
      select(ITISacceptedName, synonym) %>%
      unique()
    
  }
  
  
  sp_df <- sp_df[!sp_df$synonym %in% grep("×", sp_df$synonym, value = T, ignore.case = F),]
  sp_df <- sp_df[(sp_df$ITISacceptedName!=sp_df$synonym | is.na(sp_df$ITISacceptedName==sp_df$synonym)),]
  
  # can drop the NA row for these species
  na.drop = suppressWarnings(sp_df %>% group_by(ITISacceptedName) %>% count() %>% filter(n > 1) %>% select(ITISacceptedName))$ITISacceptedName
  sp_df = sp_df %>% filter((!is.na(synonym) & ITISacceptedName %in% na.drop) | (! ITISacceptedName %in% na.drop)) 
  assign("sp_df", sp_df[order(sp_df$ITISacceptedName),] %>% filter(!is.na(ITISacceptedName)), envir = .GlobalEnv)
  
  # synthesize full, unique species name list including synonyms
  species_search_list <<- sort(unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym))))
  
  # search the USDA plants database (update 2019-05-06) for their codes
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    
    plants.db = read.csv('usda_plants_June2020.csv', header = T, stringsAsFactors = F)
    
    sp_df <<- sp_df %>%
      left_join(plants.db, by = c('ITISacceptedName' = 'Scientific.Name')) %>%
      left_join(plants.db, by = c('synonym' = 'Scientific.Name')) %>%
      rowwise() %>%
      mutate(usda_codes = ifelse(all(is.na(c(Accepted.Symbol.x,Synonym.Symbol.x,Accepted.Symbol.y,Synonym.Symbol.y))),NA,
                                 str_flatten(unique(na.omit(c(Accepted.Symbol.x,Synonym.Symbol.x,Accepted.Symbol.y,Synonym.Symbol.y))),
                                             collapse = ','))) %>%
      select(ITISacceptedName, synonym, usda_codes)
    
    rm(plants.db)
    
    print("Species Processing Complete!") 
  } 
}