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
  
  # get required TSNs to efficiently process species names using ITIS
  t0 = unique(bind_rows(get_tsn_(searchterm = sort(sp_list),accepted = F, messages = T)))
  t0 = t0[, c('nameUsage','scientificName','tsn')]
  
  # drop names that aren't in the original search list or are 'accepted'. this speeds up synonym search time.
  t = t0[(t0$scientificName %in% sp_list | t0$nameUsage == 'accepted'),]
  
  # drop hybrids
  t = t[!t$scientificName %in% grep("X", t$scientificName, value = T, ignore.case = F),]

  # drop one word terms...too general
  t = t[str_count(t$scientificName, pattern = '\\w+') > 1,]

  # not currently using this line because ITIS sometimes won't find synonyms without very specific TSNs
  # base names will match the original searched term, thankfully
  # t = t0[str_count(t0$scientificName, '\\s') <= 2,]
  
  # find synonyms using function from the taxize library (make sure to have most recent build!)
  s0 = suppressWarnings(suppressMessages(synonyms_df(synonyms(t$tsn, db = 'itis', ask=F))))
  
  # make sure subspecies aren't included in synonym lists. can lead to erroneous results when using base names later
  # variations are dropped (i.e. var.; ssp.; cv. and so on) but not NAs (indicate a lack of synonyms)
  s0 = s0[(str_count(s0$syn_name,'\\s') <= 2 | is.na(s0$syn_name)==T),]
  
  # add accepted terms to synonym data frame
  s = s0 %>%
    right_join(t, by=c('acc_tsn'='tsn'))
  
  # unify accepted names
  s$ITISacceptedName = ifelse(is.na(s$scientificName),word(s$acc_name,1,2,' '), word(s$scientificName,1,2, ' '))
  
  # drop extra terms from synonyms b/c it's unlikely they will be found in searches
  if(!is.null(s$syn_name)){
    s$synonym_base = word(s$syn_name,1,2,' ')
  } else {  
    s$synonym_base = NA
  }
  
  # simplify data frame, deduplicate, and double check that there aren't hybrids in the synonyms
  sp_df = s %>%
    select(ITISacceptedName, synonym_base) %>%
    unique()
  sp_df = sp_df[!sp_df$synonym_base %in% grep("X", sp_df$synonym_base, value = T, ignore.case = F),]

  sp_df <- sp_df[(sp_df$ITISacceptedName!=sp_df$synonym_base | is.na(sp_df$ITISacceptedName==sp_df$synonym_base)),]
  sp_df <<- sp_df[order(sp_df$ITISacceptedName),]
  
  # synthesize full, unique species name list including synonyms
  species_search_list <<- sort(unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym_base))))
  
  # search the USDA plants database (update 2019-05-06) for their codes
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    
    plants.db = as.data.frame(gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1WkSt3EcOUkiRPRWeEZTKuQvkjhYFQre_Ox-I3FpPHbA/edit?usp=sharing"))

    sp_df <<- sp_df %>%
      left_join(plants.db, by = c('ITISacceptedName' = 'Scientific_Name')) %>%
      left_join(plants.db, by = c('synonym_base' = 'Scientific_Name')) %>%
      rowwise() %>%
      mutate(usda_codes = ifelse(all(is.na(c(Accepted_Symbol.x,Synonym_Symbol.x,Accepted_Symbol.y,Synonym_Symbol.y))),NA,
                                 str_flatten(unique(na.omit(c(Accepted_Symbol.x,Synonym_Symbol.x,Accepted_Symbol.y,Synonym_Symbol.y))),
                                             collapse = ',')))

  rm(plants.db)
  
  print("Species Processing Complete!")
  
  }

}