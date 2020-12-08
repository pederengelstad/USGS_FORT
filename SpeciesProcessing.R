#################################################################################
#     SpeciesProcessing.R
#     Author: Peder Engelstad
#     Updated: 7/18/2018
#     Purpose: Generate a list of search terms from ITIS based on accepted names
#              and accepted synonyms (NO SUBSPECIES, VAR, OR HYBRIDS!)
#
#     output
# 1.  sp_df
# 2.  species_search_list
# 3.  usda_code_vec

#################################################################################

species_processing = function(sp_list=NULL, USDA=TRUE){
  
  # library(ritis, verbose = F, quietly = T, warn.conflicts = F)
  # library(taxize, verbose = F, quietly = T, warn.conflicts = F)
  # library(tidyverse, verbose = F, quietly = T, warn.conflicts = F)
  # library(gsheet, verbose = F, quietly = T, warn.conflicts = F)
  # library(Hmisc)
  
  out <- list()
  
  # get required TSNs to efficiently process species names using ITIS
  t0 <- unique(
    dplyr::bind_rows(
      taxize::get_tsn_(sci_com = sort(sp_list),
                       accepted = F, 
                       messages = T)
    )
  )
  
  t0 <- t0[, c('nameUsage','scientificName','tsn')]
  
  # drop names that aren't in the original search list or are 'accepted'. this speeds up synonym search time.
  t <- t0[(t0$scientificName %in% sp_list | t0$nameUsage == 'accepted' | t0$nameUsage == 'valid'),]
  
  # drop hybrids
  t <- t[!t$scientificName %in% grep("×", t$scientificName, value = T, ignore.case = F),]
  
  # drop one word terms...too general
  t <- t[stringr::str_count(t$scientificName, pattern = '\\w+') > 1, ]
  
  # drop ssp and var using word term counts
  t <- t[stringr::str_count(t$scientificName, pattern = '\\S+') == 2, ]
  
  # find synonyms using function from the taxize library (make sure to have most recent build!)
  s0 <- suppressWarnings(
    suppressMessages(
      taxize::synonyms_df(
        taxize::synonyms(t$tsn, 
                         db = 'itis', 
                         ask=F)
      )
    )
  )
  
  if(nrow(s0) > 0){
    
    if(!('acc_name' %in% colnames(s0))) s0$acc_name = NA
    
    # add accepted terms to synonym data frame
    s <- s0 %>%
      dplyr::full_join(t[t$nameUsage == 'accepted',], by = c('syn_tsn' = 'tsn')) %>%
      dplyr::full_join(t[t$nameUsage != 'not accepted',], by = c('acc_tsn' = 'tsn')) %>%
      dplyr::mutate(scientificName = dplyr::coalesce(scientificName.x, scientificName.y)) %>%
      dplyr::mutate(nameUsage = dplyr::coalesce(nameUsage.x, nameUsage.y)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ITISacceptedName = ifelse(nameUsage=='accepted' | nameUsage=='valid', 
                                              stringr::word(scientificName, 1, 2, ' '), 
                                              NA),
                    synonym = ifelse(!is.null(syn_name), 
                                     Hmisc::capitalize(tolower(syn_name)), 
                                     NA)) %>%
      dplyr::mutate(ITISacceptedName = ifelse(is.na(ITISacceptedName), 
                                              stringr::word(acc_name, 1, 2, ' '), 
                                              ITISacceptedName))
    
    # simplify data frame, deduplicate, and double check that there aren't hybrids in the synonyms
    sp_df <- s %>%
      dplyr::select(ITISacceptedName, synonym, acc_tsn, syn_tsn) %>%
      dplyr::mutate(acc_tsn = as.numeric(acc_tsn), 
                    syn_tsn = as.numeric(syn_tsn)) %>%
      dplyr::filter(!(is.na(synonym) && !is.na(syn_tsn))) %>%
      unique()
    
  } else {
    
    sp_df <- t %>%
      mutate(syn_name = NA) %>%
      mutate(ITISacceptedName = ifelse(nameUsage == 'accepted' | nameUsage == 'valid', 
                                       stringr::word(scientificName,1,2,' '), 
                                       NA),
             synonym = ifelse(!is.null(syn_name), 
                              Hmisc::capitalize(tolower(syn_name)), 
                              NA)) %>%
      mutate(ITISacceptedName = ifelse(is.na(ITISacceptedName), 
                                       stringr::word(acc_name,1,2,' '), 
                                       ITISacceptedName)) %>%
      mutate(syn_tsn = NA, acc_tsn = as.numeric(tsn)) %>%
      select(ITISacceptedName, synonym, acc_tsn, syn_tsn) %>%
      unique()
    
  }
  
  
  sp_df <- sp_df[!sp_df$synonym %in% grep("×", sp_df$synonym, value = T, ignore.case = F),]
  sp_df <- sp_df[(sp_df$ITISacceptedName != sp_df$synonym | is.na(sp_df$ITISacceptedName == sp_df$synonym)),]
  
  # can drop the NA row for these species
  na.drop <- suppressWarnings(sp_df %>% 
                                dplyr::group_by(ITISacceptedName) %>% 
                                dplyr::count() %>% 
                                dplyr::filter(n > 1) %>% 
                                dplyr::select(ITISacceptedName))$ITISacceptedName
  
  sp_df <- sp_df %>% 
    dplyr::filter((!is.na(synonym) & ITISacceptedName %in% na.drop) | (! ITISacceptedName %in% na.drop)) 
  
  # synthesize full, unique species name list including synonyms
  species_search_list <- sort(unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym))))
  
  # search the USDA plants database (update 2019-05-06) for their codes
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    Sys.setenv("VROOM_SHOW_PROGRESS"="false")
    plants.db <- suppressMessages(vroom::vroom('usda_plants_Dec2020.csv', progress = F))
    colnames(plants.db) <- gsub(pattern = ' |/', replacement = '_', x = colnames(plants.db))
    
    # Find accepted name matches
    tmp1 <- sp_df %>% 
      dplyr::inner_join(plants.db, by = c('acc_tsn' = 'ITIS_TSN')) %>%
      dplyr::select(ITISacceptedName, acc_tsn, Accepted_Symbol, synonym, syn_tsn, Synonym_Symbol)
    
    tmp2 <- sp_df %>% 
      dplyr::inner_join(plants.db, by = c('syn_tsn' = 'ITIS_TSN')) %>%
      dplyr::select(ITISacceptedName, acc_tsn, Accepted_Symbol, synonym, syn_tsn, Synonym_Symbol)

    sp_df <- dplyr::bind_rows(tmp1, tmp2) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(usda_codes = ifelse(!is.na(Synonym_Symbol),
                                        paste(paste(Accepted_Symbol, Synonym_Symbol, sep =',')),
                                        Accepted_Symbol))
      
    code_list <- sort(
      unique(
        unlist(
          stringr::str_extract_all(
            stringr::str_flatten( 
              c(na.omit(sp_df$usda_codes)), collapse = ', '), stringr::boundary('word')))))
    
    rm(plants.db)
    
    print("Species Processing Complete!") 
  }
  
  out$sp_df <- sp_df
  out$species_search_list <- species_search_list
  out$usda_code_vec <- code_list
  
  return(out)
  
}