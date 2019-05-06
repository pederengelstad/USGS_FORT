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
    left_join(t, by=c('acc_tsn'='tsn'))
  
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

  
  # sometimes, the only matches are for outdated unaccepted terms or the species name may be misspelled.
  # still, it's good to include it, in case lots of sources use this term and ITIS doesn't for whatever reason.
  # if(!is.null(sp_list[!sp_list %in% c(unique(na.omit(s$ITISacceptedName)),unique(na.omit(s$synonym_base)))])){
  #   for(i in sp_list[!sp_list %in% c(unique(na.omit(s$ITISacceptedName)),unique(na.omit(s$synonym_base)))]){
  #     sp_df = rbind(sp_df,c(paste0(i," sp."),i,NA))
  #   }
  # }

  sp_df <- sp_df[(sp_df$ITISacceptedName!=sp_df$synonym_base | is.na(sp_df$ITISacceptedName==sp_df$synonym_base)),]
  sp_df <<- sp_df[order(sp_df$ITISacceptedName),]
  
  # synthesize full, unique species name list including synonyms
  species_search_list <<- sort(unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym_base))))
  
  # to search the USDA plants database for codes, two queries are generated:
  # 1. searching for the currently used code
  # 2. searching for all previously used code synonyms
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    
    plants.db = as.data.frame(gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1OCer0LGtsVjAcjNrIa6aQQ_7YzHycemO1rwI-aJePr4/edit?usp=sharing"))

    #First check for matches with ITIS accepted names
    acc.symbol.df = plants.db[plants.db$Scientific_Name %in% sp_df$ITISacceptedName,]
    syn.symbol.df = plants.db[plants.db$Scientific_Name %in% sp_df$synonym_base[!is.na(sp_df$synonym_base)],]

    #Then check through the synonyms (there should be less of these, generally)
    for(i in 1:nrow(sp_df)){
      
      if(!is.na(sp_df[i, 1])){
        acc.df <- plants.db[plants.db$Scientific_Name == sp_df[i, 1],]
        acc.df.full <- plants.db[plants.db$Accepted_Symbol == acc.df[[1]],]
        symbols.flat <- unique(c(acc.df.full[,1], acc.df.full[,2], acc.df.full[,3]))
        symbols.vec <- symbols.flat[!is.na(symbols.flat)]
      }
      
      if(!is.na(sp_df[i, 2])){
        syn.symbol <- syn.symbol.df[syn.symbol.df$Scientific_Name == sp_df[i,2],][[3]]
        symbols.vec <- unique(c(symbols.vec,syn.symbol))
      }
      
      symbols.vec <- str_flatten(symbols.vec, collapse = ',')
      sp_df$usda_codes[i] <<- ifelse(length(symbols.vec) == 0, NA, symbols.vec)
    }

    rm(plants.db)
  
  print("Species Processing Complete!")
  
  }

}