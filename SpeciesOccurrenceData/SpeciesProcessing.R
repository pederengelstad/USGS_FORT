#################################################################################
#     SpeciesProcessing.R
#     Author: Peder Engelstad
#     Updated: 7/18/2018
#     Purpose: Generate a list of search terms from ITIS based on accepted names
#              and accepted synonyms (NO SUBSPECIES, VAR, OR HYBRIDS!)
#################################################################################


species_processing <- function(sp_list=NULL, USDA=TRUE){
  
  library(ritis)
  library(taxize)
  library(tidyverse)
  library(jsonlite)
  
  # get required TSNs to efficiently process species names using ITIS
  t0 = unique(bind_rows(get_tsn_(searchterm = sort(sp_list),accepted = F, messages = T)))
  t0 = t0[, c('nameUsage','scientificName','tsn')]
  
  # drop names that aren't in the original search list or are 'accepted'. this speeds up synonym search time.
  t = t0[(t0$scientificName %in% sp_list | t0$nameUsage == 'accepted'),]
  
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
  
  # simplify data frame and deduplicate
  sp_df = s %>%
    select(ITISacceptedName, synonym_base) %>%
    unique()
  
  # sometimes, the only matches are for outdated unaccepted terms or the species name may be misspelled.
  # still, it's good to include it, in case lots of sources use this term and ITIS doesn't for whatever reason.
  if(!is.null(sp_list[!sp_list %in% c(unique(na.omit(s$ITISacceptedName)),unique(na.omit(s$synonym_base)))])){
    for(i in sp_list[!sp_list %in% c(unique(na.omit(s$ITISacceptedName)),unique(na.omit(s$synonym_base)))]){
      sp_df = rbind(sp_df,c(paste0(i," sp."),i,NA))
    }
  }

  sp_df <- sp_df[(sp_df$ITISacceptedName!=sp_df$synonym_base | is.na(sp_df$ITISacceptedName==sp_df$synonym_base)),]
  
  # synthesize full, unique species name list including synonyms
  species_search_list <<- unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym_base)))
  
  # to search the USDA plants database for codes, two queries are generated:
  # 1. searching for the currently used code
  # 2. searching for all previously used code synonyms
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    
    for (i in 1:nrow(sp_df)){
      if(!is.na(sp_df[i,1])){
        tmp_genus = ifelse(is.na(word(sp_df[i,1],1,1)), '', word(sp_df[i,1],1,1))
        tmp_sp = ifelse(is.na(word(sp_df[i,1],2,2)), '', word(sp_df[i,1],2,2))
    j <- tryCatch(fromJSON(paste0('https://plantsdb.xyz/search/?Genus=', tmp_genus,"&Species=",tmp_sp)), error=function(e) NULL)
      } else{
        j <- NULL
      }

      if(!is.na(sp_df[i,2])){
        tmp_genus = ifelse(is.na(word(sp_df[i,2],1,1)), '', word(sp_df[i,2],1,1))
        tmp_sp = ifelse(is.na(word(sp_df[i,2],2,2)), '', word(sp_df[i,2],2,2))
        k <- tryCatch(fromJSON(paste0('https://plantsdb.xyz/search/?Genus=', tmp_genus,"&Species=",tmp_sp)), error=function(e) NULL)
      } else {
        k <- NULL
      }

      if(is.null(j) & is.null(k)){
        sp_df$usda_codes[i] <- NA
        next}
      
      if(!is.null(j)){
        a <- na.omit(unique(j$data$Accepted_Symbol_x))
        b <- str_split(na.omit(unique(subset(j$data$Synonym_Symbol_x, j$data$Synonym_Symbol_x != ""))), ', ')
        
      } else{
        a <- NA
        b <- NA
      }

      if(!is.null(k)){
        c <- na.omit(unique(k$data$Accepted_Symbol_x))
        d <- str_split(na.omit(unique(subset(k$data$Synonym_Symbol_x, k$data$Synonym_Symbol_x != ""))), ',')
      } else {
        c <- NA
        d <- NA
      }
      
      vec <- unique(as.character(na.omit(c(as.character(a),as.character(b),as.character(c),as.character(d)))))
      sp_df$usda_codes[i] <- str_flatten(vec, collapse = ", ")
    }
  }
  
  sp_df <- sp_df[with(sp_df, order(ITISacceptedName,synonym_base)),]
  
  #make sure that the data frame does not contain rows with only NAs
  sp_df <<- sp_df[rowSums(is.na(sp_df)) != ncol(sp_df),]
  
  print("Species Processing Complete!")
  
  }

