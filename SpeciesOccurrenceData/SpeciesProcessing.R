species_processing <- function(sp_list=NULL, USDA=TRUE){
  
  library(ritis)
  library(taxize)
  library(tidyverse)
  library(jsonlite)
  
  t0 = unique(bind_rows(get_tsn_(searchterm = sort(sp_list),accepted = F, messages = T)))
  t0 <<- t0[, c('nameUsage','scientificName','tsn')]
  t = t0[(t0$scientificName %in% sp_list | t0$nameUsage == 'accepted'),]
  
  #make sure name variations are dropped (i.e. var.; ssp.; cv. and so on)
  t = t0[str_count(t0$scientificName, '\\s') <= 2,]
  
  s0 = suppressWarnings(suppressMessages(synonyms_df(synonyms(t$tsn, db = 'itis', ask=F))))
  s = s0 %>%
    left_join(t, by=c('acc_tsn'='tsn'))
  
  s$ITISacceptedName = ifelse(is.na(s$scientificName),word(s$acc_name,1,2,' '), word(s$scientificName,1,2, ' '))
  
  if(!is.null(s$syn_name)){
    s$synonym_base = word(s$syn_name,1,2,' ')
  } else {  
    s$synonym_base = NA
  }
  
  sp_df = s %>%
    select(ITISacceptedName, synonym_base) %>%
    unique()
  
  if(!is.null(sp_list[!sp_list %in% c(unique(na.omit(s$ITISacceptedName)),unique(na.omit(s$synonym_base)))])){
    for(i in sp_list[!sp_list %in% c(unique(na.omit(s$ITISacceptedName)),unique(na.omit(s$synonym_base)))]){
      sp_df = rbind(sp_df,c(NA,i))
    }
  }
  
  sp_df <<- sp_df[(sp_df$ITISacceptedName!=sp_df$synonym_base | is.na(sp_df$ITISacceptedName==sp_df$synonym_base)),]

  
  # 1.6 - synthesize full, unique species name list including synonyms but only include genus and species
  species_search_list <<- unique(na.omit(c(sp_df$ITISacceptedName, sp_df$synonym_base)))
  
  
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    
    for (i in 1:nrow(sp_df)){
      if(!is.na(sp_df[i,1])){
        j <- tryCatch(fromJSON(paste0('https://plantsdb.xyz/search/?Genus=', word(sp_df[i,1],1,1),"&Species=",word(sp_df[i,1],2,2))), error=function(e) NULL)
      } else{
        j <- NULL
      }

      if(!is.na(sp_df[i,2])){
        k <- tryCatch(fromJSON(paste0('https://plantsdb.xyz/search/?Genus=', word(sp_df[i,2],1,1),"&Species=",word(sp_df[i,2],2,2))), error=function(e) NULL) 
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
        d <- str_split(na.omit(unique(subset(k$data$Synonym_Symbol_x, j$data$Synonym_Symbol_x != ""))), ',')
      } else {
        c <- NA
        d <- NA
      }
      
      vec <- unique(as.character(na.omit(c(as.character(a),as.character(b),as.character(c),as.character(d)))))
      sp_df$usda_codes[i] <- str_flatten(vec, collapse = ", ")
    }
  }
  
  sp_df <<- sp_df[with(sp_df, order(ITISacceptedName,synonym_base)),]
  
  print("Species Processing Complete!")
  
  }

