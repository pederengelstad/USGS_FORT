species_processing <- function(sp_list=NULL, USDA=TRUE){
  
  library(ritis)
  library(taxize)
  library(tidyverse)

  t <- bind_rows(terms(query = sp_list, wt = 'json'))

  s = suppressMessages(synonyms_df(synonyms(t$tsn[match(sp_list, word(t$scientificName, 1, 2, " "))], db='itis', ask=F, message=F)))
  s$ITISacceptedName =   ifelse(is.na(word(t$scientificName[match(s$acc_tsn, t$tsn)], 1, 2, " "))
         						,word(s$acc_name,1,2," ")
         						,word(t$scientificName[match(s$acc_tsn, t$tsn)], 1, 2, " "))
  s$synonym_base = word(s$syn_name, 1, 2, " ")
  
  sp_df <<- unique(s) %>%
    filter(nchar(word(ITISacceptedName, 2,2)) > 1) %>%
    select(ITISacceptedName, synonym_base)

  # 1.6 - synthesize full, unique species name list including synonyms but only include genus and species
  species_search_list <<- unique(c(na.omit(sp_df$ITISacceptedName), na.omit(sp_df$synonym_base), word(sp_list, 1,2," ")))
  
  if(USDA == TRUE){
    
    print("Finding USDA species codes")
    
    for (i in 1:nrow(sp_df)){
      j <- tryCatch(fromJSON(paste0('https://plantsdb.xyz/search/?Genus=', word(sp_df[i,1],1,1),"&Species=",word(sp_df[i,1],2,2))), error=function(e) NULL)
      k <- tryCatch(fromJSON(paste0('https://plantsdb.xyz/search/?Genus=', word(sp_df[i,2],1,1),"&Species=",word(sp_df[i,2],2,2))), error=function(e) NULL) 
      
      if(is.null(j) & is.null(k)){
        sp_df$usda_codes[i] <<- NA
        next}

      else{
        a <- na.omit(unique(j$data$Accepted_Symbol_x))
        b <- str_split(na.omit(unique(subset(j$data$Synonym_Symbol_x, j$data$Synonym_Symbol_x != ""))), ', ')
        c <- na.omit(unique(k$data$Accepted_Symbol_x))
        d <- str_split(na.omit(unique(subset(k$data$Synonym_Symbol_x, j$data$Synonym_Symbol_x != ""))), ',')
        
        sp_df$usda_codes[i] <<- str_flatten(unique(c(a,b,c,d)), collapse = ", ")
      }
    }
  }

print("Species Processing Complete!")

}
