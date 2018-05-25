library(spocc)
library(jsonlite)
library(tidyverse)
library(stringr)
library(gsheet)
library(httr)

#TO DO: 1. produce dataframe describing the total number of available records, by species and data source
#       2. include country field


api_data <- function(species_list = NULL, sources=c('gbif','bison','inat','ecoengine','eddmaps')
                     , limit=5, startDate=NULL, endDate=NULL, bisonopts=NULL, gbifopts=NULL, inatopts=NULL
                     , US_only = NULL){
  
  # 2. spocc (GBIF, BISON, iNaturalist, and EcoEngine) -------------------------
  
  # 2.1 - Query sources for species data over their global extent (can be restricted to U.S. with additional parameter)
  #       What are the actual limits for GBIF and BISON record searches?
  #       Note 1: Search type for spocc is exact (???) 
  #       Note 2: depending on the number of species and databases queried, the occ() function may result in very long run-times (> 15 min), esp w/ ecoengine
  
  v <- c("gbif","bison", "inat","ecoengine")
  
  #restrict records to US or leave search open to global records
  if(US_only==T){
    edd_loc <- "&Country=926"
    
    if(is.null(gbifopts)==T){
      gbifopts <- list(country='US')
    } else{
      gbifopts <- c(gbif_options, country='US')
    }
    
    if(is.null(bisonopts)==T){
      bisonopts <- list(params=c('countryCode: US')) #if no set options, generate variable
    }
    if(is.null(bisonopts$params)==T){
      bisonopts <- c(bisonopts, params=c('countryCode: US')) #if params not included options, generate variable
    } else{
      bisonopts$params <- paste0(bisonopts$params,'; countryCode: US') # if params are included, append countryCode search option
    }
  } else{
    edd_loc = ""
  }
  
  
  if(length(v[which(v%in%sources)]) > 0) {
    
    print('Searching GBIF/BISON/iNaturalist/EcoEngine for occurrence records')
    
    spocc_df <-suppressWarnings(occ(species_list
                                    , from=v[which(v%in%sources)]
                                    , limit=limit
                                    , has_coords=TRUE 
                                    , date = c(as.Date(startDate),as.Date(endDate))
                                    , bisonopts=bisonopts
                                    , gbifopts=gbifopts
    ))
    
    if(sum(sapply(spocc_df$gbif$data, NROW)) > 0){
      gbif_df <- as.data.frame(occ2df(spocc_df$gbif)) %>%
        filter(basisOfRecord %in% c("PRESERVED_SPECIMEN", "HUMAN_OBSERVATION", "OBSERVATION")) %>%
        select(prov, name, longitude, latitude, eventDate, year, scientificName) %>%
        mutate(DataSet = prov,
               decimalLatitude = as.numeric(latitude),
               decimalLongitude = as.numeric(longitude),
               ObsDate = as.Date(eventDate),
               ObsYear = year,
               source_sp_name = word(scientificName,1,2," "),
               searched_term = name
        )
      gbif_final <- gbif_df %>%
        select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, searched_term) %>%
        unique()
    } else{ gbif_final <- data.frame()}
    
    if(sum(sapply(spocc_df$bison$data, NROW)) > 0){
      bison_df <- as.data.frame(occ2df(spocc_df$bison)) %>%
        filter(!is.na(date) & str_count(string = pointPath, pattern = 'centroid') != 1) %>%
        mutate(DataSet = prov,
               decimalLatitude = as.numeric(latitude),
               decimalLongitude = as.numeric(longitude),
               ObsDate = as.Date(date),
               ObsYear = as.integer(format(as.Date(date), "%Y")),
               source_sp_name = providedScientificName,
               searched_term = word(name,1,2," ")
        )
      bison_final <- bison_df %>%
        select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, searched_term) %>%
        unique()
    } else{ bison_final <- data.frame()}
    
    if(sum(sapply(spocc_df$inat$data, NROW)) > 0){
      inat_df <- as.data.frame(occ2df(spocc_df$inat)) %>%
        filter(location_is_exact == TRUE) %>%
        mutate(DataSet = prov,
               decimalLatitude = as.numeric(latitude),
               decimalLongitude = as.numeric(longitude),
               ObsDate = as.Date(observed_on),
               ObsYear = as.integer(format(as.Date(observed_on), "%Y")),
               source_sp_name = name,
               scientificName = word(name,1,2," ")
        )
      inat_final <- inat_df %>%
        select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, searched_term) %>%
        unique()
    } else{ inat_final <- data.frame()}
    
    if(sum(sapply(spocc_df$ecoengine$data, NROW)) > 0){
      ecoengine_df <- as.data.frame(occ2df(spocc_df$ecoengine)) %>%
        mutate(DataSet = prov,
               decimalLatitude = as.numeric(latitude),
               decimalLongitude = as.numeric(longitude),
               ObsDate = as.Date(eventDate),
               ObsYear = year,
               source_sp_name = name,
               scientificName = word(name,1,2," ")
        )
      ecoengine_final <- ecoengine_df %>%
        select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, searched_term) %>%
        unique()
    } else{ ecoengine_final <- data.frame()}
    
    # combine all available data frames
    spocc_final <- gbif_final %>%
      bind_rows(bison_final, inat_final, ecoengine_final)
    
    #add final version to global list of data frames
    df_list[['spocc']] <<- unique(spocc_final)
    
    # assign("df_list", df_list, envir=.GlobalEnv)
    
    print('GBIF/BISON/iNaturalist/EcoEngine Search Complete')
    
  }
  
  # EDDMapS API Call
  if(length(sources[which("eddmaps"%in%sources)]) > 0){
    
    print('Searching EDDMapS for occurrence records with geospatial data')
    
    # I created a publically hosted version of the plant subject data from EDDMapS instead of iteratively
    # hitting their API for each species subject ID
    g <- suppressWarnings(gsheet2tbl('https://docs.google.com/spreadsheets/d/1OnADbTEmHO4HbQd1mmPchJXz502ONOCjLDTNHxKkdwo'))
    
    edd_date_start = format(as.Date(startdate), "%m/%d/%Y")
    edd_date_end = format(as.Date(enddate), "%m/%d/%Y")
    
    g2 <- g %>%
      select(Scientificname, RowsID) %>%
      filter(word(Scientificname,1,2,' ') %in% species_list) %>%
      arrange(Scientificname)
    
    # natl_codes <- c('USFS','BLM', 'BLM ','fed','NPS',"USFW","DOD")
    edd_df <- list()
    
    #3.1: Iterate over the possible pages for all available EDDMapS records
    for (i in unique(g2$RowsID)){
      edd_sp_pages <- list()
      
      tot_rec <- fromJSON(paste0("https://sandbox.bugwood.org/rest/api/occurrence.json?&fmt=jqgrid",
                                 "&include=National_Ownership,Local_Ownership,ObservationDate,scientificname,Latitude_Decimal,Longitude_Decimal,IdentificationCredibility,Country",  # include the fields you want returned
                                 "&subjectNumber=",i
                                 ,"&div=5&negative=0&reviewed=1&spatial=1"
                                 ,"&length=1",edd_loc
                                 ,sep=''),simplifyDataFrame = T)
      
      print(paste0(tot_rec$total," records found for ",g2$Scientificname[g2$RowsID==i][1], collapse = ""))
      
      
      if(tot_rec$total > 0){
        for (j in seq(1,  ceiling(tot_rec$total/3000),1)){
          edd_sp_pages[[paste0(i,j)]] <- paste("https://sandbox.bugwood.org/rest/api/occurrence.json?&fmt=jqgrid&",
                                               "include=National_Ownership,Local_Ownership,ObservationDate,scientificname,Latitude_Decimal,Longitude_Decimal,IdentificationCredibility,Country",  # include the fields you want returned
                                               "&subjectNumber=",i,edd_loc,
                                               "&div=5&negative=0&reviewed=1&spatial=1&page=",j
                                               ,sep = "")
        }
        
        for (k in edd_sp_pages){
          tmp <- fromJSON(k, simplifyDataFrame = T)
          edd_df <- rbind(edd_df, tmp$rows)
        }
      }
    }

    # 3.2 - format EDDMaps dataframe
    if (nrow(edd_df) > 0){
      edd_format <- edd_df %>%
        filter(latitude_decimal != "") %>%
        select(latitude_decimal, longitude_decimal, hostscientificname, Observationdate, scientificname, IdentificationCredibility
               , national_ownership,local_ownership) %>%
        mutate(DataSet = "EDDMapS",
               decimalLatitude = as.numeric(latitude_decimal),
               decimalLongitude = as.numeric(longitude_decimal),
               source_sp_name = hostscientificname,
               searched_term = scientificname,
               ObsDate = as.Date(Observationdate),
               ObsYear = as.integer(format(as.Date(Observationdate), "%Y"))
        )
      
      edd_final <- edd_format %>%
        select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, searched_term, ObsDate, ObsYear, IdentificationCredibility
               ,national_ownership,local_ownership
        ) %>%
        unique()
      
      df_list[['eddmaps']] <<- edd_final
      
      print("EddMapS Search Complete!")
      
    }  
  }
  closeAllConnections()
}