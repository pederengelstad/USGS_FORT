library(spocc)
library(jsonlite)
library(tidyverse)
library(stringr)
library(gsheet)

#TO DO: produce dataframe describing the total number of available records, by species and data source


api_data <- function(species_list = NULL, sources=c('gbif','bison','inat','ecoengine','eddmaps'), limit=5, startDate=NULL, endDate=NULL, bisonopts=NULL, gbifopts=NULL, inatopts=NULL){
  
  # 2. spocc (GBIF, BISON, iNaturalist, and EcoEngine) -------------------------
  
  # 2.1 - Query sources for species data over their global extent (can be restricted to U.S. with additional parameter)
  #       Note 1: Search type for spocc is exact (???) 
  #       Note 2: depending on the number of species and databases queried, the occ() function may result in very long run-times (> 15 min), esp w/ ecoengine
  
  v <- c("gbif","bison", "inat","ecoengine")

  if(length(v[which(v%in%sources)]) > 0) {

    print('Searching GBIF/BISON/iNaturalist/EcoEngine for occurrence records')

    spocc_df <<- occ(species_list
                    , from=v[which(v%in%sources)]
                    , limit=limit
                    , has_coords=TRUE 
                    , date = c(as.Date(startDate), as.Date(endDate))
                    , bisonopts=bisonopts
                    , gbifopts=gbifopts
                    )

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

    print('Searching EDDMapS for occurrence records')

    # I created a publically hosted version of the plant subject data from EDDMapS instead of iteratively
    # hitting their API for each species subject ID
    g <- suppressWarnings(gsheet2tbl('https://docs.google.com/spreadsheets/d/1OnADbTEmHO4HbQd1mmPchJXz502ONOCjLDTNHxKkdwo'))

    edd_date_start = format(as.Date(startdate), "%m/%d/%Y")
    edd_date_end = format(as.Date(enddate), "%m/%d/%Y")
    
    g2 <- g %>%
      select(Scientificname, RowsID) %>%
      filter(Scientificname %in% species_list) %>%
      arrange(Scientificname)

    natl_codes <- c('USFS','BLM', 'BLM ','fed','NPS',"USFW","DOD")
    edd_df <- list()
    
    #to do: add credible records 
    for (i in unique(g2$RowsID)){
      print(paste0("Searching for: ",g2$Scientificname[g2$RowsID==i][1]," and synonyms", collapse = ""))
    
      #queries for verified records (US only)
      bw_url_v <- paste("https://api.bugwood.org/rest/api/occurrence.json?&fmt=jqgrid&",
                        "include=National_Ownership,Local_Ownership,ObservationDate,scientificname,Latitude_Decimal,Longitude_Decimal,IdentificationCredibility,Country",  # include the fields you want returned
                        "&subjectNumber=",i
                        ,"&div=5&negative=0&reviewed=1&spatial=1&Country=926"
                        ,'&IdentificationCredibility=verified'
                        ,"&observationdatestart=", edd_date_start
                        ,"&observationdateend=", edd_date_end
                        ,sep = "")
      
      #queries for 'credible' records (US only)
      bw_url_c <- paste("https://api.bugwood.org/rest/api/occurrence.json?&fmt=jqgrid&",
                        "include=National_Ownership,Local_Ownership,ObservationDate,scientificname,Latitude_Decimal,Longitude_Decimal,IdentificationCredibility,Country",  # include the fields you want returned
                        "&subjectNumber=",i,"&div=5&negative=0&reviewed=1&spatial=1&Country=926"
                        ,'&IdentificationCredibility=credible'
                        ,"&observationdatestart=", edd_date_start
                        ,"&observationdateend=", edd_date_end
                        ,sep = "")
      
      #pulls records to be filtered for Federal Lands only (US only)
      bw_url_f <- paste("https://api.bugwood.org/rest/api/occurrence.json?&fmt=jqgrid&",
                        "include=National_Ownership,Local_Ownership,ObservationDate,scientificname,Latitude_Decimal,Longitude_Decimal,IdentificationCredibility,Country",  # include the fields you want returned
                        "&subjectNumber=",i,"&div=5&negative=0&reviewed=1&spatial=1&Country=926"
                        ,"&observationdatestart=", edd_date_start
                        ,"&observationdateend=", edd_date_end
                        ,sep = "")

      eddmaps_call_v <- fromJSON(unique(bw_url_v), simplifyDataFrame = T)
      eddmaps_call_c <- fromJSON(unique(bw_url_c), simplifyDataFrame = T)
      eddmaps_call_f <- fromJSON(unique(bw_url_c), simplifyDataFrame = T)


      if(class(eddmaps_call_v$rows) == 'data.frame'){
        if(nrow(eddmaps_call_v$rows > 0)){
          edd_df <- rbind(edd_df, eddmaps_call_v$rows)
        }
      }
      if(class(eddmaps_call_c$rows) == 'data.frame'){
        if(nrow(eddmaps_call_c$rows > 0)){
          edd_df <- rbind(edd_df, eddmaps_call_c$rows)
        }
      }
      if(class(eddmaps_call_f$rows) == 'data.frame'){
        if(nrow(eddmaps_call_f$rows > 0)){
          edd_df <- rbind(edd_df, eddmaps_call_f$rows %>% filter(national_ownership %in% natl_codes))
          # edd_df <- rbind(edd_df, eddmaps_call_f$rows)
        }
      }
      else {next}    
    }

    # 3.2 - format EDDMaps dataframe
    if (nrow(edd_df) > 0){
      edd_format <- edd_df %>%
      filter(latitude_decimal != "") %>%
      select(latitude_decimal, longitude_decimal, hostscientificname, Observationdate, scientificname, IdentificationCredibility
        , national_ownership,local_ownership
        ) %>%
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