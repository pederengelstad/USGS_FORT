library(rgeos)
library(rgdal)
library(scrubr)
library(tidyverse)


Data_QAQC <- function(data=NULL){

  # Combine all existing dataframes from each source
  occ_merged <<- bind_rows(data)

  if('EDDMapS' %in% unique(occ_merged$DataSet)){
    occ_filter <<- occ_merged %>%
      dplyr::select(DataSet, source_sp_name, searched_term, decimalLatitude,decimalLongitude, ObsDate, ObsYear,IdentificationCredibility,national_ownership,local_ownership) %>%
      dplyr::filter(ObsDate >= startdate & ObsDate <= Sys.Date() & !is.na(ObsDate) & !is.na(decimalLatitude)) %>%    # remove NAs from individual columns
      dplyr::filter(word(searched_term,1,2,' ') %in% sp_df$ITISacceptedName | word(searched_term,1,2,' ') %in% sp_df$synonym_base)
  } else{
    occ_filter <<- occ_merged %>%
      dplyr::select(DataSet, source_sp_name, searched_term, decimalLatitude,decimalLongitude, ObsDate, ObsYear) %>%
      dplyr::filter(ObsDate >= startdate & ObsDate <= Sys.Date() & !is.na(ObsDate) & !is.na(decimalLatitude)) %>%    # remove NAs from individual columns
      dplyr::filter(word(searched_term,1,2,' ') %in% sp_df$ITISacceptedName | word(searched_term,1,2,' ') %in% sp_df$synonym_base)
  }
  
    # Sort by scientific name and retain only unique records
    occ_all <- occ_filter %>%
      mutate(ITIS_AcceptedName = ifelse(test = is.na(sp_df$ITISacceptedName[match(occ_filter$searched_term, sp_df$synonym_base)]),
                                      sp_df$ITISacceptedName[match(word(occ_filter$searched_term,1,2,' '), sp_df$ITISacceptedName)],
                                      sp_df$ITISacceptedName[match(word(occ_filter$searched_term,1,2,' '), sp_df$synonym_base)])) %>%
      arrange(ITIS_AcceptedName) %>%
      unique()  


  if(exists('state_centroid')==FALSE){
    #pull in state boundary data from US census as needed
    temp = tempfile()
    temp2 = tempfile()
    download.file(url = "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_5m.zip", destfile = temp, quiet=TRUE)
    unzip(zipfile = temp, exdir = temp2)
    state_bound <- readOGR(dsn = file.path(temp2), 'cb_2016_us_state_5m', verbose=FALSE)
    unlink(c(temp, temp2))
    state_centroid <<- as.data.frame(SpatialPointsDataFrame(gCentroid(state_bound, byid = TRUE),state_bound@data, match.ID = F))
  }

  if(exists('county_centroid')==FALSE){
    #pull in county boundary data from the US census as needed
    temp = tempfile()
    temp2 = tempfile()
    download.file(url = "http://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_county_5m.zip", destfile = temp, quiet=TRUE)
    unzip(zipfile= temp, exdir = temp2)
    county_bound = readOGR(dsn = file.path(temp2), 'cb_2016_us_county_5m', verbose=FALSE)
    unlink(c(temp, temp2))
    county_centroid <<- as.data.frame(SpatialPointsDataFrame(gCentroid(county_bound, byid = TRUE), county_bound@data, match.ID = F))
  }

    # remove possible coordinate errors, including centroids
  occ_all <- occ_all %>%
    scrubr::coord_impossible() %>%
    scrubr::coord_incomplete() %>%
    scrubr::coord_unlikely() %>%
    filter(!(latitude %in% state_centroid$y) & !(latitude %in% county_centroid$y) &
           !(longitude %in% state_centroid$x) & !(longitude %in% county_centroid$x))

  occ_all <<- occ_all[which((nchar(occ_all$latitude) > 5 & nchar(occ_all$longitude) > 5)),]

    


  #TO DO: remove museum collection lat/longs

}


# ExportBySpecies <- function(data){
#   for (i in occ_all$ITIS_acceptedName){
#   occ_filt <- occ_all %>% dplyr::filter(scientificName==i)
#   write.csv(occ_filt, file = paste0("output/sp_occ_",gsub(" ","_",i), ".csv"))
#   }
# }

# 5.5 - output data frame to csv for each species
