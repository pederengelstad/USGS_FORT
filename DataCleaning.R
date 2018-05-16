library(rgeos)
library(rgdal)
library(scrubr)
library(tidyverse)


Data_QAQC <- function(data=NULL){

  # Combine all existing dataframes from each source
  occ_merged <<- bind_rows(data)
  occ_filter <<- occ_merged %>%
    dplyr::select(DataSet, source_sp_name, scientificName, decimalLatitude,decimalLongitude, ObsDate, ObsYear) %>%
    dplyr::filter(ObsDate >= startdate & ObsDate <= Sys.Date() & !is.na(ObsDate) & !is.na(decimalLatitude)) %>%    # remove NAs from individual columns
    dplyr::filter(scientificName %in% sp_df$ITISacceptedName)                                       # remove bad fuzzy var. and spp. matches (mostly from EDDMapS)


  # Sort by scientific name and retain only unique records
  occ_all <- unique(occ_filter[order(occ_filter$scientificName, decreasing = F),])


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

    


  #TO DO: remove county and state centroid points, museum collection lat/longs

}


ExportBySpecies <- function(data){
  for (i in occ_all$ITIS_acceptedName){
  occ_filt <- occ_all %>% dplyr::filter(scientificName==i)
  write.csv(occ_filt, file = paste0("output/sp_occ_",gsub(" ","_",i), ".csv"))
  }
}

# 5.5 - output data frame to csv for each species
