library(rgeos)
library(rgdal)
library(scrubr)
library(tidyverse)


Data_QAQC <- function(data=NULL){

  occ_merged <- bind_rows(data) %>%
   dplyr::filter(ObsDate >= startdate & ObsDate <= Sys.Date() & !is.na(ObsDate) & !is.na(decimalLatitude))

  sp_df2 = sp_df %>%
    mutate(ITIS_final = ITISacceptedName)

  # Sort by scientific name and retain only unique records
  occ_merged = occ_merged %>%
    mutate(test1 = str_match(pattern = '.[:upper:].*', word(source_sp_name,start = 3,end = -1)),
           test2 = str_match(pattern = '[:upper:].*', word(source_sp_name,start = 3,end = -1))) %>%
    rowwise() %>%
    mutate(sp_clean = ifelse(length(test1)>length(test2), 
                             gsub(test1,'',source_sp_name), 
                             gsub(test2,'',source_sp_name)))  %>%
    mutate(sp_clean = ifelse(is.na(sp_clean), source_sp_name, sp_clean))

  occ_merged = occ_merged %>%
    mutate(sp_clean = gsub('( (\\())','',sp_clean)) %>%
    mutate(sp_clean = gsub(' $','',sp_clean)) %>%
    mutate(sp_clean = gsub('subsp.','ssp.',sp_clean)) %>%
    mutate(sp_clean = trimws(sp_clean)) %>%
    select(-c(test1, test2)) %>%
    unique()

  occ_merged = occ_merged %>%
    left_join(sp_df2[, c("ITIS_final","ITISacceptedName")] %>% unique(), by = c('sp_clean' = 'ITISacceptedName')) %>%
    left_join(sp_df2[, c("ITIS_final","synonym")] %>% unique(), by = c('sp_clean' = 'synonym')) %>%
    mutate(ITIS_final = coalesce(ITIS_final.x,ITIS_final.y)) %>%
    select(-c(ITIS_final.x,ITIS_final.y)) %>%
    filter(!is.na(ITIS_final))

  cat('Original number of merged records: ', nrow(occ_merged), '\n')

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
  occ_all <- occ_merged %>%
    scrubr::coord_impossible() %>%
    scrubr::coord_incomplete() %>%
    scrubr::coord_unlikely() %>%
    filter(!(decimalLatitude %in% state_centroid$y) & !(decimalLatitude %in% county_centroid$y) &
             !(decimalLongitude %in% state_centroid$x) & !(decimalLongitude %in% county_centroid$x))
             
  occ_all <- occ_all[which((nchar(occ_all$decimalLatitude) > 5 & nchar(occ_all$decimalLongitude) > 5)),]

  cat('Final number of records after filtering: ', nrow(occ_all), '\n')

  assign("occ_all", occ_all, envir = .GlobalEnv)


}