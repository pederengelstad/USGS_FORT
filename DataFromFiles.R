library(sf)

if("usda_codes" %in% names(sp_df)){
  usda_match <- sort(unique(unlist(str_extract_all(sp_df$usda_codes,'\\w+'))))
  } else{
  print('Rerun species processing function and generate USDA codes to use NISIMS data.')
}

#Functions for NISIMS data
# Note: because NISIMS geodatabase tables can be very large
LoadNISIMS <- function(type=NULL, filepath=NULL, layername=NULL){
  if(type == 'db'){

    nisims_db <<- sf::st_read(dsn = filepath, layer = layername)

  } else if(type=='file'){

    nisims_db <<- read.csv(filepath)

  } else {
      print("Please chose 'db' or 'file'")
    }
  }

# annual NISIMS data has a different coordinate parse than the larger historical database, thus the separate functions for now...
Parse_NISIMS_Annual <- function(){

  nisims_parse <- as.data.frame(nisims_db) %>%
    select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT) %>%
    filter(!is.na(CNTR_PT_CN) & as.character(SCNTFC_CD) %in% usda_match) %>%
    mutate(DataSet = "NISIMS"
           ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=X: )(-?\\d+\\.+\\d+)"))         
           ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=Y: )(-?\\d+\\.+\\d+)"))
           ,scientificName = sp_df$ITISacceptedName[match(SCNTFC_CD,unique(unlist(str_extract_all(sp_df$usda_codes,'\\w+'))))]
           ,ObsDate = as.Date(BEGIN_DT)
           ,ObsYear = format(as.Date(BEGIN_DT), "%Y"))
    nisims_parse <- nisims_parse %>% 
      filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
      select(DataSet, albersLatitude, albersLongitude, source_sp_name, scientificName, ObsDate, ObsYear)

    sf.point <- st_as_sf(x=nisims_parse
                     , coords = c("albersLatitude","albersLongitude")
                     , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

    nisims_reproj <- sf::st_transform(sf.point, crs = 4326)

    nisim_df <- as.data.frame(nisims_reproj) %>%
      mutate(decimalLatitude = as.numeric(lapply(str_extract_all(as.character(nisim_df$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 1)),
             decimalLongitude = as.numeric(lapply(str_extract_all(as.character(nisim_df$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 2)))

    nisims_final <- nisims_df %>%
      select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, scientificName, ObsDate, ObsYear)


    df_list[['nisims']] <<- nisims_final



}

#Note: must pull USDA codes for species list. NISIMS only records species with four-letter/number codes
Parse_NISIMS_Large <- function(){

  nisims_df <- data.frame(nisims_db[which(nisims_db$SCNTFC_CD %in% sp_df$acc_usda_code | nisims_db$SCNTFC_CD %in% sp_df$syn_usda_code),])

  # Format the NISIMS data and split polygon center point field into lat/long
  nisims_parse <- as.data.frame(nisims_db) %>%
    filter(SCNTFC_CD %in% sp_df$acc_usda_code | SCNTFC_CD %in% sp_df$syn_usda_code) %>%
    mutate(DataSet = "NISIMS"
           ,albersLatitude = as.numeric(vapply(strsplit(as.character(CNTR_PT_CN),","), '[', 1, FUN.VALUE = character(1)))         
           ,albersLongitude = as.numeric(vapply(strsplit(as.character(CNTR_PT_CN),","), '[', 2, FUN.VALUE = character(1)))
           ,scientificName = as.character(s$acc_base[match(nisims_df$SCNTFC_CD, sp_df$acc_usda_code)])
           ,ObsDate = as.Date(BEGIN_DT)
           ,ObsYear = format(as.Date(BEGIN_DT), "%Y")
           )
           
  nisims_parse <- as.data.frame(nisims_parse) %>%
    filter(!is.na(albersLongitude))

  # 4.3 - NISIMS center point location data is natively in Albers...
  #       Note: create a simple spatial object based on the center coordinates 
  sf.point <- st_as_sf(x=nisims_parse
                       , coords = c("albersLatitude","albersLongitude")
                       , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

  # 4.4 - create spatial object with coordinates
  nisism_reproj <- sf::st_transform(sf.point, crs = 4326)
  nisim_df <- as.data.frame(nisism_reproj)

  nisims_final <- as.data.frame(nisim_df) %>%
    mutate(decimalLatitude = as.numeric(substr(as.character(nisim_df$geometry), start = 21, stop = 37), options(digits=16)),
           decimalLongitude = as.numeric(substr(as.character(nisim_df$geometry), start = 3, stop = 19), options(digits=16))
           )

  nisims_final <- data.frame(nisims_final[which(!is.na(nisims_final$decimalLatitude) & !is.na(nisims_final$decimalLongitude)),])
  unique(is.na(nisims_final$decimalLatitude))

}





