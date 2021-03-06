#a function to quickly and cleanly grab data from non-API sources
#  right now, it's currently requested to not filter AIM and LMF for NA dates

#to do: verify that all points from NISIMS are being transformed properly.

require('tidyverse')
require('sf')

AddDataFromFiles = function(aim_file_loc = NULL
                            ,lmf_file_loc = NULL
                            ,nisims_nps_file_loc = NULL
                            ,nisims_blm_file_loc = NULL){
  
  #check settings to make sure the necessary steps have been completed
  if(is.null(df_list)){
    print("Make sure to create df_list object!"); stop()
  }
  
  if(is.null(sp_df$usda_codes)){
    print("Re-run species processing with USDA=T!"); stop()
  }
  
  #before anything gets processed, all these parsing lines NEED this
  code_list <- unique(unlist(str_extract_all(str_flatten(sp_df$usda_codes, collapse = ', '), boundary('word'))))
  
  
  #parse AIM Data
  if(!is.null(aim_file_loc)){
    # load in AIM data from file location
    aim_data = read.csv(aim_file_loc, header = T, stringsAsFactors = F)
    
    #filter data for those species found in search list and add columns to facilitate merging with other occurrence datasets
    aim_parse = aim_data %>%
      filter(code %in% code_list) %>%
      mutate(DataSet = 'BLM_AIM',
             decimalLatitude = as.numeric(Latitude),
             decimalLongitude = as.numeric(Longitude),
             Date = ifelse(is.na(UseDate.y), as.character(enddate), UseDate.y),
             ObsYear = as.integer(VisitYear),
             source_sp_name = code) %>%
      mutate(ObsDate = as.Date(Date)) %>%
      rowwise() %>%
      mutate(searched_term = unique(sp_df$ITISacceptedName[str_detect(sp_df$usda_codes, source_sp_name)==T])[1]) %>%
      select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, searched_term) %>%
      unique()
    
    df_list[['BLM_AIM']] <<- aim_parse
  }
  
#####################################################################################
  
  #parse LMF Data
  if(!is.null(lmf_file_loc)){
    # load in LMF data from file location
    lmf_data = read.csv(lmf_file_loc, header = T, stringsAsFactors = F)
    
    #filter data for those species found in search list and add columns to facilitate merging with other occurrence datasets
    lmf_parse = lmf_data %>%
      filter(code %in% code_list) %>%
      mutate(DataSet = 'BLM_LMF',
             decimalLatitude_NAD83 = as.numeric(NAD83.Y),
             decimalLongitude_NAD83 = as.numeric(NAD83.X),
             ObsDate = as.Date(VisitDate),
             ObsYear = as.integer(VisitYear),
             source_sp_name = code) %>%
      rowwise() %>%
      mutate(searched_term = unique(sp_df$ITISacceptedName[str_detect(sp_df$usda_codes, source_sp_name)==T])[1]) %>%
      select(DataSet, decimalLatitude_NAD83, decimalLongitude_NAD83, source_sp_name, ObsDate, ObsYear, searched_term) %>%
      unique()
          
    # convert from NAD83 to WGS84
    n = nrow(lmf_parse)
    if(n > 0){
      
      sf.point_lmf <- st_as_sf(x=lmf_parse
                               , coords = c("decimalLatitude_NAD83","decimalLongitude_NAD83")
                               , crs = 4269)
      
      lmf_reproj <- sf::st_transform(sf.point_lmf, crs = 4326)
      
      lmf_df <- as.data.frame(lmf_reproj) %>%
        mutate(decimalLongitude = as.numeric(lapply(str_extract_all(as.character(lmf_reproj$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 1)),
               decimalLatitude = as.numeric(lapply(str_extract_all(as.character(lmf_reproj$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 2)))
      
      lmf_final <- lmf_df %>%
        select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, searched_term, ObsDate, ObsYear) %>%
        unique()
      
      
      df_list[['BLM_LMF']] <<- lmf_final  
    } else {
      print("No records found in LMF file.")
    }
    
  }



#####################################################################################

  #parse NISIMS NPS Data
  if(!is.null(nisims_nps_file_loc)){
    
    NISIMS_NPS <- read.csv(nisims_nps_file_loc, header = T, stringsAsFactors = F)
    
      # 1. figure out which species of interest are actually included in the NISIMS dataset by matching USDA codes
  # 2. remove data without coordinates
  # 3. separate lat and long (starts in Albers Equal Area); note that there are many ways this regex might not work--look for NAs!

    NPS_PARSE <- NISIMS_NPS %>%
      select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT)%>%
      filter(SCNTFC_CD %in% code_list & BEGIN_DT >= startdate & BEGIN_DT <= enddate) %>%
      mutate(DataSet = "NISIMS_NPS"
             ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=X: ).*(?= Y)"))         
             ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<= Y: ).*"))
             ,source_sp_name = SCNTFC_CD
             ,ObsDate = as.Date(BEGIN_DT)
             ,ObsYear = as.integer(format(as.Date(BEGIN_DT), "%Y"))) %>%
      filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
      rowwise() %>%
      mutate(searched_term = unique(sp_df$ITISacceptedName[str_detect(sp_df$usda_codes, source_sp_name)==T])[1]) %>%
      select(DataSet, albersLatitude, albersLongitude, source_sp_name, ObsDate, ObsYear, searched_term) %>%
      unique()

    n = nrow(NPS_PARSE)
    
    if(n > 0){
      
      sf.point <- st_as_sf(x=NPS_PARSE
                           , coords = c("albersLatitude","albersLongitude")
                           , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
      
      nisims_reproj_NPS <- sf::st_transform(sf.point, crs = 4326)
      
      nisims_df_NPS <- as.data.frame(nisims_reproj_NPS) %>%
        mutate(decimalLongitude = as.numeric(lapply(str_extract_all(as.character(nisims_reproj_NPS$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 1)),
               decimalLatitude = as.numeric(lapply(str_extract_all(as.character(nisims_reproj_NPS$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 2)))
      
      nisims_final_NPS <- nisims_df_NPS %>%
        select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, searched_term, ObsDate, ObsYear) %>%
        unique()
      
      df_list[['nisims_nps']] <<- nisims_final_NPS  
    } else {
      print("No records found in NISIMS_NPS file.")
    }
    
  }

  ################################################################################################################
  #
  # parse BLM NISIMS records, which format the center point field slightly differently
  if(!is.null(nisims_blm_file_loc)){
    
    NISIMS_BLM <- read.csv(nisims_blm_file_loc, header = T, stringsAsFactors = F) 
    
    N_BLM_PARSE <- NISIMS_BLM %>%
      select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT)%>%
      filter(CNTR_PT_CN != '' & SCNTFC_CD %in% code_list & BEGIN_DT >= startdate & BEGIN_DT <= enddate) %>%
      mutate(DataSet = "NISIMS_BLM"
             ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, '([^,]*)'))         
             ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, '(?<=,).*'))
             ,source_sp_name = SCNTFC_CD
             ,ObsDate = as.Date(BEGIN_DT)
             ,ObsYear = as.integer(format(as.Date(BEGIN_DT), "%Y"))) %>%
      filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
      rowwise() %>%
      mutate(searched_term = unique(sp_df$ITISacceptedName[str_detect(sp_df$usda_codes, source_sp_name)==T])[1]) %>%
      select(DataSet, albersLatitude, albersLongitude, source_sp_name, ObsDate, ObsYear, searched_term) %>%
      unique()
    
    n = nrow(N_BLM_PARSE)
    if(n > 0){
      
      sf.point_BLM <- st_as_sf(x=N_BLM_PARSE
                               , coords = c("albersLatitude","albersLongitude")
                               , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
      
      nisims_reproj_BLM <- st_transform(sf.point_BLM, crs = 4326)
      
      nisims_df_BLM <- as.data.frame(nisims_reproj_BLM) %>%
        mutate(decimalLongitude = as.numeric(lapply(str_extract_all(as.character(nisims_reproj_BLM$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 1)),
               decimalLatitude = as.numeric(lapply(str_extract_all(as.character(nisims_reproj_BLM$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 2)))
      
      nisims_final_BLM <- nisims_df_BLM %>%
        select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, searched_term, ObsDate, ObsYear) %>%
        unique()
      
      df_list[['nisims_blm']] <<- nisims_final_BLM 
    } else {
      print("No records found in NISIMS_BLM file.")
    }
  }
}