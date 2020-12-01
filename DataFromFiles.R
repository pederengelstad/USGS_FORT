#a function to quickly and cleanly grab data from non-API sources
#  right now, it's currently requested to not filter AIM and LMF for NA dates

#to do: verify that all points from NISIMS are being transformed properly.

# require('tidyverse')
# require('sf')
# require('sqldf')
# require('lubridate')


AddDataFromFiles = function(code_list = NULL,
                            sp_df = NULL,
                            aim_file_loc = NULL, 
                            nisims_nps_file_loc = NULL, 
                            nisims_blm_file_loc = NULL, 
                            calflora_file_loc = NULL, 
                            imap_file_loc = NULL){
  
  
  decLong.tmp = function(x){
    tryCatch(
      as.numeric(stringr::str_extract_all(as.character(x), "(-?\\d+\\.+\\d+)")[2]),
      error = function(e){NA}
    )
  }
  
  decLat.tmp = function(x){
    tryCatch(
      as.numeric(stringr::str_extract_all(as.character(x), "(-?\\d+\\.+\\d+)")[1]),
      error = function(e){NA}
    )
  }
  
  #check settings to make sure the necessary steps have been completed  
  if(!is.null(c(aim_file_loc, nisims_nps_file_loc, nisims_nps_file_loc))){
    
    sp_df_exp = as.data.frame(sp_df) %>% 
      tidyr::separate(usda_codes, into = paste("V", 1:10), sep = ',')
    
    if(is.null(sp_df$usda_codes)){
      print("Re-run species processing with USDA=T!"); stop()
    }
    
  }  
  
  df_list = list()  
  
  # 1. Parse AIM/LMF data
  if(!is.null(aim_file_loc)){
    
    # load in data from file location
    aim_data <- vroom::vroom(aim_file_loc)
    
    #filter data for those species found in search list and add columns to facilitate merging with other occurrence datasets
    if(!(any(code_list %in% unique(aim_data$code)) == F)){
      
      aim_parse <- aim_data %>%
        dplyr::filter(code %in% code_list) %>%
        dplyr::mutate(DataSet = 'AIM_LMF',
                      decimalLatitude = as.numeric(NAD83.Y),
                      decimalLongitude = as.numeric(NAD83.X),
                      ObsDate = VisitDate,
                      ObsYear = as.integer(VisitYear),
                      pct.cover = prop.cover,
                      usda_name = code)
      
      aim_parse <- sqldf::sqldf('SELECT DISTINCT aim_parse.*, ITISacceptedName FROM aim_parse
             LEFT JOIN sp_df_exp ON (usda_name = sp_df_exp."V 1")
                OR (usda_name = sp_df_exp."V 2")
                OR (usda_name = sp_df_exp."V 3")
                OR (usda_name = sp_df_exp."V 4")
                OR (usda_name = sp_df_exp."V 5")
                OR (usda_name = sp_df_exp."V 6")') %>%
        dplyr::mutate(source_sp_name = ITISacceptedName) %>%
        dplyr::select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, usda_name, pct.cover) %>%
        unique()
      
      # convert from NAD83 to WGS84
      n <- nrow(aim_parse)
      
      if(n > 0){
        
        sf.point_aim <- sf::st_as_sf(x = aim_parse
                                     , coords = c("decimalLatitude", "decimalLongitude")
                                     , crs = 4269)
        
        aim_reproj <- sf::st_transform(sf.point_aim, crs = 4326)
        
        aim_df <- as.data.frame(aim_reproj) %>%
          mutate(decimalLatitude = as.numeric(map(geometry, decLat.tmp)),
                 decimalLongitude = as.numeric(map(geometry, decLong.tmp)))
        
        aim_final <- aim_df %>%
          select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, usda_name, ObsDate, ObsYear, pct.cover) %>%
          unique()
        
        df_list[['BLM_LMF']] <- aim_final
        
      }
      
    }
    
  }
  
  
  ###########################################
  #parse NISIMS NPS Data
  ###########################################
  
  if(!is.null(nisims_nps_file_loc)){
    
    NISIMS_NPS <- vroom::vroom(nisims_nps_file_loc)
    
    if(any(code_list %in% unique(NISIMS_NPS$SCNTFC_CD)) == T){
      
      NPS_PARSE <- NISIMS_NPS %>%
        select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT, EST_CVR_RT)%>%
        filter(CNTR_PT_CN != '' & SCNTFC_CD %in% code_list & BEGIN_DT >= startdate & BEGIN_DT <= enddate) %>%
        mutate(DataSet = "NISIMS_NPS"
               ,albersLatitude = as.numeric(stringr::str_extract(CNTR_PT_CN, pattern = "(?<=X: ).*(?= Y)"))         
               ,albersLongitude = as.numeric(stringr::str_extract(CNTR_PT_CN, pattern = "(?<= Y: ).*"))
               ,usda_name = SCNTFC_CD
               ,ObsDate = as.Date(BEGIN_DT)
               ,ObsYear = as.integer(format(as.Date(BEGIN_DT), "%Y"))) %>%
        filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
        mutate(pct.cover = EST_CVR_RT)
      
      
      NPS_PARSE = sqldf::sqldf('SELECT DISTINCT * FROM NPS_PARSE
       LEFT JOIN sp_df_exp ON (usda_name = sp_df_exp."V 1") 
          OR (usda_name = sp_df_exp."V 2")
          OR (usda_name = sp_df_exp."V 3")
          OR (usda_name = sp_df_exp."V 4")
          OR (usda_name = sp_df_exp."V 5")
          OR (usda_name = sp_df_exp."V 6")
      ') %>%
        dplyr::mutate(source_sp_name = ITISacceptedName) %>%
        dplyr::select(DataSet, albersLatitude, albersLongitude, usda_name, ObsDate, ObsYear, source_sp_name, pct.cover) %>%
        unique()
      
      n <- nrow(NPS_PARSE)
      
      if(n > 0){
        
        sf.point <- sf::st_as_sf(x=NPS_PARSE, 
                                 coords = c("albersLatitude","albersLongitude"), 
                                 crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
        
        nisims_reproj_NPS <- sf::st_transform(sf.point, crs = 4326)
        
        nisims_df_NPS <- as.data.frame(nisims_reproj_NPS) %>%
          dplyr::mutate(decimalLongitude = as.numeric(purrr::map(geometry, decLat.tmp)),
                 decimalLatitude = as.numeric(purrr::map(geometry, decLong.tmp)))
        
        nisims_final_NPS <- nisims_df_NPS %>%
          dplyr::select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, usda_name, ObsDate, ObsYear, pct.cover) %>%
          dplyr::mutate(pct.cover = ifelse(pct.cover > 1, pct.cover/100, pct.cover)) %>%
          unique()
        
        df_list[['nisims_nps']] <- nisims_final_NPS  
      }
    }
  }
  
  ####################################################################################
  # parse BLM NISIMS records, which format the center point field slightly differently
  ####################################################################################
  
  if(!is.null(nisims_blm_file_loc)){
    
    NISIMS_BLM <- vroom::vroom(nisims_blm_file_loc) 
    
    if(any(code_list %in% unique(NISIMS_BLM$SCNTFC_CD)) == T){
      
      N_BLM_PARSE <- NISIMS_BLM %>%
        dplyr::select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT, EST_CVR_RT)%>%
        dplyr::filter(CNTR_PT_CN != '' & SCNTFC_CD %in% code_list & BEGIN_DT >= startdate & BEGIN_DT <= enddate) %>%
        dplyr::mutate(DataSet = "NISIMS_BLM"
               ,albersLatitude = as.numeric(stringr::str_extract(CNTR_PT_CN, '([^,]*)'))         
               ,albersLongitude = as.numeric(stringr::str_extract(CNTR_PT_CN, '(?<=,).*'))
               ,usda_name = SCNTFC_CD
               ,ObsDate = as.Date(BEGIN_DT)
               ,ObsYear = as.integer(format(as.Date(BEGIN_DT), "%Y"))) %>%
        dplyr::filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
        dplyr::mutate(pct.cover = EST_CVR_RT)
      
      N_BLM_PARSE <- sqldf::sqldf('SELECT DISTINCT * FROM N_BLM_PARSE
       LEFT JOIN sp_df_exp ON (usda_name = sp_df_exp."V 1") 
          OR (usda_name = sp_df_exp."V 2")
          OR (usda_name = sp_df_exp."V 3")
          OR (usda_name = sp_df_exp."V 4")
          OR (usda_name = sp_df_exp."V 5")
          OR (usda_name = sp_df_exp."V 6")
      ') %>%
        mutate(source_sp_name = ITISacceptedName) %>%
        select(DataSet, albersLatitude, albersLongitude, source_sp_name, ObsDate, ObsYear, usda_name, pct.cover) %>%
        unique()
      
      n <- nrow(N_BLM_PARSE)
      
      if(n > 0){
        
        sf.point_BLM <- sf::st_as_sf(x = N_BLM_PARSE
                                 , coords = c("albersLatitude","albersLongitude")
                                 , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
        
        nisims_reproj_BLM <- sf::st_transform(sf.point_BLM, crs = 4326)
        
        nisims_df_BLM <- as.data.frame(nisims_reproj_BLM) %>%
          dplyr::mutate(decimalLongitude = as.numeric(purrr::map(geometry, decLat.tmp)),
                 decimalLatitude = as.numeric(purrr::map(geometry, decLong.tmp)))
        
        nisims_final_BLM <- nisims_df_BLM %>%
          dplyr::select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, usda_name, ObsDate, ObsYear, pct.cover) %>%
          dplyr::mutate(pct.cover = ifelse(pct.cover > 1, pct.cover/100, pct.cover)) %>%
          unique()
        
        df_list[['nisims_blm']] <- nisims_final_BLM 
      }      
    }  
  }
  
  
  # Because the download process is so manual for CalFlora and iMapInvasive data, we're assuming that
  # the species selected are already desired. Even so, non-matching results are still compared to the sp_df
  # object created in the species_processing() function.
  
  if(!is.null(calflora_file_loc)){
    
    cal.raw <- vroom::vroom(calflora_file_loc, header = T, stringsAsFactors = F)
    cal.df <- cal.raw %>%
      dplyr::filter(Location.Quality == 'high' & !grepl('var', Taxon)) %>%
      dplyr::mutate(DataSet = 'CalFlora',
             decimalLatitude = as.numeric(Latitude),
             decimalLongitude = as.numeric(Longitude),               
             ObsDate = as.Date(lubridate::parse_date_time(Date, orders = 'mdy')),
             ObsYear = as.integer(format(ObsDate, "%Y")),
             source_sp_name = Hmisc::capitalize(tolower(Taxon))) %>%
      dplyr::select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name,
             Observer, Source, County, Location.Quality)
    
    df_list[['calflora']] <- cal.df 
    
    
  }
  
  
  if(!is.null(imap_file_loc)){
    
    imap.raw <- vroom::vroom(imap_file_loc, header = T, stringsAsFactors = F)
    imap.df <- imap.raw %>%
      dplyr::mutate(scientific_name = ifelse(grepl(';',scientific_name), gsub(';','', stringr::word(scientific_name, 1, 2, ' ')), scientific_name)) %>%
      dplyr::rename(Observer = observer,
             County = county) %>%
      dplyr::mutate(DataSet = 'iMapInvasives',
             decimalLatitude = as.numeric(y),
             decimalLongitude = as.numeric(x),               
             ObsDate = as.Date(lubridate::parse_date_time(observation_date, orders = 'dmy')),
             ObsYear = as.integer(format(ObsDate, "%Y")),
             source_sp_name = scientific_name) %>%
      dplyr::select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name,
             Observer, organization_name, jurisdiction, management_area)
    
    df_list[['iMapInvasives']] <- imap.df 
    
    
  }
  
  return(df_list)
  
}