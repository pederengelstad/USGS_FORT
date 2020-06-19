#a function to quickly and cleanly grab data from non-API sources
#  right now, it's currently requested to not filter AIM and LMF for NA dates

#to do: verify that all points from NISIMS are being transformed properly.

require('tidyverse')
require('sf')
require('sqldf')
require('lubridate')


AddDataFromFiles = function(aim_file_loc = NULL
                            , lmf_file_loc = NULL
                            , nisims_nps_file_loc = NULL
                            , nisims_blm_file_loc = NULL
                            , calflora_file_loc = NULL
                            , imap_file_loc = NULL){
  
  
  decLong.tmp = function(x){
    tryCatch(
      as.numeric(str_extract_all(as.character(x), "(-?\\d+\\.+\\d+)")[2]),
      error = function(e){NA}
    )
  }
  
  decLat.tmp = function(x){
    tryCatch(
      as.numeric(str_extract_all(as.character(x), "(-?\\d+\\.+\\d+)")[1]),
      error = function(e){NA}
    )
  }
  
  #check settings to make sure the necessary steps have been completed
  df_list = list()  
  
  if(!is.null(c(aim_file_loc, lmf_file_loc, nisims_nps_file_loc, nisims_nps_file_loc))){
    
    sp_df_exp = as.data.frame(sp_df) %>% separate(usda_codes, into = paste("V", 1:10), sep = ',')
    
    if(is.null(sp_df$usda_codes)){
      print("Re-run species processing with USDA=T!"); stop()
    }
    
    #before anything gets processed, all these parsing lines NEED this
    code_list <- sort(unique(unlist(str_extract_all(str_flatten( c(na.omit(sp_df$usda_codes)), collapse = ', '), boundary('word')))))
  }  
  #parse AIM Data
  if(!is.null(aim_file_loc)){
    
    # load in AIM data from file location
    aim_data = read.csv(aim_file_loc, header = T, stringsAsFactors = F)
    
    #filter data for those species found in search list and add columns to facilitate merging with other occurrence datasets
    if(!(any(code_list %in% unique(aim_data$code)) == F)){
      
      aim_parse = aim_data %>%
        filter(code %in% code_list) %>%
        mutate(DataSet = 'BLM_AIM',
               decimalLatitude = as.numeric(Latitude),
               decimalLongitude = as.numeric(Longitude),
               Date = ifelse(is.na(UseDate.y), as.character(enddate), UseDate.y),
               ObsYear = as.integer(VisitYear),
               usda_name = code) %>%
        mutate(ObsDate = as.Date(Date)) %>%
        mutate(pct.cover = prop.cover)
      
      aim_parse = sqldf('SELECT DISTINCT aim_parse.*, ITISacceptedName FROM aim_parse
       LEFT JOIN sp_df_exp ON (usda_name = sp_df_exp."V 1") 
          OR (usda_name = sp_df_exp."V 2")
          OR (usda_name = sp_df_exp."V 3")
          OR (usda_name = sp_df_exp."V 4")
          OR (usda_name = sp_df_exp."V 5")
          OR (usda_name = sp_df_exp."V 6")') %>%
                mutate(source_sp_name = ITISacceptedName) %>%
                select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name, usda_name, pct.cover) %>%
                unique()
      
      df_list[['BLM_AIM']] <- aim_parse
    }
  }
  
  ##################################################
  #parse LMF Data
  ##################################################
  
  if(!is.null(lmf_file_loc)){
    
    # load in LMF data from file location
    lmf_data = read.csv(lmf_file_loc, header = T, stringsAsFactors = F)
    
    #filter data for those species found in search list and add columns to facilitate merging with other occurrence datasets
    if(any(code_list %in% unique(lmf_data$code)) == T){
      
      lmf_parse <- lmf_data %>%
        filter(code %in% code_list) %>%
        mutate(DataSet = 'BLM_LMF',
               decimalLatitude_NAD83 = as.numeric(NAD83.Y),
               decimalLongitude_NAD83 = as.numeric(NAD83.X),
               ObsDate = as.Date(VisitDate),
               ObsYear = as.integer(VisitYear),
               usda_name = code) %>%
        mutate(pct.cover = prop.cover)
      
      lmf_parse = sqldf('SELECT DISTINCT * FROM lmf_parse
        LEFT JOIN sp_df_exp ON (usda_name = sp_df_exp."V 1") 
          OR (usda_name = sp_df_exp."V 2")
          OR (usda_name = sp_df_exp."V 3")
          OR (usda_name = sp_df_exp."V 4")
          OR (usda_name = sp_df_exp."V 5")
          OR (usda_name = sp_df_exp."V 6")      
        ') %>%
        mutate(source_sp_name = ITISacceptedName) %>%
        select(DataSet, decimalLatitude_NAD83, decimalLongitude_NAD83, source_sp_name, ObsDate, ObsYear, usda_name, pct.cover) %>%
        unique()
      
      # convert from NAD83 to WGS84
      n = nrow(lmf_parse)
      
      if(n > 0){
        
        sf.point_lmf <- st_as_sf(x = lmf_parse
                                 , coords = c("decimalLatitude_NAD83", "decimalLongitude_NAD83")
                                 , crs = 4269)
        
        lmf_reproj <- sf::st_transform(sf.point_lmf, crs = 4326)
        
        lmf_df <- as.data.frame(lmf_reproj) %>%
          mutate(decimalLatitude = as.numeric(map(geometry, decLat.tmp)),
                 decimalLongitude = as.numeric(map(geometry, decLong.tmp)))
                 
        lmf_final <- lmf_df %>%
          select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, usda_name, ObsDate, ObsYear, pct.cover) %>%
          unique()
        
        df_list[['BLM_LMF']] <- lmf_final  
      } 
    }  
  }
  
  ###########################################
  #parse NISIMS NPS Data
  ###########################################
  
  if(!is.null(nisims_nps_file_loc)){
    
    NISIMS_NPS <- read.csv(nisims_nps_file_loc, header = T, stringsAsFactors = F)
    
    if(any(code_list %in% unique(NISIMS_NPS$SCNTFC_CD)) == T){
      
      NPS_PARSE <- NISIMS_NPS %>%
        select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT, EST_CVR_RT)%>%
        filter(CNTR_PT_CN != '' & SCNTFC_CD %in% code_list & BEGIN_DT >= startdate & BEGIN_DT <= enddate) %>%
        mutate(DataSet = "NISIMS_NPS"
               ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=X: ).*(?= Y)"))         
               ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<= Y: ).*"))
               ,usda_name = SCNTFC_CD
               ,ObsDate = as.Date(BEGIN_DT)
               ,ObsYear = as.integer(format(as.Date(BEGIN_DT), "%Y"))) %>%
        filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
        mutate(pct.cover = EST_CVR_RT)
      
      
      NPS_PARSE = sqldf('SELECT DISTINCT * FROM NPS_PARSE
       LEFT JOIN sp_df_exp ON (usda_name = sp_df_exp."V 1") 
          OR (usda_name = sp_df_exp."V 2")
          OR (usda_name = sp_df_exp."V 3")
          OR (usda_name = sp_df_exp."V 4")
          OR (usda_name = sp_df_exp."V 5")
          OR (usda_name = sp_df_exp."V 6")
      ') %>%
        mutate(source_sp_name = ITISacceptedName) %>%
        select(DataSet, albersLatitude, albersLongitude, usda_name, ObsDate, ObsYear, source_sp_name, pct.cover) %>%
        unique()
      
      n <- nrow(NPS_PARSE)
      
      if(n > 0){
        
        sf.point <- st_as_sf(x=NPS_PARSE
                             , coords = c("albersLatitude","albersLongitude")
                             , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
        
        nisims_reproj_NPS <- sf::st_transform(sf.point, crs = 4326)
        
        nisims_df_NPS <- as.data.frame(nisims_reproj_NPS) %>%
          mutate(decimalLongitude = as.numeric(map(geometry, decLat.tmp)),
                 decimalLatitude = as.numeric(map(geometry, decLong.tmp)))
        
        nisims_final_NPS <- nisims_df_NPS %>%
          select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, usda_name, ObsDate, ObsYear, pct.cover) %>%
          mutate(pct.cover = ifelse(pct.cover > 1, pct.cover/100, pct.cover)) %>%
          unique()
        
        df_list[['nisims_nps']] <- nisims_final_NPS  
      }
    }
  }
  
  ####################################################################################
  # parse BLM NISIMS records, which format the center point field slightly differently
  ####################################################################################
  
  if(!is.null(nisims_blm_file_loc)){
    
    NISIMS_BLM <- read.csv(nisims_blm_file_loc, header = T, stringsAsFactors = F) 
    
    if(any(code_list %in% unique(NISIMS_BLM$SCNTFC_CD)) == T){
      
      N_BLM_PARSE <- NISIMS_BLM %>%
        select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT, EST_CVR_RT)%>%
        filter(CNTR_PT_CN != '' & SCNTFC_CD %in% code_list & BEGIN_DT >= startdate & BEGIN_DT <= enddate) %>%
        mutate(DataSet = "NISIMS_BLM"
               ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, '([^,]*)'))         
               ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, '(?<=,).*'))
               ,usda_name = SCNTFC_CD
               ,ObsDate = as.Date(BEGIN_DT)
               ,ObsYear = as.integer(format(as.Date(BEGIN_DT), "%Y"))) %>%
        filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
        mutate(pct.cover = EST_CVR_RT)
      
      N_BLM_PARSE = sqldf('SELECT DISTINCT * FROM N_BLM_PARSE
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
      
      n = nrow(N_BLM_PARSE)
      
      if(n > 0){
        
        sf.point_BLM <- st_as_sf(x = N_BLM_PARSE
                                 , coords = c("albersLatitude","albersLongitude")
                                 , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
        
        nisims_reproj_BLM <- st_transform(sf.point_BLM, crs = 4326)
        
        nisims_df_BLM <- as.data.frame(nisims_reproj_BLM) %>%
          mutate(decimalLongitude = as.numeric(map(geometry, decLat.tmp)),
                 decimalLatitude = as.numeric(map(geometry, decLong.tmp)))
        
        nisims_final_BLM <- nisims_df_BLM %>%
          select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, usda_name, ObsDate, ObsYear, pct.cover) %>%
          mutate(pct.cover = ifelse(pct.cover > 1, pct.cover/100, pct.cover)) %>%
          unique()
        
        df_list[['nisims_blm']] <- nisims_final_BLM 
      }      
    }  
  }
  
  
  # Because the download process is so manual for CalFlora and iMapInvasive data, we're assuming that
  # the species selected are already desired. Even so, non-matching results are still compared to the sp_df
  # object created in the species_processing() function.
  
  if(!is.null(calflora_file_loc)){
    
    cal.raw = read.csv(calflora_file_loc, header = T, stringsAsFactors = F)
    cal.df = cal.raw %>%
      filter(Location.Quality == 'high' & !grepl('var', Taxon)) %>%
      mutate(DataSet = 'CalFlora',
             decimalLatitude = as.numeric(Latitude),
             decimalLongitude = as.numeric(Longitude),               
             ObsDate = as.Date(parse_date_time(Date, orders = 'mdy')),
             ObsYear = as.integer(format(ObsDate, "%Y")),
             source_sp_name = Hmisc::capitalize(tolower(Taxon))) %>%
      select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name,
             Observer, Source, County, Location.Quality)
    
    df_list[['calflora']] <- cal.df 
    
    
  }
  
  
  if(!is.null(imap_file_loc)){
    
    imap.raw = read.csv(imap_file_loc, header = T, stringsAsFactors = F)
    imap.df = imap.raw %>%
      mutate(scientific_name = ifelse(grepl(';',scientific_name), gsub(';','',word(scientific_name, 1, 2, ' ')), scientific_name)) %>%
      rename(Observer = observer,
             County = county) %>%
      mutate(DataSet = 'iMapInvasives',
             decimalLatitude = as.numeric(y),
             decimalLongitude = as.numeric(x),               
             ObsDate = as.Date(parse_date_time(observation_date, orders = 'dmy')),
             ObsYear = as.integer(format(ObsDate, "%Y")),
             source_sp_name = scientific_name) %>%
      select(DataSet, decimalLatitude, decimalLongitude, ObsDate, ObsYear, source_sp_name,
             Observer, organization_name, jurisdiction, management_area)
    
    df_list[['iMapInvasives']] <- imap.df 
    
    
  }
  
  return(df_list)
  
}