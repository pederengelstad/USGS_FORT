source("./USGS/Scripts/R/Required/DataFromFiles.R")

LoadNISIMS(type = 'db', filepath = './USGS/Data/NISIMS/USGS_presence_absence/USGS_presence_absence/NISIMS_Presences_toFY16.gdb', layername = 'WeedInfestationData')

usda_match <- sort(unique(unlist(str_extract_all(sp_df$usda_codes,'\\w+'))))

nisims_parse <- as.data.frame(nisims_db) %>%
  select(SCNTFC_CD, CNTR_PT_CN, BEGIN_DT) %>%
  filter(!is.na(CNTR_PT_CN) & as.character(SCNTFC_CD) %in% usda_match) %>%
  mutate(DataSet = "NISIMS"
         ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=X: )(-?\\d+\\.+\\d+)"))         
         ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=Y: )(-?\\d+\\.+\\d+)"))
         ,source_sp_name = as.character(SCNTFC_CD)
         ,scientificName = sp_df$ITISacceptedName[match(SCNTFC_CD,unique(unlist(str_extract_all(sp_df$usda_codes,'\\w+'))))]
         ,ObsDate = as.Date(BEGIN_DT)
         ,ObsYear = format(as.Date(BEGIN_DT), "%Y")
         )

nisims_parse <- nisims_parse %>% 
  filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
  select(DataSet, albersLatitude, albersLongitude, source_sp_name, scientificName, ObsDate, ObsYear)

sf.point <- st_as_sf(x=nisims_parse
                     , coords = c("albersLatitude","albersLongitude")
                     , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

nisims_reproj <- sf::st_transform(sf.point, crs = 4326)

nisim_df <- as.data.frame(nisims_reproj) %>%
  mutate(decimalLatitude = as.numeric(lapply(str_extract_all(as.character(nisim_df$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 1)),
         decimalLongitude = as.numeric(lapply(str_extract_all(as.character(nisim_df$geometry), "(-?\\d+\\.+\\d+)"), `[[`, 2))
          )

nisims_final <- nisims_df %>%
  select(DataSet, decimalLatitude, decimalLongitude, source_sp_name, scientificName, ObsDate, ObsYear)

nisims_final

df_list[['nisims']] <- nisims_final




#############################
# giant BLM DB (take forever to load)
#############################
LoadNISIMS(type = 'file', filepath = 'USGS//Data/NISIMS/WeedInfestationData_Large.csv')

nisims_parse <- as.data.frame(nisims_db) %>%
  filter(!is.na(CNTR_PT_CN) & (SCNTFC_CD %in% sp_df$usda_codes)) %>%
  mutate(DataSet = "NISIMS"
         ,albersLatitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=X: )(-?\\d+\\.+\\d+)"))         
         ,albersLongitude = as.numeric(str_extract(CNTR_PT_CN, pattern = "(?<=Y: )(-?\\d+\\.+\\d+)"))
         ,scientificName = as.character(sp_df$ITISacceptedName[which(nisims_db$SCNTFC_CD)])
         ,ObsDate = as.Date(BEGIN_DT)
         ,ObsYear = format(as.Date(BEGIN_DT), "%Y")
  )
nisims_parse <- nisims_parse %>% 
  filter(!is.na(albersLongitude) | !is.na(albersLongitude)) %>%
  select(DataSet, albersLatitude, albersLongitude, scientificName, ObsDate, ObsYear)

sf.point <- st_as_sf(x=nisims_parse
                     , coords = c("albersLatitude","albersLongitude")
                     , crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

nisim_reproj <- sf::st_transform(sf.point, crs = 4326)
nisim_df <- as.data.frame(nisism_reproj)
nisim_df

nisims_final <- as.data.frame(nisim_df) %>%
  mutate(decimalLatitude = as.numeric(substr(as.character(nisim_df$geometry), start = 21, stop = 37), options(digits=16)),
         decimalLongitude = as.numeric(substr(as.character(nisim_df$geometry), start = 3, stop = 19), options(digits=16))
  )