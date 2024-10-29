## external data settings

### section: National Walkability Index https://edg.epa.gov/metadata/catalog/search/resource/details.page?uuid=%7B251AFDD9-23A7-4068-9B27-A3048A7E6012%7D
## to download data if not in file ()
download_file_walkability <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  default_timeout_duration <- getOption('timeout')  # this download would take a long time
  options(timeout=99999999)
  download.file(url = "ftp://newftp.epa.gov/EPADataCommons/OP/WalkabilityIndex.zip", destfile = "external_data/downloaded_walkability.zip")
  options(timeout=default_timeout_duration)
}

# process downloaded walkability data
process_file_walkability <- function () {  # process the file and name it processed_walkability.csv 

  # get the shape files
  unzip("external_data/downloaded_walkability.zip", exdir="external_data/unzipped_walkability")
  unzip("external_data/unzipped_walkability/Natl_WI.gdb.zip", exdir="external_data/unzipped_walkability")
  
  national_walkability_index_sf <- sf::st_read(dsn="external_data/unzipped_walkability/Natl_WI.gdb", layer="NationalWalkabilityIndex")
  
  # select only the variables of intersts and makes sense in ACMT (can interpolate across census tracts)
  variables_to_select <- c("GEOID10", "COUNTHU10", "TOTPOP10", "HH", "WORKERS", "AC_TOT", "AC_WATER", "AC_LAND", "AC_UNPR", "NatWalkInd")  # GEOID plus the 9 variables
  selected_national_walkability_index_sf <- national_walkability_index_sf[variables_to_select]
  
  # convert to a normal dataframe
  selected_national_walkability_index_dataframe <- selected_national_walkability_index_sf %>%
    as_tibble() %>%
    dplyr::select(-Shape)
  
  # reformat table to match ACMT format
  acmt_formatted_walkability_index_dataframe <- selected_national_walkability_index_dataframe %>%
    gather("variable", "estimate", -GEOID10) %>%
    rename(GEOID=GEOID10)
  
  # contains NA? We checked and this file has no NA
  
  # write to processed_walkability.csv
  write_csv(acmt_formatted_walkability_index_dataframe, "external_data/processed_walkability.csv")
  
}

# Walkability settings
walkability_variable_name_to_interpolate_by_sum_boolean_mapping <- c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  FALSE)
names(walkability_variable_name_to_interpolate_by_sum_boolean_mapping) <- c("COUNTHU10", "TOTPOP10", "HH", "WORKERS", "AC_TOT", "AC_WATER", "AC_LAND", "AC_UNPR", "NatWalkInd")
external_data_presets_walkability <- list(vector_of_expected_downloaded_file_name=c("downloaded_walkability.zip"),  # the files should be downloaded for mrfei
                                          expected_processed_file_name='processed_walkability.csv',
                                          download_file=download_file_walkability,  # function to download file
                                          process_file=process_file_walkability,   # function to process file
                                          geoid_type="Block Group",
                                          variable_name_to_interpolate_by_sum_boolean_mapping=walkability_variable_name_to_interpolate_by_sum_boolean_mapping
)

# section: ParkServe data https://www.tpl.org/parkserve/downloads ####
download_file_park <- function () {  # download the external dataset and give it a name (will use it in creating external_data_name_to_info_list)
  download.file(url = "https://parkserve.tpl.org/downloads/ParkServe_Shapefiles_05042022.zip?_ga=2.103216521.887440371.1664905337-1364699585.1664905337", destfile = "external_data/ParkServe_shp.zip")
}

#run file processing function
process_file_park <- function () {  # unzip the downloaded file and save the target data layer as csv file)
  unzip("external_data/ParkServe_shp.zip", exdir="external_data/ParkServe_shp")
}

shp_directory<-'external_data/ParkServe_shp/ParkServe_Shapefiles_05042022/ParkServe_Parks.shp'

shp_preprocess <- function (shp_directory){
  #"external_data/ParkServe_shp/ParkServe_Shapefiles_05042022/ParkServe_Parks.shp"
  park_shp <- st_read(shp_directory) 
  
  #Identify states
  states_sf <- st_transform(us_states( map_date = NULL, resolution = c("low", "high"), states = NULL), 4326)
  points_sf = st_as_sf(dataset_geocoded%>%filter(!is.na(lat) & !is.na(long)), coords = c("long", "lat"), crs = 4326, agr = "constant")
  states <- as.data.frame( st_join(points_sf, states_sf, join = st_intersects) ) %>% dplyr::select(name, -geometry)%>%unique()%>% drop_na()%>% as.list() 
  
  park_shp<-park_shp%>% filter(Park_State %in% states$name)
  park_shp<-st_transform(park_shp, crs=4326)
  
  #park_shp<-st_make_valid(park_shp) ## gives error
  park_shp<-st_make_valid(park_shp %>% filter(!is.na(st_is_valid(park_shp))))
  
}

## section: NLCD ####
post_process_nlcd<-function(variable_list, prop.nlcd){
  prop.nlcd<-data.frame(prop.nlcd)
  if(nrow(prop.nlcd)==0){
    prop.nlcd<-data.frame(x=NA, Freq=NA)
  }  
  
  environmental_measures<-merge(variable_list, prop.nlcd, by=c('x'), all.x=TRUE)
  environmental_measures[is.na(environmental_measures)]<-0
  return(environmental_measures)
}

  