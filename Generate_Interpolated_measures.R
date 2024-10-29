### Generate external data estimates ####
# set up directory and filepath
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
filepath<-dirname(rstudioapi::getActiveDocumentContext()$path)

# run functions
source('ACMT_simplified.R')
source('external_data_settings.R')

# import geocoded data -- update with filepath for dataset with lat / long variables)
dataset_geocoded<-read.csv(paste0(filepath, '/dataset_geocoded.csv'))

# run loops to pull data
## 1. Walkability Data ####
# walk settings
external_data_name_to_info_list <- list(
  walkability=external_data_presets_walkability
)

walk_vars <-names(walkability_variable_name_to_interpolate_by_sum_boolean_mapping) 
names_of_variables_to_get<-walk_vars

# create walkability data frame
dataset_walk<-create_dataset(variable_list=walk_vars)
write.csv(dataset_walk, 'dataset_walk.csv', row.names = FALSE)

radius_vector<-c(400, 800)
year<-2019 # only one year of walkability data available
N<-nrow(dataset_geocoded)

# loop to pull walkability data for each participant
for(i in 1:N){
  print(paste0("Currently processing ", i, ' out of ', N))
  #check for interrupted data process:
  id<-dataset_geocoded$id[i]
  latitude<-dataset_geocoded[dataset_geocoded$id==id,]$lat #set lat
  longitude<-dataset_geocoded[dataset_geocoded$id==id,]$long #set long
  
  for(r in 1:length(radius_vector)){
    radius<-radius_vector[r]
    print(radius)
    
    #check for existing data in dataset:
    tryCatch({
      if(length(dataset_walk$id) != 0){
        if(id %in% dataset_walk$id[dataset_walk$year==year & dataset_walk$radius==radius]) next #skip the row if the data is already there
      }
      
      dataset_walk<-read.csv('dataset_walk.csv')%>%dplyr::select(walk_vars[1]:radius)
      suppressMessages(
        suppressWarnings(
          environmental_measures<-get_acmt_standard_array(long=longitude, lat=latitude, radius_meters = radius, year=year, 
                                                          external_data_name_to_info_list=external_data_name_to_info_list, fill_missing_GEOID_with_zero = TRUE)
        )
      )
      walk_measures<-environmental_measures %>% t %>% data.frame %>%row_to_names(row_number = 1)%>%mutate(id=id, year=year, radius=radius)
      
      #combine 
      dataset_walk<-rbind(dataset_walk, walk_measures)
      
      write.csv(dataset_walk, 'dataset_walk.csv', row.names = FALSE)
      
    },error=function(e){cat("ERROR :", conditionMessage(e), "\n")}) #this will print any error messages
  }
}


