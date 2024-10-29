### ACMT Simplified ####

## Set up ####
## install package ###
## Install necessary packages
list.of.packages <- c('httr', 'tidyverse', 'jsonlite', 'sf', 'tidycensus', 'geosphere', 'stringi', 'dplyr', 'units', 'raster', 'reshape2', 
                      'tigris', 'lwgeom', 'janitor', 'remotes', 'USAboundaries')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 

# if USAboundaries doesn't install (only installs on older versions of R)
if(!('USAboundaries' %in% installed.packages()[,"Package"])){
  remotes::install_version("USAboundaries", "0.4.0")}

## Read in state & county planes
state_plane_zones <- sf::st_read(dsn="ACMT", layer="spcszn83")
counties <- sf::st_read(dsn="ACMT", layer="cb_2017_us_county_500k")
counties <- st_transform(counties, 4326)


## Set standard state plane zones (state_proj from USA Bounaries file) ####
## standard_zone same as the ZONE column in the state_plane_zone file ####
state_proj$standard_zone <- state_proj$zone
state_proj$standard_zone[state_proj$zone == "west"] <- "W"
state_proj$standard_zone[state_proj$zone == "east"] <- "E"
state_proj$standard_zone[state_proj$zone == "central"] <- "C"
state_proj$standard_zone[state_proj$zone == "north"] <- "N"
state_proj$standard_zone[state_proj$zone == "south"] <- "S"
state_proj$standard_zone[state_proj$zone == "island"] <- "I"
state_proj$standard_zone[state_proj$zone == "mainland"] <- "M"
state_proj$standard_zone[state_proj$zone == "long island"] <- "LI"
state_proj$standard_zone[state_proj$zone == "north central"] <- "NC"
state_proj$standard_zone[state_proj$zone == "south central"] <- "SC"
state_proj$standard_zone[state_proj$zone == "east central"] <- "EC"
state_proj$standard_zone[state_proj$zone == "west central"] <- "WC"
state_proj$ZONE <- paste(state_proj$state, state_proj$standard_zone, sep="_")
state_proj$ZONE[is.na(state_proj$zone)] <- state_proj$state[is.na(state_proj$zone)]
state_proj$ZONE

## Test ####
## test lat / long (UW)
lat<-47.654400
long<--122.311707

# get projection geometry for lat long point -- find intersection of state plane zone and point geometry
get_projection_for_lat_long <- function(long, lat) {
  point <- st_sfc(st_point(c(long, lat)), crs=4326)
  state_plane_zones %>%
    filter(st_contains(state_plane_zones, point, sparse = F) == 1) %>%
    left_join(state_proj, by="ZONE") %>%
    {.} -> selected_zone
  if (nrow(selected_zone) == 0) {
    search_factor <- 1
    while (nrow(selected_zone) == 0) {
      point <- st_sfc(st_point(c(long+runif(1, -0.1*search_factor, 0.1*search_factor),
                                 lat+runif(1, -0.1*search_factor, 0.1*search_factor))), crs=4326)
      state_plane_zones %>%
        filter(st_contains(state_plane_zones, point, sparse = F) == 1) %>%
        left_join(state_proj, by="ZONE") %>%
        {.} -> selected_zone
      search_factor <- search_factor + 1
    }
  }
  return(selected_zone$proj4_string)
}

# get buffer geometry
get_point_buffer_for_lat_long <- function(long, lat, radius_meters) {
  proj4_string <- get_projection_for_lat_long(long, lat)
  point <- st_sfc(st_point(c(long, lat)), crs=4326)
  point_projected <- st_transform(point, proj4_string)
  radius <- set_units(radius_meters, "meters")
  point_buffer <- st_buffer(point_projected, dist=radius)
  point_buffer <- st_transform(point_buffer, crs=4326)
  return(point_buffer)
}

# Get all census tract geometries for intersecting counties
state_list <- list()
get_geometries_of_a_county <- function(state, county, 
                                       year=2017, geoid_type="Census Tract", 
                                       use_lower_resolution_geo_data=FALSE) {
  if (!geoid_type %in% c("Census Tract", "Block Group")) {
    stop("Unsupported GEOID type")
  }
  print("called get_statecounty_tracts")
  if (as.numeric(state) < 0 || as.numeric(state) > 55) { message(sprintf("error!  Unknown state %s", state)) }
  if (as.numeric(county) < 0 || as.numeric(county) > 1000) { message(sprintf("error!  Unknown county %s", county)) }
  state_counties <- state_list[[state]]
  if (is.null(state_counties)) {
    state_counties <- list()
  }
  tracts_without_water <- state_counties[[county]]
  if (is.null(tracts_without_water)) {
    print(sprintf("Looking up tracts for state %s , county %s", state, county))
    
    tracts <- NULL
    print(year)
    print(geoid_type)
    if (geoid_type == "Census Tract") {
      tracts <- st_as_sf(tracts(state = state, county = county, year=year, cb=use_lower_resolution_geo_data))
    } else if (geoid_type == "Block Group") {
      tracts <- st_as_sf(block_groups(state = state, county = county, year=year, cb=use_lower_resolution_geo_data))
    } else {
      stop("Unsupported GEOID type")
    }
    
    # try to get water area for this year
    water <- NULL
    tryCatch({
      water <- st_union(st_as_sf(area_water(state = state, county = county, year=year)))
    }, error = function (condition) {
      if (condition$message == "area_water is not currently available for years prior to 2011.  To request this feature,\n                   file an issue at https://github.com/walkerke/tigris."){
        warning(paste0("Water area not available for years prior to 2011. The requested year is ", as.character(year)))
      } else {
        stop("Unknown error in getting water area")
      }
    })
    if (!is.null(water)) {
      tracts <- st_difference(tracts, water)  # if has water,  substract the water from tracts
    }
    
    state_counties[[county]] <- tracts
  }
  state_list[[state]] <- state_counties
  return(tracts)
}

get_count_variable_for_lat_long <- function(long, lat, radius_meters, acs_var_names=NULL, year=year, external_data=NULL, 
                                            geoid_type = "Census Tract", 
                                            fill_missing_GEOID_with_zero=FALSE, use_lower_resolution_geo_data=FALSE, variable_name_to_interpolate_by_sum_boolean_mapping=NULL, return_point_estimate=FALSE, custom_buffer=NULL) {  # count_results might not have the variable measures for all GEOIDs in census tracts, in that case, use 0 for the measure; if this is not done, the returned result will be NA
  
  if (is.null(variable_name_to_interpolate_by_sum_boolean_mapping)) {
    stop("Function get_count_variable_for_lat_long is not provided with variable_name_to_interpolate_by_sum_boolean_mapping")
  }
  
  # check for if asked to handle external data
  using_external_data <- FALSE
  names_of_interested_variables <- acs_var_names   # variable names we want to output
  if (is.null(acs_var_names) &&
      ##is.null(year) && 
      !is.null(external_data)) {  # when no acs_var_names, year, but have external_data
    using_external_data <- TRUE
    names_of_interested_variables <- unique(external_data$variable)
  }
  
  # find counties intersecting the point buffer
  if(is.null(custom_buffer)) {
    point_buffer <- get_point_buffer_for_lat_long(long=long, lat=lat, radius_meters=radius_meters)
  } else {
    point_buffer <- custom_buffer
  }
  if(return_point_estimate&&!is.null(custom_buffer)) {
    stop("Please either get measure for the custom buffer or get point estimate for a lat/long")
  }
  index_of_intersecting_counties <- st_intersects(point_buffer, counties)
  if (return_point_estimate) {  # TODO return_point_estimate: might not need this. Speed up by just looking for one county
    point_buffer <- get_point_buffer_for_lat_long(long, lat, 1)
    index_of_intersecting_counties <- st_intersects(point_buffer, counties)
    stopifnot("When asking for point estimate, only the lat/long will be used for locating the county, thus there should be one intersecting county only" = length(index_of_intersecting_counties)==1)
  }
  if (length(index_of_intersecting_counties) < 1) {
    message("get_count_variable_for_lat_long error: buffer does not overlap US counties")
  }
  intersecting_counties_fips <- unique(as.character(counties$GEOID[index_of_intersecting_counties[[1]]]))
  print(intersecting_counties_fips)
  intersecting_counties_fips_state_codes <- substr(intersecting_counties_fips, 1, 2)
  intersecting_counties_fips_county_codes <- substr(intersecting_counties_fips, 3, 5)
  
  # get measures and geometries to prepare for interpolatation
  geoid_and_columns_of_variable_value_to_geometry_dataframe_list <- list()
  for (i in seq_along(intersecting_counties_fips)) {
    geoid_to_geometry_dataframe <- get_geometries_of_a_county(state=intersecting_counties_fips_state_codes[i], county=intersecting_counties_fips_county_codes[i], 
                                                              year=year, ## Added year designation so that it doesn't use the default year
                                                              geoid_type=geoid_type, use_lower_resolution_geo_data=use_lower_resolution_geo_data) %>% st_make_valid()
    
    geoid_to_variable_name_to_variable_value_dataframe <- NA
    
    if (using_external_data) {
      geoid_to_variable_name_to_variable_value_dataframe <- external_data
    } else {
      # Census API throws intermittent errors with old years.  Add a retry mechanism to try to track it down
      tries <- 0
      while (length(geoid_to_variable_name_to_variable_value_dataframe) == 1 && is.na(geoid_to_variable_name_to_variable_value_dataframe) && tries < 10) {  # the first condition ensures the loop breaks without executing is.na (such that no warning is made)
        tries <- tries + 1
        try(
          geoid_to_variable_name_to_variable_value_dataframe <- get_acs_results_for_available_variables(
            acs_var_names=names_of_interested_variables,
            state=intersecting_counties_fips_state_codes[i],
            county=intersecting_counties_fips_county_codes[i],
            year=year)
        )
      }
      if (length(unique(geoid_to_variable_name_to_variable_value_dataframe$variable)) < length(names_of_interested_variables)) {   # if missing variables were pruned, update names_of_interested_variables to let it only include the available variables
        names_of_interested_variables <- names_of_interested_variables[names_of_interested_variables %in% geoid_to_variable_name_to_variable_value_dataframe$variable]  # not assigning acs_results$variable directly to names_of_interested_variables because although they are the same, the order of variables is different due to calling get_acs; in short, we want to keep the order of the variables to pass tests
      }
    }
    
    geoid_to_variable_name_to_variable_value_dataframe$estimate[is.na(geoid_to_variable_name_to_variable_value_dataframe$estimate)] <- 0
    geoid_to_columns_of_variable_value_dataframe <- dcast(geoid_to_variable_name_to_variable_value_dataframe, GEOID ~ variable, value.var="estimate" )
    
    columns_of_variable_value_to_geometry_dataframe <- left_join(x=geoid_to_geometry_dataframe, y=geoid_to_columns_of_variable_value_dataframe, by="GEOID")  # GEOID is used for matching features to geometries; thus in ACMT, only the GEOID between census tract and features should match
    #columns_of_variable_value_to_geometry_dataframe_list[[i]]  <- columns_of_variable_value_to_geometry_dataframe[, names_of_interested_variables]
    geoid_and_columns_of_variable_value_to_geometry_dataframe_list[[i]]  <- columns_of_variable_value_to_geometry_dataframe[, c("GEOID", names_of_interested_variables)] # TODO include geoid
  }
  
  if (length(geoid_and_columns_of_variable_value_to_geometry_dataframe_list) < 1) {
    message("get_count_variable_for_lat_long: No block group data returned from census")
  }
  # debug start TODO just to introduce a version of dataframe that contains geoid
  all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe <- do.call(rbind, geoid_and_columns_of_variable_value_to_geometry_dataframe_list)
  all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe <- st_transform(all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe, 4326)
  all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe[, names_of_interested_variables]
  # debug end
  
  #all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- do.call(rbind, columns_of_variable_value_to_geometry_dataframe_list)
  #all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe <- st_transform(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe, 4326)
  if (fill_missing_GEOID_with_zero) {
    all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[is.na(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe)] <- 0
  }
  
  for (variable_name in names_of_interested_variables) {
    if (is.na(variable_name_to_interpolate_by_sum_boolean_mapping[variable_name])) {
      stop(paste("Interploation by sum boolean for", variable_name, "does not exist."))
    }
  }
  
  # interpolate measures from the point buffer
  interpolate_one_varaible <- function(variable_name, variable_name_to_interpolate_by_sum_boolean_mapping) {
    suppressWarnings(st_interpolate_aw(all_intersecting_counties_columns_of_variable_value_to_geometry_dataframe[, variable_name], point_buffer, extensive=variable_name_to_interpolate_by_sum_boolean_mapping[variable_name])[[variable_name]]) # e.g. population is extensive, population density is non-extensive (intensive)
  }
  values_of_interested_variables <- lapply(names_of_interested_variables, FUN = interpolate_one_varaible, variable_name_to_interpolate_by_sum_boolean_mapping)
  if (return_point_estimate) {  # TODO overwrite values_of_interested_variables
    # get geoid for the lat/long
    geoid_for_lat_long <- get_geoid_for_lat_long_annonymous(lat = lat, lon = long, geoid_type = geoid_type)
    # get variable_to_value_dataframe for the GEOID
    variable_to_value_dataframe_for_the_geoid <- all_intersecting_counties_geoid_and_columns_of_variable_value_to_geometry_dataframe %>%
      filter(GEOID==geoid_for_lat_long) %>%
      st_drop_geometry() %>%
      dplyr::select(-GEOID)
    values_of_interested_variables_for_the_geoid <- vector("list", length(names_of_interested_variables))
    names(values_of_interested_variables_for_the_geoid) <- names_of_interested_variables
    # extract the values and re-assign values_of_interested_variables
    for (name_of_variable in names_of_interested_variables) {
      values_of_interested_variables_for_the_geoid[[name_of_variable]] <- variable_to_value_dataframe_for_the_geoid[[name_of_variable]]
    }
    values_of_interested_variables <- values_of_interested_variables_for_the_geoid
  }
  return(data.frame(name=names_of_interested_variables, estimate=unlist(values_of_interested_variables)))
}

## Function to get measure estimates ####
get_acmt_standard_array <- function(lat, long, radius_meters, year=year, external_data_name_to_info_list=external_data_to_info_list, 
                                    fill_missing_GEOID_with_zero=FALSE, use_lower_resolution_geo_data=TRUE, return_point_estimate=FALSE, custom_buffer=NULL
) {
  # check input validity
  if (is.na(long) | is.na(lat)) {stop("Null lat or long passed to get_acmt_standard_array")}
  
  #create a blank dataframe 
    context_measurement_dataframe<-data.frame(matrix(nrow=0, ncol=2)) 
    colnames(context_measurement_dataframe)<-c('names', 'values')

  # interpolate external dataset measures
  if(!is.null(external_data_name_to_info_list)) {
    external_data_list <- load_external_data(external_data_name_to_info_list)
    
    variable_to_value_dataframe_list <- list()
    for (external_data_name in names(external_data_name_to_info_list)) {
      external_data_weighted_over_point_buffer_dataframe <- get_count_variable_for_lat_long(long=long, lat=lat, radius_meters=radius_meters,
                                                                                            year=year,
                                                                                            external_data=external_data_list[[external_data_name]], geoid_type = external_data_name_to_info_list[[external_data_name]]$geoid_type, 
                                                                                            fill_missing_GEOID_with_zero=fill_missing_GEOID_with_zero, use_lower_resolution_geo_data=use_lower_resolution_geo_data, 
                                                                                            variable_name_to_interpolate_by_sum_boolean_mapping=external_data_name_to_info_list[[external_data_name]]$variable_name_to_interpolate_by_sum_boolean_mapping, 
                                                                                            return_point_estimate=return_point_estimate, custom_buffer=custom_buffer)
      variable_to_value_dataframe <- data.frame(names=external_data_weighted_over_point_buffer_dataframe$name, values=external_data_weighted_over_point_buffer_dataframe$estimate)
      variable_to_value_dataframe_list[[external_data_name]] <- variable_to_value_dataframe
    }
    external_data_measurement_dataframe <- do.call(rbind, variable_to_value_dataframe_list)
    
    
    context_measurement_dataframe <- rbind(context_measurement_dataframe, external_data_measurement_dataframe)
  }
  
  return(context_measurement_dataframe)
}


# function to load data
# the working directory should be
load_external_data <- function (external_data_name_to_info_list=NULL) {
  if (!dir.exists("external_data")) {
    dir.create("external_data")
  }

  vector_of_external_data_names <- names(external_data_name_to_info_list)
  list_of_loaded_data_dataframe <- c()

  vector_of_existing_file_names <- list.files("external_data")

  for (external_data_name in vector_of_external_data_names) {
    vector_of_expected_downloaded_file_name <- external_data_name_to_info_list[[external_data_name]]$vector_of_expected_downloaded_file_name

    expected_processed_file_name <- external_data_name_to_info_list[[external_data_name]]$expected_processed_file_name
    if(is.null(expected_processed_file_name)) {  # when expected_processed_file_name not specified in the info list
      expected_processed_file_name <- paste0("processed_", external_data_name, ".csv")
    }

    file_downloading_completed <- is_empty(setdiff(vector_of_expected_downloaded_file_name, vector_of_existing_file_names))  # could have multiple downloaded files
    file_processing_completed <- expected_processed_file_name %in% vector_of_existing_file_names  # could only have one processed file

    download_file <- external_data_name_to_info_list[[external_data_name]]$download_file
    process_file <- external_data_name_to_info_list[[external_data_name]]$process_file

    if (file_processing_completed) {

    } else {
      if (file_downloading_completed) {
        process_file()
      } else {
        download_file()
        process_file()
      }
    }

    list_of_loaded_data_dataframe[[external_data_name]] <- read_csv(paste0("external_data/", expected_processed_file_name), col_types=cols('GEOID'=col_character()))
  }
  return(list_of_loaded_data_dataframe)
}



## Park serve functions ##
get_distance_to_shapefile <- function(lat, long, radius_meters, shp_processed){
  park_shp <- shp_processed
  loc <- get_point_buffer_for_lat_long(long=long, lat=lat, radius_meters)
  area_intersect <- st_intersection(park_shp, loc)
  if (nrow(area_intersect) == 0){
    return(NA)
  }
  long <- c(long)
  lat <- c(lat)
  lonlat <- data.frame(cbind(long, lat))
  point = st_as_sf(lonlat, coords=c("long", "lat"), crs=4326)
  dist <- st_distance(point, area_intersect, by_element = TRUE)
  return(min(dist))
}

# The function to calculate the proportion of park area within the circle centered at (lat, long) point with radius = radius_meter
get_proportion_in_shapefile <- function(lat, long, radius_meters, shp_processed){
  park_shp <- shp_processed
  loc <- get_point_buffer_for_lat_long(long=long, lat=lat, radius_meters)
  area_intersect <- st_intersection(park_shp, loc)
  proportion <- sum(st_area(area_intersect))/st_area(loc)
  return(proportion)
}

## function to create measures dataframe
create_dataset<-function(variable_list=variable_list){
  var.cols<-data.frame(matrix(ncol=length(variable_list)))#make a columns for each variable in the list
  colnames(var.cols)<-variable_list #name the columns
  dataset<-var.cols%>%mutate(id=NA, year=NA, radius=NA)%>%filter(!is.na(id))#add the columns to the dataset
  return(dataset)
}
