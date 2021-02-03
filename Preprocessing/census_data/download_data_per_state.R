#-------------------------------------------------------------------------------
# Download all variables from the 5 year 2016-2018 community survey
# 1- Get variables
# 2- load tracts for each state
# 3- save locally as shapefile
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Packages
#-------------------------------------------------------------------------------

library(tidycensus)
library(sf)
library(USAboundaries)

#-------------------------------------------------------------------------------
# Initialize
#-------------------------------------------------------------------------------

# Initiate census API
census_api_key(" ")

# List of state names and abbreviations to iterate/select 
usstates <- us_states()
state_abbr_census <- usstates$state_abb

#-------------------------------------------------------------------------------
# function download and save
#-------------------------------------------------------------------------------

download_all_acs5_variables <- function(state_abbreviations){
  
  # get all available variables
  variable_names <- load_variables(2018,"acs5")
  
  census_data <- get_acs(geography = "tract",
                         variables = variable_names$name,
                         state = state_abbreviations,
                         survey = "acs5",
                         year = 2018,
                         geometry = TRUE,
                         cb = FALSE,
                         output = "wide") %>% 
                  st_transform(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  st_write(census_data,paste0("C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/census_data/acs_tract_shapefiles/acs5_2018_", state_abbreviations, ".shp") , driver = "ESRI Shapefile", append=FALSE )

}

#-------------------------------------------------------------------------------
# Apply function for each state
#-------------------------------------------------------------------------------

lapply(state_abbr_census, download_all_acs5_variables)
