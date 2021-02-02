#-------------------------------------------------------------------------------
# Combine all data
# 
# 
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Packages
#-------------------------------------------------------------------------------

library(sf)
library(st)
library(data.table)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(USAboundaries)
library(raster)
library(ggspatial)
library(lubridate)
library(tigris)

#-------------------------------------------------------------------------------
# Initialize
#-------------------------------------------------------------------------------

# List of state names and abbreviations to iterate/select 
usstates <- us_states()
state_abbr <- usstates$state_abbr
state_names <- usstates$state_name
state_fp <- usstates$statefp

# Initial case studies for Georgia and California
state_abbr <- c("GA", "CA")
state_names <- c("Georgia", "California")
state_fp <- c("13", "06")

#for (i in 1:length(state_abbr)){

i <- 1

state_outline <- usstates %>% filter(statefp == state_fp[i])

#-------------------------------------------------------------------------------
# Disaster Data
#-------------------------------------------------------------------------------

disaster_data <- fread("C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/fema_warnings/disaster_declaration/DisasterDeclarationsSummaries.csv",data.table = FALSE) 

# When was this data refreshed at minimum
min(disaster_data$lastRefresh)


# Select disaster Type flood and state
disaster_data <-
  disaster_data %>% 
  filter(incidentType == "Flood") %>% 
  filter(state == state_abbr[i]) 

#-------------------------------------------------------------------------------
# communities and eligibility
# Previously edited according to the 2020 FEMA Community status book in 
# "NFIP/Preprocessing/eligible_communities/load_and_update_nfip_communities.R"
#-------------------------------------------------------------------------------

community_sf <- sf::st_read(
  dsn = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/eligible_communities/eligible_communities/community_merge.shp", 
  query = paste0("SELECT * FROM community_merge WHERE J_STATE=","\'", state_fp[i],"\'")) 

community_sf_yes <-
  community_sf %>% 
  filter(El_2020 == "Y") %>% 
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>% 
  mutate(elig_year = year(elig_dt))


#-------------------------------------------------------------------------------
# Policies Data
#-------------------------------------------------------------------------------

pol_sf <- sf::st_read(
  dsn = "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/policies/policies.shp", 
  query = paste0("SELECT * FROM policies WHERE prprtyS=","\'", state_abbr[i],"\'")) %>% 
  st_set_crs("+proj=longlat +datum=WGS84 +no_defs")  %>% 
  st_as_sf(pol_sf)

# Overlapping points for visualization -----------------------------------------

pol_sf$X <- st_coordinates(pol_sf)[,1]
pol_sf$Y <- st_coordinates(pol_sf)[,2]

pol_sf_point_size <- 
  pol_sf %>% 
  group_by(X,Y) %>% 
  count() %>% 
  st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs")

# pol_n_per_year <- 
#   pol_sf %>% 
#   mutate(effective_year = year(plcyEfD)) %>% 
#   group_by(effective_year) %>% 
#   count()

# Spread points inside anonymization zone of one decimal degree -----------------

# Calculate random offset of X and Y within 0.1 degrees
set.seed(123)
random_offset_y <- runif(n=nrow(pol_sf), min=-0.05, max=0.05)
random_offset_x <- runif(n=nrow(pol_sf), min=-0.05, max=0.05)

pol_sf_dist <- 
  data.frame( "Y" = st_coordinates(pol_sf)[,2] +random_offset_y, "X" = st_coordinates(pol_sf)[,1] + random_offset_x) %>% 
   st_as_sf( coords = c("X", "Y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# Overwrite anonymized coordinates and reformat to sf
pol_sf <- 
  st_as_sf(cbind(st_drop_geometry(pol_sf), pol_sf_dist)) %>%
  mutate(X = pol_sf$X+random_offset_x, Y =  pol_sf$Y+random_offset_y ) %>% 
  st_crop(community_sf_yes)

# Uncomment for plot of deanonymization
# cellsize <- 0.1
# grd_lrg <- st_make_grid(st_as_sfc(st_bbox(pol_sf) + c(-cellsize/2, -cellsize/2,cellsize/2, cellsize/2)), what="polygons", cellsize=cellsize)
# X11()
# plot(grd_lrg)
# plot(st_geometry(pol_sf_dist),pch = 20, add = TRUE)
# plot(st_geometry(pol_sf), col = "red", pch = 20, add = TRUE)

#-------------------------------------------------------------------------------
# Flood zone
# first run "NFIP/Preprocessing/flood_zones/gdb_to_raster.R"
#-------------------------------------------------------------------------------

flood_zone_rasters <- list.files("C:/Users/veigel/Documents/Data/FEMA/floodplains/preprocessed", full.names = TRUE)
flood_zone_rasters  <- flood_zone_rasters[c(which(grepl(paste0("*", state_names[i], "*"),flood_zone_rasters)))] # select names with state

flood_zone <- raster(flood_zone_rasters[1])
flood_zone <- merge(flood_zone,raster(flood_zone_rasters[2]),tolerance = 0.5)
flood_zone <- merge(flood_zone,raster(flood_zone_rasters[3]),tolerance = 0.5)


#-------------------------------------------------------------------------------
# census Tracts
#-------------------------------------------------------------------------------


#B19013_001 Income
#B00001_001 UNWEIGHTED SAMPLE COUNT OF THE POPULATION
#B00002_001 UNWEIGHTED SAMPLE HOUSING UNITS 

# Intersection between polygon and points ---------------------------------

intersection <- st_intersection(x = census_data, y = pol_sf)

int_result <- intersection %>% 
  group_by(GEOID) %>% 
  count()

n_pol <- as.data.frame(int_result)[,-3]

#census_n_pol_pop <- merge(census_data %>% filter(variable == "B00002_001"), n_pol, by.x = "GEOID", by.y = "GEOID", all.x = TRUE, duplicateGeometries = TRUE)

#census_n_pol_income <- st_drop_geometry(merge(census_data %>% filter(variable == "B19013_001"), n_pol, by.x = "GEOID", by.y = "GEOID", all.x = TRUE, duplicateGeometries = TRUE))

census_ratio <-
  census_n_pol_pop %>% 
    #filter(variable == "B00002_001")%>% 
    mutate(ratio = n/estimate) %>%
    merge(census_n_pol_income, by = "GEOID")


#-------------------------------------------------------------------------------
# Export spatial data so far ( for presentation of preliminary results)
#-------------------------------------------------------------------------------

# st_write( community_sf_yes,"C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/community_sf_yes.shp", driver = "ESRI Shapefile", append=FALSE )
# st_write(pol_sf,"C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/pol_sf.shp", driver = "ESRI Shapefile", append=FALSE )
# st_write(census_ratio,"C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/census_n_pol.shp", driver = "ESRI Shapefile", append=FALSE )
# writeRaster(flood_zone, file="C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/flood_zones_GA.tif", format="GTiff")

#-------------------------------------------------------------------------------
# leaflet
#-------------------------------------------------------------------------------

# NEEDS EDITING AS FUNCTIONS!!
#source("C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/Leaflet_maps.R")

#leaflet_flood_map(flood_zone, state_abbr[i])

#-------------------------------------------------------------------------------
# tmap community eligibility
#-------------------------------------------------------------------------------


if (!(file.exists(paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/eligibility_year/",state_names[i],"/", community_sf_yes$elig_year[1], ".png")))) { source("C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/eligibility_development_maps.R")}



#-------------------------------------------------------------------------------
# Graph community eligibility
# OUTSOURCE
#-------------------------------------------------------------------------------

statewide_disaster <- 
  disaster_data %>% 
  filter(designatedArea == "Statewide") %>% 
  mutate(year_incident = year(incidentEndDate))

county_disaster <- 
  disaster_data %>% 
  filter(designatedArea != "Statewide") %>% 
  mutate(year_incident = year(incidentEndDate))

community_sf_plot <- 
  community_sf_yes %>% 
    st_drop_geometry() %>% 
    group_by(elig_year) %>% 
    count() %>% 
    mutate(n_communities = n)

plot_data <- merge(community_sf_plot, pol_n_per_year %>% st_drop_geometry(), by.x = "elig_year", by.y = "effective_year", all = TRUE)

plot_data$n.y <- as.numeric(plot_data$n.y)
plot_data$n_communities <- as.numeric(plot_data$n_communities)

plot_data$nyscaled <- scales::rescale(plot_data$n.y, range(plot_data$n_communities, na.rm = TRUE))

    ggplot(plot_data) +
    ggtitle(state_names[i], subtitle = "Community eligibility, disaster and policy effective dates" )+
    geom_point(aes(x = elig_year, y = n_communities, color="community"),size = 1.5)+
    geom_point(aes(x= elig_year, y = nyscaled, color="policy"),  size = 1.5)+
    geom_line(aes(x= elig_year, y = rep(-1,nrow(plot_data)), color="statewide warning"),  size = 0)+
    geom_line(aes(x= elig_year, y = rep(-1,nrow(plot_data)), color="county warning"),  size = 0)+
    scale_y_continuous(name = "Number of communities becoming eligible", limits = c(0,max(plot_data$n_communities, na.rm = TRUE)+10),expand = c(0,0),
                     sec.axis = sec_axis(~., name = "additional effective insurances", labels = function(b){round(scales::rescale(b, range(plot_data$n.y, na.rm = TRUE)),0) })) +
    geom_vline(xintercept = statewide_disaster$year_incident, colour = "red", linetype = 5)+
    geom_vline(xintercept = county_disaster$year_incident,colour = "orange", linetype = 5)+
    theme_bw() +
    theme(    axis.ticks.y.right = element_line(color = "blue"),
              axis.text.y.right = element_text(color = "blue"),
              axis.title.y.right = element_text(color = "blue"),
              axis.title.x = element_blank())+
      scale_colour_manual(name='', values=c('statewide warning'='red', 'county warning'='orange', "policy" = "blue", "community" = "black"), guide='legend') +
      guides(colour = guide_legend(override.aes = list(linetype=c(0,5,0,5), shape=c(16,NA,16, NA), size = c(2,0.8,2,0.8))))+
      theme(legend.position = "bottom")+
    ggsave(paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_eligibility_development.png"), width = 8, height = 6)

    
  