#-------------------------------------------------------------------------------
# Community shapefiles
# FEMA provides a list of eligible communities but without spatial information
# 
# 1- Read csv with eligible communities
#     --> the csv data can be downloaded here: 
#         https://www.fema.gov/flood-insurance/work-with-nfip/community-status-book
# 2- clean
# 3- Merge with tigris shapefiles
# 
#-------------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(tigris)
library(leaflet)
library(ggspatial)
library(leaflet)
library(mapview)

path_to_files <- "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/eligible_communities/"

#-------------------------------------------------------------------------------
# Read community CSV
#-------------------------------------------------------------------------------

elig_communities <- read.table(paste0(path_to_files, "csvfema_crs_eligible-communities_oct-2020.csv"), sep = ";", dec = ".", stringsAsFactors = FALSE, header = TRUE)[-571,]

#-------------------------------------------------------------------------------
# Towns and Cities in US
#-------------------------------------------------------------------------------


city_boundaries <- places(unique(elig_communities$ï..State)[1])

for (i in 2:length(unique(elig_communities$ï..State))){
  
city_boundaries_2 <- places(unique(elig_communities$ï..State)[i])
city_boundaries <- rbind(city_boundaries,city_boundaries_2 )

}

#-------------------------------------------------------------------------------
# Clean name character
#-------------------------------------------------------------------------------

elig_communities$city_names <- gsub(", City of", "", elig_communities$Community.Name)
elig_communities$city_names <- gsub(", Town of", "", elig_communities$city_names)
elig_communities$city_names <- gsub(", Borough of", "", elig_communities$city_names)
elig_communities$city_names <- gsub(", Village of", "", elig_communities$city_names)
elig_communities$city_names <- trimws(elig_communities$city_names)

#-------------------------------------------------------------------------------
# Extract eligible communities
#-------------------------------------------------------------------------------

city_polygons <-city_boundaries[which(gsub("(.*),.*", "\\1", city_boundaries$NAME) %in% elig_communities$city_names),]

#-------------------------------------------------------------------------------
# Create maps
#-------------------------------------------------------------------------------

leaflet(city_polygons) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", ALAND )(ALAND ),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  mapshot(url = paste0(path_to_files, "map_eligible_communities/map_eligible_communities.html"))

community_map <- 
  ggplot() +
  geom_sf(data = city_polygons)+
  scale_fill_viridis_c() +
  ggtitle("Eligible communities")+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()+
  ggsave(paste0(path_to_files, "map_eligible_communities/map_eligible_communities.png"))
