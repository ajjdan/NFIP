#-------------------------------------------------------------------------------
# 
# 
# 
#-------------------------------------------------------------------------------

library(data.table)
library(tidyverse)
library(tmap)
library(sf)
library(USAboundaries)

warnings_data <- fread("C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/fema_warnings/disaster_declaration/DisasterDeclarationsSummaries.csv",data.table = FALSE) %>% filter(incidentType == "Flood") 

nfipdir <- "C:\\Users\\veigel\\Documents\\Data\\FEMA\\NFIP\\"

pol_sf <- fread(paste0(nfipdir, "policies\\FimaNfipPolicies.csv"), header = TRUE, stringsAsFactors=FALSE, data.table = FALSE ) #%>% drop_na("longitude","latitude") %>%  st_as_sf( coords = c("longitude","latitude"))

pol_sf_year <- pol_sf %>% group_by(year(policyEffectiveDate), propertyState) %>% summarise(sum_policies = n()) %>% ungroup()

names(pol_sf_year) <- c("yearEffectiveDate", "propertyState", "sum_policies", "geometry")

usstates <- us_states()
usstates <- usstates[-which(usstates$name == "Alaska"),]
usstates <- usstates[-which(usstates$name == "Hawaii"),]

warnings_sf <- merge(usstates, warnings_data, by.x = "stusps", by.y = "state", all.y = TRUE, duplicateGeometries = TRUE)
pol_state_sf <- merge(usstates, pol_sf_year, by.x = "stusps", by.y = "propertyState", all.y = TRUE, duplicateGeometries = TRUE)

#-------------------------------------------------------------------------------
# Warnings per state
#-------------------------------------------------------------------------------

#for (year_select in min(unique(warnings_sf$fyDeclared)):max(unique(warnings_sf$fyDeclared))) {
  
  plot_warnings <- warnings_sf %>% 
  #filter(fyDeclared  == year_select) %>% 
  group_by(stusps, fyDeclared) %>% 
  summarise(sum_floods = n()) %>% 
  ungroup()
  
  us_states_map <-  tm_shape(usstates, projection = 2163) + tm_polygons() + 
    tm_layout(frame = TRUE)+ 
    tm_shape(plot_warnings) + 
    tm_polygons(col ="sum_floods")+
    tm_fill(title = "number of floods") +
    #tm_shape(plot_sf) + 
    #tm_dots()+
    #tm_dots(size = "policyCost") +
    tm_facets(along = "fyDeclared", free.coords = FALSE, drop.units = FALSE)
  
  tmap_animation(us_states_map, filename = paste0("C:/Users/veigel/Documents/Data/FEMA/map_animation/", "warnings_animfull", year_select, ".gif"), delay = 50)
  
#}

#-------------------------------------------------------------------------------
# Visualite policies effective Dates
#-------------------------------------------------------------------------------

#for (year_select in min(unique(pol_state_sf$yearEffectiveDate)):max(unique(pol_state_sf$yearEffectiveDate))) {
  
  plot_sf <- pol_state_sf %>% 
    #filter(yearEffectiveDate == year_select)
    mutate(sum_policies_log = log10(sum_policies))
    
  us_states_map <-  tm_shape(usstates, projection = 2163) + tm_polygons() + 
    tm_layout(frame = TRUE)+ 
    tm_shape(plot_sf) + 
    tm_polygons(col ="sum_policies_log")+
    tm_fill(title = "log number of policies") +
    tm_facets(along = "yearEffectiveDate", free.coords = FALSE, drop.units = FALSE)
  
  tmap_animation(us_states_map, filename = paste0("C:/Users/veigel/Documents/Data/FEMA/map_animation/", "policies_animfull", year_select, ".gif"), delay = 50)
  
#}
