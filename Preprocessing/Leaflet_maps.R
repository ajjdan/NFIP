#-------------------------------------------------------------------------------
# Plot the data created in Combine_datasets as leaflet maps
# 
# 
#-------------------------------------------------------------------------------

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}


library(leaflet)
library(mapview)



#-------------------------------------------------------------------------------
# Flood zones (raster Image) and points for each policy with 
#-------------------------------------------------------------------------------

leaflet_flood_map <- function(flood_zone, state_abbr, census_n_pol_pop, pol_sf){

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(flood_zone),
                    na.color = "transparent")

zones <- c("A" = 1, "A99" =2, "AE" = 3, "AH" = 4, "AO" = 5, "D" = 6, "X" = 7, "V" =8 , "VE" = 9 , "AREA NOT INCLUDED" = 10, "OPEN WATER" = 11)

flood_policy_map <-
  leaflet() %>% addTiles() %>%
  addRasterImage(flood_zone, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(flood_zone), title = "Flood zone", labels = names(zones))  %>%
  addPolylines(data=census_n_pol_pop, weight = 1, color = "black") %>% 
  addCircles(lng = pol_sf$X, lat = pol_sf$Y, weight = 2,
             color = "indianred", 
             stroke = TRUE, opacity = 0.5, fillOpacity = 0.7) #%>%
  #addLegendCustom(colors = c("indianred", "indianred", "indianred"),  
                  #labels = c(min(pol_sf_point_size$n), round(mean(pol_sf_point_size$n),0), max(pol_sf_point_size$n)), sizes = c(5, 10, 20))

if(png == TRUE){
mapshot(flood_policy_map, file = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_flood_leaflet.png"), remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar", "drawToolbar", "easyButton"))}

if(html == TRUE){
mapshot(flood_policy_map, url = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_flood_leaflet.html"))}

}

#-------------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------------

pal_census_policy <- colorQuantile("YlOrRd", census_n_pol_pop$n, n = 8, na.color = "transparent")

# the extra code building off the existing pal
qpal_colors <- unique(pal_census_policy(sort(census_n_pol_pop$n))) # hex codes
qpal_labs <- quantile(na.omit(census_n_pol_pop$n), seq(0, 1, 1/8)) # depends on n from pal
qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA


Census_policy_map <-
  
  leaflet() %>% 
  
  addTiles()  %>%  
  
  addPolygons(data=census_n_pol_pop, stroke = FALSE, fillOpacity = 1, color = ~pal_census_policy(n)
  ) %>%
  
  addLegend(colors = qpal_colors, labels = qpal_labs, opacity = 1) %>% 
  addCircles(lng = pol_sf$X, lat = pol_sf$Y, weight = 2,
             color = "black", 
             stroke = FALSE,fillOpacity = 0.7) 


mapshot(Census_policy_map, file = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_census_leaflet.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                            "drawToolbar", "easyButton")) 

mapshot(Census_policy_map, url = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_census_leaflet.html"))

#-------------------------------------------------------------------------------
# 
#-------------------------------------------------------------------------------

pal_census_policy <- colorQuantile("YlOrRd", census_ratio$ratio, n = 8, na.color = "transparent")

# the extra code building off the existing pal
qpal_colors <- unique(pal_census_policy(sort(census_ratio$ratio))) # hex codes
qpal_labs <- quantile(na.omit(census_ratio$ratio), seq(0, 1, 1/8)) # depends on n from pal
qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA


Ratio_policy_map <-
  
  leaflet() %>% 
  
  addTiles()  %>%  
  
  addPolygons(data=census_ratio, stroke = FALSE, fillOpacity = 1, color = ~pal_census_policy(ratio)
  ) %>%
  
  addLegend(colors = qpal_colors, labels = qpal_labs, opacity = 1) %>% 
  addCircles(lng = pol_sf$X, lat = pol_sf$Y, weight = 2,
             color = "black", 
             stroke = FALSE,fillOpacity = 0.7) 


mapshot(Ratio_policy_map, file = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_ratio_leaflet.png"),
        remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                            "drawToolbar", "easyButton")) 
mapshot(Ratio_policy_map, url = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/",state_abbr[i],"_ratio_leaflet.html"))
