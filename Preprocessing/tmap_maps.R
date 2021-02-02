#-------------------------------------------------------------------------------
# Plot development of eligible communities
# 
# 
#-------------------------------------------------------------------------------


library(tmap)


for (year_select in unique(community_sf_yes$elig_year)) {
  
  community_sf_yes_year <- community_sf_yes[which(community_sf_yes$elig_year <= year_select),]
  community_sf_yes_year_2 <- community_sf_yes[which(community_sf_yes$elig_year == year_select),]
  
  us_states_map <-  tm_shape(state_outline, projection = 2163) + tm_polygons() + 
    tm_layout(frame = FALSE)+ 
    tm_shape(community_sf_yes_year) + 
    tm_polygons(col ="#0C2C84" )+
    tm_borders("black", lwd = 0.5)+
    tm_layout(year_select)+
    tm_credits(paste0("Number of new communities", "\n in ", state_names[i], ": " , nrow(community_sf_yes_year_2)), position=c("left", "bottom"))
  # tm_facets(along = "elig_year", free.coords = FALSE, drop.units = FALSE)
  
  tmap_save(us_states_map, filename = paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/eligibility_year/",state_names[i],"/", year_select, ".png"))
  
}

library(magick)
library(magrittr)

anim_pngs <- list.files(path= paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/eligibility_year/",state_names[i]), pattern = '*.png', full.names = TRUE)  
  anim_pngs[seq(from = 1, to = length(anim_pngs), by = 2)] %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=5,loop = 1) %>% # animates, can opt for number of loops
  image_write(paste0("C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/maps/eligibility_year/",state_names[i],"/FileName.gif")) # write to current dir
  