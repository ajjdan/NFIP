#-------------------------------------------------------------------------------
# Uniformly distribute points
# 
# 
#-------------------------------------------------------------------------------

pol_points <- st_transform(pol_sf_point_size, "+proj=utm +zone=42N +datum=WGS84 +units=km")

pol_buffer <-  st_buffer(pol_points, 5)
#pol_buffer$ID_2 <- 1:nrow(pol_buffer)

rpnt <- st_geometry(pol_points[1,])

# 
# for(i in 2:10){ #nrow(pol_buffer)
#   i <- 3
#   pp <- pol_buffer[which(pol_buffer$ID_2==i),]
#   
#   if(pp$n == 1){pnts <-  st_geometry(pol_points[i,])} else {pnts <- st_sample(x =pp,size =pp$n ,type = "regular" ) } 
#   
#   rpnt <- merge(pnts, rpnt, by = "XY")
#   
# }

pnts <- st_sample(x =pol_buffer[which(pol_buffer$n != 1),],size =pol_buffer$n ,type = "regular",  by_polygon = TRUE )

st_coordinates(pnts)

plot(st_geometry(pnts))
plot(st_geometry(pol_buffer[which(pol_buffer$n != 1),]), add = TRUE)

plot(st_geometry(pol_buffer), col = "azure2")
plot(st_geometry(rpnt))

plot(st_geometry(pol_buffer))
plot(st_geometry(rpnt))

st_write(rpnt,"C:/Users/veigel/Documents/GitHub/NFIP/Study_Georgia_California/GA_dist_pol.shp", driver = "ESRI Shapefile", append=FALSE )

