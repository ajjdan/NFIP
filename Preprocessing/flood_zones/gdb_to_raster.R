#-------------------------------------------------------------------------------
# Pre-processing flood Zone data
# Optimized for Large dataset that exceeds RAM capacity
# Flood zone information were downloaded per state as geodatabase
#
# 1- Read the length of the attribute table 
# 2- Split the number of objects in 3 and read them seperately
#
# 3- repair geometries of the polygons
# 4- dissolve flood zones
# 5- write raster
# 
#-------------------------------------------------------------------------------

library(sf)
library(gdalUtils)
library(rgdal)
library(tidyverse)
library(tigris)
library(data.table)
library(raster)
library(rasterDT)


#-------------------------------------------------------------------------------
# Find folder and geodatabase names
#-------------------------------------------------------------------------------

path_to_files <- "C:/Users/veigel/Documents/Data/FEMA/floodplains/"

# Load states from tigris shapefiles for double-checking
states <- states(cb = TRUE)
state_names <- states$NAME

floodmap_folders <- list.dirs(path_to_files, full.names = FALSE)

floodmap_gdb_names <- floodmap_folders[c(which(grepl("*.gdb",floodmap_folders)))] # select geodatabeses

floodmap_folders <- floodmap_folders[-c(which(grepl("*.gdb",floodmap_folders)))] #remove geodatabases from folder list
floodmap_folders <- na.omit(floodmap_folders[-c(which(!(floodmap_folders %in% state_names)))]) #remove wrong names in Folders


#-------------------------------------------------------------------------------
# Rasterize and save
#-------------------------------------------------------------------------------

st <- Sys.time()
for (i in 1:length(floodmap_gdb_names)) {
  i <- length(floodmap_gdb_names)
  ## load only objectid for subsetting
  prepare_iterations <- st_drop_geometry(sf::st_read(
    dsn = paste0(path_to_files, floodmap_gdb_names[i]), 
    layer = "S_FLD_HAZ_AR",
    query = "SELECT OBJECTID FROM S_FLD_HAZ_AR"))

  rd_st <- Sys.time()
  ## read and manipulate half of the polygons
  floodmap <- sf::st_read(
    dsn = paste0(path_to_files, floodmap_gdb_names[i]), 
    layer = "S_FLD_HAZ_AR",
    query = paste0("SELECT FLD_ZONE FROM S_FLD_HAZ_AR WHERE OBJECTID < ", ceiling(nrow(prepare_iterations)/3))
  )
  floodmap <- st_cast(floodmap, "MULTIPOLYGON")
  
  floodmap$FLD_ZONE <- recode(floodmap$FLD_ZONE, "A" = 1, "A99" =2, "AE" = 3, "AH" = 4, "AO" = 5, "D" = 6, "X" = 7, "V" =8 , "VE" = 9 , "AREA NOT INCLUDED" = 10, "OPEN WATER" = 11 )
  
  r <- raster::raster(extent(floodmap), resolution = c(0.001, 0.001), crs = crs(floodmap))
  r <- fasterizeDT(floodmap, r, field = "FLD_ZONE")
  
  ## export raster
  writeRaster(r,paste0(path_to_files,"preprocessed/", floodmap_folders[i], ".tif"), format="GTiff", overwrite=TRUE)
  
  print("Time after 1/3 of polygons ")
  print(Sys.time()-rd_st)
  print(paste0(" Estimated time for ", floodmap_folders[i], ((Sys.time()-rd_st))*3 ))
  
  ## read and manipulate second half of the polygons
  floodmap <- sf::st_read(
    dsn = paste0(path_to_files, floodmap_gdb_names[i]), 
    layer = "S_FLD_HAZ_AR",
    query =  paste0("SELECT FLD_ZONE FROM S_FLD_HAZ_AR WHERE OBJECTID >= ", ceiling(nrow(prepare_iterations)/3), " AND OBJECTID < ", 2*ceiling(nrow(prepare_iterations)/3))
  )
  
  floodmap <- st_cast(floodmap, "MULTIPOLYGON")
  
  floodmap$FLD_ZONE <- recode(floodmap$FLD_ZONE, "A" = 1, "A99" =2, "AE" = 3, "AH" = 4, "AO" = 5, "D" = 6, "X" = 7, "V" =8 , "VE" = 9 , "AREA NOT INCLUDED" = 10, "OPEN WATER" = 11)
  
  r <- raster::raster(extent(floodmap), resolution = c(0.001, 0.001), crs = crs(floodmap))
  r <- fasterizeDT(floodmap, r, field = "FLD_ZONE")
  
  ## export raster
  writeRaster(r,paste0(path_to_files,"preprocessed/", floodmap_folders[i], "_2", ".tif"), format="GTiff", overwrite=TRUE)

  print("Time after 2/3 of polygons: ")
  print(Sys.time()-rd_st)
  print(paste0(" Estimated time for ", floodmap_folders[i], ((Sys.time()-rd_st)/2)*3 ))
  
  floodmap <- sf::st_read(
    dsn = paste0(path_to_files, floodmap_gdb_names[i]), 
    layer = "S_FLD_HAZ_AR",
    query =  paste0("SELECT FLD_ZONE FROM S_FLD_HAZ_AR WHERE OBJECTID >= ", 2*ceiling(nrow(prepare_iterations)/3))
  )
  
  floodmap <- st_cast(floodmap, "MULTIPOLYGON")
  
  floodmap$FLD_ZONE <- recode(floodmap$FLD_ZONE, "A" = 1, "A99" =2, "AE" = 3, "AH" = 4, "AO" = 5, "D" = 6, "X" = 7, "V" =8 , "VE" = 9 , "AREA NOT INCLUDED" = 10, "OPEN WATER" = 11)
  
  
  r <- raster::raster(extent(floodmap), resolution = c(0.001, 0.001), crs = crs(floodmap))
  r <- fasterizeDT(floodmap, r, field = "FLD_ZONE")
  
  ## export raster
  writeRaster(r,paste0(path_to_files,"preprocessed/", floodmap_folders[i], "_3", ".tif"), format="GTiff", overwrite=TRUE)
  
  rm(list="floodmap")
  gc()
  
  
  et <- Sys.time()
  print(paste0("Time after all ", floodmap_folders[i],"  polygons: "))
  print(Sys.time()-et)
  
}

#-------------------------------------------------------------------------------
# Combine everything with GDAL 
#-------------------------------------------------------------------------------

rasterfiles <- paste0(path_to_files,"preprocessed/",list.files(paste0(path_to_files,"preprocessed/")))

e <- extent(24.9493,	49.5904,	-125.0011,	-66.9326)
template <- raster(e)
projection(template) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'

writeRaster(template, file=paste0(path_to_files,"preprocessed/flood_zones_USA.tif"), format="GTiff")

mosaic_rasters(gdalfile=rasterfiles,dst_dataset=paste0(path_to_files,"preprocessed/flood_zones_USA.tif"),of="GTiff")


#-------------------------------------------------------------------------------
# Upload Asset
#-------------------------------------------------------------------------------

library(rgee)

# Initialize Earth Engine!
ee_Initialize()

upload_fld_zones <- raster(paste0(path_to_files,"preprocessed/flood_zones_USA.tif"))

assetId <- sprintf("%s/%s",ee_get_assethome(),'flood_zones_USA')

#  Method 2
 ee_stars_02 <- raster_as_ee(
   x = upload_fld_zones,
   overwrite = TRUE,
   assetId = assetId,
   bucket = "rgee_dev"
 )
 
Map$centerObject(ee_stars_02)
Map$addLayer(ee_stars_02, list(min = 0, max = 11))
