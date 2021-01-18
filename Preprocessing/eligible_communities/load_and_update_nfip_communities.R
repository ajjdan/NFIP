#-------------------------------------------------------------------------------
# NFIP Communities
# Use the ArcGIS Rest API to download community borders in QGIS
#"https://services.arcgis.com/XG15cJAlne2vxtgt/ArcGIS/rest/services/CRC_Community_Layers_May_2017/FeatureServer/3"
# FEMA provides a list of eligible communities but without spatial information
# 
# 1- Read csv with eligible communities
#     --> the csv data can be downloaded here: 
#         https://www.fema.gov/flood-insurance/work-with-nfip/community-status-book
# 2- Read polygons from Esri REST service
#     --> Update eligiblity wit community status book
# 3- Include eligibility for community rating system (Still in progress)
#

#-------------------------------------------------------------------------------

library("sf")
library("tidyverse")
library("lubridate")

path_to_files <- "C:/Users/veigel/Documents/GitHub/NFIP/Preprocessing/eligible_communities"

#-------------------------------------------------------------------------------
# FEMA Community status book
#-------------------------------------------------------------------------------

eligible_communities <- read.csv(paste0(path_to_files,"/community_status_book/com_status_book.csv"), header = TRUE, sep = ";", na.strings = c("NA", "", " "))

# remove empty columns

eligible_communities <- eligible_communities[which(!is.na(eligible_communities$ï..CID)),]
eligible_communities <- eligible_communities[which(!(eligible_communities$ï..CID == "CID")),]

# clean and format date column

eligible_communities$elig_date <- mdy(eligible_communities$Init.FIRMIdentified)

# Clean Community Names
eligible_communities$J_CIS_NAME <- trimws(eligible_communities$Community.Name)
eligible_communities$J_CID <- gsub("[^[:digit:]]", "", eligible_communities$ï..CID )

eligible_communities$El_2020 <- "Y"

# Select relevant columns
eligible_communities <- eligible_communities[,c(2,3,9,10,11,12)]

#-------------------------------------------------------------------------------
# NFIP Communities from ArcGIS REST service polygons
#-------------------------------------------------------------------------------

community_polygons <- st_read(paste0(path_to_files,"/eligible_communities/eligible_communities_2017.shp"))
community_polygons$J_CIS_NAME <-trimws(community_polygons$J_CIS_NAME)

#-------------------------------------------------------------------------------
# Combine Via name
#-------------------------------------------------------------------------------

community_merge <- merge(community_polygons, eligible_communities, by = "J_CID", all.x = TRUE)

no_match <- cbind(community_merge$J_CIS_NAME.y[which(!(community_merge$J_CIS_NAME.y == community_merge$J_CIS_NAME.x))],
community_merge$J_CIS_NAME.x[which(!(community_merge$J_CIS_NAME.y == community_merge$J_CIS_NAME.x))])

names(community_merge) <-  c("J_CID" , "OBJECTID"  , "J_AREA_ID"  ,  "J_STATEFP1"  ,   "J_COUNTYFP"  , "J_PLACEFP1"   ,  "J_GEOID10"   , "J_NAME10"  ,   "J_NAMELSAD"  , "J_LSAD10" , "J_REGION"   ,  "CIS_NAMEX",   "J_CIS_COMM",   "J_POP10"  ,    "J_SOURCE"  ,   "CID"   ,  "Community"   ,   "ST" , "CountyX"  ,   "CRS_CLASS"  ,  "Participat" ,    "SHAPE_Leng" ,  "Shape__Are"  ,   "Shape__Len"  ,   "CommunityName" ,"CountyY"  ,   "elig_date"   ,   "CIS_NAMEY"  , "El_2020"   , "geometry" ) 


st_write(community_merge,paste0(path_to_files,"/eligible_communities/community_merge.shp"), driver = "ESRI Shapefile", append = FALSE )
#-------------------------------------------------------------------------------
# Read community rating system CSV
#-------------------------------------------------------------------------------

elig_communities <- read.table(paste0(path_to_files, "csvfema_crs_eligible-communities_oct-2020.csv"), sep = ";", dec = ".", stringsAsFactors = FALSE, header = TRUE)[-571,]