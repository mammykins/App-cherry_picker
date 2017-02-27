
# What is global.R? -------------------------------------------------------

# a script executed before app launches
# the objects generated here can be used both in user and sever
# these are actions that can be done once per session
# such as library calls, data source loading and custom function sourcing


# PACKAGES ---------------------------------------------------------------

library(leaflet)
library(RColorBrewer)
library(sp)
library(tidyverse)
library(rgdal)
library(testthat)
library(xtable)
library(DT)
# library(checkpoint)
# checkpoint("2016-12-28")  # ymd

# DATA INPUT -----------------------------------------------------------------

# LA POLYGON --------------------------------------------------------------

# Get polygon for LA of interest boundary
## load LA shapefile (to identify LA boundaries)
la_s <- readOGR("data/.", "England_LA_2009a", verbose = FALSE)
## convert LA shapefile coordinates to lat/long
la_s_ll <- spTransform(la_s, CRS("+init=epsg:4326"))

# QA ----------------------------------------------------------------------
expect_equal(length(la_s_ll@data$LEA_NAME), 152,
             info = "there are 152 LA, do we have data for all?")

# SCHOOL COORD, SCAP and Surplus land data ----------------------------------
school_locations <- read_rds("data/school_locations.rds")
apples <- read_rds("data/apples_data.rds")
pears <- read_rds("data/pears_data.rds")


# JOIN DATA ---------------------------------------------------------------
fruits <- dplyr::left_join(apples, pears)


# MAPPING SETUP -----------------------------------------------------------

# Variables for holding the coordinate system types (see: # http://www.epsg.org/ for details) 
ukgrid <- "+init=epsg:27700" 
latlong <- "+init=epsg:4326"

# Create coordinates variable
coords <- dplyr::select(fruits, easting, northing)
# Create the SpatialPointsDataFrame, note coords and data are distinct slots in S4 object 
surplus_sp <- sp::SpatialPointsDataFrame(coords,
                                         data = dplyr::select(fruits, -easting, -northing),
                                         proj4string = CRS("+init=epsg:27700"))

# CONVERT TO LONG & LAT ---------------------------------------------------
# Convert from Eastings and Northings to Latitude and Longitude
surplus_sp_ll <- spTransform(surplus_sp, CRS(latlong))
# we also need to rename the columns
colnames(surplus_sp_ll@coords)[colnames(surplus_sp_ll@coords) == "easting"] <- "longitude" 
colnames(surplus_sp_ll@coords)[colnames(surplus_sp_ll@coords) == "northing"] <- "latitude"

# With the data in place, the user, via the app, can select the appropriate LA of interest to filter for


# EFFICIENCY --------------------------------------------------------------
la_code_list_sorted <- sort(unique(school_locations$la_number))
# USER Friendly labels for drop down list
la_user_friendly_list <- unique(select(school_locations, la_number, la)) %>%
  mutate(la_combined = paste(la_number, la, sep = " - ")) %>%
  select(la_combined) %>%
  as.vector()


# POPUP DIAGNOSIS ---------------------------------------------------------
# Useful if your Geography is bad
# 1 for longitude, 2 for latitude
make_popup_vector_from_numeric <- function(la_polygon, long1_or_lat2) {
  stopifnot(long1_or_lat2 == 1 | 2)
  stopifnot(class(la_polygon[[1]]) == "Polygons")
  output <- numeric(length = 152)
  
  for (i in 1:152) {
    output[i] <- la_s_ll@polygons[[i]]@labpt[long1_or_lat2]
  }
  return(output)
}


# TEST --------------------------------------------------------------------
# make_popup_vector_from_numeric(la_s_ll@polygons, 1)
# as.character(la_s_ll@data$LEA_NAME)
