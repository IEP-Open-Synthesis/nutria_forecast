
# Purpose: download and process vegetation data for the IEP nutria forecasting project
# Author: Vanessa Tobias (vanessadtobias@gmail.com)
# Disclaimer: 
#    This code is part of a draft analysis and it will be updated as the project continues. 
#    This code is shared with no guarantees about its accuracy or usefulness.
# License: CC-BY 2.0


# Load Libraries ####
library(sp)
library(rgdal)
library(sf)
library(raster) #useful functions for extents, projections



# Define Functions ####
dl_zip <- function(loc_url){
  temp <- tempfile() #create a place to put the download
download.file(loc_url, temp) #download the zip file
unzip(temp, exdir = getwd()) #unzips the file and saves the unziped file to your working directory
rm(temp) #delete the temporary file
gc() #clean up memory
}


# GET DATA ####
# Download Data #
# Data used here is from CDFW's VegCAMP program.
# Additional information can be found here: https://www.wildlife.ca.gov/Data/GIS/Vegetation-Data

# Suisun Marsh
dl_zip("ftp://ftp.wildlife.ca.gov/BDB/GIS/BIOS/Public_Datasets/2600_2699/ds2676.zip")
# Delta
dl_zip("ftp://ftp.wildlife.ca.gov/BDB/GIS/BIOS/Public_Datasets/200_299/ds292.zip")
# Central Valley
# This is big - wait to run it until I've gotten the rest of the stuff working
dl_zip("ftp://ftp.wildlife.ca.gov/BDB/GIS/BIOS/Public_Datasets/2600_2699/ds2632.zip")

# Import shapefiles
# subset(ogrDrivers(), grepl("GDB", name))
# print(ogrListLayers("./ds2676.gdb"))
suisun <- readOGR(dsn="./ds2676.gdb",layer="ds2676")  #ds2676.gdb
# summary(suisun)
# plot(suisun)
# print(ogrListLayers("./ds292.gdb"))
delta <- readOGR(dsn="./ds292.gdb",layer="ds292")
# summary(delta)
# summary(delta@data$NVCSMG)
crs(delta)
# CRS arguments:
#   +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000
# +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 

print(ogrListLayers("./v3_final/ds2632.gdb"))
# readOGR makes an error, but st_read works
# valley <- readOGR(dsn="./v3_final/ds2632.gdb",layer="ds2632_ftp",
#                   require_geomType = "wkbPolygon") #this file has some geomType = NA.
valley <- st_read(dsn="./v3_final/ds2632.gdb",layer="ds2632_ftp", type = 3) #type=3 tells it to make polygons
valley <- as(valley, "Spatial") #convert back to sp
# summary(valley)

# CLEAN UP DATA ####
# Make crs the same for all shapefiles
suisun <- spTransform(suisun, crs(delta))
valley <- spTransform(valley, crs(delta))

# Standardize variable names
names(suisun)
names(delta)
names(valley)
names(suisun)[c(1:2, 15:22, 25:30)] <- names(delta)[c(1:2, 15:22, 23, 26, 24, 25, 27, 28)]
# Remove non-matching variables for merging
suisun@data[-c(1:2, 15:22, 25:30)] <- NULL
delta@data[-c(1:2, 15:22, 23, 26, 24, 25, 27, 28)] <- NULL
valley@data[-c(1:2, 22:25, 28:29, 32:33)] <- NULL

# Visualize Data #
# helpful blog: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS3_MakingMaps_part1_mappingVectorData.html
# plot(delta)
# plot(suisun, add = TRUE)


# 0. CHOOSE VEG CLASSES OR MAKE NEW VEG CLASSES THAT ARE USEFUL FOR NUTRIA ####
# or use:
# CWHRType - "CA wildlife habitat relationships" - dominant species in weltands, includes urban & water
#  CWHRCode
# CalVegName - fresh emergent wetland, annual grassland
#  CalVegCode
# NVCSAlliance - alliance
# NVCSGroup - group
# NVCSMG - macrogroup - fresh marsh, vernal pool, etc.
# ===> USE CWHRCode FOR TESTING

# 1. Create a single shapefile #### 
#  because identical(unique(suisun$CWHRCode), unique(delta$CWHRCODE)) = FALSE
veg.pol <- bind(suisun, delta, valley)
extent(veg.pol)

# 2. CONVERT VEG SHAPEFILE TO RASTER ####
# 2.1 Make the extents the same - i.e. the grand min and max of extents for all shapefiles
#  create an empty raster to use as y in rasterize
extent(suisun)
extent(delta)
extent(veg.pol)
# class       : Extent 
# xmin        : -213096.5 
# xmax        : 128190.3 
# ymin        : -345869.4 
# ymax        : 289941.6 

veg.rast <- raster(extent(veg.pol), resolution = 1000) #1000 m pixels

  # raster(xmn = -186817, xmx = -105365.4, #extent matches extent of veg.pol
  #             ymn = -42854.62, ymx = 65115.83,
  #             resolution = 100)
crs(veg.rast) <- crs(delta) #crs has to match for raster and points

# 2.2 Convert veg polygon to raster
veg <- rasterize(veg.pol, veg.rast, field = veg.pol$CWHRCODE)
# image(veg, col = rev(rainbow(length(unique(values(veg))))))

# s.rast <- rasterize(suisun, veg, field = suisun$CWHRCode)
# d.rast <- rasterize(delta, veg, field = delta$CWHRCODE) #why don't names match across shapefiles?!
# 1.3 Combine the rasters into a single veg raster layer
# veg <- raster::merge(d.rast, s.rast) #x and y are rasters with the same origin and resolution. Keeps the values from x if values overlap
# raster::mosaic(x, y) makes cell averages or other computations

#why is the water two different colors?
levels(veg.pol$CWHRCODE)
# from QGIS... rivers = 21 (RIV), suisun bay = 9 (LAC, EST)
# NEED TO RECODE ALL WATER?

writeRaster(veg, "veg.tiff", overwrite = FALSE)
# It's easier to pick out values in QGIS or ArcMap


## 3 LATER - WHEN WE HAVE OTHER DATA TYPES
# 3.1 STACK RASTERS of different types
# raster::stack 
# s <- stack(x) # where x can be a list (then don't supply other arguments)

## 4. EXTRACT VALUES THAT MATCH THE LOCATION OF ANIMALS































