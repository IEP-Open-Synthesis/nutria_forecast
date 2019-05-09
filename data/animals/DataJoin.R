# Purpose: join locations of animals with habitat data
# Author: Vanessa Tobias (vanessadtobias@gmail.com)
# Disclaimer: 
#    This code is part of a draft analysis and it will be updated as the project continues. 
#    This code is shared with no guarantees about its accuracy or usefulness.
# License: CC-BY 2.0
# Caution: This code downloads and manipulates large files. 
#          Be aware that it uses considerable memory resources and time.

# 1. SETUP ####
library(sp)
library(rgdal)
library(sf)
library(raster) #useful functions for extents, projections
library(foreign)
library(doBy)
library(stringr)
library(dplyr)


# 2. LOAD DATA ####
source("./animals/locations.R")
source("./habitat/vegetation.R")
#source("./habitat/water.R") #The data files for water files are really big. Don't source this unless you explictly need the water files.

# 3. JOIN ANIMAL LOCATIONS TO HABITAT ####
# There's probably a more efficient way to do this step.
VegAnimals <- over(Animals, veg.pol) #extract vegetation information from combined shapefile. From vegetation.R: veg.pol = vegetation polygons
Animals <- SpatialPointsDataFrame(coordinates(Animals), cbind(Animals@data, VegAnimals)) #merge animal and vegetable data
