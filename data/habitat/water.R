
# Purpose: download and process water data for the IEP nutria forecasting project
# Author: Vanessa Tobias (vanessadtobias@gmail.com)
# Disclaimer: 
#    This code is part of a draft analysis and it will be updated as the project continues. 
#    This code is shared with no guarantees about its accuracy or usefulness.
# License: CC-BY 2.0
# Caution: This code downloads and manipulates large files. 
#          Be aware that it uses considerable memory resources and time.

# 1. SETUP ####
# Load Libraries ####
library(sp)
library(rgdal)
library(sf)
library(raster) #useful functions for extents, projections

# Define Functions ####
# !!!CAUTION!!! I was lazy and I copied this from vegetation.R on 2019-01-24
# We should probably make a separate file for common functions.
dl_zip <- function(loc_url){
  temp <- tempfile() #create a place to put the download
  download.file(loc_url, temp) #download the zip file
  unzip(temp, exdir = getwd()) #unzips the file and saves the unziped file to your working directory
  rm(temp) #delete the temporary file
  gc() #clean up memory
}

# 2. GET DATA ####
# Download Data #
# Data used here is from the USGS National Hydrography Dataset.
# Additional information can be found here: https://www.sciencebase.gov/catalog/item/5ab25ecde4b081f61ab45e04
# This is the CA state-wide shapefile. It's huge, but it was what was available during the shutdown when the 
#  National Map servers went down.

# Check if the file already exists in the working directory. If it doesn't, download it.
#   Yes, the zip file is actually named "Shape" when you download it.
if(list.files(path = "./", pattern = "Shape") != "Shape") dl_zip("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/HighResolution/Shape/NHD_H_California_State_Shape.zip")

# Flowlines ####
# Import shapefiles
shp.list <- list.files(path = "./Shape", pattern = "NHDFlowline(?:[0-9]+|).shp$")
for(i in 1:length(shp.list))
{
  num <- grep("[0-9]+", shp.list)[i]
  assign(paste0("flowline", num), readOGR(dsn = "./Shape", layer = unlist(strsplit(shp.list[i], "[.]"))[1]))
}
# This works ok. Heads up: "flowline" wihtout a number comes out as "flowlineNA" which I think is fine if you know what it's doing.
#   I'll keep an eye on this and if it becomes problematic, I'll fix it.

plot(flowline10[which(flowline10@data$FCode == "46000"),])
plot(flowline11[which(flowline11@data$FCode == "46000"),], add = TRUE)
plot(flowline2[which(flowline2@data$FCode == "46000"),], add = TRUE)

# Select HUCs within our spatial extent
# c(1802, 1803, 1804, 1805)
# ReachCode starts with the HUC region (18) and the subregion (0x)
# Helpful reference: https://water.usgs.gov/GIS/huc_name.html#Region18

for(i in 1:length(shp.list)){
  num <- grep("[0-9]+", shp.list)[i]
  assign(paste0("flowline", num, ".small"), 
         eval(parse(text = paste0("flowline", num)))[which(substring(eval(parse(text = parse(text = paste0("flowline", num, "@data$ReachCode")))), 1, 4) %in% c(1802, 1803, 1804, 1805)),])
} 
rm(list=paste0("flowline", 2:11))
rm(flowlineNA)

# Merge flowlines into one SpatialLinesDataFrame
# class(ls()[grep("flowline", ls())])
# bind(eval(parse(text = paste0("list =", ls()[grep("flowline", ls())]))))
# Not sure why I can't get bind() to accept an actual list, but for now I'll manually write it out so I can move past it:
flowline.small <- bind(flowline11.small, flowline10.small, flowline9.small, flowline8.small, flowline7.small, flowline6.small, flowline5.small, flowline4.small, flowline3.small, flowline2.small, flowlineNA.small)
# Binding this many big things takes a little while - It took 10-15 minutes on my work laptop with a Core i7 7th gen processor.
plot(flowline.small)
# plotting also takes a longtime.

rm(list=paste0("flowline", c("NA", 2:11), ".small"))
gc() #isn't triggering a memory dump. Not sure why. Seems like it should be enough.


# Remove the "not water" objects
# List of FCode values: https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm
#  Codes for pipelines & underground conduit all start with "42" and nothing else does.
length(flowline.small@data$OBJECTID[which(flowline.small@data$FCode %in% c("46000", "46003", "46006", "46007", "33600", "33601", "33603", "33400"))])
#wl <- flowline.small[which(flowline.small@data$FCode %in% c("46000", "46003", "46006", "46007", "33600", "33601", "33603", "33400")),]
# Just the perennial flowlines:
wl <- flowline.small[which(flowline.small@data$FCode %in% c("33600", "33601", "33603", "33400", "46006")),]

# wl = "water lines" because we're going to need to add water bodies, too.
# includes canal/ditch, connector, stream/river. NOT coastline (yet).

# Make CRS match the others
wl <- spTransform(wl, crs(delta))
writeOGR(obj = wl, dsn = ".", layer = "waterlines", driver = "ESRI Shapefile")

rm(flowline.small)
gc()

# Waterbodies ####
wb <- readOGR(dsn = "./Shape", layer = "NHDWaterbody")
# plot(wb)
# Select waterbodies in HUCS that are within our geographical range:
#  Just keep perennial water body types
#  Now includes all reservoirs (43...)!
wb <- wb[which(substring(wb@data$ReachCode, 1, 4) %in% c(1802, 1803, 1804, 1805) & 
                 (wb@data$FCode %in% c("39004", "36100", "46600", "46601", "46602", "49300") | 
                    substring(wb@data$FCode, 1, 2) == "43")),]

summary(wb@data$FCode)
wb@data$FCode <- droplevels(wb@data$FCode)
# All of the FCodes seem reasonable to keep for now.
# 36100 (playa) & 46600 (swamp/marsh) might be IDed by the vegetation layer, though.
# Other categories: 49...Estuary 39...Lake/Pond 43...Reservoir

# Classify water bodies into understandable categories:
wb@data$water.type <- NA
wb@data$water.type[which(substring(wb@data$FCode, 1, 2) == "36")] <- "Playa"
wb@data$water.type[which(substring(wb@data$FCode, 1, 2) == "39")] <- "Lake-Pond"
wb@data$water.type[which(substring(wb@data$FCode, 1, 2) == "49")] <- "Estuary"
wb@data$water.type[which(substring(wb@data$FCode, 1, 2) == "46")] <- "Swamp-Marsh"
wb@data$water.type[which(substring(wb@data$FCode, 1, 2) == "43")] <- "Reservoir"
#wb@data$water.type[which(substring(wb@data$FCode, 1, 2) == "37")] <- "Ice"
wb@data$water.type <- as.factor(wb@data$water.type)
levels(wb@data$water.type)

plot(wb, col = wb@data$water.type, border = wb@data$water.type, main = "Waterbodies")
legend("topleft", lwd = 2, 
       col = 1:length(unique(wb@data$water.type)), 
       legend = levels(wb@data$water.type),
       title = "Type", cex = 0.75)

# Make CRS match the others
wb <- spTransform(wb, crs(delta))


# 3. Create Water Indices ####
# Proximity to water?
#  -- include flowlines and waterbodies
#  -- depending on pixel size, all pixels might have water
# Water "density" in a pixel?
#  -- area of pixel that overlaps flowlines and water bodies?

# Create rasters of each type of water
#  -- Use the mask raster as the base so they all match up in terms of extent, resolution, origin, and CRS AND you can mask it all in one step.
#  Use veg.rast (an empty raster) if you don't want to run all of the veg code! (Today's lesson!)
#  Lesson 2: if the CRS doesn't match, you get a raster full of NAs
identical(crs(wb), crs(habmask.rast))
identical(crs(wl), crs(habmask.rast))
wl.rast <- rasterize(wl, habmask.rast, field = wl@data$FCode, mask = TRUE, fun = "count") #Friday 4:41
wb.rast <- rasterize(wb, habmask.rast, field = wb@data$FCode, mask = TRUE, fun = "count")
#  -- Instructions near the end of this blog post:
#     http://www.mikemeredith.net/blog/1212_GIS_layer_for_Distance_from.htm
# merge() combines rasters using functions like mean or sum.
# raster::distance() computes distance to the nearest non-NA cell
# gdistance package might also be useful

image(wb.rast)
image(distance(wb.rast))

# X. Questions to Answer ####
#  -- Should we include intermittent and/or ephemeral water? Or should we only include perennial water sources?
#   Right now the code includes perennial and intermittent/ephemeral, but water pretty much covers the whole area
