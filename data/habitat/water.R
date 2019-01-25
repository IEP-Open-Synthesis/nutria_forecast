
# Purpose: download and process water data for the IEP nutria forecasting project
# Author: Vanessa Tobias (vanessadtobias@gmail.com)
# Disclaimer: 
#    This code is part of a draft analysis and it will be updated as the project continues. 
#    This code is shared with no guarantees about its accuracy or usefulness.
# License: CC-BY 2.0
# Caution: this code downloads and manipulates large files. Be aware that it uses considerable memory resources and time.

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

# GET DATA ####
# Download Data #
# Data used here is from the USGS National Hydrography Dataset.
# Additional information can be found here: https://www.sciencebase.gov/catalog/item/5ab25ecde4b081f61ab45e04
# This is the CA state-wide shapefile. It's huge, but it was what was available during the shutdown when the 
#  National Map servers went down.

# Flowlines
dl_zip("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/State/HighResolution/Shape/NHD_H_California_State_Shape.zip")

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
wl <- flowline.small[which(flowline.small@data$FCode %in% c("46000", "46003", "46006", "46007", "33600", "33601", "33603", "33400")),]
# wl = "water lines" because we're going to need to add water bodies, too.
# includes canal/ditch, connector, stream/river. NOT coastline yet.


# Waterbodies





