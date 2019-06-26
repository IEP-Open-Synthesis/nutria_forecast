
# Purpose: manipulate data from CDFW's trapping group into a dataset of animal records for the IEP nutria forecasting project
# Author: Vanessa Tobias (vanessa_tobias@fws.gov)
# Disclaimer: 
#    This code is part of a draft analysis and it will be updated as the project continues. 
#    This code is shared with no guarantees about its accuracy or usefulness.
# License: CC-BY 2.0
# Data source:
#    The author of this code does not have permission to distribute data on locations of nutria.
#    To obtain data on the locations of nutria, please contact CDFW: 
#      https://www.wildlife.ca.gov/Conservation/Invasives/Species/Nutria/Infestation
#    This version runs on the data packed on 20190517. It does not work with earlier versions of the data.

# 1. SETUP ####
dataRoot <- "C:/users/vtobias/Documents/Nutria/Data/"

# Load Libraries #
library(sp)
library(rgdal)
library(sf)
library(raster) #useful functions for extents, projections
library(foreign)
library(doBy)
library(stringr)
library(dplyr)
library(readxl)

# 2. LOAD DATA ####

#---- 2.1 Capture Details ----
# CaptureDetails is a table. Even thought it looks like a shapefile, it doesn't have any geometry.
CaptureDetails <- read_xlsx(paste0(dataRoot, "Nutria_Survey_Data_20190517/NutriaCaptureDetails20190517.xlsx")) #read.dbf(paste0(dataRoot, "Nutria_Survey_Data_20190517/NutriaCaptureDetails20190517.dbf"))
CaptureDetails$CarcassID <- as.character(CaptureDetails$CarcassID)
# GlobalID = unique ID for capture records
# CarcassID = connection to Nutria Collected
# NutriaCapt = CarcassID
# Remove field that we dont' need:
CaptureDetails <- CaptureDetails[, c("GlobalID", "DeceaseDate", "ReportDate", "NxDate", "CarcassID", "OtherID",
                                     "COD", "CODOther", "CaptureType" , "Disposition", "AgeEstimate", "Sex",
                                     "WeightKg", "BodyLengthCm", "TailLengthCm", "RHindFootCm", "BCS", "Pregnant",
                                     "Lactation", "FetusCount", "ReprodComments", "Comments", "NutriaCaptureID", "County",
                                     "TotalLengthCm", "NeckCircumference", "PenisFullyEversible")]


#---- 2.2 NutriaCollected ----
NutriaCollected <- readOGR(dsn=paste0(dataRoot, "Nutria_Survey_Data_20190517/Nutria Collected.shp"),layer="Nutria Collected")
# 205 records
# GlobalID = "CarcassID" in other datasets
# TrapID
# Remove field that we dont' need:
NutriaCollected@data <- NutriaCollected@data[, -c(5:11)]

#---- 2.3 Traps ----
Trap <- readOGR(dsn=paste0(dataRoot, "Nutria_Survey_Data_20190517/Trap.shp"),layer="Trap")
# GlobalID = "TrapID" in other datasets
# Remove field that we dont' need:
Trap@data <- Trap@data[, -c(11:17)]
Trap$GlobalID <- as.character(Trap$GlobalID)

#---- 2.4 TrapCheck ----
TrapCheck <- read_xlsx(paste0(dataRoot, "Nutria_Survey_Data_20190517/TrapCheck20190517.xlsx"))
# Remove field that we dont' need:
TrapCheck <- TrapCheck[, -c(1, 5, 13, 16:19)]   #[, -c(4, 12, 15:18)]
# make the CarcassID into a character vector instead of a factor
TrapCheck$CarcassID <- as.character(TrapCheck$CarcassID)
#---- 2.4.1 Standardize CarcassID ----
# make all of the letters uppercase
TrapCheck$CarcassID <- toupper(TrapCheck$CarcassID)
# fix records with multiple carcasses:
# - try string length to ID them:
hist(str_length(TrapCheck$CarcassID[which(!is.na(TrapCheck$CarcassID))]))
TrapCheck$CarcassID[which(str_length(TrapCheck$CarcassID) > 25)]
# - string legnth isn't a good marker. Maybe use commas and spaces?
TrapCheck$CarcassID[grep("[(?:,|[:space:]+]", TrapCheck$CarcassID)]
# - handle revisions outside of the TrapCheck dataset
MultiplesTrap <- TrapCheck[grep("[(?:,|[:space:]+]", TrapCheck$CarcassID),]
# Identifying how many records by hand because I can't see a way to do it automatically with the inconsistent formatting
#  !!!! WOULD CAPTURECOUNT WORK? I JUST NOTICED IT EXISTS IN THE NEW DATASET
MultiplesTrap$HowMany <- c(2, 3, 2, 2, 2,
                           2, 2, 2, 3, 2,
                           3, 5, 2, 2, 2,
                           3, 2, 2, 2, 2,
                           5)
# repeat rows the specified number of times
MultiplesTrap <- MultiplesTrap[rep(1:nrow(MultiplesTrap), MultiplesTrap$HowMany),]
# Correct the CarcasIDs, by hand because of the formatting
MultiplesTrap$CarcassID <- c("ME1376-TF2-112818-01", "ME1376-TF2-112818-02",        
                             "ME1376-TF2-112918-01", "ME1376-TF2-112918-02", "ME1376-TF2-112918-03",                    
                             "ME1375-TF15-112918-01", "ME1375-TF15-112918-02",      
                             "ME1375-TF13-120418-01", "ME1375-TF13-120418-02",    
                             "ME2621-BET05-08272018-1", "ME2621-BET05-08272018-2",   
                             
                             "ME2622-BET35-09052018-1", "ME2622-BET35-09052018-2",   
                             "ME8606-CI1-042618A", "ME8606-CI1-042618B",             
                             "ME14544-CC38-073118-A", "ME14544-CC38-073118-B",       
                             "ME7103-S14-060118-A", "ME7103-S14-060118-B", "ME7103-S14-060118-C",                           
                             "ME7103-S14-060818-A", "ME7103-S14-060818-B",                            
                             
                             "ME7103-S20-060718-A", "ME7103-S20-060718-B", "ME7103-S20-060718-C",                           
                             "ME7103-S21-060818-A", "ME7103-S21-060818-B", "ME7103-S21-060818-C", "ME7103-S21-060818-D", "ME7103-S21-060818-E",                       
                             "ME8964-S133-083018-A", "ME8964-S133-083018-B",                          
                             "SJ18412-WSA08-092218-A", "SJ18412-WSA08-092218-B",     
                             "SJ19343-WS14-02122019-A", "SJ19343-WS14-02122019-B",                     
                             
                             "ME9128-CI88-030719-A", "ME9128-CI88-030719-B", "ME9128-CI88-030719-C",                        
                             "ME9493-CI130-041019-01", "ME9493-CI130-041019-02",
                             "ME9493-CI138-041019-01", "ME9493-CI138-041019-02",
                             "ME16209-HOL55-05102019A", "ME16209-HOL55-05102019B",                         
                             "ME18601-LB81-051519-1", "ME18601-LB81-051519-2",       
                             
                             "ME18809-LB86-051619-01", "ME18809-LB86-051619-02", "ME18809-LB86-051619-03", "ME18809-LB86-051619-04", "ME18809-LB86-051619-05")

# remove rows with multiple carcasses from TrapCheck
TrapCheck <- TrapCheck[-grep("[(?:,|[:space:]+]", TrapCheck$CarcassID),]
# add the multiples back into TrapCheck, but now with one row for each carcass
TrapCheck <- rbind(TrapCheck, MultiplesTrap[,1:12])

# 3. CREATE DATASET OF ANIMAL LOCATIONS ####
# The plan is to combine locations where animals were trapped and found into a single
#   dataset so that we can pull habitat information from where animals were located.
#   Vanessa has immediate plans to use this for habitat summaries for the SWS presentation,
#   but it might be useful in other applications as well.

#---- 3.1 AnimalsFound ----
AnimalsFound <- merge.default(x = NutriaCollected[which(NutriaCollected$PointRep == "Site"),],
                              y = CaptureDetails,
                              by.x = "GlobalID", by.y = "NutriaCaptureID",
                              all.x = TRUE)
names(AnimalsFound)
# remove columns we don't need:
# AnimalsFound <- AnimalsFound[,-c(5:11, 16:17, 26, 41:44, 48:49)]
AnimalsFound$Method <- "report"
# convert back to a spatial dataset
AnimalsFound <- SpatialPointsDataFrame(coordinates(cbind(AnimalsFound$coords.x1, AnimalsFound$coords.x2)), AnimalsFound)


#---- 3.2 AnimalsTrapped ----
TrapCheck <- data.frame(TrapCheck)
# GlobalIDs aren't matching. Looks like a problem with capitalization?
Trap$GlobalID <- toupper(Trap$GlobalID)
TrapCheck$TrapGlobalID <- toupper(TrapCheck$TrapGlobalID)
# Check: Yes, that seems to fix it.
which(gsub("[^a-zA-Z0-9]+", "", TrapCheck$TrapGlobalID) %in% gsub("[^a-zA-Z0-9]+", "", Trap$GlobalID))
AnimalsTrapped <- merge(x = TrapCheck, y = Trap,
                        by.x = "TrapGlobalID", by.y = "GlobalID", all.x = TRUE)


#head(AnimalsTrapped)
# keep nutria captures:
AnimalsTrapped <- AnimalsTrapped[which(!is.na(AnimalsTrapped$CarcassID)),]
AnimalsTrapped$CarcassID <- as.character(AnimalsTrapped$CarcassID)
# add capture details
# AnimalsTrapped2 <- merge.default(x = AnimalsTrapped, y = CaptureDetails,
#                                  by.x = "GlobalID", by.y = "TrapGlobal",
#                                  all.x = TRUE)
AnimalsTrapped <- merge.default(x = AnimalsTrapped, y = CaptureDetails,
                                by.x = "CarcassID", by.y = "CarcassID",
                                all.y = TRUE)
names(AnimalsTrapped)
#AnimalsTrapped <- AnimalsTrapped[,names(AnimalsFound)]

# AnimalsTrapped <- AnimalsTrapped[, -c(5:11, 13:20, 25, 28:34, 38:39, 47, 62:66, 70:71)]
AnimalsTrapped$Method <- "trap"
AnimalsTrapped <- AnimalsTrapped[!is.na(AnimalsTrapped$coords.x1),]
AnimalsTrapped <- SpatialPointsDataFrame(coordinates(cbind(AnimalsTrapped$coords.x1, AnimalsTrapped$coords.x2)), AnimalsTrapped)
# plot(AnimalsTrapped)

plot(AnimalsFound, col = "red")
points(AnimalsTrapped)

#---- 3.3 Combined Animals ----
# Combine all animals into a single SpatialPointsDataFrame

# We need both datasets to have identical colums:
# Which names are missing from AnimalsFound?
names(AnimalsTrapped)[-which(names(AnimalsTrapped) %in% names(AnimalsFound))]

AnimalsFound$TrapGlobal <- AnimalsFound$GlobalID.x <- AnimalsFound$TrapCheckD <- NA
AnimalsFound$TrapAction <- AnimalsFound$TrapStatus <- AnimalsFound$Rebait <- AnimalsFound$Bait <- NA
Species <- AnimalsFound$CaptureCou <- AnimalsFound$Dispositio.x <- AnimalsFound$BaitOther.x  <- NA
TrapActive <- AnimalsFound$DateAdded <- AnimalsFound$SetID <- AnimalsFound$TrapType <- AnimalsFound$Bait <- NA
BaitOther.y <- AnimalsFound$DateRemove <- AnimalsFound$Dispositio.y <- AnimalsFound$Comments <- AnimalsFound$NutriaCapt <- NA

# Which names are missing from AnimalsTrapped?
names(AnimalsFound)[-which(names(AnimalsFound) %in% names(AnimalsTrapped))]
# AnimalsTrapped$GlobalID <- AnimalsTrapped$CaptureDat <- AnimalsTrapped$PointRep <- NA
AnimalsTrapped$Dispositio <- NA

# Create a single dataset by binding the rows together:
Animals <- dplyr::bind_rows(data.frame(AnimalsTrapped), data.frame(AnimalsFound))
# Animals <- rbind(AnimalsFound, AnimalsTrapped[, c()])
Animals <- SpatialPointsDataFrame(coordinates(cbind(Animals$coords.x1, Animals$coords.x2)),data = Animals)
Animals@proj4string <- Trap@proj4string 
Animals <- spTransform(Animals,  CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 
                                     +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) #crs(delta))

# 4. MAP ANIMALS ####
library(png)
library(grid)

# Plotting the animals with a nutria image this way doesn't quite work. 
# You get some interesting modern art, though.
# NutriaPNG <- png::readPNG(source = "U:\\IEP Work Plan\\2019 Nutria 337\\nutria_thumb_PhyloPic.png")
# NutriaGrob <- rasterGrob(NutriaPNG)

# veg comes from another R file that downloads and processes vegetation data.
plot(veg, 
      xlim = c(-120000, -20000),
      ylim = c(-100000, -20000),
      col = topo.colors(38))
# plot(Animals, pch = NutriaGrob)
offset = 3000
rasterImage(NutriaPNG, xleft = coordinates(Animals)[,1]-offset, ybottom = coordinates(Animals)[,2]-offset, 
            xright = coordinates(Animals)[,1]+offset, ytop = coordinates(Animals)[,2]+offset)

# just a plain old plot of the points:
plot(Animals, pch = 16)
# there's a suspcious one out by San Jose.



