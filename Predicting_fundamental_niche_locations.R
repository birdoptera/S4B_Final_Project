#!/bin/bash/Rscript

###This code was written on R 3.4.4 and tested on R 3.5.1

###to download and make a raster from species data
###documentation on ridigbio https://cran.r-project.org/web/packages/ridigbio/ridigbio.pdf

#load package 
#(we're not loading them all at once, because sometimes they mask each other)
# install.packages("ridigbio")  #run install.papackages if you don't have ridigbio library yet
library(ridigbio)

#search and download data from iDigBio. 
#replace 'genus' and 'specificEpithet' with the species you want. You can get rid of 'specificEpithet' if you just want to use
#genus- you can also use higher taxa if you want to; use 'data.dwc:family' or 'data.dwc:order'. Don't be tempted to use
#'data.dwc:scientifiName'- that category seems to be full of errors. If you do use a higher order, you might want to add
#'genus' and 'specificEpithet' to the 'fields' to check for errors.

#Prompt for genus and species
## the original code for this wasn't working in my terminal, so I switched it to this and commented out the originals
cat("Enter a genus name: ")
genus <- readLines("stdin", n=1)
cat("Enter a species name: ")
species <- readLines("stdin", n=1)
##genus <- readline(prompt= "Enter a genus name: ")
##species <- readline(prompt= "Enter the species part of a species name: ")

#I chose Myzus persicae (pea aphid) because I know it's a common pest and so would have good collection data 
## I added hard-coded nomenclature variables and a limit parameter. The latter probably isn't necessary since idigbio is a smaller database, but it might save time for larger species datasets -Iwo
datum_idigbio <- idig_search_records(rq = list(geopoint=list(type="exists"), genus = sprintf("%s", genus)), "data.dwc:specificEpithet" = sprintf("%s", species), fields = c("geopoint"), limit=5000)

#check your data
head(datum_idigbio)

#save it so you don't have to download it all over again

write.csv(datum_idigbio, "datum_idigbio.csv")

# these are inserts to let you know where the file stopped working
cat("idigbio finished", fill=TRUE)

## START of Iwo's edits

#install and load package
# install.packages("rgbif")
library(rgbif)

##ok, ran this script and it errored out here with the message:
##Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
##  namespace ‘curl’ 3.2 is already loaded, but >= 3.3 is required
##does anyone know what went wrong here and how we can fix it -chloe

#search for all GBIF records of a given "[Genus] [species]". Returns a long/lat matrix with up to 5000 results, including US records with location data (no NAs) only
## I included an exaggerated limit of 5000 bc the default is 500 (too low), and the "per request maximum" is 300 -Iwo
## I don't understand that part of the help page on CRAN, but "5000" does the trick so I'm sticking with it -Iwo
## I figured it out. This is a better command: hard-coded variables, returns up to 5000 records with long/lat data only, not necessary to remove NA cases later -Iwo
datum_gbif <- occ_search(scientificName = sprintf("%s %s", genus, species), country= "us", return = "data",limit="5000", hasCoordinate=TRUE, fields=c('decimalLongitude','decimalLatitude'))

#check your data
head(datum_gbif)

cat("gbif finished", fill=TRUE)

#rename gbif columns according to idigbio
names(datum_gbif) <- names(datum_idigbio)

#combine idigbio and gbif occurrence records
speciesdata <- rbind(datum_idigbio, datum_gbif)

#Remove cases with NA lat/long data
speciesdata <- speciesdata[complete.cases(speciesdata), ]

#save it so you don't have to download it all over again
write.csv(speciesdata, "speciesdata.csv")

cat("speciesdata finished", fill=TRUE)

## END of Iwo's edits 

###convert your data into a raster map

#load these packages
library(raster)
library(rgdal)

#download map files to use as raster masks
#I'm only including data from one country, because some of the niche data, like landuse, or soil type,
#only seem available on a national level, but there might be ways around this
US <- getData('GADM', country = 'USA', level=1)


cat("US map downloaded", fill=TRUE)
#creating shapefile from speciesdata. Lat and lon are switched. CRS of US map is applied to species data
speciesdataSP <- SpatialPointsDataFrame(speciesdata[,1:2], speciesdata, proj4string = crs(US))

cat("shapefile created", fill=TRUE)
#plot species data against US map
plot(speciesdataSP)
plot(US, add = TRUE)

#crops species data to only include data collected in the US
cropspeciesdataSP <- speciesdataSP[US, ]

# do this if you are running this line by line
#plot cropped data against US map
#plot(cropspeciesdataSP, pch = '.')
#plot(US, add = TRUE)

#saves only the data portion of the shapefile as a dataframe. Basically, the cropped portion of the 
#original datafile
USspeciesdata <- cropspeciesdataSP@data

#why not save the file just in case?
write.csv(USspeciesdata, "USspeciesdata.csv")

cat("USspecies data created", fill=TRUE)
#making raster file
#create empty raster based on cropspeciesdataSP
#create values to plug into raster files
#I chose 2.5 minutes of degree arcs because worldclim data comes in that resolution 
cell_size <- 2.5

#use extent from cropspeciesdataSP
extent(cropspeciesdataSP)

#I'm doing this manually, but there's probably an easy way to code it (x = lon, y = lat)
lon_min <- -159.77; lon_max <- -68.01; lat_min <- 21.30; lat_max <- 48.43

#This is calculating how the cells are going to be laid out in the raster
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1

#create an empty raster
##Is res being specified by the same thing as cell size above? If so, just use variable -chloe k.
speciesraster <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, 
                     res=cell_size, crs="+proj=longlat +datum=WGS84")
cat("empty raster created", fill=TRUE)
#rasterize species data
speciesraster <- rasterize(USspeciesdata, speciesraster, fun = "count")

# save the data to the drive
writeRaster(speciesraster, "species_raster", overwrite=TRUE)

cat("species raster created", fill=TRUE)
#load packages
library(raster)
library(rgdal)
library(dismo)

###niche construction

### We're going to build a raster stack to use as our niche model. It's going to include climate data
### (precipitation and temperature) and non-climate environmental data. The climate data will change according
### to the climate change prediction model, but the non-climate environmental data will not. For this reason
### we have omitted to include factors that seem climate-dependent such as biomes, NPP, and aridity in 
### our niche model, because these things are likely to shift as a result of climate change. I have 
### included 'land use' however, even though land use is likely to change somewhat 
### with climate change, because so much of land use is driven by human activities.

### Ideally, your niche should be tailored to your species. If your species has a narrow diet, you may need
### a more complex niche model including your species' food source, because the species can only survive where
### it's food survives. One way you could do this is to follow this script for the food source, create 
### the potential niche maps for present and future, and then include the those maps in the correct niche stack
### (present in present, future in future).

### Constructing non-climatic environmental niches


# This script involves a lot of downloads. This script will only work if you create a temporary directory,
# set it as your working directory. This will also make it easier to delete the downloads when you are done.

# Before you begin, go to https://nrcs.app.box.com/v/soils/folder/53525984812 and download the file soil_color_CONUS_v2.zip. 
# Extract the file "CONUS_brigh.tif" into your working directory.

# elevation data (source: the data is from NASA, processed by CGIAR-CSI http://srtm.csi.cgiar.org/)


# make a raster template
 require(raster)
 r.raster <- raster()
 extent(r.raster) <- c(-161, -66.9, 18, 49.4)
 res(r.raster) <- .04166667

# download elevation data and modify the raster grid. We're going to need to modify all of the rasters so that
# when we layer them on top of each other, they'll match up
# this code worked line-by-line but did not work when run all at once, so I've replace it with loading these files

# elevation <- getData('alt', download = TRUE, country= 'USA')
# elevation <- (elevation[[4]])
# elevation <- resample(elevation, r.raster)
elevation <- raster("elevation")
cat("elevation raster loaded", fill=TRUE)
### solar radiation data (source: National Renewable Energy Laboratory)

# make a temporary file, download the data, and unzip it
## this worked in RStudio, but isn't working on the ASC (I think it just takes too long, so I'm just going to put it in the github 
##tmp <- tempfile()
##download.file("https://www.nrel.gov/gis/assets/data/us9809_dni_updated.zip", destfile = tmp)
##unzip(tmp, exdir = ".")

# read in the shapefile and modify the raster template
##solrad <- readOGR(dsn= ".", "us9809_dni_updated")
##extent(r.raster) <- extent(solrad)
##res(r.raster) <- .04166667

# change the shapefile into a raster, removing all the monthly data
##solradraster <- rasterize(solrad, r.raster, field = solrad@data$ANN_DNI)
##solradraster <- solradraster[speciesraster, ]

solradraster <- raster("solradraster")
cat("solrad raster loaded", fill=TRUE)
### soil type raster from NRCS, STATSTOGO.

## Because of the difficulty of working with the soils data, I have produced raster files we can just fit in

# load the soils data
#soilsraster <- raster("CONUS_brigh.tif")
#soilsraster <- projectRaster(soilsraster, crs=crs(solradraster))
#soilsraster <- resample(soilsraster, r.raster)

soilsraster <- raster("soilsraster")
cat("soilsraster loaded", fill=TRUE)
### bio is a raster stack which includes all of the 12 worldclim variables, all of them having to do with
# temperature and precipitation. These are the variables we're going to use as climate data. The 'CMIP5' data
# uses the same variables as worldclim, giving us two raster stacks, present and future, which are the same
# every case except for the altered climate.


# download worldclim data and convert into a rasterstack
presentclimstack <- getData('worldclim', download = TRUE, var = 'bio', res = 2.5)

cat("present bioclim data downloaded", fill=TRUE)

presentclimstack <- resample(presentclimstack, r.raster)
crs(presentclimstack) <- crs(solradraster)

# crop to the size of the other rasterlayers

presentclimstack <- crop(presentclimstack, elevation)

cat("present bioclim stack altered", fill=TRUE)

# add other layers to raster stack

presentclimbrick <- addLayer(presentclimstack, c(elevation, solradraster, soilsraster))

writeRaster(presentclimbrick, "presentclimbrick", overwrite = TRUE)

cat("present brick completed", fill=TRUE)

### We're sourcing our climate projection data from CMIP5, the Coupled Model Intercomparison Project
# I've chosen to use the GFDL data set from NOAA, with the representative concentration pathway (rcp) (how
# severe the climate change is expected to be based on how much greenhouse gases are emitted.
# 85 is the most extreme projection), with the year set to 50. There are other models, rcp, and year available,
# The options are given in the documentation for 'raster'.
# Again, I'm downloading the same variables as in the present climstack. 

## This might be overkill considering our deadline is imminent, but including prompts to choose (and hard-code) from a selection
## of rcps, model datasets, and scenario years wouldn't be too difficult.- Iwo 

futureclimstack <- getData('CMIP5', var = 'bio', res = 2.5, rcp = 85, model = 'GF', download = TRUE, 
                        year = '50')

cat("future clim data downloaded", fill=TRUE)

futureclimstack <- resample(futureclimstack, r.raster)
crs(futureclimstack) <- crs(solradraster)
futureclimstack <- crop(futureclimstack, elevation)

cat("future clim data altered", fill=TRUE)

futureclimbrick <- addLayer(futureclimstack, c(elevation, solradraster, soilsraster))

names(futureclimbrick) <- names(presentclimbrick)

cat("future brick completed", fill=TRUE)

# write to file so you can use later. This is actually going to write two files and you're going to need both.
writeRaster(futureclimbrick, "futureclimbrick", overwrite = TRUE)

### Make a dataframe of the niche values at each occurance point 
speciespresent <- extract(presentclimbrick, USspeciesdata) 

# select random points from the raster to use as background data and make a dataframe of those values
set.seed(0)
backgrounddata <- randomPoints(presentclimbrick, 500)
backgroundvalues <- extract(presentclimbrick, backgrounddata)
presencebackground <- c(rep(1, nrow(speciespresent)), rep(0, nrow(backgroundvalues)))

cat("dataframes for modeling created", fill=TRUE)

# make a new data frame of the occurrence values and the background values 
speciesdistdata <- data.frame(cbind(presencebackground, rbind(speciespresent, backgroundvalues)))

# save the dataframe of both occurrence values and background values
saveRDS(speciesdistdata, "sdm.Rds")

# save just the occurrence values
saveRDS(speciespresent, "pvals.Rds")

#move 'sdm', 'presencevalues' and 'futureclimbrick' from temporary folder before deleting it!!!

#run a generalized linear model (we're running it with 5 factors: bio1 = annual mean temperature,
# bio 12 = annual precipitation, USA1_msk_alt = elevation, layer = solar radiation, and CONUS_brigh = soil types)
## these names are terrible. To change them you have to change the names of the climbricks

m1 <- glm(presencebackground ~ bio1 + bio12 + USA1_msk_alt + layer + CONUS_brigh, data = speciesdistdata)

#map the present possible distribution            
p <- predict(presentclimbrick, m1)            
#figure out a way to output plots
plot(p)            

#map the future possible distribution
f <- predict(futureclimbrick, m1)            
plot(f)

#Save plot_present to wd in .jpg format
jpeg("plot_present.jpg")
plot(p)
dev.off()

#Save plot_future to wd in .jpg format
jpeg("plot_future.jpg")
plot(f)
dev.off()

d <- overlay(f,p,fun=function(r1, r2){return(r1-r2)})

#Save plot_diff to wd in .jpg format
jpeg("plot_diff2.jpg")
plot(d)
dev.off()

cat("SDM plots saved", fill=TRUE)

#print glm summary
summary(m1)

#Done
cat("Done", fill=TRUE)

#maybe find a way to plot the occurrence data on both plots?

#maybe we could do some kind of model evaluation? like splitting the data into 2 parts, running one and trying to use that to predict the other?

