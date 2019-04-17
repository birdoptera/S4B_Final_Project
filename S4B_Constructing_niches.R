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
#Extract the file "CONUS_brigh.tif" into your working directory.

# elevation data (source: the data is from NASA, processed by CGIAR-CSI http://srtm.csi.cgiar.org/)

# make a raster template
require(raster)
r.raster <- raster()
extent(r.raster) <- c(-161, -66.9, 18, 49.4)
res(r.raster) <- .04166667

# download elevation data and modify the raster grid. We're going to need to modify all of the rasters so that
# when we layer them on top of each other, they'll match up
elevation <- getData('alt', download = TRUE, country= 'USA')
elevation <- (elevation[[4]])
elevation <- resample(elevation, r.raster)

### solar radiation data (source: National Renewable Energy Laboratory)

# make a temporary file, download the data, and unzip it
tmp <- tempfile()
download.file("https://www.nrel.gov/gis/assets/data/us9809_dni_updated.zip", destfile = tmp)
unzip(tmp, exdir = ".")

# read in the shapefile and modify the raster template
solrad <- readOGR(dsn= ".", "us9809_dni_updated")
extent(r.raster) <- extent(solrad)
res(r.raster) <- .04166667

# change the shapefile into a raster, removing all the monthly data
solradraster <- rasterize(solrad, r.raster, field = solrad@data$ANN_DNI)
##so, tried to run it and hit the error that we haven't defined speciesraster earlier -chloe
solradraster <- solradraster[speciesraster, ]

### soil type raster from NRCS, STATSTOGO.

# load the 
soilsraster <- raster("CONUS_brigh.tif")
soilsraster <- projectRaster(soilsraster, crs=crs(solradraster))
soilsraster <- resample(soilsraster, r.raster)


### bio is a raster stack which includes all of the 12 worldclim variables, all of them having to do with
# temperature and precipitation. These are the variables we're going to use as climate data. The 'CMIP5' data
# uses the same variables as worldclim, giving us two raster stacks, present and future, which are the same
# every case except for the altered climate.


# download worldclim data and convert into a rasterstack
presentclimstack <- getData('worldclim', download = TRUE, var = 'bio', res = 2.5)
presentclimstack <- resample(presentclimstack, r.raster)
crs(presentclimstack) <- crs(solradraster)
# crop to the size of the other rasterlayers
presentclimstack <- crop(presentclimstack, elevation)
# add other layers to raster stack
presentclimbrick <- addLayer(presentclimstack, c(elevation, solradraster, soilsraster))



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
futureclimstack <- resample(futureclimstack, r.raster)
crs(futureclimstack) <- crs(solradraster)
futureclimstack <- crop(futureclimstack, elevation)
futureclimbrick <- addLayer(futureclimstack, c(elevation, solradraster, soilsraster))

# write to file so you can use later. This is actually going to write two files and you're going to need both.
writeRaster(futureclimbrick, "futureclimbrick")

### Make a dataframe of the niche values at each occurance point 
speciespresent <- extract(presentclimbrick, USspeciesdata) 

# select random points from the raster to use as background data and make a dataframe of those values
set.seed(0)
backgrounddata <- randomPoints(presentclimbrick, 500)
backgroundvalues <- extract(presentclimbrick, backgrounddata)
presencebackground <- c(rep(1, nrow(speciespresent)), rep(0, nrow(backgroundvalues)))

# make a new data frame of the occurrence values and the background values 
speciesdistdata <- data.frame(cbind(presencebackground, rbind(speciespresent, backgroundvalues)))

# save the dataframe of both occurrence values and background values
saveRDS(speciesdistdata, "sdm.Rds")

# save just the occurrence values
saveRDS(speciespresent, "presencevalues.Rds")

#move 'sdm', 'presencevalues' and 'futureclimbrick' from temporary folder before deleting it!!!
