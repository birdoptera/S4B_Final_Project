
###to download and make a raster from species data
###documentation on ridigbio https://cran.r-project.org/web/packages/ridigbio/ridigbio.pdf

#load package 
#(we're not loading them all at once, because sometimes they mask each other)
install.packages("ridigbio")  #run install.papackages if you don't have ridigbio library yet
library(ridigbio)

#search and download data from iDigBio. 
#replace 'genus' and 'specificEpithet' with the species you want. You can get rid of 'specificEpithet' if you just want to use
#genus- you can also use higher taxa if you want to; use 'data.dwc:family' or 'data.dwc:order'. Don't be tempted to use
#'data.dwc:scientifiName'- that category seems to be full of errors. If you do use a higher order, you might want to add
#'genus' and 'specificEpithet' to the 'fields' to check for errors.

##I think we should be able to pretty easily make this generalizable by allowing the user to input a list (or type in) of genera and species using
##variables, then make the call and check a function with user input on if the data is correct and call it with a for loop on the list of genera/species?
## chloe k.
## There's easier ways to do that then a for function. If you look at the documentation for ridigbio, there are ways to preset
## the variables and imput multiple species etc. If you want to change the code to that, please go ahead. -Gwendolyn
## Not sure where everyone has been working, but I am adding code below that downloads and combines long/lat data from idigbio (with Gwendolyn's code) and GBIF -Iwo

#I chose Myzus persicae (pea aphid) because I know it's a common pest and so would have good collection data
speciesdata <- idig_search_records(rq = list(geopoint=list(type="exists"), genus = "myzus", "data.dwc:specificEpithet" = "persicae"),

                                   fields = c("geopoint"))
#check your data
head(speciesdata)

#save it so you don't have to download it all over again
write.csv(speciesdata, "speciesdata.csv")

## START of Iwo's edits

#install and load package
install.packages("rgbif")
library(rgbif)

#search for all GBIF records of a give "[Genus] [species]"
## I included an exaggerated limit of 5000 bc the default is 500 (too low), and the "per request maximum" is 300 -Iwo
## I don't understand that part of the help page on CRAN, but "5000" does the trick so I'm sticking with it -Iwo
datum <- occ_search(scientificName = "Myzus persicae", return = "data",limit="5000"

#format columns to match Ridigbio data
datum_gbif <- datum[,4:3]

#check your data
head(datum_gbif)

#rename gbif columns according to idigbio
## Is this necessary? Not sure -Iwo
names(datum_gbif) <- names(speciesdata)

#combine idigbio and gbif occurrence records
datum_combined <- rbind(speciesdata, datum_gbif)

#Remove cases with NA lat/long data
## Several NAs made it through gbif, probably because my search wasn't sophisticated enough- Iwo
datum_combined <- datum_combined[complete.cases(datum_combined, ]

#save it so you don't have to download it all over again
write.csv(datum_combined, "datum_combined.csv")

## END of Iwo's edits 

###convert your data into a raster map

#load these packages
library(raster)
library(rgdal)

#download map files to use as raster masks
#I'm only including data from one country, because some of the niche data, like landuse, or soil type,
#only seem available on a national level, but there might be ways around this
US <- getData('GADM', country = 'USA', level=1)

#creating shapefile from speciesdata. Lat and lon are switched. CRS of US map is applied to species data
speciesdataSP <- SpatialPointsDataFrame(speciesdata[,1:2], speciesdata, proj4string = crs(US))

#plot species data against US map
plot(speciesdataSP)
plot(US, add = TRUE)

#crops species data to only include data collected in the US
cropspeciesdataSP <- speciesdataSP[US, ]

#plot cropped data against US map
plot(cropspeciesdataSP, pch = '.')
plot(US, add = TRUE)

#saves only the data portion of the shapefile as a dataframe. Basically, the cropped portion of the 
#original datafile
USspeciesdata <- cropspeciesdataSP@data

#why not save the file just in case?
write.csv(USspeciesdata, "USspeciesdata.csv")

#making raster file
#create empty raster based on cropspeciesdataSP
#create values to plug into raster files
#I chose 2.5 minutes of degree arcs because worldclim data comes in that resolution 
cell_size <- 2.5

#use extent from cropspeciesdataSP
extent(cropspeciesdataSP)

##I would say variables that can be input by the user when they call the script defaults that are below -chloe k.
##I don't understand what you mean by that -GB
#I'm doing this manually, but there's probably an easy way to code it (x = lon, y = lat)
lon_min <- -159.77; lon_max <- -68.01; lat_min <- 21.30; lat_max <- 48.43

#This is calculating how the cells are going to be laid out in the raster
ncols <- ((lon_max - lon_min)/cell_size)+1; nrows <- ((lat_max - lat_min)/cell_size)+1

#create an empty raster
##Is res being specified by the same thing as cell size above? If so, just use variable -chloe k.
speciesraster <- raster(nrows=nrows, ncols=ncols, xmn=lon_min, xmx=lon_max, ymn=lat_min, ymx=lat_max, 
                     res=cell_size, crs="+proj=longlat +datum=WGS84")

#rasterize species data
speciesraster <- rasterize(USspeciesdata, speciesraster, fun = "count")

##should probably save the raster at this point, but I am not sure how to do that 
