# S4B_Final_Project

Okay, what I've got so far is a script that downloads species data from iDigBio, crops it to the US, and creates a raster map from it.

I've also figured out how to download climatic data (temp, precipitation from worldclim). I think that this can be the data we expect to change the most with climate change

What we need to do:

1. Check out my script and see if there's anything in it should be changed - everyone

2. Start working on a README - Chloe

3. I got collection data from iDigBio, but GBIF (www.gbif.org) also has collection data you can download with R, it's just a little trickier. Someone can figure that out, or maybe there are other data sources out there? -Iwo

4. Figure out what other niche aspects to use and how to incorporate them into the model. Some ideas: elevation, NPP, solar radiation, soil type, biome, landuse. In reality, niches should be tailored to the species, but this is fast and dirty. The data has to be in a raster file or be convertable into a raster file. - Gwendolyn

5. Is CMIP5 (https://cmip.llnl.gov/cmip5/) a good source of climate prediction data? What model and 'rcp' should we use? -Chloe

6. There's some modeling people use with species distribution modeling, we might want to look into. The tutorial https://rspatial.org/sdm/ goes into it -Sopa

7. When we have all our niche variables in a nice raster stack, and we've figured out what we do with the modeling, we can use them to create current and future potential niches for our species. Then we can use those to make pretty graphs.-Gabriela

8. Are there other ways we could improve this all? Pipelines and bash etc? -Everyone

9. Write proposal -Gwendolyn

10. Begin writing paper -Everyone


Some sources:
More info about species distribution models, if you want to read up about it: 
tutorial: https://rspatial.org/sdm/, 
journal paper: https://www.annualreviews.org/doi/full/10.1146/annurev.ecolsys.110308.120159

Tutorial for working with raster and shapefile data:
https://www.neonscience.org/resources/data-tutorials

Climatic data:
http://worldclim.org/bioclim


Packages needed so far: 

for data download: 'ridigbio'
for mapping: 'raster' and 'rgdal' (one of these is hard to install- you may have to go to the cran pages and try to install the dependencies first.)


- Step 1: Obtain collection records
    I'm using a package to download collection records from https://www.idigbio.org/ but https://www.gbif.org/ also has collection records. There's an R package to download it- rgbif- but it's kind of a huge pain, if you want to try to figure it out.

- Step 2: Create a raster map from collection records (some nice tutorials about working with raster and shapefiles in R: https://www.neonscience.org/resources/data-tutorials

- Step 3: Obtain niche data and identify niches
    How are we creating our niches? Really, niches should be species specific. Since I'm using aphids, I probably should be including host plant distribution- but I'm using a generalist, so that may not be such a big issue.
    I'm using worldclim for the climatic data, but I'd like some other data- elevation, biome and/or landuse, solar radiation, NPP, for example. If someone wants to figure out a way to incorporate some, or all of that data.
    
- Step 4: Create a new map, showing the potential niche of the species in the current climate
    Otherwise we will be comparing a realized niche map to a potential niche map 
    
	 Step 4.5: the tutorial https://rspatial.org/sdm/ gives examples of using models to check accuracy of niche modeling. Should we be incorporating these?

- Step 5: Find climate change data and figure out how to use it
r package 'raster' function getData allows you to download climate prediction data 'CMIP5' https://cmip.llnl.gov/cmip5/ Not sure how good this data is. It's cool though; it seems to provide the same climatic data as worldclim does, which I think really simplifies the process; we can just create two rasterstacks: the one with the current climate data (worldclim) and the one with the predicted data (CMIP5), and iterate over both to create realized niche maps.

- Step 6: Use climate change projections to create new potential niche map
