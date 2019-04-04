# S4B_Final_Project

More info about species distribution models, if you want to read up about it: https://rspatial.org/sdm/, https://www.annualreviews.org/doi/full/10.1146/annurev.ecolsys.110308.120159



Packages needed: 

for data download: 'ridigbio'
for mapping: 'raster' and 'rgdal' (one of these is hard to install- you may have to go to the cran pages and try to install the dependencies first.)


Step 1: Obtain collection records
    I'm using a package to download collection records from https://www.idigbio.org/ but https://www.gbif.org/ also has collection records. There's an R package to download it- rgbif- but it's kind of a huge pain, if you want to try to figure it out.

Step 2: Create a raster map from collection records (some nice tutorials about working with raster and shapefiles in R: https://www.neonscience.org/resources/data-tutorials

Step 3: Obtain niche data and identify niches
    How are we creating our niches?
    I'm using worldclim or bioclim, but is there other data I could be using?

Step 4: Create a new map, showing the potential niche of the species in the current climate
    Otherwise we will be comparing a realized niche map to a potential niche map 

Step 4: Find climate change data and figure out how to use it

Step 5: Use climate change projections to create new potential niche map
