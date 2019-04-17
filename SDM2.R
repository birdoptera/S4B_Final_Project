# Modified Species distribution modeling script from https://jcoliver.github.io/learn-r/011-species-distribution-models.html
# Author Jeff Oliver

#install needed packages
install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))

# Load library
library("sp")
library("raster")
library("maptools")
library("rgdal")
library("dismo")

# Download bioclim data and store in bioclim.data variable
bioclim.data <- getData(name = "worldclim",
                        var = "bio",
                        res = 2.5,
                        path = "data/")

# Read in speciesdata
# code below needs adjust to source for data. Not sure which one to use.
obs.data <- read.csv(file = "data/speciesdata.csv")

# Drop any rows with NAs
obs.data <- obs.data[!is.na(obs.data$latitude), ]

# Only pull out those columns of interest and in the order we want them
obs.data <- obs.data[, c("longitude", "latitude")]

# Determine geographic extent of our data
max.lat = ceiling(max(obs.data$latitude))
min.lat = floor(min(obs.data$latitude))
max.lon = ceiling(max(obs.data$longitude))
min.lon = floor(min(obs.data$longitude))
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Crop the bioclim data to geographic extent of saguaro
bioclim.data <- crop(x = bioclim.data, y = geographic.extent)

# Create pseudo-absence, or background, points
# Use the bioclim data files for sampling resolution
bil.files <- list.files(path = "data/wc2-5", 
                          pattern = "*.bil$", 
                          full.names = TRUE)

# We only need one file, so use the first one in the list of .bil files
mask <- raster(bil.files[1])

# Randomly sample points
background <- randomPoints(mask = mask,     # Provides resolution of sampling points
                   n = nrow(obs.data),      # Number of random points
                   ext = geographic.extent, # Spatially restricts sampling
                   extf = 1.25)             # Expands sampling a little bit

# Arbitrarily assign group 1 as the testing data group
testing.group <- 1

# Create vector of group memberships
group.presence <- kfold(x = obs.data, k = 5) # kfold is in dismo package

# Separate observations into training and testing groups
presence.train <- obs.data[group.presence != testing.group, ]
presence.test <- obs.data[group.presence == testing.group, ]

# Repeat the process for pseudo-absence points
group.background <- kfold(x = background, k = 5)
background.train <- background[group.background != testing.group, ]
background.test <- background[group.background == testing.group, ]

# Build a model using training data
bc.model <- bioclim(x = bioclim.data, p = presence.train)

# Predict presence from model
predict.presence <- dismo::predict(object = bc.model, 
                                   x = bioclim.data, 
                                   ext = geographic.extent)

# Use testing data for model evaluation
bc.eval <- evaluate(p = presence.test,   # The presence testing data
                    a = background.test, # The absence testing data
                    model = bc.model,    # The model we are evaluating
                    x = bioclim.data)    # Climatic variables for use by model

# Determine minimum threshold for "presence"
bc.threshold <- threshold(x = bc.eval, stat = "spec_sens")

# Load map data for plotting
data(wrld_simpl)

# Plot base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

# Plot areas where probability of occurrence is greater than the threshold
plot(predict.presence > bc.threshold, 
     add = TRUE, 
     legend = FALSE, 
     col = c(NA, "olivedrab"))

# add observations
points(x = obs.data$longitude, 
       y = obs.data$latitude, 
       col = "black",
       pch = "+", 
       cex = 0.6)

# Redraw country borders
plot(wrld_simpl, add = TRUE, border = "grey5")
box()

# Trying logistic regression
presvals <- readRDS("presencevalues.Rds")
#prediction
bio1 = c(40, 150, 200)
bio5 = c(60, 115, 290)
bio12 = c(600, 1600, 1700)
pd = data.frame(cbind(bio1, bio5, bio12))
predict(bc, pd)
response(bc)
# the code below needs adjustment
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
names(predictors)
glm.map1<- predict(predictors, bc)
#plot
plot(glm.map1)

presvals <- extract(predictors, speciesdata)
set.seed(0)
backgr <- randomPoints(predictors, 500)
nr <- nrow(speciesdata)
s <- sample(nr, 0.25 * nr)
train <- rbind(presence.train, background.train)
envtrain <- extract(predict, train)
gm1 <- glm(pa ~ bio1 + bio5 + bio6 + bio12, family = binomial(link = "logit"), data=envtrain)
summary(gm1)
coef(gm1)
gm1 <- glm(pa ~ bio1+ bio5 + bio12 + bio16 + bio17,
            family = gaussian(link = "identity"), data=envtrain)
evaluate(testpres, testbackg, gm1)     
ge2 <- evaluate(testpres, testbackg, gm2)
ge2
pg <- predict(predictors, gm2, ext=ext)               
par(mfrow=c(1,2))
plot(pg, main='GLM/gaussian, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')          
tr <- threshold(ge2, 'spec_sens')
plot(pg > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')                  
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)

