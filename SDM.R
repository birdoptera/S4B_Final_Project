setwd("/home/aubcls67/S4B_Final_Project")

# install needed packages
install.packages(c('raster', 'rgdal', 'dismo', 'rJava')) #
install.packages("maxnet", dep=TRUE) #MAXENT

#loading libraries

library(maxnet)
library(plotmo)
library(dismo)
library(raster)
library(maptools)
library(sp)
library(gbm)

#load presences and predictors
load("./S4B_Final_Project/presencevalues.Rds")
#load sdm data
load("./S4B_Final_Project/sdm.Rds")

sdmdata <- readRDS("sdm.Rds")
presvals <- readRDS("presencevalues.Rds")

#################################################################
#GENERALIZED LINEAR MODELS (GLM)
#################################################################

m1<-glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
class(m1)
#summary
summary(m1)
m2 = glm(pb ~., data=sdmdata)
m2

#presencevalues.Rds data

bc <- bioclim(presvals[,c('bio1', 'bio5', 'bio12')])
class(bc)
bc
pairs(bc)

#response curve
plotmo(bc, level=0.68)

#prediction
bio1 = c(40, 150, 200)
bio5 = c(60, 115, 290)
bio12 = c(600, 1600, 1700)
pd = data.frame(cbind(bio1, bio5, bio12))
predict(m1, pd)
predict(bc, pd)
response(bc)
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
names(predictors)
glm.map1<- predict(predictors, m1)
#plot
plot(glm.map1)
x11()
plot(glm.map2)
points(presence[, c("x","y")] cex=0.1)
q()
