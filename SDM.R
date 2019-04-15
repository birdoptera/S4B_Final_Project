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

#CALIBRATING MODELS
#################################################################
#################################################################

#################################################################
#BIOCLIM
#################################################################
#fitting model (ONLY PRESENCE)
bioclim.model=bioclim(predictors, presence[, c("x", "y")])

#plot model (use names(predictors) as reference for the numbers in "a" and "b")
x11()
par(mfrow=c(2,2), mar=c(4,4,3,3))
plot(bioclim.model, a=1, b=2)
plot(bioclim.model, a=1, b=3)
plot(bioclim.model, a=1, b=4)
plot(bioclim.model, a=1, b=5)

#predict map and plot
bioclim.model.map=predict(predictors, bioclim.model)
x11()
plot(bioclim.model.map)


#################################################################
#DOMAIN
#################################################################
#fitting model (ONLY PRESENCE)
domain.model=domain(predictors, presence[, c("x", "y")])

#predict map and plot
domain.model.map=predict(x=predictors, domain.model) #FREAKING SLOW
x11()
plot(domain.model.map)

 

#################################################################
#GENERALIZED LINEAR MODELS (GLM)
#################################################################

m1<-glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
class(m1)
#summary
summary(m1)
m2 = glm(pb ~., data=sdmdata)
m2

#presencvalue data

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
pd
predict(m1, pd)
predict(bc, pd)
response(bc)
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
names(predictors)
glm.map1<- predict(predictors, m1)
#plot
plot(glm.map1)

glm.map2<-predict(predictors, bc, type="response")
x11()
plot(glm.map2)
points(presence[, c("x","y")] cex=0.1)


#################################################################
#GENERALIZED ADDITIVE MODELS
#################################################################
#formula
formula.gam<-as.formula(paste("presence ~ s(", paste(names(predictors), collapse=", k=4) + s("), ", k=4)", collapse=""))
formula.gam

#background
gam.model.background<-gam(formula.gam, family=binomial(link=logit), data=presence.background)

#weighted background (sloooooow)
gam.model.backgroundw<-gam(formula.gam, family=quasibinomial(link=logit), data=presence.background, weights=WeightPresenceBackground(presence.background[ , "presence"]))

#absence
gam.model.absence<-gam(formula.gam, family=binomial(link=logit), data=presence.absence)

#pseudoabsence
gam.model.pseudoabsence<-gam(formula.gam, family=binomial(link=logit), data=presence.pseudoabsence)

#response curves
x11()
plotmo(gam.model.background, level=0.68, type="response")
x11()
plotmo(gam.model.backgroundw, level=0.68, type="response")
x11()
plotmo(gam.model.absence, level=0.68, type="response")
x11()
plotmo(gam.model.pseudoabsence, level=0.68, type="response")

#summary
summary(gam.model.background)
summary(gam.model.backgroundw)
summary(gam.model.absence)
summary(gam.model.pseudoabsence)

#explained deviance
Dsquared(gam.model.background)
Dsquared(gam.model.backgroundw)
Dsquared(gam.model.absence)
Dsquared(gam.model.pseudoabsence)

#maps
gam.model.background.map<-predict(predictors, gam.model.background, type="response")
gam.model.backgroundw.map<-predict(predictors, gam.model.backgroundw, type="response")
gam.model.absence.map<-predict(predictors, gam.model.absence, type="response")
gam.model.pseudoabsence.map<-predict(predictors, gam.model.pseudoabsence, type="response")

#maps
x11()
par(mfrow=c(2,2))
plot(gam.model.background.map, main="background")
plot(gam.model.backgroundw.map, main="background ponderado")
plot(gam.model.absence.map, main="absence")
plot(gam.model.pseudoabsence.map, main="pseudo-absence")

#################################################################
#MULTIVARIATE ADAPTIVE REGRESSION SPLINES (MARS)
#################################################################
#we cannot use a formula here..
#-------------------------
formula.mars<-as.formula(paste("presence ~ ", paste(names(predictors), collapse="+")))
formula.mars

#background
mars.model.background<-earth(formula.mars, glm = list(family=binomial(link="logit")), data=presence.background)

#weighted background SLOOOOOOOOW
# mars.model.backgroundw<-earth(formula.mars, glm = list(family=quasibinomial(link="logit")), data=presence.background, weights=WeightPresenceBackground(presence.background[ , "presence"]))

#absence
mars.model.absence<-earth(formula.mars, glm = list(family=binomial(link="logit")), data=presence.absence)

#pseudoabsence
mars.model.pseudoabsence<-earth(formula.mars, glm = list(family=binomial(link="logit")), data=presence.pseudoabsence)

#response curves
x11()
plotmo(mars.model.background, type="response")
# x11()
# plotmo(mars.model.backgroundw, type="response")
x11()
plotmo(mars.model.absence, type="response")
x11()
plotmo(mars.model.pseudoabsence, letype="response")

#summary
summary(mars.model.background)
# summary(mars.model.backgroundw)
summary(mars.model.absence)
summary(mars.model.pseudoabsence)

#variable importance
mars.model.background.evimp<-evimp(mars.model.background)
# summary(mars.model.backgroundw)
mars.model.absence.evimp<-evimp(mars.model.absence)
mars.model.pseudoabsence.evimp<-evimp(mars.model.pseudoabsence)


#maps
mars.model.background.map<-predict(predictors, mars.model.background, type="response")
# mars.model.backgroundw.map<-predict(predictors, mars.model.backgroundw, type="response")
mars.model.absence.map<-predict(predictors, mars.model.absence, type="response")
mars.model.pseudoabsence.map<-predict(predictors, mars.model.pseudoabsence, type="response")

#maps
x11()
par(mfrow=c(2,2))
plot(mars.model.background.map, main="background")
# plot(mars.model.backgroundw.map, main="background ponderado")
plot(mars.model.absence.map, main="absence")
plot(mars.model.pseudoabsence.map, main="pseudo-absence")

q()
