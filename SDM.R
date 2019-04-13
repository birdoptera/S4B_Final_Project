#################
#CALIBRATING SDMs
#################

setwd("/home/aubcls67/S4B_Final_Project")

#we need a few new packages
install.packages("tree", dep=TRUE) #regression trees
install.packages("rJava", dep=TRUE) #run "sudo R CMD javareconf" in linux
install.packages("partykit", dep=TRUE) #conditional inference trees (requires rJava)
install.packages("randomForest", dep=TRUE) #random forest
install.packages("kernlab", dep=TRUE) #support vector machines
install.packages("nnet", dep=TRUE) #artificial neural networks
install.packages("mgcv", dep=TRUE) #GAM
install.packages("plotmo", dep=TRUE) #response curves
install.packages("earth", dep=TRUE) #MARS
install.packages("maxnet", dep=TRUE) #MAXENT
install.packages("vegan", dep=TRUE) #NMDS
install.packages("lm.beta", dep=TRUE) #standardized coefficients for GLM
install.packages("glmulti", dep=TRUE) #variable selection for glm
install.packages("gbm", dep=TRUE) #gradient boosting modelling (boosted regression trees)
install.packages("devtools", dep=TRUE) #to access functions stored in Github

#loading libraries
library(tree)
library(partykit)
library(randomForest)
library(kernlab)
library(nnet)
library(mgcv)
library(plotmo)
library(earth)
library(maxnet)
library(dismo)
library(raster)
library(vegan)
library(lm.beta)
library(glmulti)
library(gbm)
library(devtools)


#load some functions
source("SDM_functions.R")

#load presences and predictors
load("./S4B_Final_Project/presencevalues.Rds")


##############################################
#PRELIMINARY EXPLORATION OF PRESENCE DATA
################################################

#saving plot
#pdf("./results/exploratory_plots.pdf", width=15, height=6, pointsize=30)

#iterating through predictors
for (predictor in names(predictors)){

par(mfrow=c(1,3))

#límites de la predictor en los gráficos
predictor.min<-min(presence.background[, predictor])
predictor.max<-max(presence.background[, predictor])

#density plot
#------------
#presences vs. background
d0<-density(presence.background[presence.background$presence==0, predictor], from=predictor.min, to=predictor.max)
d1<-density(presence.background[presence.background$presence==1, predictor], from=predictor.min, to=predictor.max)
plot(d0, col="red", xlim=c(predictor.min,predictor.max), lwd=3, main=paste(predictor, " vs. background", sep=""), xlab=predictor, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)

#presences vs absences
d0<-density(presence.absence[presence.absence$presence==0, predictor], from=predictor.min, to=predictor.max)
d1<-density(presence.absence[presence.absence$presence==1, predictor], from=predictor.min, to=predictor.max)
plot(d0, col="red", xlim=c(predictor.min,predictor.max), lwd=3, main=paste(predictor, " vs. absences", sep=""), xlab=predictor, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)

#presences vs pseudoabsences
d0<-density(presence.pseudoabsence[presence.pseudoabsence$presence==0, predictor], from=predictor.min, to=predictor.max)
d1<-density(presence.pseudoabsence[presence.pseudoabsence$presence==1, predictor], from=predictor.min, to=predictor.max)
plot(d0, col="red", xlim=c(predictor.min,predictor.max), lwd=3, main=paste(predictor, " vs. pseudoabsences", sep=""), xlab=predictor, ylim=c(0, max(d0$y, d1$y)))
lines(d1, col="forestgreen", lwd=3)
}

#closing pdf
#dev.off()


####################################
#2D PLOTS
####################################
#creating a temporary table
#NOTE: WE USE BACKGROUND DATA BECAUSE IT REPRESENTS THE WHOLE RANGE OF AVAILABLE CONDITIONS
table.visualization<-presence.background
#reordering the table to have presences at the end (so they will be plotted on top)
table.visualization<-table.visualization[order(table.visualization$presence, decreasing=FALSE), ]
#colors
table.visualization$color<-"gray60"
table.visualization[which(table.visualization$presence==1), "color"]<-"red4" 
#different size for points
table.visualization$cex<-0.6
table.visualization[which(table.visualization$presence==1), "cex"]<-0.8

#let's see the data in 2D with scatterplots
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(table.visualization$bio5 ~ table.visualization$bio6, col=table.visualization$color, cex=table.visualization$cex, ylab="bio5", xlab="bio6", pch=20)
plot(table.visualization$bio5 ~ table.visualization$bio14, col=table.visualization$color, cex=table.visualization$cex, ylab="bio5", xlab="bio14", pch=20)
plot(table.visualization$landcover_veg_tree ~ table.visualization$ndvi_range, col=table.visualization$color, cex=table.visualization$cex, ylab="landcover_veg_tree", xlab="ndvi_range", pch=20)
plot(table.visualization$human_footprint ~ table.visualization$topo_diversity, col=table.visualization$color, cex=table.visualization$cex, ylab="human_footprint", xlab="topo_diversity", pch=20)


##########################################
#NON METRIC MULTIDIMENSIONAL SCALING
##########################################
#WE USE PSEUDOABSENCES BECAUSE NMDS TAKE FOREVER WHEN DONE WITH THOUSANDS OF POINTS
table.visualization<-presence.pseudoabsence
#reordering the table to have presences at the end (so they will be plotted on top)
table.visualization<-table.visualization[order(table.visualization$presence, decreasing=FALSE), ]
#colors
table.visualization$color<-"gray60"
table.visualization[which(table.visualization$presence==1), "color"]<-"red4" 
#different size for points
table.visualization$cex<-1
table.visualization[which(table.visualization$presence==1), "cex"]<-0.8

#NMDS
nmds<-metaMDS(table.visualization[,names(predictors)], distance="euclidean")
#rules to interpret stress:
#stress < 0.05: excellent representation
#stress < 0.1: good representation
#stress < 0.2: acceptable representation,
#stress > 0.3: unsatisfactory representation

#PLOT
# pdf("./results/nmds.pdf", width=10, height=7, pointsize=15)
x11()
plot(nmds, type="n", main="NMDS")
points(nmds$points, col=table.visualization$color, cex=table.visualization$cex, pch=19)
ordisurf(nmds, table.visualization$bio6, add=T, col="blue", lwd=2, cex=3)
ordisurf(nmds, table.visualization$bio5, add=T, col="forestgreen", lwd=2, cex=3)
legend("bottomright", c("pseudoabsence","presence"), pch=19, col=c("gray60","red4"))
legend("topright", title="Lines", c("Bio6", "Bio5"), lty=1, lwd=2, col=c("blue", "forestgreen"))
# dev.off()

#REMOVING OBJECTS WE DON'T NEED ANY LONGER
rm(d0, d1, nmds,table.visualization, predictor.max, predictor, predictor.min)
gc()



#################################################################
#################################################################
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

#TWO PREDICTORS
#--------------
glm.model<-glm(presence ~ bio5 + ndvi_range, family=binomial(link=logit), data=presence.pseudoabsence)
#family=binomial(link=logit) -> we are doing LOGISTIC REGRESSION HERE

#summary
summary(glm.model)
#NOTE: estimates cannot be interpreted as effect size if predictors are not in the same scale

#we can compute standardized coefficients with lm.beta
lm.beta(glm.model)

#D-squared (explained deviance)
Dsquared(glm.model)

#response curve
plotmo(glm.model, level=0.68)

#prediction
glm.map<-predict(predictors, glm.model, type="response")

#plot
x11()
plot(glm.map)
points(presence[, c("x","y")], cex=0.1)


#AHORA CON DOS predictors QUE INTERACCIONAN
#-----------------------------------------
glm.model<-glm(presence ~ bio5 * ndvi_range, family=binomial(link=logit), data=presence.pseudoabsence)

summary(glm.model)
lm.beta(glm.model)
Dsquared(glm.model)
plotmo(glm.model, level=0.68, all2=TRUE)
glm.map<-predict(predictors, glm.model, type="response")
x11()
plot(glm.map)
points(presence[, c("x","y")], cex=0.1)


#TWO INTERACTING POLYNOMIAL PREDICTORS
#----------------------------------------------------
glm.model<-glm(presence ~ poly(bio5, 2) * poly(ndvi_range, 2), family=binomial(link=logit), data=presence.pseudoabsence)

summary(glm.model)
lm.beta(glm.model)
Dsquared(glm.model)
plotmo(glm.model, level=0.68, all2=TRUE)
glm.map<-predict(predictors, glm.model, type="response")
x11
plot(glm.map)
points(presence[, c("x","y")], cex=0.1)


#TWO INTERACTING POLYNOMIAL PREDICTORS (FOURTH DEGREE POLYNOMIAL)
#----------------------------------------------------
glm.model<-glm(presence ~ poly(bio5, 4) * poly(ndvi_range, 4), family=binomial(link=logit), data=presence.pseudoabsence)

summary(glm.model)
lm.beta(glm.model)
Dsquared(glm.model)
plotmo(glm.model, level=0.68, all2=TRUE)
glm.map<-predict(predictors, glm.model, type="response")
plot(glm.map)
points(presence[, c("x","y")], cex=0.1)



#WORKING WITH ALL PREDICTORS, SECOND DEGREE POLYNOMIAL, NO INTERACTIONS
#----------------------------------------
#formula
formula.regression.poly<-as.formula(paste("presence ~ poly(", paste(names(predictors), collapse=", 2) + poly("), ", 2)", collapse=""))
formula.regression.poly

#NOTE: we'll calibrate the models with every dataset (background, weighted background, absence and pseudoabsence)

#BACKGROUND
glm.model.background<-glm(formula.regression.poly, family=binomial(link=logit), data=presence.background)

#WEIGHTED BACKGROUND (check "weights")
glm.model.backgroundw<-glm(formula.regression.poly, family=quasibinomial(link=logit), data=presence.background, weights=WeightPresenceBackground(presence.background[ , "presence"]))

#ABSENCE
glm.model.absence<-glm(formula.regression.poly, family=binomial(link=logit), data=presence.absence)

#PSEUDOabsence
glm.model.pseudoabsence<-glm(formula.regression.poly, family=binomial(link=logit), data=presence.pseudoabsence)


#response curves
x11()
plotmo(glm.model.background, level=0.68)
x11()
plotmo(glm.model.backgroundw, level=0.68)
x11()
plotmo(glm.model.absence, level=0.68)
x11()
plotmo(glm.model.pseudoabsence, level=0.68)

#summary
summary(glm.model.background)
summary(glm.model.backgroundw)
summary(glm.model.absence)
summary(glm.model.pseudoabsence)

#lm.beta
lm.beta(glm.model.background)
lm.beta(glm.model.backgroundw)
lm.beta(glm.model.absence)
lm.beta(glm.model.pseudoabsence)

#explained deviance
Dsquared(glm.model.background)
Dsquared(glm.model.backgroundw)
Dsquared(glm.model.absence)
Dsquared(glm.model.pseudoabsence)

#map
glm.model.background.map<-predict(predictors, glm.model.background, type="response")
glm.model.backgroundw.map<-predict(predictors, glm.model.backgroundw, type="response")
glm.model.absence.map<-predict(predictors, glm.model.absence, type="response")
glm.model.pseudoabsence.map<-predict(predictors, glm.model.pseudoabsence, type="response")

#maps
x11()
par(mfrow=c(2,2))
plot(glm.model.background.map, main="background")
plot(glm.model.backgroundw.map, main="background ponderado")
plot(glm.model.absence.map, main="absence")
plot(glm.model.pseudoabsence.map, main="pseudo-absence")


#variable selection with glmulti
#-------------------------------
#first we fit a model
glm.model<-glm(formula.regression.poly, family=binomial(link="logit"), data=presence.pseudoabsence)

#model selection (many options here, read the help file!)
glm.model.selection<-glmulti(glm.model, level=1, method="g")

#result
summary(glm.model.selection)

#best model
formula.best.model=as.formula(summary(glm.model.selection)$bestmodel)
formula.best.model
#and from here you can just fit the model again with the new formula.


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
