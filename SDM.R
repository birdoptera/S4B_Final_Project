#install needed packages
install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))
install.packages("maxnet", dep=TRUE) #MAXENT 

library(maxnet)
library(dismo)
library(raster)
library(maptools)
library(sp)
library(rgdal)
library(rJava)

#load presences and predictors

sdmdata <- readRDS("sdm.Rds")
presvals <- readRDS("presencevalues.Rds")
USspeciesdata <- read.csv(file="USspeciesdata.csv", header=TRUE, sep=",")

#################################################################
#GENERALIZED LINEAR MODELS (GLM)
#################################################################

#presencevalues.Rds data

bc <- bioclim(presvals[,c('bio1', 'bio5', 'bio12')])
class(bc)
bc
pairs(bc)


#prediction
bio1 = c(40, 150, 200)
bio5 = c(60, 115, 290)
bio12 = c(600, 1600, 1700)
pd = data.frame(cbind(bio1, bio5, bio12))
predict(bc, pd)
response(bc)
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern='grd$', full.names=TRUE ))
names(predictors)
glm.map1<- predict(predictors, bc)
#plot
plot(glm.map1)

p <- rnorm(50, mean=0.7, sd=0.3)
a <- rnorm(50, mean=0.4, sd=0.4)
par(mfrow=c(1, 2))
plot(sort(p), col='red', pch=21)
points(sort(a), col='blue', pch=24)
legend(1, 0.95 * max(a,p), c('presence', 'absence'),
          pch=c(21,24), col=c('red', 'blue'))
comb <- c(p,a)
group <- c(rep('presence', length(p)), rep('absence', length(a)))
boxplot(comb~group, col=c('blue', 'red'))

group = c(rep(1, length(p)), rep(0, length(a))) 
cor.test(comb, group)$estimate
mv <- wilcox.test(p,a)
auc <- as.numeric(mv$statistic) / (length(p) * length(a))
auc


e <- evaluate(p=p, a=a)
class(e) 
e
par(mfrow=c(1, 2))
density(e)
boxplot(e, col=c('blue', 'red'))

samp <- sample(nrow(sdmdata), round(0.75 * nrow(sdmdata)))
traindata <- sdmdata[samp,]
traindata <- traindata[traindata[,1] == 1, 2:9]
testdata <- sdmdata[-samp,]
bc <- bioclim(traindata)
##e <- evaluate(testdata[testdata==1,], testdata[testdata==0,], bc)
##e
##plot(e, 'ROC')

pres <- sdmdata[sdmdata[,1] == 1, 2:9]
back <- sdmdata[sdmdata[,1] == 0, 2:9]

k <- 5
group <- kfold(pres, k)
group[1:10]
unique(group)

e <- list()
for (i in 1:k) {
    train <- pres[group != i,]
    test <- pres[group == i,]
    bc <- bioclim(train)
    e[[i]] <- evaluate(p=test, a=back, bc)
}  

auc <- sapply(e, function(x){x@auc}) 
auc
mean(auc)
sapply( e, function(x){ threshold(x)['spec_sens'] } )
#file <- file.path(system.file(package="dismo"), "USspeciesdata.csv")
USspeciesdata <- read.table(file,  header=TRUE,  sep=',')
USspeciesdata <- USspeciesdata[,-1]
presvals <- extract(predictors, USspeciesdata)
set.seed(0)
backgr <- randomPoints(predictors, 500)
nr <- nrow(USspeciesdata)
s <- sample(nr, 0.25 * nr)
pres_train <- USspeciesdata[-s, ]
pres_test <- USspeciesdata[s, ]
nr <- nrow(backgr)
set.seed(9)
s <- sample(nr, 0.25 * nr)
back_train <- backgr[-s, ]
back_test <- backgr[s, ]

sb <- ssb(pres_test, back_test, pres_train)
sb[,1] / sb[,2]
i <- pwdSample(pres_test, back_test, pres_train, n=1, tr=0.1)
pres_test_pwd <- pres_test[!is.na(i[,1]), ]
back_test_pwd <- back_test[na.omit(as.vector(i)), ]
sb2 <- ssb(pres_test_pwd, back_test_pwd, pres_train)
sb2[1]/ sb2[2]

bc <- bioclim(predictors, pres_train)
evaluate(bc, p=pres_test, a=back_test, x=predictors)
evaluate(bc, p=pres_test_pwd, a=back_test_pwd, x=predictors)

#
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] <- as.factor(sdmdata[,'biome'])

pred_nf <- dropLayer(predictors, 'biome')

set.seed(0)
group <- kfold(USspeciesdata, 5)
pres_train <- USspeciesdata[group != 1, ]
pres_test <- USspeciesdata[group == 1, ]

ext <- extent(-90, -32, -33, 23)

set.seed(10)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

r <- raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

bc <- bioclim(pred_nf, pres_train)
plot(bc, a=1, b=2, p=0.85)

e <- evaluate(pres_test, backg_test, bc, pred_nf)
e

tr <- threshold(e, 'spec_sens')
tr


pb <- predict(pred_nf, bc, ext=ext, progress='')
pb
par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(wrld_smpl, add=TRUE, border='dark grey')
plot(pb > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

dm <- domain(pred_nf, pres_train)
e <- evaluate(pres_test, backg_test, dm, pred_nf)
e
pd = predict(pred_nf, dm, ext=ext, progress='')
par(mfrow=c(1,2))
plot(pd, main='Domain, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(pd > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

mm <- mahal(pred_nf, pres_train)
e <- evaluate(pres_test, backg_test, mm, pred_nf)
e
pm = predict(pred_nf, mm, ext=ext, progress='')
par(mfrow=c(1,2))
pm[pm < -10] <- -10
plot(pm, main='Mahalanobis distance')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(pm > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

train <- rbind(pres_train, backg_train)
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
envtrain <- extract(predictors, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )
envtrain[,'biome'] = factor(envtrain[,'biome'], levels=1:14)
head(envtrain)
testpres <- data.frame( extract(predictors, pres_test) )
testbackg <- data.frame( extract(predictors, backg_test) )
testpres[ ,'biome'] = factor(testpres[ ,'biome'], levels=1:14)
testbackg[ ,'biome'] = factor(testbackg[ ,'biome'], levels=1:14)

# logistic regression:
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

q()

